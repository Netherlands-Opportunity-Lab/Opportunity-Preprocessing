# Kansenkaart data preparation pipeline -- regions at the current address
#
# Functions shared by the samples for the intragenerational analysis
# (age35_current_address, age45_current_address, age55_current_address):
#
#   add_current_regions()       replace the regions of a cohort by the regions of
#                               the address at a given age (postcode 3/4,
#                               municipality, neighbourhood, COROP), and add the
#                               Amsterdam stadsdelen/gebieden and the Amsterdam
#                               Metropolitan Area dummy
#   add_intra_categories()      the categorical outcomes the intragenerational
#                               analysis uses as x-variables (<prefix>*_ranked)
#
# Written for the CBS RA package versions (dplyr 1.0.2). Needs checked_join()
# from P_2_1_checks.R and read_cbs() from sql_helpers.R; the runner loads both.
#
# (c) Netherlands Opportunity Lab 2026

library(tidyverse)
library(lubridate)
library(haven)
library(readxl)


# cohort_dat: one row per person, with RINPERSOONS, RINPERSOON and birthdate
# age:        the age at which the address is taken
# cfg, loc:   the sample's config (postcode/gwb/corop target dates,
#             amsterdam_metropolitan_area; data_folder, gbaao_data, postcode_data,
#             vslgwb_data, corop_data, amsterdam_areas)
# People without an address on that date, or whose address has no postcode or
# municipality on the target dates, are dropped (inner joins).
add_current_regions <- function(cohort_dat, age, cfg, loc) {

  #### 1. ADDRESS AT THE GIVEN AGE ####
  address_tab <- read_cbs(file.path(loc$data_folder, loc$gbaao_data),
                          filter_in = list(RINPERSOON = cohort_dat$RINPERSOON)) %>%
    as_factor(only_labelled = TRUE, levels = "values") %>%
    mutate(
      GBADATUMAANVANGADRESHOUDING = ymd(GBADATUMAANVANGADRESHOUDING),
      GBADATUMEINDEADRESHOUDING = ymd(GBADATUMEINDEADRESHOUDING)
    )

  # the date at which the person has the given age
  age_tab <- cohort_dat %>%
    select(RINPERSOONS, RINPERSOON, birthdate) %>%
    mutate(home_address_date = birthdate %m+% years(age)) %>%
    select(-birthdate)

  # the address registration on that date
  home_tab <-
    address_tab %>%
    filter(RINPERSOON %in% cohort_dat$RINPERSOON & RINPERSOONS %in% cohort_dat$RINPERSOONS) %>%
    left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
    # addresses that are still open on that date
    filter(
      GBADATUMAANVANGADRESHOUDING <= home_address_date &
        GBADATUMEINDEADRESHOUDING >= home_address_date
    ) %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    summarise(
      current_home = RINOBJECTNUMMER[1],
      type_current_home = SOORTOBJECTNUMMER[1]) %>%
    ungroup()

  cohort_dat <- checked_join(cohort_dat, home_tab, by = c("RINPERSOONS", "RINPERSOON"), type = "inner")
  rm(address_tab, home_tab, age_tab)


  #### 2. REGIONS OF THAT ADDRESS ####
  # postal codes valid on the target date, and the postcode-3 level
  vslpc_tab <- read_cbs(file.path(loc$data_folder, loc$postcode_data),
                        filter_in = list(RINOBJECTNUMMER = cohort_dat$current_home)) %>%
    mutate(
      SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
      DATUMAANVPOSTCODENUMADRES = ymd(DATUMAANVPOSTCODENUMADRES),
      DATUMEINDPOSTCODENUMADRES = ymd(DATUMEINDPOSTCODENUMADRES),
      POSTCODENUM = ifelse(POSTCODENUM == "----", NA, POSTCODENUM)
    ) %>%
    filter(!is.na(POSTCODENUM)) %>%
    filter(dmy(cfg$postcode_target_date) %within% interval(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES)) %>%
    mutate(postcode3_current = as.character(floor(as.numeric(zap_labels(POSTCODENUM))/10))) %>%
    select(SOORTOBJECTNUMMER, RINOBJECTNUMMER, postcode4_current = POSTCODENUM, postcode3_current)

  cohort_dat <- checked_join(cohort_dat, vslpc_tab, type = "inner",
                             by = c("type_current_home" = "SOORTOBJECTNUMMER",
                                    "current_home" = "RINOBJECTNUMMER"))

  # municipality and neighbourhood on the target date
  gwb_year <- year(dmy(cfg$gwb_target_date))
  vslgwb_tab <- read_cbs(file.path(loc$data_folder, loc$vslgwb_data),
                         filter_in = list(RINOBJECTNUMMER = cohort_dat$current_home)) %>%
    mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values")) %>%
    select("type_current_home"         = "SOORTOBJECTNUMMER",
           "current_home"              = "RINOBJECTNUMMER",
           "municipality_code_current" = paste0("gem", gwb_year),
           "neighborhood_code_current" = paste0("wc", gwb_year)) %>%
    mutate(municipality_code_current = ifelse(municipality_code_current == "----", NA, municipality_code_current)) %>%
    filter(!is.na(municipality_code_current))

  cohort_dat <- checked_join(cohort_dat, vslgwb_tab, type = "inner",
                             by = c("type_current_home", "current_home"))

  # COROP regions
  corop_tab <- read_excel(paste0(loc$corop_data, year(dmy(cfg$corop_target_date)), ".xlsx")) %>%
    select("municipality_code_current" = "gemeenten|Code",
           "corop_code_current" = "COROP-gebieden|Code") %>%
    unique()

  cohort_dat <- checked_join(cohort_dat, corop_tab, by = "municipality_code_current") %>%
    select(-c(type_current_home, current_home))
  rm(vslpc_tab, vslgwb_tab, corop_tab)

  # replace the regions (if any) with the current ones
  cohort_dat <- cohort_dat %>%
    mutate(postcode3 = postcode3_current,
           postcode4 = postcode4_current,
           municipality_code = municipality_code_current,
           neighborhood_code = neighborhood_code_current,
           corop_code = corop_code_current) %>%
    select(-c("postcode3_current", "postcode4_current", "municipality_code_current",
              "neighborhood_code_current", "corop_code_current")) %>%
    mutate(across(c("postcode3", "postcode4", "municipality_code",
                    "neighborhood_code", "corop_code"), as.factor))


  #### 3. AMSTERDAM AREAS ####
  # stadsdelen, gebieden and wijken of Amsterdam, and the Amsterdam Metropolitan Area
  ams_areas <- read_xlsx(loc$amsterdam_areas) %>%
    mutate(neighborhood_code = substr(neighborhood_code, 3, 8))

  cohort_dat %>%
    checked_join(ams_areas, by = "neighborhood_code") %>%
    mutate(d_amsterdam_metropolitan_area = ifelse(municipality_code %in% cfg$amsterdam_metropolitan_area, 1, 0)) %>%
    select(-"neighborhood_name")
}


# The categorical outcomes of the intragenerational analysis, from the outcomes
# with the given prefix (e.g. "c35_"): <prefix>ses_ranked, <prefix>mhc_ranked,
# <prefix>contract_ranked, <prefix>education_ranked, and the dummy
# d_has_migration_background. <prefix>homeowner and the ranked variables become
# factors.
add_intra_categories <- function(cohort_dat, prefix) {
  v <- function(name) cohort_dat[[paste0(prefix, name)]]

  cohort_dat[[paste0(prefix, "ses_ranked")]] <-
    ifelse(v("employed") == 1, "Employed",
           ifelse(v("disability") == 1, "Disability Insurance",
                  ifelse(v("social_assistance") == 1, "Social Assistance Benefit",
                         "None")))
  cohort_dat[[paste0(prefix, "mhc_ranked")]] <-
    ifelse(v("specialist_mhc") == 1, "Specialist",
           ifelse(v("basic_mhc") == 1, "Basic",
                  "None"))
  cohort_dat[["d_has_migration_background"]] <-
    ifelse(cohort_dat$migration_background == "No Migration Background", 0, 1)
  cohort_dat[[paste0(prefix, "contract_ranked")]] <- case_when(
    is.na(v("permanent_contract")) ~ "No Salaried Employment",
    v("permanent_contract") == 1  ~ "Permanent",
    v("permanent_contract") == 0 ~ "Fixed-term",
    TRUE ~ ""
  )
  cohort_dat[[paste0(prefix, "education_ranked")]] <-
    ifelse(v("wo_attained") == 1, "WO",
           ifelse(v("hbo_attained") == 1 , "HBO", "None"))

  cohort_dat %>%
    mutate(across(all_of(paste0(prefix, c("homeowner", "contract_ranked", "education_ranked",
                                          "ses_ranked", "mhc_ranked"))), as.factor))
}
