# Kansenkaart data preparation pipeline
#
# age35_current_address: the age35 sample with its regions measured at the
# address at age 35 (instead of the childhood address), plus the Amsterdam
# areas and the categorical outcomes of the intragenerational analysis.
#   - Reads output/age35_cohort.rds (run the age35 sample first).
#   - Writes output/age35_current_address_cohort.rds, the input of
#     Opportunity-Analysis-Intragenerational.
#
# Moved here from Opportunity-Analysis-Intragenerational
# (Scripts/Preprocessing/age35/A_2_0_I1_01b_preprocess_age35_sample.R) on
# 30-10-2026: assigning people to areas belongs in the preprocessing. Same
# result as the old script (checked on synthetic data). Changes:
#   - the file is written once, after the categorical outcomes (c35_*_ranked);
#   - joins go through checked_join(), which stops if an address or area table
#     has more than one row per key (the old inner_join would duplicate people);
#   - the list of Amsterdam Metropolitan Area municipalities is in the config.
#
# (c) Netherlands Opportunity Lab 2026


#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)


#### LOAD THE SAMPLE ####
cohort_dat <- read_rds(file.path(loc$output_folder, paste0(cfg$base_cohort, "_cohort.rds")))


#### 1. CURRENT ADDRESS ####
address_tab <- read_sav(file.path(loc$data_folder, loc$gbaao_data)) %>%
  as_factor(only_labelled = TRUE, levels = "values") %>%
  mutate(
    GBADATUMAANVANGADRESHOUDING = ymd(GBADATUMAANVANGADRESHOUDING),
    GBADATUMEINDEADRESHOUDING = ymd(GBADATUMEINDEADRESHOUDING)
  )

# the date at which the person has the given age
age_tab <- cohort_dat %>%
  select(RINPERSOONS, RINPERSOON, birthdate) %>%
  mutate(home_address_date = birthdate %m+% years(cfg$current_home_age)) %>%
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


#### 2. REGIONS OF THE CURRENT ADDRESS ####
# postal codes valid on the target date, and the postcode-3 level
vslpc_tab <- read_sav(file.path(loc$data_folder, loc$postcode_data)) %>%
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
vslgwb_tab <- read_sav(file.path(loc$data_folder, loc$vslgwb_data)) %>%
  mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values")) %>%
  select("type_current_home"         = "SOORTOBJECTNUMMER",
         "current_home"              = "RINOBJECTNUMMER",
         "municipality_code_current" = paste0("gem", year(dmy(cfg$gwb_target_date))),
         "neighborhood_code_current" = paste0("wc", year(dmy(cfg$gwb_target_date)))) %>%
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

# replace the childhood regions with the current ones
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

cohort_dat <- checked_join(cohort_dat, ams_areas, by = "neighborhood_code") %>%
  mutate(d_amsterdam_metropolitan_area = ifelse(municipality_code %in% cfg$amsterdam_metropolitan_area, 1, 0)) %>%
  select(-"neighborhood_name")
rm(ams_areas)


#### 4. CATEGORICAL OUTCOMES FOR THE INTRAGENERATIONAL ANALYSIS ####
cohort_dat <- cohort_dat %>%
  mutate(c35_ses_ranked = ifelse(c35_employed == 1, "Employed",
                                 ifelse(c35_disability == 1, "Disability Insurance",
                                        ifelse(c35_social_assistance == 1, "Social Assistance Benefit",
                                               "None"))),
         c35_mhc_ranked = ifelse(c35_specialist_mhc == 1, "Specialist",
                                 ifelse(c35_basic_mhc == 1, "Basic",
                                        "None")),
         d_has_migration_background = ifelse(migration_background == "No Migration Background", 0, 1),
         c35_contract_ranked = case_when(
           is.na(c35_permanent_contract) ~ "No Salaried Employment",
           c35_permanent_contract == 1  ~ "Permanent",
           c35_permanent_contract == 0 ~ "Fixed-term",
           TRUE ~ ""
         ),
         c35_education_ranked =  ifelse(c35_wo_attained == 1, "WO",
                                        ifelse(c35_hbo_attained == 1 , "HBO", "None")),
         across(c("c35_homeowner", "c35_contract_ranked", "c35_education_ranked",
                  "c35_ses_ranked", "c35_mhc_ranked"), as.factor))


#### WRITE OUTPUT ####
write_rds(cohort_dat, file.path(loc$output_folder, paste0(cfg$cohort_name, "_cohort.rds")))
