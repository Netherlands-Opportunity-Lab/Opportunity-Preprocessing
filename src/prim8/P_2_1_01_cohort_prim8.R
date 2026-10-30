# Kansenkaart data preparation pipeline
#
# 1. Cohort creation. 
#   - Selecting the cohort based on filtering criteria. 
#   - Adding parent information to the cohort.
#   - Adding postal code / region information to the cohort.
#   - Writing `temp/<cohort>/01_cohort.rds`.
#
# SQLite version: reads the converted .db files via read_cbs() (src/lib/sql_helpers.R)
# instead of read_sav(). Only the rows that are needed leave the database; all
# processing after the read is unchanged. Changes are marked with [SQL].
#
# (c) ODISSEI Social Data Science team 2026


#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)
library(dplyr)


#### SELECT COHORT FROM GBA ####
gba_path <- file.path(loc$data_folder, loc$gba_data)

# [SQL] the same cleaning as before, now in a function because GBA is read twice:
#       once for the (class) cohort, once for the parents (see AGE PARENTS AT BIRTH)
prep_gba <- function(dat) {
  dat %>%
    mutate(birthdate = dmy(paste(GBAGEBOORTEDAG, GBAGEBOORTEMAAND, GBAGEBOORTEJAAR, sep = "-"))) %>%
    select(-GBAGEBOORTEJAAR, -GBAGEBOORTEMAAND, -GBAGEBOORTEDAG) %>%
    mutate(RINPERSOONS = haven::as_factor(RINPERSOONS, levels = "values"),
           GBAHERKOMSTGROEPERING = haven::as_factor(GBAHERKOMSTGROEPERING, levels = "labels"),
           GBAGENERATIE = haven::as_factor(GBAGENERATIE, levels = "labels"),
           GBAGESLACHT = haven::as_factor(GBAGESLACHT, levels = "labels")) %>%
    mutate(GBAGESLACHT = as.factor(as.character(GBAGESLACHT))) %>%
    rename(sex = GBAGESLACHT) %>%
    mutate(sex = recode(sex, 'Mannen' = 'Men', 'Vrouwen' = 'Women'))
}

gba_cols <- c("RINPERSOONS", "RINPERSOON", "GBAGESLACHT",
              "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", "GBAGENERATIE", "GBAHERKOMSTGROEPERING",
              #add parents birth country for adding foreign born parents outcome later
              "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER")

# [SQL] only read people born in the years of the (class) cohort window; the exact
#       date filter below is unchanged. The year range is a superset of the window.
cohort_years <- seq(year(dmy(cfg$child_birth_date_min) - years(2)),
                    year(dmy(cfg$child_birth_date_max) + years(2)))
gba_dat <-
  read_cbs(gba_path, cols = gba_cols,
          filter_in = list(GBAGEBOORTEJAAR = cohort_years)) %>%
  prep_gba()


# to calculate classroom composition later, we construct a larger classroom sample with potential classmates of children in our cohort
cohort_dat <- 
  gba_dat %>% 
  filter(birthdate %within% interval((dmy(cfg$child_birth_date_min) - years(2)), ((dmy(cfg$child_birth_date_max)) + years(2))))


sample_size <- tibble(
  n_0_birth_cohort = nrow(cohort_dat %>% 
                            filter(birthdate%within%interval(dmy(cfg$child_birth_date_min), dmy(cfg$child_birth_date_max))))
)


#### PARENT LINK ####
# add parent id to cohort
kindouder_path <- file.path(loc$data_folder, loc$kind_data)
cohort_dat <- inner_join(
  x = cohort_dat, 
  # [SQL] only the children in the cohort (the inner join drops all others anyway)
  y = read_cbs(kindouder_path, filter_in = list(RINPERSOON = cohort_dat$RINPERSOON)) %>%
    select(-(XKOPPELNUMMER)) %>%
    haven::as_factor(only_labelled = TRUE, levels = "values"),
  by = c("RINPERSOONS", "RINPERSOON")
)


# record sample size
sample_size <- sample_size %>% 
  mutate(n_1_child_parent_link = 
           nrow(cohort_dat %>% filter(birthdate%within%interval(dmy(cfg$child_birth_date_min), dmy(cfg$child_birth_date_max)))))


#### AGE PARENTS AT BIRTH ####

# [SQL] gba_dat only holds the cohort birth years, so read the parents separately
#       (same filter as before: RINPERSOON in mothers or fathers)
gba_dat <-
  read_cbs(gba_path, cols = gba_cols,
          filter_in = list(RINPERSOON = c(cohort_dat$RINPERSOONMa, cohort_dat$RINPERSOONpa))) %>%
  prep_gba()

parents_age <-
  gba_dat %>%
  filter(RINPERSOON %in% c(cohort_dat$RINPERSOONMa, cohort_dat$RINPERSOONpa)) %>%
  select("RINPERSOONS", "RINPERSOON", "birthdate")


# add parents date to child birth date
cohort_dat <-
  cohort_dat %>%
  left_join(parents_age %>% rename(birthdate_ma = birthdate), 
            c("RINPERSOONSMa" = "RINPERSOONS",
              "RINPERSOONMa" = "RINPERSOON"))
cohort_dat <-
  cohort_dat %>%
  left_join(parents_age %>% rename(birthdate_pa = birthdate), 
            c("RINPERSOONSpa" = "RINPERSOONS",
              "RINPERSOONpa" = "RINPERSOON"))


# age of parents at child birth
cohort_dat <-
  cohort_dat %>%
  mutate(age_ma_at_birth = as.numeric(birthdate - birthdate_ma) %/% 365.25, 
         age_pa_at_birth = as.numeric(birthdate - birthdate_pa) %/% 365.25)

rm(parents_age, gba_dat)



#### REGION LINK ####

# find childhood home
adres_path <- file.path(loc$data_folder, loc$gbaao_data)
# [SQL] only addresses of the cohort children and their mothers: these are the only
#       two filters applied to adres_tab below (childhood home, home at birth)
adres_tab  <- read_cbs(adres_path,
                      filter_in = list(RINPERSOON = c(cohort_dat$RINPERSOON, cohort_dat$RINPERSOONMa))) %>%
  haven::as_factor(only_labelled = TRUE, levels = "values") %>%
  mutate(
    GBADATUMAANVANGADRESHOUDING = ymd(GBADATUMAANVANGADRESHOUDING),
    GBADATUMEINDEADRESHOUDING = ymd(GBADATUMEINDEADRESHOUDING)
  )


# take date at which the child is a specific age
age_tab <- cohort_dat %>%
  select(RINPERSOONS, RINPERSOON, birthdate) %>%
  mutate(home_address_date = birthdate %m+% years(cfg$childhood_home_age)) %>%
  select(-birthdate)

# take the address registration on a specific age
home_tab <-
  adres_tab %>%
  filter(RINPERSOON %in% cohort_dat$RINPERSOON & RINPERSOONS %in% cohort_dat$RINPERSOONS) %>%
  # add home_address_age to dat
  left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  # take addresses that are still open on a specific date
  filter(
    GBADATUMAANVANGADRESHOUDING <= home_address_date &
      GBADATUMEINDEADRESHOUDING >= home_address_date
  ) %>%
  group_by(RINPERSOONS, RINPERSOON) %>%
  summarise(
    childhood_home = RINOBJECTNUMMER[1],
    type_childhood_home = SOORTOBJECTNUMMER[1])



# add childhood home to data
cohort_dat <- inner_join(cohort_dat, home_tab)

# free up memory
rm(home_tab, age_tab)


# clean the postcode table
vslpc_path <- file.path(loc$data_folder, loc$postcode_data)
# [SQL] only the dwellings that occur in adres_tab (all homes we link below come from there)
vslpc_tab  <- read_cbs(vslpc_path, filter_in = list(RINOBJECTNUMMER = adres_tab$RINOBJECTNUMMER)) %>%
  mutate(
    SOORTOBJECTNUMMER = haven::as_factor(SOORTOBJECTNUMMER, levels = "values"),
    DATUMAANVPOSTCODENUMADRES = ymd(DATUMAANVPOSTCODENUMADRES),
    DATUMEINDPOSTCODENUMADRES = ymd(DATUMEINDPOSTCODENUMADRES),
    POSTCODENUM = ifelse(POSTCODENUM == "----", NA, POSTCODENUM)
  ) %>%
  filter(!is.na(POSTCODENUM)) %>%
  mutate(postcode3 = as.character(floor(as.numeric(POSTCODENUM)/10))) %>%
  # mutate(postcode3 = substr(POSTCODENUM, 1, 3)) %>%
  rename(postcode4 = POSTCODENUM)

# only consider postal codes valid on target_date and create postcode-3 level
vslpc_tab <- 
  vslpc_tab %>% 
  filter(dmy(cfg$postcode_target_date) %within% interval(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES)) %>% 
  select(SOORTOBJECTNUMMER, RINOBJECTNUMMER, postcode4, postcode3)

# add the postal codes to the cohort
cohort_dat <- inner_join(cohort_dat, vslpc_tab, 
                         by = c("type_childhood_home" = "SOORTOBJECTNUMMER", 
                                "childhood_home" = "RINOBJECTNUMMER"))

# add region/neighbourhood codes to cohort
vslgwb_path <- file.path(loc$data_folder, loc$vslgwb_data) 
# [SQL] only the columns used below, and only the dwellings that occur in adres_tab
vslgwb_tab  <- read_cbs(vslgwb_path,
                       cols = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER",
                                paste0("gem", year(dmy(cfg$gwb_target_date))),
                                paste0("wc", year(dmy(cfg$gwb_target_date)))),
                       filter_in = list(RINOBJECTNUMMER = adres_tab$RINOBJECTNUMMER)) %>%
  mutate(SOORTOBJECTNUMMER = haven::as_factor(SOORTOBJECTNUMMER, levels = "values"))

# select region/neighbourhood from the target date
vslgwb_tab <- 
  vslgwb_tab %>% 
  select("type_childhood_home" = "SOORTOBJECTNUMMER", 
         "childhood_home"      = "RINOBJECTNUMMER", 
         "municipality_code"       = paste0("gem", year(dmy(cfg$gwb_target_date))), 
         "neighborhood_code"           = paste0("wc", year(dmy(cfg$gwb_target_date))))

cohort_dat <- inner_join(cohort_dat, vslgwb_tab)


# add corop regions
corop_tab  <- read_excel(paste0(loc$corop_data, year(dmy(cfg$corop_target_date)), ".xlsx")) %>%
  select("municipality_code" = "gemeenten|Code", 
         "corop_code" = "COROP-gebieden|Code") %>%
  mutate(corop_code = gsub("CR", "", corop_code)) %>%
  unique()

cohort_dat <- left_join(cohort_dat, corop_tab, by = "municipality_code") %>%
  select(-c(type_childhood_home, childhood_home))


# mutate factor regions
cohort_dat <- 
  cohort_dat %>% 
  mutate(across(c("postcode3", "postcode4", "municipality_code", 
                  "neighborhood_code", "corop_code"), as.factor))


#### REGION LINK AT BIRTH FOR YOUTH PROTECTION ####

mother_child_tab <- 
  cohort_dat %>%
  select(RINPERSOON, birthdate, RINPERSOONMa, RINPERSOONSMa) %>%
  rename("RINPERSOON_kind" = "RINPERSOON")

# get home address of mother at time of child's birth
birth_adres <- 
  adres_tab %>% 
  # only consider the addresses of mother's that can be linked to the adres tab
  filter(RINPERSOON %in% mother_child_tab$RINPERSOONMa & RINPERSOONS %in% mother_child_tab$RINPERSOONSMa) %>%
  left_join(mother_child_tab, by = c("RINPERSOON" = "RINPERSOONMa", "RINPERSOONS" = "RINPERSOONSMa"), 
            relationship = "many-to-many") %>%
  # only consider addresses at the time of child's birth
  filter(birthdate %within% interval(GBADATUMAANVANGADRESHOUDING, GBADATUMEINDEADRESHOUDING)) %>%
  group_by(RINPERSOON_kind, RINPERSOON) %>%
  summarise(birth_home = RINOBJECTNUMMER[1],
            type_birth_home = SOORTOBJECTNUMMER[1]) 

# select postal codes
birth_adres <- 
  inner_join(birth_adres, vslpc_tab, 
             by = c("type_birth_home" = "SOORTOBJECTNUMMER", 
                    "birth_home" = "RINOBJECTNUMMER")) %>%
  rename("postcode4_birth" = "postcode4",
         "postcode3_birth" = "postcode3")


# add municipalities
vslgwb_tab <- 
  vslgwb_tab %>% 
  rename("municipality_code_birth" = "municipality_code", 
         "neighborhood_code_birth" = "neighborhood_code")

birth_adres <- inner_join(birth_adres, vslgwb_tab, 
                          by = c("type_birth_home" = "type_childhood_home", 
                                 "birth_home" = "childhood_home"))

# add corop regions
corop_tab  <- read_excel(paste0(loc$corop_data, year(dmy(cfg$corop_target_date)), ".xlsx")) %>%
  select("municipality_code_birth" = "gemeenten|Code", 
         "corop_code_birth" = "COROP-gebieden|Code") %>%
  mutate(corop_code_birth = gsub("CR", "", corop_code_birth)) %>%
  unique()

birth_adres <- 
  birth_adres %>%
  left_join(corop_tab, by = "municipality_code_birth") %>%
  select(-c(type_birth_home, birth_home))


# merge with cohort data and only keep records where address information is known
cohort_dat <- 
  left_join(cohort_dat, birth_adres, by = c("RINPERSOON" = "RINPERSOON_kind")) %>% 
  mutate(across(c("postcode4_birth", "postcode3_birth", "municipality_code_birth", 
                  "neighborhood_code_birth", "corop_code_birth"), as.factor)) %>%
  filter(!is.na(postcode3_birth), !is.na(municipality_code_birth), !is.na(neighborhood_code_birth)) %>% 
  relocate(c("postcode4_birth", "postcode3_birth", "municipality_code_birth", 
             "neighborhood_code_birth", "corop_code_birth"), .after = "corop_code") %>%
  select(-RINPERSOON.y)

rm(birth_adres, corop_tab, mother_child_tab, vslgwb_tab, vslpc_tab, adres_tab)

# mutate factor regions
cohort_dat <- 
  cohort_dat %>% 
  mutate(across(c("postcode4_birth", "postcode3_birth", "municipality_code_birth", 
                  "neighborhood_code_birth", "corop_code_birth"), as.factor))


# record sample size
sample_size <- sample_size %>% 
  mutate(n_2_region_link = 
           nrow(cohort_dat %>% filter(birthdate%within%interval(dmy(cfg$child_birth_date_min), dmy(cfg$child_birth_date_max)))))

#### WRITE OUTPUT TO TEMP ####
write_rds(cohort_dat, file.path(loc$temp_folder, "01_cohort.rds"))

write_rds(sample_size, file.path(loc$temp_folder, "01_sample_size.rds"))
