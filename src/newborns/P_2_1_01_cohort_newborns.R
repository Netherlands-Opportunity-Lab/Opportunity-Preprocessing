# Kansenkaart data preparation pipeline
#
# 1. Cohort creation. 
#   - Selecting the cohort based on filtering criteria. 
#   - Adding parent information to the cohort.
#   - Adding postal code / region information to the cohort.
#   - Writing `temp/<cohort>/01_cohort.rds`.
#
# SQLite version: the files that are available as converted .db (GBAPERSOON,
# GBAADRESOBJECT, KINDOUDER, GBAHUISHOUDENS, VSLPOSTCODE, VSLGWB, LEVCYCLWOONNIETWOON)
# are read with read_cbs() (src/lib/sql_helpers.R): only the rows that are
# needed leave the database. All other files are still read with read_sav(), exactly
# as before. Changes are marked with [SQL].
#
# (c) ODISSEI Social Data Science team 2025



#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)
library(dplyr)
library(stringr)


# # load data
# cohort_dat <- read.csv('L:/8151_PERINED_20240805.csv', sep = ';', header = TRUE, row.names = NULL)
# 
# 
# # select relevant columns
# cohort_dat <-
#   cohort_dat %>%
#   select(c(CBKSoortNr_Kind, Rinpersoon_Kind, jaar, ddgeb, 
#            CBKSoortNr_Moeder, Rinpersoon_Moeder,
#            amddd, sterfte, gesl, geboortegew, etnic))

# # save data
# write_rds(cohort_dat, 'H:/IGM project/kansenkaart_preprocessing/resources/8151_PERINED.rds')


#### SELECT COHORT FROM PERINED ####
# only consider births where gestational age >= 24 weeks.
# drop observations where RIN is unknown
perined_path <- file.path(loc$perined_data)
cohort_dat <- 
  read_rds(perined_path) %>%
  filter(!is.na(Rinpersoon_Kind)) %>%
  mutate(gesl = factor(gesl, levels = c("1", "2", "3", "UNK", ""),
                           labels = c("jongen", "meisje", NA, NA, NA)),
         sterfte =  factor(sterfte, levels = c("0", "1", "2", "3", "4", "5", "6"),
                           labels = c("Niet overleden", "Ante partum", "Durante partum", 
                                      "Postpartum 0-7 dagen", "Postpartum 8-28 dagen", 
                                      "Postpartum > 28 dagen", "Fase overlijden onduidelijk")),
         etnic = factor(etnic, levels = c("12", "3", "4", "5", "13", "8", "14", "11", "UNK"),
                        labels = c("Kaukasisch", "Noord-Afrikaans", "Overig Afrikaans", "Turks",
                                   "Hindoestaans", "(Overig) Aziatisch", "Latijns Amerikaans",
                                   "Overig (waaronder gemengd)", "Onbekend")),
         # ddgeb = ymd(ddgeb)
         birthdate  = str_sub(ddgeb, 1, 10),
         birthdate = ymd(birthdate), 
         Rinpersoon_Kind = as.character(Rinpersoon_Kind)
         ) %>%
  rename("RINPERSOONS" = "CBKSoortNr_Kind",
         "RINPERSOON" = "Rinpersoon_Kind",
         "sex" = "gesl",
         "etniciteit" = "etnic") %>%
  select(-ddgeb) %>%
  # select birth dates for children 
  filter(birthdate %within% interval(dmy(cfg$child_birth_date_min), 
                                 dmy(cfg$child_birth_date_max))) %>%
  mutate(sex = recode(sex, 'jongen' = 'Men', 'meisje' = 'Women'))


# remove gestational age below given days
cohort_dat <- 
  cohort_dat %>%
  filter(!(amddd < cfg$cut_off_days_min), !(amddd > cfg$cut_off_days_max)) %>%
  # remove records without necessary information, such as birth weight
  filter(!is.na(geboortegew)) 


  
#### MIGRATION BACKGROUND OF CHILDREN ####

# load gba data for migration background
gba_path <- file.path(loc$data_folder, loc$gba_data)

# [SQL] the same cleaning as before, now in a function because GBA is read twice:
#       once for the children (here), once for the parents (see PARENT LINK)
prep_gba <- function(dat) {
  dat %>%
    mutate(birthdate = dmy(paste(GBAGEBOORTEDAG, GBAGEBOORTEMAAND, GBAGEBOORTEJAAR, sep = "-"))) %>% 
    select(-GBAGEBOORTEJAAR, -GBAGEBOORTEMAAND, -GBAGEBOORTEDAG) %>%
    mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
           GBAHERKOMSTGROEPERING = as_factor(GBAHERKOMSTGROEPERING, levels = "labels"),
           GBAGENERATIE = as_factor(GBAGENERATIE, levels = "labels"),
           GBAGESLACHT = as_factor(GBAGESLACHT), levels = "labels") %>%
    mutate(across(c("GBAHERKOMSTGROEPERING", "GBAGENERATIE", "GBAGESLACHT"),
                  as.character)) 
}

gba_cols <- c("RINPERSOONS", "RINPERSOON", "GBAGESLACHT", 
              "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", "GBAGENERATIE", 
              "GBAHERKOMSTGROEPERING")

# [SQL] only the children in the cohort (here gba_dat is only joined on the children)
gba_dat <-  
  read_cbs(gba_path, cols = gba_cols,
           filter_in = list(RINPERSOON = cohort_dat$RINPERSOON)) %>% 
  prep_gba()
  

# There are some records where RINPERSOONS is not R in perined, but do show up in GBAPersoontab
# by linking by both RINPERSOONS and RINPERSOON we lose these records (391 records in total)

# add infant migration background information to cohort
# should this be an inner_join? That would delete all records not in GBApersoontab,
# while left_join would keep the unmatched records and fill in NA.

# deal with missing genders by replacing missing ones with gender from gbapersoontab.
# does not work, for now use gender variable from GBApersoontab
cohort_dat <- 
  cohort_dat %>% 
  left_join(gba_dat %>% select(RINPERSOONS, RINPERSOON, GBAHERKOMSTGROEPERING, 
                                GBAGENERATIE, GBAGESLACHT), 
             by = c("RINPERSOONS", "RINPERSOON")) %>% 
  mutate(sex = as.character(sex),
         GBAGESLACHT = as.character(GBAGESLACHT),
         GBAGESLACHT = ifelse(is.na(GBAGESLACHT), sex, GBAGESLACHT),
         GBAGESLACHT = ifelse(GBAGESLACHT == "Mannen", "Men", GBAGESLACHT), 
         GBAGESLACHT = ifelse(GBAGESLACHT == "Vrouwen", "Women", GBAGESLACHT)
         ) %>%
  select(-sex) %>%
  rename("sex" = "GBAGESLACHT") %>%
  filter(!is.na(sex)) 

# record sample size
sample_size <- tibble(n_0_birth_cohort = nrow(cohort_dat))



#### PARENT LINK ####
# add parent id to cohort
kindouder_path <- file.path(loc$data_folder, loc$kind_data)
cohort_dat <-
  cohort_dat %>%
  # [SQL] only the children in the cohort (the left join ignores all other rows)
  left_join(read_cbs(kindouder_path, filter_in = list(RINPERSOON = cohort_dat$RINPERSOON)) %>%
              select(-(XKOPPELNUMMER)) %>%
               as_factor(only_labelled = TRUE, levels = "values"),
             by = c("RINPERSOONS", "RINPERSOON"))


# replace NA RINPERSOONSMA with KINDOUDETAB RINPERSOONMA
cohort_dat <-
  cohort_dat %>%
  mutate(
    CBKSoortNr_Moeder = as.character(CBKSoortNr_Moeder),
    RINPERSOONSMa = as.character(RINPERSOONSMa)
  ) %>%
  mutate(
    CBKSoortNr_Moeder = ifelse(CBKSoortNr_Moeder == 'G', RINPERSOONSMa, CBKSoortNr_Moeder),
    Rinpersoon_Moeder = ifelse(is.na(Rinpersoon_Moeder), RINPERSOONMa, Rinpersoon_Moeder)
  ) %>%
  filter(!is.na(Rinpersoon_Moeder) & (CBKSoortNr_Moeder != 'G'))


# [SQL] gba_dat only holds the children, so read the parents separately: the mothers
#       for the migration background below and both parents for AGE PARENTS AT BIRTH
#       (cohort_dat does not change in between, so this covers every lookup below)
gba_dat <-  
  read_cbs(gba_path, cols = gba_cols,
           filter_in = list(RINPERSOON = c(cohort_dat$Rinpersoon_Moeder, 
                                           cohort_dat$RINPERSOONMa, cohort_dat$RINPERSOONpa))) %>% 
  prep_gba()


# for children for whom we cannot find a match use the migration background of the mother
cohort_dat <- 
  cohort_dat %>% 
  left_join(gba_dat %>% select(RINPERSOONS, RINPERSOON, 
                               GBAHERKOMSTGROEPERING, GBAGENERATIE), 
            by = c("CBKSoortNr_Moeder" = "RINPERSOONS",
                   "Rinpersoon_Moeder" = "RINPERSOON"), 
            suffix = c("", "_ma")) %>%
  mutate(GBAHERKOMSTGROEPERING = 
           ifelse(is.na(GBAHERKOMSTGROEPERING), GBAHERKOMSTGROEPERING_ma, GBAHERKOMSTGROEPERING),
         GBAGENERATIE = ifelse(is.na(GBAGENERATIE), GBAGENERATIE_ma, GBAGENERATIE)) %>%
  select(-c(GBAHERKOMSTGROEPERING_ma, GBAGENERATIE_ma)) 


# record sample size
sample_size <- sample_size %>% mutate(n_1_child_parent_link = nrow(cohort_dat))


#### AGE PARENTS AT BIRTH ####

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
# The region of every child (live birth or stillbirth) is the mother's address on
# the date of birth. The child's own first known address is only a fallback, for
# children whose mother has no address on that date in GBAADRESOBJECT.
# (Until Nov 2026 this was the other way round: the child's first known address,
# and the mother's address only for children without an address of their own.)
adres_path <- file.path(loc$data_folder, loc$gbaao_data)
# [SQL] only addresses of the children and their mothers: these are the only two
#       filters applied to adres_tab below (mother's home at birth, child's first home).
#       Rinpersoon_Moeder is the mother id we link on below (Perined, completed
#       with KINDOUDER above), so filter on that one.
adres_tab <- read_cbs(adres_path,
                      filter_in = list(RINPERSOON = c(cohort_dat$RINPERSOON, cohort_dat$Rinpersoon_Moeder))) %>%
  as_factor(only_labelled = TRUE, levels = "values") %>%
  mutate(GBADATUMAANVANGADRESHOUDING = ymd(GBADATUMAANVANGADRESHOUDING),
         GBADATUMEINDEADRESHOUDING = ymd(GBADATUMEINDEADRESHOUDING))

# address of the mother on the date of birth (main address).
# - Per mother AND birth date: a mother can have more than one child in the cohort,
#   and each child gets the address at its own birth date.
# - If the birth date falls in two address periods (a move on the day of birth),
#   keep the address that started last, i.e. where the mother lived at the end of that day.
home_tab_mom <-
  inner_join(adres_tab %>% filter(RINPERSOON %in% cohort_dat$Rinpersoon_Moeder),
             cohort_dat %>% distinct(CBKSoortNr_Moeder, Rinpersoon_Moeder, birthdate),
             by = c("RINPERSOONS" = "CBKSoortNr_Moeder",
                    "RINPERSOON" = "Rinpersoon_Moeder"),
             relationship = "many-to-many") %>%
  filter(birthdate %within% interval(GBADATUMAANVANGADRESHOUDING,
                                     GBADATUMEINDEADRESHOUDING)) %>%
  arrange(desc(GBADATUMAANVANGADRESHOUDING)) %>%
  group_by(RINPERSOONS, RINPERSOON, birthdate) %>%
  summarise(mom_home = RINOBJECTNUMMER[1],
            type_mom_home = SOORTOBJECTNUMMER[1],
            .groups = "drop")

# earliest known address of the child (fallback). Sort on the start date first:
# the row order of the file (.sav or .db) is not guaranteed to be chronological.
home_tab_child <-
  adres_tab %>% filter(RINPERSOON %in% cohort_dat$RINPERSOON) %>%
  arrange(GBADATUMAANVANGADRESHOUDING) %>%
  group_by(RINPERSOONS, RINPERSOON) %>%
  summarise(childhood_home = RINOBJECTNUMMER[1],
            type_childhood_home = SOORTOBJECTNUMMER[1],
            .groups = "drop")


# join mother home address at birth
cohort_dat <- left_join(cohort_dat, home_tab_mom,
                        by = c("CBKSoortNr_Moeder" = "RINPERSOONS",
                               "Rinpersoon_Moeder" = "RINPERSOON",
                               "birthdate"))

# join child home address
cohort_dat <- left_join(cohort_dat, home_tab_child,
                        by = c("RINPERSOONS", "RINPERSOON"))


# use the mother's address at birth; the child's first address only when that is missing.
# Address and type always come from the same source.
cohort_dat <-
  cohort_dat %>%
  mutate(
    type_childhood_home = as.character(type_childhood_home),
    type_mom_home = as.character(type_mom_home),
    childhood_home = ifelse(!is.na(mom_home), mom_home, childhood_home),
    type_childhood_home = ifelse(!is.na(mom_home), type_mom_home, type_childhood_home),
    type_childhood_home = as.factor(type_childhood_home)
    )

# log how many children got which address (aggregate counts, stays in the run log)
message(sprintf("Region link: %d mother's address at birth, %d child's first address (fallback), %d no address (dropped)",
                sum(!is.na(cohort_dat$mom_home)),
                sum(is.na(cohort_dat$mom_home) & !is.na(cohort_dat$childhood_home)),
                sum(is.na(cohort_dat$childhood_home))))

cohort_dat <- cohort_dat %>% filter(!is.na(childhood_home))
 


# free up memory
rm(adres_tab, home_tab_child, home_tab_mom)


# clean the postcode table
vslpc_path <- file.path(loc$data_folder, loc$postcode_data)
# [SQL] only the dwellings we link below (childhood_home, which came from adres_tab)
vslpc_tab  <- read_cbs(vslpc_path, filter_in = list(RINOBJECTNUMMER = cohort_dat$childhood_home)) %>%
  mutate(
    SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
    DATUMAANVPOSTCODENUMADRES = ymd(DATUMAANVPOSTCODENUMADRES),
    DATUMEINDPOSTCODENUMADRES = ymd(DATUMEINDPOSTCODENUMADRES),
    POSTCODENUM = ifelse(POSTCODENUM == "----", NA, POSTCODENUM) 
  ) %>%
  filter(!is.na(POSTCODENUM))

vslpc_tab <- 
  vslpc_tab %>% 
  filter(dmy(cfg$postcode_target_date) %within% interval(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES)) %>% 
  mutate(postcode3 = as.character(floor(as.numeric(POSTCODENUM)/10))) %>% 
  select(SOORTOBJECTNUMMER, RINOBJECTNUMMER, postcode4 = POSTCODENUM, postcode3)

# add the postal codes to the cohort
cohort_dat <- inner_join(cohort_dat, vslpc_tab, 
                         by = c("type_childhood_home" = "SOORTOBJECTNUMMER", 
                                "childhood_home" = "RINOBJECTNUMMER"))

# add region/neighbourhood codes to cohort
vslgwb_path <- file.path(loc$data_folder, loc$vslgwb_data) 
# [SQL] only the columns used below, and only the dwellings we link below
vslgwb_tab  <- read_cbs(vslgwb_path,
                        cols = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER",
                                 paste0("gem", year(dmy(cfg$gwb_target_date))),
                                 paste0("wc", year(dmy(cfg$gwb_target_date)))),
                        filter_in = list(RINOBJECTNUMMER = cohort_dat$childhood_home)) %>%
  mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"))

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
  unique()

cohort_dat <- inner_join(cohort_dat, corop_tab, by = "municipality_code") %>%
  select(-c(type_childhood_home, childhood_home, 
            mom_home, type_mom_home))


# free up memory
rm(vslpc_tab, vslgwb_tab, corop_tab)


# mutate factor regions
cohort_dat <- 
  cohort_dat %>%
  mutate(across(c("postcode3", "postcode4", "municipality_code",
                  "neighborhood_code", "corop_code"), as.factor))

# record sample size
sample_size <- sample_size %>% mutate(n_2_region_link = nrow(cohort_dat))


#### WRITE OUTPUT TO TEMP ####
write_rds(cohort_dat, file.path(loc$temp_folder, "01_cohort.rds"))

write_rds(sample_size, file.path(loc$temp_folder, "01_sample_size.rds"))
