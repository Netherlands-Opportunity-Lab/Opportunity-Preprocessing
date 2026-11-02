# Kansenkaart data preparation pipeline
#
# Samples of the intragenerational analysis without a parent link
# (age45_current_address, age55_current_address), step 1 of 3:
#   01  this script: everyone in GBA born in the birth window, with sex and
#       migration background. No parent link, no childhood address.
#   03  src/age35/03_outcomes_age35: the outcomes at the outcome age (the same
#       script as age35, with the blocks listed in the config).
#   04  src/current_address/04_current_address: regions of the address at the
#       outcome age, Amsterdam areas, categorical outcomes.
# Writes temp/<cohort>/02_predictors.rds and 02_sample_size.rds: the files the
# outcomes script reads.
#
# Migration background is the person's own GBAHERKOMSTGROEPERING, coded as in
# age35 (02_predictors_age35, MIGRATION BACKGROUND). Without parents there is no
# "third generation" correction, so for the rare third-generation person
# migration background is Dutch here.
#
# (c) Netherlands Opportunity Lab 2026


#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)


#### SELECT COHORT FROM GBA ####
gba_path <- file.path(loc$data_folder, loc$gba_data)
gba_cols <- c("RINPERSOONS", "RINPERSOON", "GBAGESLACHT",
              "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG",
              "GBAGENERATIE", "GBAHERKOMSTGROEPERING")

# [SQL] only the birth years of the window are read; the exact date filter follows
cohort_years <- seq(year(dmy(cfg$child_birth_date_min)), year(dmy(cfg$child_birth_date_max)))
cohort_dat <-
  read_cbs(gba_path, cols = gba_cols, filter_in = list(GBAGEBOORTEJAAR = cohort_years)) %>%
  mutate(birthdate = dmy(paste(GBAGEBOORTEDAG, GBAGEBOORTEMAAND, GBAGEBOORTEJAAR, sep = "-"))) %>%
  select(-GBAGEBOORTEJAAR, -GBAGEBOORTEMAAND, -GBAGEBOORTEDAG) %>%
  mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
         GBAHERKOMSTGROEPERING = as_factor(GBAHERKOMSTGROEPERING, levels = "labels"),
         GBAGENERATIE = as_factor(GBAGENERATIE, levels = "labels"),
         GBAGESLACHT = as_factor(GBAGESLACHT, levels = "labels")) %>%
  mutate(GBAGESLACHT = as.factor(as.character(GBAGESLACHT))) %>%
  rename(sex = GBAGESLACHT) %>%
  mutate(sex = recode(sex, 'Mannen' = 'Men', 'Vrouwen' = 'Women')) %>%
  filter(birthdate %within% interval(dmy(cfg$child_birth_date_min), dmy(cfg$child_birth_date_max))) %>%
  mutate(birth_year = year(birthdate))

# record sample size
sample_size <- tibble(n_0_birth_cohort = nrow(cohort_dat))


#### MIGRATION BACKGROUND ####
# the same coding as age35, from the person's own origin
cohort_dat <-
  cohort_dat %>%
  mutate(
    migration_background = as.character(GBAHERKOMSTGROEPERING),
    migration_background =
      ifelse(!(migration_background %in%
                 c("Nederland", "Turkije", "Marokko", "Suriname",
                   'Aruba', 'Bonaire', 'Curaçao', 'Saba', 'Sint Eustatius',
                   'Sint Maarten', 'Nederlandse Antillen (oud)')),
             "Other", migration_background)) %>%
  # create migration antilles
  mutate(migration_background = ifelse(migration_background %in%
                                         c('Aruba', 'Bonaire', 'Curaçao', 'Saba', 'Sint Eustatius',
                                           'Nederlandse Antillen (oud)', 'Sint Maarten'), 'Dutch Caribbean',
                                       migration_background),
         migration_background = as.factor(migration_background)) %>%
  # recode migration background
  mutate(migration_background = recode(migration_background,
                                       'Nederland' = 'No Migration Background',
                                       'Turkije' = 'Turkiye',
                                       'Marokko' = 'Morocco'),
         has_migration = ifelse(migration_background == "No Migration Background", 0, 1)) %>%
  select(-c(GBAHERKOMSTGROEPERING, GBAGENERATIE))


#### WRITE OUTPUT TO TEMP ####
# (named as the outcomes script expects them)
write_rds(cohort_dat, file.path(loc$temp_folder, "02_predictors.rds"))
write_rds(sample_size, file.path(loc$temp_folder, "02_sample_size.rds"))
