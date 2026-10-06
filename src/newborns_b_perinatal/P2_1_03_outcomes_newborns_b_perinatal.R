# Kansenkaart data preparation pipeline
#
# 3. Outcome creation.
#   - Adding perinatal outcomes to the cohort.
#   - Writing `scratch/03_outcomes.rds`.
#
# (c) ODISSEI Social Data Science team 2025



#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)



#### CONFIGURATION ####
# load main cohort dataset
cohort_dat <- read_rds(file.path(loc$scratch_folder, "02_predictors.rds"))

sample_size <- read_rds(file.path(loc$scratch_folder, "02_sample_size.rds"))



#### DEATHS (ONLY USED TO DEFINE THE BIG2 SAMPLE) ####

#### DO AND DOODOORZTAB ####

## CLEAN DO ##

# function to get latest do version of specified year 
get_do_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "GezondheidWelzijn/DO"), 
    pattern = paste0("DO", year, "V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest do version of specified year 
get_do_map_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "GezondheidWelzijn/DO", year), 
    pattern = paste0("DO ", year, "V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

death_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(), 
                    date_of_death = character(), year = as.numeric())
for (year in seq(format(dmy(cfg$child_birth_date_min), "%Y"),
                 format(dmy(cfg$child_birth_date_max), "%Y"))) {
  
  if (year <= 2011) {
    death_dat <- read_sav(get_do_filename(year), 
                          col_select = matches("^(rinpersoons|rinpersoon|ovljr|ovlmnd|ovldag)", ignore.case = TRUE)) %>%
      # convert to uppercase
      rename_all(toupper) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
             date_of_death = paste(OVLJR, OVLMND, OVLDAG, sep = "-"),
             year = year, 
             RINPERSOON = ifelse(RINPERSOON == "", NA, RINPERSOON)) %>%
      select(-c(OVLJR, OVLMND, OVLDAG)) %>%
      # add to death data
      bind_rows(death_dat, .)
    
  } else if (year == 2012) {
    death_dat <- read_sav(get_do_map_filename(year), 
                          col_select = matches("^(rinpersoons|rinpersoon|ovljr|ovlmnd|ovldag)", ignore.case = TRUE)) %>%
      # convert to uppercase
      rename_all(toupper) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
             date_of_death = paste(OVLJR, OVLMND, OVLDAG, sep = "-"),
             year = year, 
             RINPERSOON = ifelse(RINPERSOON == "", NA, RINPERSOON)) %>%
      select(-c(OVLJR, OVLMND, OVLDAG)) %>%
      # add to death data
      bind_rows(death_dat, .)
  }
}

# post_processing
death_dat <- 
  death_dat %>%
  mutate(date_of_death = ymd(date_of_death))


## CLEAN DOODOORZTAB ##

# function to get latest doodoorztab version of specified year 
get_dood_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "GezondheidWelzijn/DOODOORZTAB", year),
    pattern = paste0("DOODOORZ", year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest gbaoverlijdenstab version of specified year 
get_gba_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "Bevolking/GBAOVERLIJDENTAB", year),
    pattern = "(?i)(.sav)",
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


dood_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(), year = numeric())
gba_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(), date_of_death = character())
for (year in seq(2013, cfg$health_year_max )) {       #format(dmy(cfg$child_birth_date_max) + 1, "%Y") <- insert as max year once DOODSOORZ 2025 is available
  
  dood_dat <- read_sav(get_dood_filename(year), 
                       col_select = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
           year = year) %>%
    # add to death data
    bind_rows(dood_dat, .)
  
  gba_dat <- read_sav(get_gba_filename(year)) %>%
    rename(date_of_death = GBADatumOverlijden) %>%
    mutate(
      RINPERSOONS = as_factor(RINPERSOONS, levels = "value")
    ) %>% distinct() %>%
    # add to death data
    bind_rows(gba_dat, .)
}

# post-processing
dood_dat <- inner_join(dood_dat, gba_dat) %>%
  mutate(date_of_death = ymd(date_of_death)) %>%
  # select(-year) %>%
  distinct()


# create one death data
death_dat <- rbind(death_dat, dood_dat) %>%
  select(-year) %>%
  unique() %>%
  distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE)
rm(dood_dat, gba_dat)


cohort_dat <- left_join(cohort_dat, death_dat)

# free up memory
rm(death_dat)

# create difference between birth and death date
cohort_dat <- 
  cohort_dat %>%
  mutate(
    birthdate = ymd(birthdate),
    diff_days = as.numeric(difftime(date_of_death, birthdate, units = "days"))
  )


# flag stillbirths and deaths within the first 7 days, using the same definition
# as c00_perinatal_mortality in the mortality pipeline. This is NOT an outcome:
# these children are excluded from the BIG2 (perinatal) sample in 04_postprocessing.
cohort_dat <- 
  cohort_dat %>% 
  mutate(
    perinatal_death = ifelse(
      diff_days <= 7 | (sterfte %in% c('Ante partum', 'Durante partum', 
                                       'Postpartum 0-7 dagen')), 1, 0),
    perinatal_death = ifelse(is.na(perinatal_death), 0, perinatal_death)
  ) %>%
  select(-c(date_of_death, diff_days))


#### BIG2 OUTCOMES ####


# import percentile weight boys & girls
boys_weight_tab <- read_excel(loc$birthweight_data, sheet = loc$boys_sheet, skip = 2) %>%
  rename(
    gestational_age = "Totaal aantal dagen",
    p10_boys = "p10...6"
  ) %>%
  select(c(gestational_age, p10_boys))

girls_weight_tab <- read_excel(loc$birthweight_data, sheet = loc$girls_sheet, skip = 2) %>%
  rename(
    gestational_age = "Totaal aantal dagen",
    p10_girls = "p10...6"
  ) %>%
  select(c(gestational_age, p10_girls))


# merge percentile to perined data
cohort_dat <- left_join(cohort_dat, boys_weight_tab, by = c("amddd" = "gestational_age"))
cohort_dat <- left_join(cohort_dat, girls_weight_tab, by = c("amddd" = "gestational_age"))


# small for gestational age & preterm birth cohort
cohort_dat <- 
  cohort_dat %>%
  mutate(
    # preterm birth for infants with < 259 gestational age
    preterm_birth = ifelse(amddd < 259, 1, 0),
    
    # create small for gestational age outcome
    sga = ifelse((sex == "Men" & geboortegew < p10_boys) |
                   (sex == "Women" & geboortegew < p10_girls), 1, 0)) %>%
  select(-c(p10_boys, p10_girls))


# free up memory
rm(girls_weight_tab, boys_weight_tab)


#### TOTAL HEALTH COSTS ####


# create a table with incomes at the cpi_base_year level
# first, load consumer price index data (2015 = 100)
# source: CBS statline
cpi_tab <- read_excel(loc$cpi_index_data) %>%
  mutate(year = as.numeric(year))

# set cpi_base_year = 100
cpi_tab <- cpi_tab %>%
  mutate(
    cpi = cpi / cpi_tab %>% filter(year == cfg$cpi_base_year) %>% pull(cpi) * 100)



# function to get latest ZVWZORGKOSTENTAB version of specified year
get_health_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "GezondheidWelzijn/ZVWZORGKOSTENTAB", year),
    pattern = paste0("ZVWZORGKOSTEN", year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


health_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(),
                     total_health_costs = double(), year = integer())
for (year in seq(as.integer(cfg$health_year_min), as.integer(cfg$health_year_max))) {

  if (year == 2014) {
    health_tab <- read_sav(get_health_filename(2014),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKGEBOORTEZORG",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF")) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)

  } else if (year == 2015 | year == 2016 | year == 2017) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKGEBOORTEZORG",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF")) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)

  } else if (year == 2018 | year == 2019) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF")) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)
  } else if (year == 2020) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF")) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)
    
  } else if (year == 2021) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF")) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)
    
  } else if (year == 2022) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF", "ZVWKGGZZPMTOTAAL" )) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)
  } else if (year == 2023 | year == 2024 | year == 2025) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF", "ZVWKGGZZPMTOTAAL" )) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)
  }

  health_tab <- health_tab %>%
    # replace negative values with 0
    mutate(across(grep("^ZVWK", names(health_tab), value = TRUE),
                  function(x) ifelse(x < 0, 0, x))) %>%
    # sum of all healthcare costs
    mutate(
      total_health_costs = rowSums(across(grep("^ZVWK", names(health_tab), value = TRUE)), na.rm = TRUE),
      total_health_costs = total_health_costs - NOPZVWKHUISARTSINSCHRIJF,
      year = year # add year
    ) %>%
    select(RINPERSOONS, RINPERSOON, total_health_costs, year)

  # add to health dat
  health_dat <- bind_rows(health_dat, health_tab)

}
rm(health_tab)


# deflate
health_dat <-
  health_dat %>%
  left_join(cpi_tab %>% select(year, cpi), by = "year") %>%
  mutate(total_health_costs = total_health_costs / (cpi / 100)) %>%
  select(-cpi)


# add to data
child_years <- 
  bind_rows(
    cohort_dat %>% select(RINPERSOONS, RINPERSOON, birth_year) %>% mutate(year = birth_year),
    cohort_dat %>% select(RINPERSOONS, RINPERSOON, birth_year) %>% mutate(year = birth_year + 1)
  ) %>%
  select(RINPERSOONS, RINPERSOON, year)

health_dat_summed <-
  child_years %>%
  left_join(health_dat, by = c("RINPERSOONS", "RINPERSOON", "year")) %>%
  mutate(total_health_costs = ifelse(is.na(total_health_costs), 0, total_health_costs)) %>%
  group_by(RINPERSOONS, RINPERSOON) %>% 
  summarise(total_health_costs = sum(total_health_costs), .groups = "drop")

cohort_dat <- left_join(cohort_dat, health_dat_summed, by = c("RINPERSOONS", "RINPERSOON"))
rm(health_dat, health_dat_summed, child_years, cpi_tab) 

# convert NA to 0
cohort_dat <- cohort_dat %>%
  mutate(total_health_costs = ifelse(is.na(total_health_costs), 0, total_health_costs))


#### PREFIX ####

# add prefix to outcomes
outcomes <- c('sga', 'preterm_birth', 'total_health_costs')
suffix <- "c00_"


# rename outcomes
cohort_dat <- 
  cohort_dat %>%
  rename_with(~str_c(suffix, .), .cols = all_of(outcomes)) %>% 
  ungroup() 

# record sample size
sample_size <- sample_size %>% mutate(n_4_child_outcomes = nrow(cohort_dat))

#### WRITE OUTPUT TO SCRATCH ####
write_rds(cohort_dat, file.path(loc$scratch_folder, "03_outcomes.rds"))

#write sample size reduction table to scratch
sample_size <- sample_size %>% mutate(cohort_name = cohort)
write_rds(sample_size, file.path(loc$scratch_folder, "03_sample_size.rds"))

