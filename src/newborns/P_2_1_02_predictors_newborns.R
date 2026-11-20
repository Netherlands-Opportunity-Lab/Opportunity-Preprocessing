# Kansenkaart data preparation pipeline
#
# 2. Predictor creation.
#    - Adding parent income and income percentile to the cohort.
#      Income = the mother's household income, deflated to cpi_base_year per
#      year and then averaged over the years income_year_offsets around the
#      birth year (default c(-1, 0): the year before birth and the birth year).
#      The percentile is ranked within birth year among the cohort itself
#      (income_rank_population: "cohort", default) or among all children in NL
#      born that year ("national", used by JGZ since 20-11-2026). Functions in
#      src/lib/P_2_1_parent_income.R.
#    - Adding migration background information to the cohort.
#    - Writing `temp/<cohort>/02_predictor.rds`
#
# SQLite version: the files that are available as converted .db (GBAPERSOON,
# GBAADRESOBJECT, KINDOUDER, GBAHUISHOUDENS, VSLPOSTCODE, VSLGWB, LEVCYCLWOONNIETWOON)
# are read with read_cbs() (src/lib/sql_helpers.R): only the rows that are
# needed leave the database. All other files are still read with read_sav(), exactly
# as before. Changes are marked with [SQL].
#
# (c) ODISSEI Social Data Science team 2024



#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)


#### CONFIGURATION ####
# load main cohort dataset
cohort_dat <- read_rds(file.path(loc$temp_folder, "01_cohort.rds")) %>%
  mutate(birth_year = year(birthdate))

sample_size <- read_rds(file.path(loc$temp_folder, "01_sample_size.rds"))

source_versioned("src/lib", "parent_income")

# ---- parental income: window, rank group and reference population ----------
# Two ways to define the income window (src/lib/P_2_1_parent_income.R):
#  * per birth year (default, newborns): income_year_offsets around the birth
#    year (default -1, 0), one income per child (income_parents, _perc), ranked
#    within birth year;
#  * per outcome age (income_outcome_ages set, JGZ since 20-11-2026): for every
#    age a, the mean over the income_window_years years up to and including the
#    calendar year in which the child reaches a (age in days from
#    outcome_target_days), ranked among the children who reach a in that same
#    year: income_parents_<a> and income_parents_perc_<a>.
# income_rank_population: "cohort" (default) ranks among the cohort itself;
# "national" among all children in NL (GBAPERSOON with a known mother in
# KINDOUDERTAB, the rule of the JGZ cohort), with the cohort part of it.
income_min_years <- if (is.null(cfg$income_min_years)) 1L else as.integer(cfg$income_min_years)
income_rank_population <- if (is.null(cfg$income_rank_population)) "cohort" else cfg$income_rank_population
if (!income_rank_population %in% c("cohort", "national")) {
  stop("income_rank_population must be \"cohort\" or \"national\", not \"", income_rank_population, "\".")
}
income_years <- c(as.integer(cfg$parent_income_year_min), as.integer(cfg$parent_income_year_max))
income_by_age <- !is.null(cfg$income_outcome_ages)

if (income_by_age) {
  income_window_years <- if (is.null(cfg$income_window_years)) 5L else as.integer(cfg$income_window_years)
  income_ages_days <- unlist(cfg$outcome_target_days)[unlist(cfg$income_outcome_ages)]
  if (anyNA(income_ages_days)) {
    stop("income_outcome_ages not in outcome_target_days: ",
         paste(setdiff(unlist(cfg$income_outcome_ages), names(cfg$outcome_target_days)), collapse = ", "))
  }
  message(sprintf(paste("Parental income per outcome age (%s): mean over %d years up to and including the year",
                        "the age is reached (incomes %d-%d), ranked among the %s reaching the age that year."),
                  paste(names(income_ages_days), collapse = ", "), income_window_years,
                  income_years[1], income_years[2],
                  if (income_rank_population == "national") "children in NL" else "cohort children"))
  # reference children reach an age in a year with a complete window if born
  # between (first window end - oldest age) and (last income year)
  ref_birth_years <- seq(income_years[1] + income_window_years - 1 - ceiling(max(income_ages_days) / 365.25) - 1,
                         income_years[2])
} else {
  income_offsets <- if (is.null(cfg$income_year_offsets)) c(-1L, 0L) else
    as.integer(unlist(cfg$income_year_offsets))
  message(sprintf("Parental income: years %s relative to birth (at least %d with an income), ranked within birth year among the %s.",
                  paste(income_offsets, collapse = ", "), income_min_years,
                  if (income_rank_population == "national") "children in NL born that year" else "cohort"))
  # the configured income years must cover every child's window
  needed_income_years <- seq(min(cohort_dat$birth_year) + min(income_offsets),
                             max(cohort_dat$birth_year) + max(income_offsets))
  missing_income_years <- setdiff(needed_income_years, seq(income_years[1], income_years[2]))
  if (length(missing_income_years) > 0) {
    stop("parent_income_year_min/max in the config do not cover income years: ",
         paste(missing_income_years, collapse = ", "))
  }
  rm(needed_income_years, missing_income_years)
  ref_birth_years <- unique(cohort_dat$birth_year)
}

# national reference population; their mothers' household income is computed
# exactly as for the cohort below
income_reference <- NULL
if (income_rank_population == "national") {
  kindouder_path <- if (!is.null(loc$kindouder_data)) loc$kindouder_data else loc$kindouder_file
  if (is.null(kindouder_path)) stop("income_rank_population = national needs kindouder_data in file_locations.")
  income_reference <- reference_children(
    gba_path       = file.path(loc$data_folder, loc$gba_data),
    kindouder_path = file.path(loc$data_folder, kindouder_path),
    birth_years    = ref_birth_years)
  print(table(`reference children per birth year` = income_reference$birth_year))
}


# ---- price level: every yearly amount is converted to the price level of the
# reference year (cpi_base_year; referentiejaar, 2024 in S_2) BEFORE averaging.
# Source: CBS StatLine, consumentenprijzen (2015 = 100). Stops when the table
# lacks the reference year or a year that is converted (income or wealth), as a
# missing year would otherwise give NA amounts that are silently dropped.
cpi_tab <- read_excel(loc$cpi_index_data) %>%
  mutate(year = as.numeric(year))
cpi_tab <- cpi_to_reference_year(
  cpi_tab, reference_year = cfg$cpi_base_year,
  years = c(seq(income_years[1], income_years[2]),
            seq(as.integer(cfg$parent_wealth_year_min), as.integer(cfg$parent_wealth_year_max))))


#### HOUSEHOLD INCOME ####


# get income data from each requested year into single data frame
get_ipi_filename <- function(year) {
  # function to get latest ipi version of specified year
  # get all ipi files with the specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "InkomenBestedingen/INTEGRAAL PERSOONLIJK INKOMEN", year),
    pattern = paste0("PERSOONINK", year, "TABV[0-9]+\\.sav"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

get_inpa_filename <- function(year) {
  # function to get latest inpa version of specified year
  # get all inpa files with the specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "InkomenBestedingen/INPATAB"),
    pattern = paste0("INPA", year, "TABV[0-9]+\\.sav"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


# LINK MOTHER RINPERSOON TO HOUSEHOLD RINPERSOON

mothers <- unique(c(cohort_dat$CBKSoortNr_Moeder, cohort_dat$Rinpersoon_Moeder,
                    income_reference$Rinpersoon_Moeder))
mothers_rinpersoon <- tibble(RINPERSOONS = factor(), RINPERSOON = character(), 
                             RINPERSOONSHKW = factor(), RINPERSOONHKW = character(),
                             year = double())
for (year in seq(as.integer(cfg$parent_income_year_min), as.integer(cfg$parent_income_year_max))) {
  if (year < 2011) {
    # use IPI tab
    mothers_rinpersoon <- 
      # read file from disk
      read_sav(get_ipi_filename(year), col_select = c("RINPERSOONS", "RINPERSOON",
                                                      "RINPERSOONSKERN", "RINPERSOONKERN")) %>% 
      rename(RINPERSOONSHKW = RINPERSOONSKERN,
             RINPERSOONHKW = RINPERSOONKERN) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
             RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "value"),
             year = year) %>%
      # select only incomes of mothers
      filter(RINPERSOON %in% mothers) %>% 
      # add to mothers rinpersoon
      bind_rows(mothers_rinpersoon, .)
  } else {
    # use INPA tab
    mothers_rinpersoon <- 
      # read file from disk
      read_sav(get_inpa_filename(year), col_select = c("RINPERSOONS", "RINPERSOON", 
                                                       "RINPERSOONSHKW", "RINPERSOONHKW")) %>% 
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
             RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "value"),
             year = year) %>%
      # select only mothers
      filter(RINPERSOON %in% mothers) %>% 
      # add to mothers
      bind_rows(mothers_rinpersoon, .)
  }
}



#### HOUSEHOLD INCOME ####
# get income data from each requested year into single data frame
get_ihi_filename <- function(year) {
  # function to get latest ihi version of specified year
  # get all ihi files with the specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "InkomenBestedingen/INTEGRAAL HUISHOUDENS INKOMEN", year),
    pattern = paste0(year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

get_inha_filename <- function(year) {
  # function to get latest inha version of specified year
  # get all inha files with the specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "InkomenBestedingen/INHATAB"),
    pattern = paste0(year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


income_household <- tibble(RINPERSOONSHKW = factor(), RINPERSOONHKW = character(), 
                           household_income = double(), year = integer())
for (year in seq(as.integer(cfg$parent_income_year_min), as.integer(cfg$parent_income_year_max))) {
  if (year < 2011) {
    # use IHI tab
    income_household <- 
      # read file from disk
      read_sav(get_ihi_filename(year), col_select = c("RINPERSOONSKERN", "RINPERSOONKERN", 
                                                      "BVRBRUTINKH")) %>% 
      rename(household_income = BVRBRUTINKH,
             RINPERSOONSHKW = RINPERSOONSKERN,
             RINPERSOONHKW = RINPERSOONKERN) %>% 
      mutate(RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "value"),
             year = year) %>%
      # add to income household
      bind_rows(income_household, .)
  } else {
    # use INHA tab
    income_household <- 
      # read file from disk
      read_sav(get_inha_filename(year), col_select = c("RINPERSOONSHKW", "RINPERSOONHKW", 
                                                       "INHBRUTINKH")) %>% 
      rename(household_income = INHBRUTINKH) %>% 
      mutate(RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "value"),
             year = year) %>%
      # add to income household
      bind_rows(income_household, .)
  }
}


# remove negative and NA incomes
income_household <-
  income_household %>% 
  mutate(household_income = ifelse(household_income == 999999999 | 
                                     household_income == 9999999999, NA,
                                   household_income)) 

# deflate
income_household <- 
  income_household %>% 
  left_join(cpi_tab %>% select(year, cpi), by = "year") %>% 
  mutate(household_income = household_income / (cpi / 100)) %>% 
  select(-cpi)


# one household income record per mother per year
income_household <-
  income_household %>%
  inner_join(mothers_rinpersoon, by = c("RINPERSOONSHKW", "RINPERSOONHKW", "year")) %>%
  select(RINPERSOONS, RINPERSOON, year, household_income) %>%
  distinct()

# free up memory
rm(mothers_rinpersoon)


if (income_by_age) {
  n_before_income <- nrow(cohort_dat)
  income_ages <- income_by_outcome_age(
    cohort = cohort_dat, reference = income_reference, income_household = income_household,
    ages_days = income_ages_days, window_years = income_window_years,
    min_years = income_min_years, income_years = income_years)
  # children without a parental income at any age are dropped (as before)
  cohort_dat <- cohort_dat %>% inner_join(income_ages, by = c("RINPERSOONS", "RINPERSOON"))
  message(sprintf("Children with a parental income at one or more ages: %s of %s",
                  format(nrow(cohort_dat), big.mark = ","), format(n_before_income, big.mark = ",")))
  if (!is.null(income_reference)) {
    # where the cohort sits in the national distribution, per age (internal
    # check: with a national rank the cohort's quintiles need not hold 20% each)
    perc_cols <- grep("^income_parents_perc_", names(cohort_dat), value = TRUE)
    print(round(sapply(cohort_dat[perc_cols], function(p)
      prop.table(table(cut(p, seq(0, 1, 0.2), include.lowest = TRUE)))), 3))
    sample_size <- sample_size %>% mutate(n_income_reference = nrow(income_reference))
  }
  rm(income_ages, n_before_income)
} else {
  # mean per CHILD over its own window (not per mother over the whole income
  # range, since a mother can have several children born in different years);
  # negative means count as unknown and those children are dropped
  n_before_income <- nrow(cohort_dat)
  cohort_dat <- child_income_window(cohort_dat, income_household,
                                    offsets = income_offsets, min_years = income_min_years)
  message(sprintf("Children with a parental income: %s of %s",
                  format(nrow(cohort_dat), big.mark = ","), format(n_before_income, big.mark = ",")))

  # table of the number of years the mean income is based on
  print(table(`income years` = cohort_dat$household_income_n))

  if (!is.null(income_reference)) {
    income_reference <- child_income_window(income_reference, income_household,
                                            offsets = income_offsets, min_years = income_min_years)
  }

  # percentile within birth year, among the cohort or the national reference
  cohort_dat <- income_percentile(cohort_dat, reference = income_reference,
                                  value = "household_income") %>%
    select(-household_income_n) %>%
    rename(income_parents      = household_income,
           income_parents_perc = household_income_perc)

  if (!is.null(income_reference)) {
    print(round(prop.table(table(`national income quintile` =
                                   cut(cohort_dat$income_parents_perc, seq(0, 1, 0.2),
                                       include.lowest = TRUE))), 3))
    sample_size <- sample_size %>% mutate(n_income_reference = nrow(income_reference))
  }
  rm(n_before_income)
}

rm(income_household, income_reference)



#### MOTHERS WEALTH ####


# get wealth data from each requested year into single data frame
get_veh_filename <- function(year) {
  # function to get latest version of specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "InkomenBestedingen/VEHTAB"),
    pattern = paste0("VEH", year, "TAB"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

get_koppelpersoon_filename <- function(year) {
  # function to get latest version of specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "InkomenBestedingen/INHATAB"),
    pattern = paste0("KOPPELPERSOONHUISHOUDEN", year),
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

get_koppeltabel_filename <- function(year) {
  # function to get latest version of specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "InkomenBestedingen/VEHTAB"),
    pattern = paste0("KOPPELTABEL", year),
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

parents_dat <-
  cohort_dat %>%
  select(RINPERSOONS, RINPERSOON, Rinpersoon_Moeder, CBKSoortNr_Moeder)

parents <- c(cohort_dat$Rinpersoon_Moeder, cohort_dat$CBKSoortNr_Moeder)

wealth_parents <- tibble(RINPERSOONS = factor(), RINPERSOON = character(),
                         wealth_parents = double(), year = integer())
for (year in seq(as.integer(cfg$parent_wealth_year_min), as.integer(cfg$parent_wealth_year_max))) {
  
  if (year <= 2010) {
    
    # KOPPELTABEL only exists from 2006 onward, but parent_wealth_year_min 
    # can start at 2005. For 2005, we reuse the 2006 koppeltabel as the best
    # available approximation
    koppeltabel_year <- max(year, 2006)
    # load koppel data person to household
    koppel_hh <- read_sav(get_koppeltabel_filename(koppeltabel_year)) %>%
      # select only incomes of parents
      filter(RINPERSOON %in% parents) %>% 
      mutate(RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "value"), 
             RINPERSOONS = as_factor(RINPERSOONS, levels = "value"))
    
  } else {
    
    # load koppel data person to household
    koppel_hh <- read_sav(get_koppelpersoon_filename(year)) %>%
      # select only incomes of parents
      filter(RINPERSOON %in% parents) %>% 
      mutate(RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "value"), 
             RINPERSOONS = as_factor(RINPERSOONS, levels = "value"))
  }

  # same 2005 issue as with KOPPELTABEL
  veh_year <- max(year, 2006)
  
  # use VEH tab
  veh_parents <-
    # read file from disk
    read_sav(get_veh_filename(veh_year), col_select = c("RINPERSOONSHKW", "RINPERSOONHKW",
                                                    "VEHW1000VERH")) %>%
    rename(wealth = VEHW1000VERH) %>%
    mutate(RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "value"), 
           year = year) %>%
    inner_join(koppel_hh, by = c("RINPERSOONSHKW", "RINPERSOONHKW")
    ) %>%
    # remove NA incomes
    mutate(wealth = ifelse(wealth == 99999999999, NA, wealth)) %>%
    left_join(cpi_tab %>% select(year, cpi), by = "year") %>%
    mutate(wealth = wealth / (cpi / 100)) %>%
    select(-c(cpi, year))
  
  
  # add to data
  wealth_dat <- parents_dat %>%
    left_join(veh_parents, 
              by = c("CBKSoortNr_Moeder" = "RINPERSOONS",
                     "Rinpersoon_Moeder" = "RINPERSOON")
              ) %>%
    # add year
    mutate(year = year) %>% 
    rename('wealth_parents' = 'wealth') %>%
    select(RINPERSOONS, RINPERSOON, wealth_parents, year)
  
  
  wealth_parents <- bind_rows(wealth_parents, wealth_dat)
}
rm(veh_parents, koppel_hh, wealth_dat, parents_dat)


# add year of child specific age at which we measure parental wealth
wealth_age <- abs(min(cohort_dat$birth_year) - 
                    as.integer(cfg$parent_wealth_year_min))

cohort_dat <- 
  cohort_dat %>%
  mutate(wealth_age = as.numeric(birth_year + wealth_age))

cohort_dat <- 
  cohort_dat %>%
  left_join(wealth_parents, by = c("RINPERSOONS" = "RINPERSOONS",
                                   "RINPERSOON" = "RINPERSOON",
                                   "wealth_age"  = "year")) %>%
  select(-wealth_age)

rm(wealth_parents, wealth_age)


# compute wealth transformations
cohort_dat <- 
  cohort_dat %>% 
  group_by(birth_year) %>%
  mutate(
    wealth_parents_rank = rank(wealth_parents, ties.method = "average"),
    wealth_parents_perc = wealth_parents_rank / max(wealth_parents_rank)
  ) %>%
  select(-wealth_parents_rank) %>%
  ungroup()



#### THIRD GENERATION ####

# import gba for mothers generation and origin
gba_path <- file.path(loc$data_folder, loc$gba_data)
gba_dat <-  
  # [SQL] only the mothers (gba_dat is only left-joined on the mother ids)
  read_cbs(gba_path, cols = c("RINPERSOONS", "RINPERSOON",
                              "GBAGENERATIE", "GBAHERKOMSTGROEPERING"),
           filter_in = list(RINPERSOON = cohort_dat$Rinpersoon_Moeder)) %>% 
  mutate(RINPERSOONS  = as_factor(RINPERSOONS,  levels = "values"),
         GBAGENERATIE = as_factor(GBAGENERATIE, levels = "label"),
         GBAHERKOMSTGROEPERING = as_factor(GBAHERKOMSTGROEPERING, levels = "labels"))

# add mothers generation to cohort
cohort_dat <- 
  cohort_dat %>% 
  left_join(gba_dat,
            by = c("Rinpersoon_Moeder" = "RINPERSOON", "CBKSoortNr_Moeder" = "RINPERSOONS"),
            suffix = c("", "_ma"))


# replace autochtoon to third generation if the mother is second generation  (2) 
# and the child is native (0)
cohort_dat <- 
  cohort_dat %>%
  mutate(across(c("GBAGENERATIE", "GBAGENERATIE_ma"), as.character)) %>% 
  mutate(
    GBAGENERATIE_third = as.character(GBAGENERATIE),
    GBAGENERATIE_third = ifelse(
      GBAGENERATIE == "autochtoon" & GBAGENERATIE_ma == "tweede generatie allochtoon" & !is.na(GBAGENERATIE_ma), 
      "derde generatie allochtoon", 
      GBAGENERATIE_third
    )
  )

# replace gbaherkomstgroepering if the child is third generation
cohort_dat <- 
  cohort_dat %>%
  mutate(
    across(c(GBAHERKOMSTGROEPERING, GBAHERKOMSTGROEPERING_ma), 
           as.character)
  ) %>%
  mutate(
    # third generation child gets mom's origin
    GBAHERKOMSTGROEPERING_third = ifelse(
      GBAGENERATIE_third == "derde generatie allochtoon" & !is.na(GBAHERKOMSTGROEPERING_ma),
      GBAHERKOMSTGROEPERING_ma, 
      GBAHERKOMSTGROEPERING
    )
  )


#### MIGRATION BACKGROUND ####


# create migration variable: origin with third generation
cohort_dat <- 
  cohort_dat %>%
  mutate(
    migration_background = as.character(GBAHERKOMSTGROEPERING_third), 
    migration_background = 
      ifelse(!(migration_background %in% 
                 c("Nederland", "Turkije", "Marokko", "Suriname", 
                   'Aruba', 'Bonaire', 'Curaçao', 'Saba', 'Sint Eustatius', 
                   'Sint Maarten', 'Nederlandse Antillen (oud)')), 
             "Other", migration_background)) %>%
  # create migration antilles
  mutate(migration_background = ifelse(migration_background %in% 
                                         c('Aruba', 'Bonaire', 'Curaçao', 'Saba', 'Sint Eustatius', 
                                           'Sint Maarten', 'Nederlandse Antillen (oud)'), 
                                       'Dutch Caribbean', migration_background),
         migration_background = as.factor(migration_background)) %>%
  # recode migration background
  mutate(migration_background = recode(migration_background, 
                                       'Nederland' = 'No Migration Background', 
                                       'Turkije' = 'Turkiye', 
                                       'Marokko' = 'Morocco'))


# remove unnecessary outcomes
cohort_dat <- 
  cohort_dat %>%
  select(-c(GBAHERKOMSTGROEPERING_ma, GBAGENERATIE_ma, 
            GBAHERKOMSTGROEPERING, GBAGENERATIE, 
            GBAGENERATIE_third, GBAHERKOMSTGROEPERING_third))


# create variable for individuals with a migration background
cohort_dat <-
  cohort_dat %>%
  mutate(has_migration = ifelse(migration_background == "No Migration Background", 0, 1))


# free up memory
rm(gba_dat)



#### TYPE HOUSEHOLDS ####

cohort_rinpersoons <- 
  c(cohort_dat$RINPERSOONS, cohort_dat$RINPERSOON, 
    cohort_dat$CBKSoortNr_Moeder, cohort_dat$Rinpersoon_Moeder)

# import household  data
household_dat <-
  # [SQL] the filter below is now also done inside SQLite (same ids)
  read_cbs(file.path(loc$data_folder, loc$household_data),
           cols = c("RINPERSOONS", "RINPERSOON", "DATUMAANVANGHH",
                    "DATUMEINDEHH", "TYPHH"),
           filter_in = list(RINPERSOON = cohort_rinpersoons)) %>%
  filter(RINPERSOON %in% cohort_rinpersoons) %>%
  mutate(
    RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
    DATUMAANVANGHH = as.numeric(DATUMAANVANGHH),
    DATUMEINDEHH = as.numeric(DATUMEINDEHH),
    TYPHH = as_factor(TYPHH, levels = "value")
  ) %>%
  mutate(
    DATUMAANVANGHH = ymd(DATUMAANVANGHH),
    DATUMEINDEHH = ymd(DATUMEINDEHH)
  ) 


# keep the first known record that we observe for the child
hh_tab <- 
  household_dat %>%
  group_by(RINPERSOONS, RINPERSOON) %>% 
  filter(row_number() == 1)

# create type household outcome for child
hh_tab <- 
  hh_tab %>%
  mutate(type_household = ifelse(TYPHH %in% "6", "Single Parent", "Other"),
         type_household = ifelse(TYPHH %in% c("2", "3", "4", "5"), "Two Parents", type_household)) %>%
  select(-c(TYPHH, DATUMAANVANGHH, DATUMEINDEHH))


# if not observed use household type of the mother
hh_tab_mom <- 
  inner_join(household_dat, 
             cohort_dat %>% select(c(CBKSoortNr_Moeder, Rinpersoon_Moeder, birthdate)), 
             by = c("RINPERSOONS" = "CBKSoortNr_Moeder",
                    "RINPERSOON" = "Rinpersoon_Moeder"), 
             relationship = "many-to-many") %>%
  filter(birthdate %within% interval(DATUMAANVANGHH, 
                                     DATUMEINDEHH)) %>%
  group_by(RINPERSOONS, RINPERSOON) %>% 
  summarise(TYPHH_mom = TYPHH[1])


# create type household outcome for mother
hh_tab_mom <- 
  hh_tab_mom %>%
  mutate(type_household_mom = ifelse(TYPHH_mom %in% "6", "Single Parent", "Other"),
         type_household_mom = ifelse(TYPHH_mom %in% c("2", "3", "4", "5"), 
                              "Two Parents", type_household_mom)) %>%
  select(-c(TYPHH_mom))


# join to cohort
cohort_dat <- 
  cohort_dat %>%
  left_join(hh_tab, by = c("RINPERSOONS", "RINPERSOON"))


cohort_dat <- 
  cohort_dat %>%
  left_join(hh_tab_mom, by = c("CBKSoortNr_Moeder" = "RINPERSOONS",
                               "Rinpersoon_Moeder" = "RINPERSOON"))


# if child type_household is NA use type_household_mom
cohort_dat <-
  cohort_dat %>%
  mutate(
    type_household = ifelse(is.na(type_household), type_household_mom, type_household),
    type_household = as.factor(type_household)     
  )  %>%
  select(-type_household_mom) %>%
  filter(!is.na(type_household))


rm(household_dat, hh_tab, hh_tab_mom)



#### EDUCATION PARENTS ####

get_hoogstopl_filename <- function(year) {
  # get all HOOGSTEOPLTAB files with the specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "Onderwijs/HOOGSTEOPLTAB", year),
    pattern = "(?i)(.sav)", 
    full.names = TRUE,
    ignore.case = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


# load data for linking education numbers to education levels
edu_link <- read_sav(loc$opleiding_data) %>%
  select(OPLNR, CTO2021V) %>%
  left_join(read_sav(loc$cto_data) %>%
              select(CTO, OPLNIVSOI2016AGG4HB), by = c("CTO2021V" = "CTO")) 

parents_education <- tibble(RINPERSOONS = factor(), RINPERSOON = character(),
                            education = integer(), year = integer())
for (year in seq((as.integer(format(dmy(cfg$child_birth_date_min), "%Y")) + cfg$childhood_home_age),
                 (as.integer(format(dmy(cfg$child_birth_date_max), "%Y")) + cfg$childhood_home_age))) {
  
  if (year < 2013) {
    parents_education <- read_sav(get_hoogstopl_filename(year), 
                                  col_select = c("RINPERSOONS", "RINPERSOON", 
                                                 "OPLNRHB")) %>%
      rename(education_number = OPLNRHB) %>%
      mutate(
        RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
        education_number = ifelse(education_number == "----", NA, education_number),
        year = year
      ) %>%
      # add education numbers to education data
      left_join(edu_link, by = c("education_number" = "OPLNR")) %>%
      select(RINPERSOONS, RINPERSOON, OPLNIVSOI2016AGG4HB, year) %>%
      rename(education = OPLNIVSOI2016AGG4HB) %>%
      mutate(education = ifelse(education == "----", NA, education), 
             education = as.numeric(education)) %>%
      # add to data 
      bind_rows(parents_education, .)
    
  }  else if (year >= 2013) {
    parents_education <- read_sav(get_hoogstopl_filename(year), 
                                  col_select = c("RINPERSOONS", "RINPERSOON", 
                                                 "OPLNIVSOI2016AGG4HBMETNIRWO")) %>%
      rename(education = OPLNIVSOI2016AGG4HBMETNIRWO) %>%
      mutate(
        RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
        education = ifelse(education == "----", NA, education),
        education = as.numeric(education),
        year = year
      ) %>%
      # add to data 
      bind_rows(parents_education, .)
  }
}

# create parents education variable
parents_education <- 
  parents_education %>%
  mutate(
    parents_edu = ifelse(education %in% c(3110, 3111, 3112, 3210, 3211), "Higher Professional Education", "Other"), 
    parents_edu = ifelse(education %in% c(3113, 3212, 3213), "University", parents_edu)
  ) %>%
  select(-education)


# determine parents education of child at a specific age
cohort_dat <- 
  cohort_dat %>%
  mutate(year = as.numeric(format(birthdate, "%Y")) + cfg$childhood_home_age)


# add parents education to cohort
cohort_dat <- 
  cohort_dat %>% 
  left_join(parents_education,
            by = c("Rinpersoon_Moeder" = "RINPERSOON", "CBKSoortNr_Moeder" = "RINPERSOONS", "year")) %>%
  select(-year)


# convert NA to other category
cohort_dat <-
  cohort_dat %>%
  mutate(parents_edu = ifelse(is.na(parents_edu), "Other", parents_edu)) %>%
  rename('parents_education' = 'parents_edu')


rm(parents_education, edu_link)

# record sample size
sample_size <- sample_size %>% mutate(n_3_parent_characteristics = nrow(cohort_dat))

#### WRITE OUTPUT TO TEMP ####
write_rds(cohort_dat, file.path(loc$temp_folder, "02_predictors.rds"))

write_rds(sample_size, file.path(loc$temp_folder, "02_sample_size.rds"))
