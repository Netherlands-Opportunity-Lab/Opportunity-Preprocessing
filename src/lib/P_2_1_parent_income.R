# Parental household income per child, and its percentile rank against a
# reference population. Used by the shared newborn predictor step
# (src/newborns/P_2_1_02_predictors_newborns.R), which the newborn cohorts and
# the JGZ cohort run.
#
# - child_income_window(): the mother's household income averaged over a window
#   of years around the child's birth year (income_year_offsets in the config;
#   newborns: the year before birth and the birth year).
# - income_percentile(): percentile rank within birth year, either among the
#   cohort itself (newborns) or among a reference population that contains the
#   cohort (JGZ: all children in NL born in the same year, decided 20-11-2026).
#
# Both functions work on plain tibbles, so they are tested without CBS data in
# tests/testthat/test-parent-income.R.
#
# (c) Netherlands Opportunity Lab 2026


#' Mother's household income averaged over a window of years around birth.
#'
#' @param children tibble with one row per child: the key columns in `key`,
#'   CBKSoortNr_Moeder, Rinpersoon_Moeder and birth_year.
#' @param income_household tibble with RINPERSOONS, RINPERSOON (the mother),
#'   year and household_income, already deflated to the CPI base year (deflate
#'   BEFORE averaging) and with one record per mother per year.
#' @param offsets years relative to the birth year that make up the window,
#'   e.g. c(-1, 0) (newborns) or -1:3 (JGZ).
#' @param min_years minimum number of years with a known income in the window;
#'   children with fewer get no income.
#' @param key column(s) that identify a child in `children`.
#' @return `children` (only those with an income) with household_income (mean
#'   over the known years in the window; negative means set to missing and
#'   dropped, as before) and household_income_n (number of years used).
child_income_window <- function(children, income_household, offsets = c(-1, 0),
                                min_years = 1, key = c("RINPERSOONS", "RINPERSOON")) {
  offsets <- sort(unique(as.integer(offsets)))
  if (length(offsets) == 0) stop("income_year_offsets is empty.")
  missing_cols <- setdiff(c(key, "CBKSoortNr_Moeder", "Rinpersoon_Moeder", "birth_year"), names(children))
  if (length(missing_cols) > 0) stop("children lacks columns: ", paste(missing_cols, collapse = ", "))
  if (anyDuplicated(children[key]) > 0) stop("children has more than one row per child (key: ",
                                              paste(key, collapse = ", "), ").")

  income_household <- income_household %>%
    mutate(RINPERSOONS = as.character(RINPERSOONS), RINPERSOON = as.character(RINPERSOON))

  window_income <-
    children %>%
    select(all_of(key), CBKSoortNr_Moeder, Rinpersoon_Moeder, birth_year) %>%
    mutate(CBKSoortNr_Moeder = as.character(CBKSoortNr_Moeder),
           Rinpersoon_Moeder = as.character(Rinpersoon_Moeder)) %>%
    tidyr::crossing(income_offset = offsets) %>%
    mutate(year = birth_year + income_offset) %>%
    inner_join(income_household,
               by = c("CBKSoortNr_Moeder" = "RINPERSOONS",
                      "Rinpersoon_Moeder" = "RINPERSOON",
                      "year")) %>%
    group_by(across(all_of(key))) %>%
    summarize(household_income_n = sum(!is.na(household_income)),
              household_income   = mean(household_income, na.rm = TRUE),
              .groups = "drop") %>%
    # mean() of only NAs gives NaN, which is.na() also catches
    filter(!is.na(household_income), household_income_n >= min_years) %>%
    # a negative mean income counts as unknown (as in all cohorts)
    filter(household_income >= 0)

  children %>% inner_join(window_income, by = key)
}


#' Percentile rank of household income within birth year.
#'
#' @param cohort tibble with `key`, birth_year and `value` (one row per child).
#' @param reference NULL to rank within the cohort itself, or a tibble with the
#'   same columns: the population to rank against. Cohort children that are not
#'   in the reference are added to it, so every cohort child is ranked against
#'   the reference plus itself (normally the cohort is part of the reference).
#' @return `cohort` with a column `<value>_perc` in (0, 1]: rank (ties averaged)
#'   divided by the highest rank within birth year, as in the other cohorts.
income_percentile <- function(cohort, reference = NULL, value = "household_income",
                              key = c("RINPERSOONS", "RINPERSOON")) {
  perc_col <- paste0(value, "_perc")
  cols <- c(key, "birth_year", value)

  pool <- cohort %>% select(all_of(cols))
  if (!is.null(reference)) {
    # cohort first, so a child in both keeps the cohort's value (they should be equal)
    pool <- bind_rows(pool, reference %>% select(all_of(cols))) %>%
      distinct(across(all_of(key)), .keep_all = TRUE)
  }

  ranked <- pool %>%
    filter(!is.na(.data[[value]])) %>%
    group_by(birth_year) %>%
    mutate(.rank = rank(.data[[value]], ties.method = "average"),
           !!perc_col := .rank / max(.rank)) %>%
    ungroup() %>%
    select(all_of(c(key, perc_col)))

  cohort %>% left_join(ranked, by = key)
}


#' Reference children for a national income rank: everyone in GBAPERSOON born
#' in `birth_years` with a known mother in KINDOUDERTAB who is not a "G" person
#' (the same rule as the JGZ cohort in src/jgz/01d_cohort_jgz.R).
#'
#' @param gba_path,kindouder_path full paths to GBAPERSOONTAB and KINDOUDERTAB
#'   (.db or .sav, read with read_cbs() from src/lib/sql_helpers.R).
#' @return tibble RINPERSOONS, RINPERSOON, birth_year, CBKSoortNr_Moeder,
#'   Rinpersoon_Moeder (all character except birth_year).
reference_children <- function(gba_path, kindouder_path, birth_years) {
  chr <- function(x) trimws(as.character(haven::zap_labels(x)))
  birth_years <- sort(unique(as.integer(birth_years)))

  gba <- read_cbs(gba_path, cols = c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTEJAAR"),
                  filter_in = list(GBAGEBOORTEJAAR = as.character(birth_years)))
  names(gba) <- toupper(names(gba))
  gba <- gba %>%
    transmute(RINPERSOONS = chr(RINPERSOONS), RINPERSOON = chr(RINPERSOON),
              birth_year = suppressWarnings(as.integer(chr(GBAGEBOORTEJAAR)))) %>%
    filter(birth_year %in% birth_years) %>%
    distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE)
  if (nrow(gba) == 0) {
    stop("No children born in ", paste(range(birth_years), collapse = "-"), " found in ",
         basename(gba_path), ". Check the path and how GBAGEBOORTEJAAR is stored.")
  }

  kindouder <- read_cbs(kindouder_path, filter_in = list(RINPERSOON = gba$RINPERSOON))
  names(kindouder) <- toupper(names(kindouder))
  kindouder <- kindouder %>%
    transmute(RINPERSOONS = chr(RINPERSOONS), RINPERSOON = chr(RINPERSOON),
              CBKSoortNr_Moeder = chr(RINPERSOONSMA), Rinpersoon_Moeder = chr(RINPERSOONMA)) %>%
    distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE)

  gba %>%
    inner_join(kindouder, by = c("RINPERSOONS", "RINPERSOON")) %>%
    filter(!is.na(Rinpersoon_Moeder), Rinpersoon_Moeder != "",
           !is.na(CBKSoortNr_Moeder), CBKSoortNr_Moeder != "G")
}
