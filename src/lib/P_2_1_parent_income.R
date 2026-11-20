# Parental household income per child, and its percentile rank against a
# reference population. Used by the shared newborn predictor step
# (src/newborns/P_2_1_02_predictors_newborns.R), which the newborn cohorts and
# the JGZ cohort run.
#
# - cpi_to_reference_year(): CPI table rescaled so the reference year
#   (referentiejaar, cpi_base_year in the configs; 2024 in S_2) = 100, and a stop
#   when a year that is needed is missing (a missing year would otherwise give
#   NA incomes that are silently dropped).
# - child_income_window(): the mother's household income, already converted
#   per year to the price level of the reference year, averaged over a window of
#   years relative to an anchor year per child (the birth year for the newborns;
#   the year the child reaches the outcome age for JGZ).
# - income_percentile(): percentile rank within groups (birth year, or the year
#   the outcome age is reached), among the cohort itself or among a reference
#   population that contains the cohort (JGZ: all children in NL).
# - reference_children(): that national reference population from GBA and
#   KINDOUDER.
#
# Decided with Bastian (20-11-2026, living calendar): "Inkomen ouders (gebruik
# heel Nederland): middel (zet jaarbedragen voorafgaand aan het middelen om in
# het prijspeil van het referentiejaar (in S_2 is dit 2024)) over 5 jaar tot en
# met kalenderjaar waarin kind de uitkomstleeftijd heeft bereikt, geef rank
# binnen kinderen die in hetzelfde jaar de uitkomstleeftijd bereikten."
#
# All functions work on plain tibbles (or small files), so they are tested
# without CBS data in tests/testthat/test-parent-income.R.
#
# (c) Netherlands Opportunity Lab 2026


#' CPI table with the reference year = 100.
#'
#' @param cpi_tab tibble with columns year and cpi (any base, e.g. 2015 = 100).
#' @param reference_year the year whose price level all amounts are converted to.
#' @param years the years that will be converted; all must be in the table.
#' @return cpi_tab with cpi rescaled so cpi[reference_year] = 100. Divide an
#'   amount of year t by cpi[t] / 100 to express it in reference-year euros.
cpi_to_reference_year <- function(cpi_tab, reference_year, years = NULL) {
  cpi_tab <- cpi_tab %>%
    mutate(year = as.integer(year), cpi = as.numeric(cpi)) %>%
    filter(!is.na(year), !is.na(cpi))
  needed  <- unique(as.integer(c(reference_year, years)))
  missing <- setdiff(needed, cpi_tab$year)
  if (length(missing) > 0) {
    stop("The CPI table (cpi_index_data) has no value for: ", paste(sort(missing), collapse = ", "),
         ". Update the CPI file (CBS StatLine, Consumentenprijzen; prijsindex 2015=100, jaarcijfers) ",
         "so it covers the reference year and every income year.")
  }
  if (anyDuplicated(cpi_tab$year) > 0) stop("The CPI table has more than one value for a year.")
  ref_cpi <- cpi_tab$cpi[cpi_tab$year == reference_year]
  cpi_tab %>% mutate(cpi = cpi / ref_cpi * 100)
}


#' Mother's household income averaged over a window of years per child.
#'
#' @param children tibble with one row per child: the key columns in `key`,
#'   CBKSoortNr_Moeder, Rinpersoon_Moeder and the `anchor` column.
#' @param income_household tibble with RINPERSOONS, RINPERSOON (the mother),
#'   year and household_income, already converted per year to reference-year
#'   prices (convert BEFORE averaging), one record per mother per year.
#' @param offsets years relative to the anchor year that make up the window,
#'   e.g. c(-1, 0) with anchor birth_year (newborns), or -4:0 with anchor the
#'   year the outcome age is reached (JGZ: 5 years up to and including it).
#' @param min_years minimum number of years with a known income in the window.
#' @param anchor column with the anchor year per child.
#' @param key column(s) that identify a child in `children`.
#' @return `children` (only those with an income) with household_income (mean
#'   over the known years in the window; children with a negative mean are
#'   dropped, as before) and household_income_n (number of years used).
child_income_window <- function(children, income_household, offsets = c(-1, 0),
                                min_years = 1, anchor = "birth_year",
                                key = c("RINPERSOONS", "RINPERSOON")) {
  offsets <- sort(unique(as.integer(offsets)))
  if (length(offsets) == 0) stop("The income window is empty.")
  missing_cols <- setdiff(c(key, "CBKSoortNr_Moeder", "Rinpersoon_Moeder", anchor), names(children))
  if (length(missing_cols) > 0) stop("children lacks columns: ", paste(missing_cols, collapse = ", "))
  if (anyDuplicated(children[key]) > 0) stop("children has more than one row per child (key: ",
                                              paste(key, collapse = ", "), ").")

  income_household <- income_household %>%
    mutate(RINPERSOONS = as.character(RINPERSOONS), RINPERSOON = as.character(RINPERSOON))

  window_income <-
    children %>%
    select(all_of(c(key, anchor)), CBKSoortNr_Moeder, Rinpersoon_Moeder) %>%
    mutate(CBKSoortNr_Moeder = as.character(CBKSoortNr_Moeder),
           Rinpersoon_Moeder = as.character(Rinpersoon_Moeder),
           .anchor = as.integer(.data[[anchor]])) %>%
    tidyr::crossing(income_offset = offsets) %>%
    mutate(year = .anchor + income_offset) %>%
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


#' Percentile rank of household income within groups.
#'
#' @param cohort tibble with `key`, `by` and `value` (one row per child).
#' @param reference NULL to rank within the cohort itself, or a tibble with the
#'   same columns: the population to rank against. Cohort children that are not
#'   in the reference are added to it, so every cohort child is ranked against
#'   the reference plus itself (normally the cohort is part of the reference).
#' @param by the grouping column: birth_year (newborns) or the year the outcome
#'   age is reached (JGZ).
#' @return `cohort` with a column `<value>_perc` in (0, 1]: rank (ties averaged)
#'   divided by the highest rank within the group, as in the other cohorts.
income_percentile <- function(cohort, reference = NULL, value = "household_income",
                              by = "birth_year", key = c("RINPERSOONS", "RINPERSOON")) {
  perc_col <- paste0(value, "_perc")
  cols <- c(key, by, value)
  as_chr_keys <- function(d) d %>% mutate(across(all_of(key), as.character))

  pool <- cohort %>% select(all_of(cols)) %>% as_chr_keys()
  if (!is.null(reference)) {
    # cohort first, so a child in both keeps the cohort's value (they should be equal)
    pool <- bind_rows(pool, reference %>% select(all_of(cols)) %>% as_chr_keys()) %>%
      distinct(across(all_of(key)), .keep_all = TRUE)
  }

  ranked <- pool %>%
    filter(!is.na(.data[[value]])) %>%
    group_by(across(all_of(by))) %>%
    mutate(.rank = rank(.data[[value]], ties.method = "average"),
           !!perc_col := .rank / max(.rank)) %>%
    ungroup() %>%
    select(all_of(c(key, perc_col)))

  key_types <- lapply(cohort[key], class)
  out <- cohort %>% mutate(.row = row_number()) %>% as_chr_keys() %>%
    left_join(ranked, by = key) %>% arrange(.row) %>% select(-.row)
  # give the key columns back their original type (e.g. factor RINPERSOONS)
  for (k in key) if (!identical(class(out[[k]]), key_types[[k]])) out[[k]] <- cohort[[k]]
  out
}


#' The year in which a child reaches an age, given the age in days.
outcome_age_year <- function(birthdate, age_days) {
  as.integer(format(as.Date(birthdate) + as.integer(age_days), "%Y"))
}


#' Reference children for a national income rank: everyone in GBAPERSOON born
#' in `birth_years` with a known mother in KINDOUDERTAB who is not a "G" person
#' (the same rule as the JGZ cohort in src/jgz/01d_cohort_jgz.R).
#'
#' @param gba_path,kindouder_path full paths to GBAPERSOONTAB and KINDOUDERTAB
#'   (.db or .sav, read with read_cbs() from src/lib/sql_helpers.R).
#' @return tibble RINPERSOONS, RINPERSOON, birthdate, birth_year,
#'   CBKSoortNr_Moeder, Rinpersoon_Moeder.
reference_children <- function(gba_path, kindouder_path, birth_years) {
  chr <- function(x) trimws(as.character(haven::zap_labels(x)))
  birth_years <- sort(unique(as.integer(birth_years)))

  gba <- read_cbs(gba_path, cols = c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTEJAAR",
                                     "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG"),
                  filter_in = list(GBAGEBOORTEJAAR = as.character(birth_years)))
  names(gba) <- toupper(names(gba))
  gba <- gba %>%
    transmute(RINPERSOONS = chr(RINPERSOONS), RINPERSOON = chr(RINPERSOON),
              birth_year = suppressWarnings(as.integer(chr(GBAGEBOORTEJAAR))),
              birthdate  = suppressWarnings(as.Date(sprintf(
                "%04d-%02d-%02d", birth_year,
                as.integer(chr(GBAGEBOORTEMAAND)), as.integer(chr(GBAGEBOORTEDAG)))))) %>%
    filter(birth_year %in% birth_years, !is.na(birthdate)) %>%
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


#' Parental income per outcome age (JGZ, 20-11-2026).
#'
#' For each age a (in days): the year Y_a in which the child reaches a; the
#' mother's household income averaged over Y_a - (window_years - 1) .. Y_a;
#' its percentile among all children (cohort, or reference + cohort) who reach
#' a in the same year Y_a. Only computed when the whole window lies within the
#' available income years; otherwise NA for that child and age.
#'
#' @param cohort children (key, mother ids, birthdate).
#' @param reference NULL or reference children (same columns).
#' @param ages_days named vector: age label (e.g. "m24") -> age in days.
#' @param income_years c(first, last) available income year.
#' @return tibble with `key` and, per age label a, income_parents_<a> and
#'   income_parents_perc_<a>; only children with an income at one age or more.
income_by_outcome_age <- function(cohort, reference, income_household, ages_days,
                                  window_years = 5, min_years = 1, income_years,
                                  key = c("RINPERSOONS", "RINPERSOON")) {
  offsets <- seq(-(window_years - 1), 0)
  out <- cohort %>% select(all_of(key))
  for (a in names(ages_days)) {
    add_year <- function(d) {
      if (is.null(d)) return(NULL)
      d %>% mutate(outcome_year = outcome_age_year(birthdate, ages_days[[a]])) %>%
        filter(outcome_year + min(offsets) >= income_years[1], outcome_year <= income_years[2])
    }
    coh_a <- add_year(cohort)
    if (nrow(coh_a) == 0) {
      # keep the columns, so every age has them (all NA)
      message(sprintf("  %-5s no children with a complete income window", a))
      out[[paste0("income_parents_", a)]] <- NA_real_
      out[[paste0("income_parents_perc_", a)]] <- NA_real_
      next
    }
    ref_a <- add_year(reference)
    if (!is.null(ref_a)) ref_a <- ref_a %>% filter(outcome_year %in% unique(coh_a$outcome_year))

    inc_coh <- child_income_window(coh_a, income_household, offsets, min_years,
                                   anchor = "outcome_year", key = key)
    inc_ref <- if (is.null(ref_a)) NULL else
      child_income_window(ref_a, income_household, offsets, min_years,
                          anchor = "outcome_year", key = key)
    ranked <- income_percentile(inc_coh, inc_ref, value = "household_income",
                                by = "outcome_year", key = key)
    message(sprintf("  %-5s years %d-%d: %s children with an income (reference: %s)",
                    a, min(inc_coh$outcome_year), max(inc_coh$outcome_year),
                    format(nrow(inc_coh), big.mark = ","),
                    if (is.null(inc_ref)) "cohort" else format(nrow(inc_ref), big.mark = ",")))
    out <- out %>%
      left_join(ranked %>%
                  select(all_of(key),
                         !!paste0("income_parents_", a)      := household_income,
                         !!paste0("income_parents_perc_", a) := household_income_perc),
                by = key)
  }
  inc_cols <- grep("^income_parents_", names(out), value = TRUE)
  out %>% filter(if_any(all_of(inc_cols), ~ !is.na(.x)))
}
