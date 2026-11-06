# Household poverty and the cumulation outcomes, used by src/age35/03_outcomes_age35.R
# (so by age35, age45_current_address and age55_current_address).
#
# household_below_poverty: the person's household is below the poverty line of CBS,
# Nibud and SCP (INHARMATAB) in the outcome year (one year only). From INHARMOEDE
# (household income as % of the poverty line):
#   1-99            -> 1 (poor)
#   100-500, 991    -> 0 (991 = income below the line but wealth above it; CBS does not
#                         count this as poor), also 992
#   998 (students), 999 (institutional household or no observation) -> NA
# People without a household in KOPPELPERSOONHUISHOUDEN that year are NA too.
# INHARMATAB starts in 2018.
#
# Cumulation (accumulation of disadvantage), from three indicators:
#   1. no higher education: hbo_attained == 0 (no hbo or wo diploma)
#   2. below the poverty line: household_below_poverty == 1
#   3. mental health care: mhc == 1 (basic or specialist GGZ)
# cum_educ_poverty, cum_poverty_mhc, cum_educ_mhc = 1 if the person has AT LEAST those
# two (whatever the third); cum_educ_poverty_mhc = 1 if all three. NA only when the
# answer depends on a missing poverty value (1 & NA = NA, but 0 & NA = 0).
# Timing and missing education: see README, "Cumulation".
#
# Needs dplyr and haven (called as haven::, so forcats cannot mask them) and
# read_cbs() from src/lib/sql_helpers.R.

#' INHARMOEDE -> 1 (below the poverty line), 0 (not), NA (students, institutions).
poverty_from_inharmoede <- function(inharmoede) {
  x <- as.numeric(haven::zap_labels(inharmoede))
  dplyr::case_when(x %in% c(998, 999) ~ NA_real_,
                   x < 100 ~ 1,
                   TRUE ~ 0)
}

#' Add household_below_poverty for the outcome year (column `year`) of each person.
#'
#' @param cohort_dat   data frame with RINPERSOONS, RINPERSOON and year (the outcome year)
#' @param koppel_file  function(year) -> path of KOPPELPERSOONHUISHOUDEN of that year
#' @param poverty_file function(year) -> path of INHARMATAB of that year
add_household_poverty <- function(cohort_dat, koppel_file, poverty_file) {
  poverty_tab <- NULL
  for (yr in sort(unique(cohort_dat$year))) {
    if (yr < 2018) stop("INHARMATAB starts in 2018; there is no poverty data for ", yr)
    persons <- cohort_dat$RINPERSOON[cohort_dat$year == yr]

    # person -> household (hoofdkostwinner) in that year
    koppel_hh <- read_cbs(koppel_file(yr),
                          cols = c("RINPERSOONS", "RINPERSOON", "RINPERSOONSHKW", "RINPERSOONHKW"),
                          filter_in = list(RINPERSOON = persons)) %>%
      mutate(RINPERSOONS = haven::as_factor(RINPERSOONS, levels = "values"),
             RINPERSOONSHKW = haven::as_factor(RINPERSOONSHKW, levels = "values"))

    poverty_yr <- read_cbs(poverty_file(yr),
                           cols = c("RINPERSOONSHKW", "RINPERSOONHKW", "INHARMOEDE"),
                           filter_in = list(RINPERSOONHKW = koppel_hh$RINPERSOONHKW)) %>%
      mutate(RINPERSOONSHKW = haven::as_factor(RINPERSOONSHKW, levels = "values"),
             household_below_poverty = poverty_from_inharmoede(INHARMOEDE)) %>%
      inner_join(koppel_hh, by = c("RINPERSOONSHKW", "RINPERSOONHKW")) %>%
      transmute(RINPERSOONS, RINPERSOON, household_below_poverty, year = yr)

    poverty_tab <- bind_rows(poverty_tab, poverty_yr)
  }

  # one row per person (a person should be in one household per year)
  n_dup <- sum(duplicated(poverty_tab[, c("RINPERSOONS", "RINPERSOON")]))
  if (n_dup > 0) message("Poverty: ", n_dup, " people linked to more than one household; the first is kept")
  poverty_tab <- distinct(poverty_tab, RINPERSOONS, RINPERSOON, .keep_all = TRUE)

  # match RINPERSOONS on its values, whatever its type in cohort_dat
  poverty_tab$RINPERSOONS <- as.character(poverty_tab$RINPERSOONS)
  out <- cohort_dat %>%
    mutate(.rins_chr = as.character(RINPERSOONS)) %>%
    left_join(poverty_tab %>% rename(.rins_chr = RINPERSOONS),
              by = c(".rins_chr", "RINPERSOON", "year")) %>%
    select(-.rins_chr)
  out
}

#' Add the four cumulation outcomes (see the header of this file).
add_cumulation <- function(cohort_dat) {
  needed <- c("hbo_attained", "household_below_poverty", "mhc")
  if (!all(needed %in% names(cohort_dat))) {
    stop("The cumulation block needs the blocks education, poverty and health; missing: ",
         paste(setdiff(needed, names(cohort_dat)), collapse = ", "))
  }
  no_higher_educ <- cohort_dat$hbo_attained == 0
  below_poverty  <- cohort_dat$household_below_poverty == 1
  uses_mhc       <- cohort_dat$mhc == 1
  cohort_dat$cum_educ_poverty     <- as.integer(no_higher_educ & below_poverty)
  cohort_dat$cum_poverty_mhc      <- as.integer(below_poverty & uses_mhc)
  cohort_dat$cum_educ_mhc         <- as.integer(no_higher_educ & uses_mhc)
  cohort_dat$cum_educ_poverty_mhc <- as.integer(no_higher_educ & below_poverty & uses_mhc)
  cohort_dat
}
