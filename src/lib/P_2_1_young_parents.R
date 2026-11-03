# Young parenthood: did a person become a parent before a given age?
#
# Shared by the outcome scripts of age21 (c21_young_parents), age35
# (c35_young_parents) and, through src/age35/03_outcomes_age35.R, age45_current_address
# and age55_current_address (c45_/c55_young_parents). The code is the block that
# used to be in src/age21/03_outcomes_age21.R; the result for age21 is unchanged.
#
# Definition: in KINDOUDER, take every child of whom the person is the father or
# the mother, compute the person's age at the birth of the first child, and set
# young_parents = 1 if that age is below `max_age` (20). People without a child in
# KINDOUDER get 0. Fathers and mothers both count; the sex filter of the analysis
# gives young motherhood.
#
# Known limitation (see README, "Young parents"): a child only counts if the child
# and BOTH its parents are in the GBA (RINPERSOONS == "R" for all three). A child
# with an unknown or non-GBA other parent does not count for the person, so young
# parenthood is probably underestimated, especially for young single mothers.
# Kept as it is for now.
#
# Needs dplyr and lubridate (loaded by the outcome scripts), haven (called as
# haven::as_factor, so forcats::as_factor cannot mask it) and read_cbs()
# from src/lib/sql_helpers.R.

#' Add `young_parents` (0/1) to the cohort.
#'
#' @param cohort_dat data frame with RINPERSOONS and RINPERSOON (one row per person)
#' @param kind_path  full path to KINDOUDER (.sav or converted .db)
#' @param gba_path   full path to GBAPERSOON (.sav or converted .db)
#' @param max_age    a person is a young parent if the first child was born before this age
#' @return cohort_dat with the column young_parents
add_young_parents <- function(cohort_dat, kind_path, gba_path, max_age = 20) {

  # children of whom a cohort member is the mother or the father
  # [SQL] only rows where the mother or the father is in the cohort (the first condition of
  #       the filter below, now also done when reading); XKOPPELNUMMER is dropped afterwards
  kindouder_dat <- read_cbs(kind_path,
                            filter_in = list(RINPERSOONMa = cohort_dat$RINPERSOON,
                                             RINPERSOONpa = cohort_dat$RINPERSOON),
                            match = "any") %>%
    select(-XKOPPELNUMMER) %>%
    haven::as_factor(only_labelled = TRUE, levels = "values") %>%
    filter((RINPERSOONMa %in% cohort_dat$RINPERSOON | RINPERSOONpa %in% cohort_dat$RINPERSOON),
           RINPERSOONS == "R", RINPERSOONSMa == "R", RINPERSOONSpa == "R") %>%
    rename(RINPERSOONS_infant = RINPERSOONS,
           RINPERSOON_infant = RINPERSOON)

  # birth dates of the children and their parents
  # [SQL] only the infants and their parents (gba_dat is only joined on those three ids)
  gba_dat <-
    read_cbs(gba_path, cols = c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTEJAAR",
                                "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG"),
             filter_in = list(RINPERSOON = c(kindouder_dat$RINPERSOON_infant,
                                             kindouder_dat$RINPERSOONMa, kindouder_dat$RINPERSOONpa))) %>%
    mutate(RINPERSOONS = haven::as_factor(RINPERSOONS, levels = "values"),
           birthdate = dmy(paste(GBAGEBOORTEDAG, GBAGEBOORTEMAAND, GBAGEBOORTEJAAR, sep = "-"))) %>%
    select(-c(GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG))

  # add the child's birth date
  kindouder_dat <-
    kindouder_dat %>%
    left_join(gba_dat, by = c("RINPERSOONS_infant" = "RINPERSOONS",
                              "RINPERSOON_infant" = "RINPERSOON")) %>%
    rename(birthdate_infant = birthdate) %>%
    select(-c(RINPERSOONS_infant, RINPERSOON_infant))

  # add the parents' birth dates
  kindouder_dat <-
    kindouder_dat %>%
    left_join(gba_dat, by = c("RINPERSOONSMa" = "RINPERSOONS",
                              "RINPERSOONMa" = "RINPERSOON")) %>%
    rename(birthdate_ma = birthdate) %>%
    left_join(gba_dat, by = c("RINPERSOONSpa" = "RINPERSOONS",
                              "RINPERSOONpa" = "RINPERSOON")) %>%
    rename(birthdate_pa = birthdate)

  # one long table: each parent, their birth date and the birth date of their child
  kindvader_dat <-
    kindouder_dat %>%
    select(RINPERSOONSpa, RINPERSOONpa, birthdate_pa, birthdate_infant) %>%
    rename(RINPERSOONS = RINPERSOONSpa, RINPERSOON = RINPERSOONpa,
           birthdate_parent = birthdate_pa)
  kindmoeder_dat <-
    kindouder_dat %>%
    select(RINPERSOONSMa, RINPERSOONMa, birthdate_ma, birthdate_infant) %>%
    rename(RINPERSOONS = RINPERSOONSMa, RINPERSOON = RINPERSOONMa,
           birthdate_parent = birthdate_ma)
  kindouder_dat <- rbind(kindvader_dat, kindmoeder_dat)

  # age at which the person became a parent (first child)
  kindouder_dat <-
    kindouder_dat %>%
    mutate(age_at_birth_parent = interval(birthdate_parent, birthdate_infant) / years(1)) %>%
    select(-c(birthdate_infant, birthdate_parent)) %>%
    arrange(RINPERSOON, age_at_birth_parent) %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    summarize(age_at_birth_parent = age_at_birth_parent[1], .groups = "drop_last")

  # young parent outcome; 0 if no child
  cohort_dat %>%
    left_join(kindouder_dat, by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(young_parents = ifelse(age_at_birth_parent < max_age, 1, 0),
           young_parents = ifelse(is.na(young_parents), 0, young_parents)) %>%
    select(-age_at_birth_parent)
}
