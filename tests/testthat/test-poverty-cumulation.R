# household_below_poverty and the cumulation outcomes (src/lib/P_2_1_poverty_cumulation.R),
# used by 03_outcomes_age35 for age35, age45 and age55. Toy files, no CBS data needed.
root <- file.path("..", "..")
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})
source(file.path(root, "src", "lib", "sql_helpers.R"))
source(file.path(root, "src", "lib", "P_2_1_poverty_cumulation.R"))

rins <- function(x) labelled(x, c("RINPERSOON WEL IN GBA" = "R"))

test_that("INHARMOEDE coding: 1-99 poor, 100-500/991/992 not poor, 998/999 missing", {
  expect_equal(poverty_from_inharmoede(c(1, 99, 100, 500, 991, 992, 998, 999)),
               c(1, 1, 0, 0, 0, 0, NA, NA))
  expect_equal(poverty_from_inharmoede(labelled(c(50, 998), c(student = 998))), c(1, NA))
})

test_that("add_household_poverty uses the household of the person's own outcome year", {
  d <- tempfile(); dir.create(d)
  # koppel: a and b share household h1 in 2021; c is in h2 (student) in 2022; in 2022 a
  # is in a poor household h3, but a's outcome year is 2021; d has no household
  koppel <- list(
    `2021` = tibble(RINPERSOONS = rins(c("R", "R")), RINPERSOON = c("a", "b"),
                    RINPERSOONSHKW = rins(c("R", "R")), RINPERSOONHKW = c("h1", "h1")),
    `2022` = tibble(RINPERSOONS = rins(c("R", "R")), RINPERSOON = c("a", "c"),
                    RINPERSOONSHKW = rins(c("R", "R")), RINPERSOONHKW = c("h3", "h2")))
  inharma <- list(
    `2021` = tibble(RINPERSOONSHKW = rins("R"), RINPERSOONHKW = "h1", INHARMOEDE = 80),
    `2022` = tibble(RINPERSOONSHKW = rins(c("R", "R")), RINPERSOONHKW = c("h2", "h3"),
                    INHARMOEDE = c(998, 40)))
  for (y in names(koppel)) {
    write_sav(koppel[[y]], file.path(d, paste0("K", y, ".sav")))
    write_sav(inharma[[y]], file.path(d, paste0("P", y, ".sav")))
  }
  cohort <- tibble(RINPERSOONS = factor("R"), RINPERSOON = c("a", "b", "c", "d"),
                   year = c(2021, 2021, 2022, 2022))
  out <- add_household_poverty(cohort,
                               koppel_file  = function(y) file.path(d, paste0("K", y, ".sav")),
                               poverty_file = function(y) file.path(d, paste0("P", y, ".sav")))
  expect_equal(out$RINPERSOON, cohort$RINPERSOON)
  expect_equal(out$household_below_poverty, c(1, 1, NA, NA))
  expect_error(add_household_poverty(tibble(RINPERSOONS = "R", RINPERSOON = "x", year = 2017),
                                     identity, identity), "starts in 2018")
})

test_that("cumulation: at least the two indicators; NA only when it depends on poverty", {
  g <- expand.grid(hbo_attained = 0:1, household_below_poverty = c(0, 1, NA), mhc = 0:1)
  out <- add_cumulation(g)
  no_educ <- g$hbo_attained == 0
  expect_equal(out$cum_educ_mhc, as.integer(no_educ & g$mhc == 1))
  expect_equal(out$cum_educ_poverty,
               ifelse(!no_educ, 0L, ifelse(is.na(g$household_below_poverty), NA,
                                           as.integer(g$household_below_poverty == 1))))
  # all three: 1 only if all three, 0 as soon as a known indicator is 0
  expect_equal(out$cum_educ_poverty_mhc[no_educ & g$mhc == 1 & g$household_below_poverty %in% 1], 1L)
  expect_true(all(out$cum_educ_poverty_mhc[!no_educ | g$mhc == 0] == 0))
  # "at least": educ & mhc does not care about poverty
  expect_equal(out$cum_educ_mhc[no_educ & g$mhc == 1], rep(1L, 3))
  expect_error(add_cumulation(g[, c("hbo_attained", "mhc")]), "poverty")
})

test_that("configs that compute cumulation also compute its inputs and have the INHARMATAB path", {
  for (f in list.files(file.path(root, "config"), "\\.yml$", full.names = TRUE)) {
    b <- config::get("data_preparation", file = f)$outcome_blocks
    if (!is.null(b) && "cumulation" %in% b) {
      expect_true(all(c("education", "poverty", "health") %in% b), info = f)
    }
    if (!is.null(b) && "poverty" %in% b) {
      expect_false(is.null(config::get("file_locations", file = f)$poverty_data), info = f)
    }
  }
})

test_that("the mhc bug is fixed: specialist users are no longer set to NA", {
  for (f in c("src/age21/P_2_0_03_outcomes_age21.R", "src/age35/P_2_0_03_outcomes_age35.R")) {
    code <- readLines(file.path(root, f))
    expect_false(any(grepl("is.na(basic_mhc) | (specialist_mhc)", code, fixed = TRUE)), info = f)
  }
})
