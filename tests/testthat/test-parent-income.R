# Parental income per child and its percentile rank (src/lib/P_2_1_parent_income.R),
# used by the shared newborn predictor step (newborn cohorts and JGZ). Toy data,
# no CBS files needed.
root <- file.path("..", "..")
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})
source(file.path(root, "src", "lib", "sql_helpers.R"))
source(file.path(root, "src", "lib", "P_2_1_parent_income.R"))

# mother m1 has two children born in different years; m2 one child; m3 only
# negative income; m4 no income records at all
toy_children <- tibble(
  RINPERSOONS = "R",
  RINPERSOON  = c("k1", "k2", "k3", "k4", "k5"),
  CBKSoortNr_Moeder = "R",
  Rinpersoon_Moeder = c("m1", "m1", "m2", "m3", "m4"),
  birth_year  = c(2010L, 2013L, 2010L, 2010L, 2010L))

toy_income <- tibble(
  RINPERSOONS = "R",
  RINPERSOON  = c(rep("m1", 7), rep("m2", 2), "m3", "m3"),
  year        = c(2009:2015, 2010L, 2011L, 2009L, 2010L),
  household_income = c(10, 20, 30, 40, 50, 60, 70,   100, NA,   -5, -5))

test_that("child_income_window: default window is the year before birth and the birth year", {
  out <- child_income_window(toy_children, toy_income)
  expect_equal(out$RINPERSOON, c("k1", "k2", "k3"))
  # k1: 2009-2010 of m1; k2: 2012-2013 of m1 (per child, not per mother); k3: only 2010 known
  expect_equal(out$household_income, c(15, 45, 100))
  expect_equal(out$household_income_n, c(2L, 2L, 1L))
})

test_that("child_income_window: a 5-year window, minimum years, negative means dropped", {
  out <- child_income_window(toy_children, toy_income, offsets = -1:3)
  # k1: 2009-2013 = 10..50; k2: 2012-2016, only 2012-2015 known = 40..70
  expect_equal(out$household_income[out$RINPERSOON == "k1"], 30)
  expect_equal(out$household_income[out$RINPERSOON == "k2"], 55)
  expect_equal(out$household_income_n[out$RINPERSOON == "k2"], 4L)
  expect_false("k4" %in% out$RINPERSOON)   # negative mean
  expect_false("k5" %in% out$RINPERSOON)   # no income at all
  out3 <- child_income_window(toy_children, toy_income, offsets = -1:3, min_years = 3)
  expect_false("k3" %in% out3$RINPERSOON)  # only 1 known year
  expect_true(all(c("k1", "k2") %in% out3$RINPERSOON))
})

test_that("child_income_window refuses duplicate children", {
  expect_error(child_income_window(bind_rows(toy_children, toy_children[1, ]), toy_income),
               "more than one row")
})

test_that("income_percentile within the cohort matches the old newborn code", {
  set.seed(1)
  cohort <- tibble(RINPERSOONS = factor("R"), RINPERSOON = sprintf("c%03d", 1:200),
                   birth_year = rep(2010:2011, 100),
                   household_income = round(rlnorm(200, 10, 0.5), -3))  # rounding gives ties
  old <- cohort %>%
    group_by(birth_year) %>%
    mutate(r = rank(household_income, ties.method = "average"),
           income_household_perc = r / max(r)) %>%
    ungroup()
  new <- income_percentile(cohort)
  expect_equal(new$household_income_perc, old$income_household_perc)
  expect_equal(nrow(new), nrow(cohort))
})

test_that("income_percentile against a reference: the national rank, not the cohort rank", {
  # reference: 100 children per birth year with incomes 1..100 (thousands)
  reference <- tibble(RINPERSOONS = "R",
                      RINPERSOON  = c(sprintf("n%03d", 1:100), sprintf("p%03d", 1:100)),
                      birth_year  = rep(c(2010L, 2011L), each = 100),
                      household_income = rep(1:100, 2) * 1000)
  # a rich cohort: its children are reference children 81-100 of 2010
  cohort <- reference %>% filter(birth_year == 2010L, household_income > 80000) %>%
    mutate(RINPERSOONS = factor(RINPERSOONS), extra = 1)
  out <- income_percentile(cohort, reference = reference)
  expect_equal(out$household_income_perc, (81:100) / 100)
  expect_true("extra" %in% names(out))
  # within the cohort the same children would span 0.05..1
  expect_equal(min(income_percentile(cohort)$household_income_perc), 1 / 20)
})

test_that("income_percentile adds cohort children that are missing from the reference", {
  reference <- tibble(RINPERSOONS = "R", RINPERSOON = sprintf("n%02d", 1:9),
                      birth_year = 2010L, household_income = (1:9) * 10)
  cohort <- tibble(RINPERSOONS = "R", RINPERSOON = "x", birth_year = 2010L, household_income = 55)
  out <- income_percentile(cohort, reference = reference)
  # x is 6th of 10
  expect_equal(out$household_income_perc, 0.6)
})

test_that("reference_children: everyone born in the years with a non-G mother, from .sav and .db", {
  d <- tempfile(); dir.create(d)
  rins <- function(x) labelled(x, c("RINPERSOON WEL IN GBA" = "R",
                                    "RINPERSOON NIET IN GBA, GEEN SOFINR" = "G"))
  gba <- tibble(RINPERSOONS = rins(rep("R", 6)),
                RINPERSOON = c("k1", "k2", "k3", "k4", "k5", "k6"),
                GBAGEBOORTEJAAR = c("2009", "2010", "2011", "2012", "2010", "2010"))
  kind <- tibble(RINPERSOONS = rins(rep("R", 5)),
                 RINPERSOON = c("k1", "k2", "k3", "k4", "k5"),
                 RINPERSOONSpa = rins(rep("R", 5)), RINPERSOONpa = "f",
                 RINPERSOONSMa = rins(c("R", "R", "R", "R", "G")),
                 RINPERSOONMa = c("m1", "m2", "m3", "m4", "m5"))
  write_sav(gba, file.path(d, "GBAPERSOON.sav"))
  write_sav(kind, file.path(d, "KINDOUDER.sav"))

  out <- reference_children(file.path(d, "GBAPERSOON.sav"), file.path(d, "KINDOUDER.sav"), 2010:2011)
  # k1 born 2009 and k4 born 2012 are outside; k5 has a G mother; k6 has no KINDOUDER row
  expect_equal(sort(out$RINPERSOON), c("k2", "k3"))
  expect_equal(out$birth_year[out$RINPERSOON == "k2"], 2010L)
  expect_equal(out$Rinpersoon_Moeder[out$RINPERSOON == "k3"], "m3")

  # .db with the birth year stored as INTEGER: the year filter must still match
  db_g <- file.path(d, "GBAPERSOON.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_g)
  DBI::dbWriteTable(con, "GBAPERSOON", data.frame(RINPERSOONS = "R", RINPERSOON = gba$RINPERSOON,
                                                  GBAGEBOORTEJAAR = as.integer(gba$GBAGEBOORTEJAAR)))
  DBI::dbDisconnect(con)
  db_k <- file.path(d, "KINDOUDER.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_k)
  DBI::dbWriteTable(con, "KINDOUDER", as.data.frame(zap_labels(kind)))
  DBI::dbDisconnect(con)
  out_db <- suppressWarnings(reference_children(db_g, db_k, 2010:2011))   # no value_labels table
  expect_equal(sort(out_db$RINPERSOON), c("k2", "k3"))

  expect_error(reference_children(file.path(d, "GBAPERSOON.sav"), file.path(d, "KINDOUDER.sav"), 1990),
               "No children born")
})

test_that("cohorts with a national income rank have the paths and income years they need", {
  for (f in list.files(file.path(root, "config"), "\\.yml$", full.names = TRUE)) {
    cfg <- config::get("data_preparation", file = f)
    loc <- config::get("file_locations", file = f)
    if (!identical(cfg$income_rank_population, "national")) next
    expect_true(!is.null(loc$gba_data), info = basename(f))
    expect_true(!is.null(loc$kindouder_data) || !is.null(loc$kindouder_file), info = basename(f))
    offsets <- as.integer(unlist(cfg$income_year_offsets))
    first_birth <- as.integer(substr(cfg$child_birth_date_min, 7, 10))
    last_birth  <- as.integer(substr(cfg$child_birth_date_max, 7, 10))
    expect_lte(as.integer(cfg$parent_income_year_min), first_birth + min(offsets))
    expect_gte(as.integer(cfg$parent_income_year_max), last_birth + max(offsets))
  }
})
