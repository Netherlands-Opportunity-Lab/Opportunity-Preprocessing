source(file.path("..", "..", "src", "lib", "P_2_1_checks.R"), chdir = TRUE)

test_that("check_keys accepts a valid key and returns the data", {
  dat <- data.frame(RINPERSOONS = c("R", "R"), RINPERSOON = c("1", "2"))
  expect_identical(check_keys(dat, c("RINPERSOONS", "RINPERSOON")), dat)
})

test_that("check_keys stops on duplicate, missing or absent keys", {
  dup <- data.frame(id = c(1, 1))
  expect_error(check_keys(dup, "id"), "1 duplicate")
  mis <- data.frame(id = c(1, NA))
  expect_error(check_keys(mis, "id"), "1 row\\(s\\) with a missing")
  expect_error(check_keys(dup, "other"), "not found")
})

test_that("checked_join joins like left_join and reports unmatched rows", {
  x <- data.frame(id = 1:3, a = c("x", "y", "z"))
  y <- data.frame(id = 1:2, b = c(10, 20))
  expect_message(out <- checked_join(x, y, by = "id"), "1 of 3 rows without a match")
  expect_equal(out, dplyr::left_join(x, y, by = "id"))
  expect_equal(nrow(suppressMessages(checked_join(x, y, by = "id", type = "inner"))), 2)
})

test_that("checked_join supports differently named keys", {
  x <- data.frame(id = 1:2)
  y <- data.frame(key = 1:2, b = 3:4)
  out <- suppressMessages(checked_join(x, y, by = c("id" = "key")))
  expect_equal(out$b, 3:4)
})

test_that("checked_join stops when the right-hand table is not unique (m:1)", {
  x <- data.frame(id = 1:2)
  y <- data.frame(id = c(1, 1), b = 1:2)
  expect_error(checked_join(x, y, by = "id"), "right-hand table")
  expect_error(checked_join(data.frame(id = c(1, 1)), data.frame(id = 1), by = "id",
                            relationship = "1:1"), "left-hand table")
})


# check_final_cohort() --------------------------------------------------------

final_dat <- data.frame(
  RINPERSOONS = "R", RINPERSOON = as.character(1:3),
  birthdate = as.Date(c("2005-01-01", "2010-06-15", "2014-12-31")),
  amddd = c(168, 280, 294),
  c00_sga = c(0, 1, 0)
)
final_cfg <- list(child_birth_date_min = "01/01/2005", child_birth_date_max = "31/12/2014",
                  cut_off_days_min = 168, cut_off_days_max = 294)
final_checks <- list(
  expected_columns = list("c00_sga"),
  forbidden_columns = list("perinatal_death", "c00_perinatal_mortality"),
  dropped_in_last_step = list("perinatal_death"),
  ranges = list(birthdate = list("child_birth_date_min", "child_birth_date_max"),
                amddd = list("cut_off_days_min", "cut_off_days_max"))
)
final_temp <- function(n) {
  d <- tempfile(); dir.create(d)
  saveRDS(data.frame(n_0_birth_cohort = 10, n_4_child_outcomes = n), file.path(d, "03_sample_size.rds"))
  d
}

test_that("check_final_cohort passes a correct cohort file and reports what it checked", {
  report <- check_final_cohort(final_dat, final_checks, final_cfg, final_temp(3),
                               columns_before = c(names(final_dat), "perinatal_death"), name = "x")
  expect_true(any(grepl("3 rows = n_4_child_outcomes", report)))
  expect_true(any(grepl("dropped: perinatal_death", report)))
  expect_true(any(grepl("birthdate within", report)))
})

test_that("check_final_cohort finds missing, forbidden and dropped columns", {
  bad <- final_dat[, names(final_dat) != "c00_sga"]
  bad$c00_perinatal_mortality <- 0
  expect_error(
    check_final_cohort(bad, final_checks, final_cfg, final_temp(3),
                       columns_before = c(names(final_dat), "income_parents")),
    "3 check\\(s\\)")
  expect_error(check_final_cohort(bad, final_checks, final_cfg, final_temp(3)), "expected column\\(s\\) missing: c00_sga")
  expect_error(check_final_cohort(bad, final_checks, final_cfg, final_temp(3)), "forbidden column\\(s\\) present: c00_perinatal_mortality")
  expect_error(
    check_final_cohort(final_dat, final_checks, final_cfg, final_temp(3),
                       columns_before = c(names(final_dat), "income_parents")),
    "dropped column\\(s\\) it should keep: income_parents")
})

test_that("check_final_cohort compares the rows with the most recent sample size table", {
  expect_error(check_final_cohort(final_dat, final_checks, final_cfg, final_temp(4)),
               "3 rows, but n_4_child_outcomes in 03_sample_size.rds is 4")
  d <- final_temp(4)
  Sys.sleep(1.1)   # file.mtime has a resolution of 1 second on some systems
  saveRDS(data.frame(n_5_excl = 3), file.path(d, "04_sample_size.rds"))
  expect_silent(check_final_cohort(final_dat, final_checks, final_cfg, d))
})

test_that("check_final_cohort checks value ranges and the range settings", {
  out <- final_dat
  out$birthdate[1] <- as.Date("2004-12-31")
  out$amddd[2] <- 300
  expect_error(check_final_cohort(out, final_checks, final_cfg, final_temp(3)),
               "birthdate: 1 value\\(s\\) outside.*amddd: 1 value\\(s\\) outside")
  missing_setting <- final_checks
  missing_setting$ranges$amddd <- list("cut_off_days_min", "no_such_setting")
  expect_error(check_final_cohort(final_dat, missing_setting, final_cfg, final_temp(3)),
               "not two settings")
})

test_that("check_final_cohort without final_checks only checks the row count", {
  expect_silent(check_final_cohort(final_dat, NULL, final_cfg, final_temp(3)))
  expect_error(check_final_cohort(final_dat, NULL, final_cfg, final_temp(2)), "3 rows")
})
