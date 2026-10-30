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
