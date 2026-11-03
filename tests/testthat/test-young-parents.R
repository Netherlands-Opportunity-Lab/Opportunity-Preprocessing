# young_parents (src/lib/P_2_1_young_parents.R): one function for age21, age35,
# age45_current_address and age55_current_address. Toy KINDOUDER and GBAPERSOON
# files, no CBS data needed.
root <- file.path("..", "..")
source(file.path(root, "src", "lib", "P_2_1_run_helpers.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(haven)
})
source(file.path(root, "src", "lib", "sql_helpers.R"))
source(file.path(root, "src", "lib", "P_2_1_young_parents.R"))

test_that("add_young_parents: first child before age 20 = 1, otherwise or no child = 0", {
  d <- tempfile(); dir.create(d)
  # cohort: a mother at 19, b father at 25, c no child, d mother at 18 of a child
  # whose father is not in the GBA, e first child at 19 and second at 22
  cohort <- tibble(RINPERSOONS = "R", RINPERSOON = c("a", "b", "c", "d", "e"))
  # the RINPERSOONS columns carry value labels, as in the CBS files
  rins <- function(x) labelled(x, c("RINPERSOON WEL IN GBA" = "R",
                                    "RINPERSOON NIET IN GBA, GEEN SOFINR" = "G"))
  kind <- tibble(
    RINPERSOONS   = rins(rep("R", 5)),
    RINPERSOON    = c("k1", "k2", "k3", "k4", "k5"),
    RINPERSOONSpa = rins(c("R", "R", "G", "R", "R")),
    RINPERSOONpa  = c("x", "b", "y", "z", "z"),
    RINPERSOONSMa = rins(rep("R", 5)),
    RINPERSOONMa  = c("a", "w", "d", "e", "e"),
    XKOPPELNUMMER = "1")
  born <- c(a = "1990-01-01", b = "1990-01-01", c = "1990-01-01", d = "1990-01-01",
            e = "1990-01-01", x = "1985-01-01", w = "1990-01-01", y = "1985-01-01",
            z = "1985-01-01", k1 = "2009-06-01", k2 = "2015-06-01", k3 = "2008-06-01",
            k4 = "2009-06-01", k5 = "2012-06-01")
  gba <- tibble(RINPERSOONS = rins(rep("R", length(born))), RINPERSOON = names(born), b = ymd(born)) %>%
    transmute(RINPERSOONS, RINPERSOON,
              GBAGEBOORTEJAAR = as.character(year(b)),
              GBAGEBOORTEMAAND = sprintf("%02d", month(b)),
              GBAGEBOORTEDAG = sprintf("%02d", day(b)))
  write_sav(kind, file.path(d, "KINDOUDER.sav"))
  write_sav(gba, file.path(d, "GBAPERSOON.sav"))

  out <- add_young_parents(cohort, file.path(d, "KINDOUDER.sav"), file.path(d, "GBAPERSOON.sav"))
  expect_equal(nrow(out), nrow(cohort))
  expect_equal(out$young_parents, c(1, 0, 0, 0, 1))  # d: the known limitation (see README)
})

test_that("every config that computes young_parents has the KINDOUDER and GBA paths", {
  for (f in list.files(file.path(root, "config"), "\\.yml$", full.names = TRUE)) {
    b <- config::get("data_preparation", file = f)$outcome_blocks
    if (!is.null(b) && "young_parents" %in% b) {
      loc <- config::get("file_locations", file = f)
      expect_false(is.null(loc$kind_data), info = f)
      expect_false(is.null(loc$gba_data), info = f)
    }
  }
})
