# The age35 outcomes script (src/age35/03_outcomes_age35) on a small synthetic,
# CBS-like dataset, for the blocks that join address, property and dwelling data:
# homeowner, gifts and living_space. The data contain the cases that once gave
# several rows per person in cohort_dat (and, in the RA, a run that stopped with
# "cannot allocate vector of size 1.3 Gb", 17-11-2026):
#   - a move on 1 January of the year at 35 (and one at 34): two address spells
#     both cover that date
#   - a dwelling with two rows in EIGENDOMTAB (two owners / owner types)
#   - a dwelling with two life cycles in LEVCYCLWOONNIETWOON that meet on 1 January
#   - two cohort members (twins) at the same dwelling
#   - gifts received in several years
# The script runs in a separate R process (callr), as P_2_1_run_cohort.R runs it.
root <- normalizePath(file.path("..", ".."))
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

rins <- function(x) labelled(x, c("RINPERSOON WEL IN GBA" = "R"))
# CBS .sav code columns are labelled; the script reads them with as_factor()
lab <- function(x) labelled(x, setNames(sort(unique(x)), sort(unique(x))))

# Writes the synthetic CBS files to `dir`; returns the cfg and loc the script needs
make_synthetic_age35 <- function(dir, n = 200, seed = 35) {
  set.seed(seed)
  data_folder <- file.path(dir, "G")
  temp_folder <- file.path(dir, "temp")
  for (d in c(data_folder, temp_folder, file.path(data_folder, "BouwenWonen", "EIGENDOMTAB"))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  ids <- sprintf("%09d", seq_len(n))
  birthdate <- as.Date("1985-01-01") + sample(0:(5 * 365 - 1), n, replace = TRUE)
  birth_year <- as.integer(format(birthdate, "%Y"))
  year35 <- birth_year + 35
  obj <- sprintf("%016d", seq_len(n))
  obj[6] <- obj[5]                                # twins 5 and 6 live in one dwelling

  # cohort after step 02
  cohort <- tibble(RINPERSOONS = factor("R"), RINPERSOON = ids,
                   birthdate = birthdate, birth_year = birth_year)
  saveRDS(cohort, file.path(temp_folder, "02_predictors.rds"))
  saveRDS(tibble(n_1_cohort = n), file.path(temp_folder, "02_sample_size.rds"))

  # GBAADRESOBJECT: one open spell per person, plus a move on 1 January at 35
  # (person 1) and at 34 (person 2)
  adres <- tibble(RINPERSOONS = "R", RINPERSOON = ids, SOORTOBJECTNUMMER = "VBO",
                  RINOBJECTNUMMER = obj, start = "19900101", end = "20501231")
  move <- function(i, yr) {
    jan1 <- paste0(yr, "0101")
    adres$end[i] <<- jan1
    adres <<- bind_rows(adres, tibble(RINPERSOONS = "R", RINPERSOON = ids[i], SOORTOBJECTNUMMER = "VBO",
                                      RINOBJECTNUMMER = sprintf("9%015d", i), start = jan1, end = "20501231"))
  }
  move(1, year35[1])
  move(2, year35[2] - 1)
  adres <- adres %>%
    transmute(RINPERSOONS = rins(RINPERSOONS), RINPERSOON, SOORTOBJECTNUMMER = lab(SOORTOBJECTNUMMER), RINOBJECTNUMMER,
              GBADATUMAANVANGADRESHOUDING = start, GBADATUMEINDEADRESHOUDING = end)
  write_sav(adres, file.path(data_folder, "gbaao.sav"))
  objects <- unique(adres$RINOBJECTNUMMER)

  # EIGENDOMTAB per year: one row per dwelling, two for the dwelling of person 3
  for (yr in 2019:2024) {
    eig <- tibble(SOORTOBJECTNUMMER = "VBO", RINOBJECTNUMMER = objects,
                  TypeEigenaar = sample(c("E", "H", "V"), length(objects), replace = TRUE),
                  AantalBewoners = sample(1:4, length(objects), replace = TRUE))
    eig$TypeEigenaar[eig$RINOBJECTNUMMER == obj[3]] <- "H"
    eig$AantalBewoners[eig$RINOBJECTNUMMER == obj[4]] <- 2
    eig$AantalBewoners[eig$RINOBJECTNUMMER == sprintf("9%015d", 1)] <- 1
    eig <- bind_rows(eig, tibble(SOORTOBJECTNUMMER = "VBO", RINOBJECTNUMMER = obj[3],
                                 TypeEigenaar = "E", AantalBewoners = 2))
    eig <- eig %>% mutate(SOORTOBJECTNUMMER = lab(SOORTOBJECTNUMMER), TypeEigenaar = lab(TypeEigenaar))
    write_sav(eig, file.path(data_folder, "BouwenWonen", "EIGENDOMTAB", paste0("EIGENDOM", yr, "TABV1.sav")))
  }

  # LEVCYCLWOONNIETWOON: one life cycle per dwelling; the dwelling of person 4 is
  # rebuilt on 1 January of the year at 35 (80 m2 before, 100 m2 after)
  woon <- tibble(SOORTOBJECTNUMMER = "VBO", RINOBJECTNUMMER = objects,
                 VBOOPPERVLAKTE = sample(40:150, length(objects), replace = TRUE),
                 AANVLEVCYCLWOONNIETWOON = "20000101", EINDLEVCYCLWOONNIETWOON = "88888888")
  woon$VBOOPPERVLAKTE[woon$RINOBJECTNUMMER == obj[1]] <- 60
  woon$VBOOPPERVLAKTE[woon$RINOBJECTNUMMER == sprintf("9%015d", 1)] <- 120
  i4 <- woon$RINOBJECTNUMMER == obj[4]
  woon$VBOOPPERVLAKTE[i4] <- 80
  woon$EINDLEVCYCLWOONNIETWOON[i4] <- paste0(year35[4], "0101")
  woon <- bind_rows(woon, tibble(SOORTOBJECTNUMMER = "VBO", RINOBJECTNUMMER = obj[4], VBOOPPERVLAKTE = 100,
                                 AANVLEVCYCLWOONNIETWOON = paste0(year35[4], "0101"),
                                 EINDLEVCYCLWOONNIETWOON = "88888888"))
  write_sav(woon %>% mutate(SOORTOBJECTNUMMER = lab(SOORTOBJECTNUMMER)), file.path(data_folder, "woon.sav"))

  # SCHTAB per year: persons 7-50 receive gifts at 23, 25 and 27
  gifts <- tibble(RINPERSOON = rep(ids[7:50], each = 3),
                  year = rep(birth_year[7:50], each = 3) + c(23, 25, 27),
                  SCHBRUTSCHK = 1000)
  for (yr in 2007:2022) {
    sch_dir <- file.path(data_folder, "InkomenBestedingen", "SCHTAB", yr)
    dir.create(sch_dir, recursive = TRUE, showWarnings = FALSE)
    g <- gifts %>% filter(year == yr)
    write_sav(tibble(RINPERSOONSONTVANGER = rins(rep("R", nrow(g))), RINPERSOONONTVANGER = g$RINPERSOON,
                     SCHTYPEVRIJ = labelled(rep("1", nrow(g)), c(gift = "1")), SCHBRUTSCHK = g$SCHBRUTSCHK),
              file.path(sch_dir, paste0("SCH", yr, "TABV1.sav")))
  }

  cpi_file <- file.path(dir, "cpi.xlsx")
  writexl::write_xlsx(data.frame(year = 2000:2025, cpi = 100), cpi_file)

  list(
    cfg = list(cohort_name = "synthetic_age35", outcome_age = 35,
               child_live_start = 34, child_live_end = 35, child_live_slack_days = 31,
               cpi_base_year = 2024, child_outcome_year_min = 2020, child_outcome_year_max = 2024,
               gifts_dat_start = 2007, gifts_dat_end = 2022, gifts_year_start = 22, gifts_year_end = 33,
               outcome_blocks = c("homeowner", "gifts", "living_space"),
               required_outcomes = character(0)),
    loc = list(data_folder = data_folder, temp_folder = temp_folder, gbaao_data = "gbaao.sav",
               woon_data = "woon.sav", cpi_index_data = cpi_file),
    ids = ids, obj = obj, year35 = year35)
}

# Runs step 03 in a fresh R process; returns 03_outcomes.rds
run_outcomes_age35 <- function(cfg, loc) {
  callr::r(function(root, cfg, loc) {
    setwd(root)
    source(list.files("src/lib", "run_helpers\\.R$", full.names = TRUE))
    source_versioned("src/lib", "checks")
    source_versioned("src/lib", "sql_helpers")
    assign("cfg", cfg, envir = globalenv())
    assign("loc", loc, envir = globalenv())
    suppressMessages(suppressWarnings(source_versioned("src/age35", "03_outcomes_age35")))
    readRDS(file.path(loc$temp_folder, "03_outcomes.rds"))
  }, args = list(root = root, cfg = cfg, loc = loc))
}

test_that("age35 outcomes keep one row per person with multiple owners, 1-Jan moves and rebuilt dwellings", {
  skip_if_not_installed("callr")
  skip_if_not_installed("writexl")
  d <- tempfile("synth_age35_"); dir.create(d)
  s <- make_synthetic_age35(d)
  out <- run_outcomes_age35(s$cfg, s$loc)

  expect_equal(nrow(out), length(s$ids))
  expect_equal(anyDuplicated(out[, c("RINPERSOONS", "RINPERSOON")]), 0)

  person <- function(i) out[out$RINPERSOON == s$ids[i], ]
  # dwelling with an owner-occupier row and a rental row: owner-occupier wins
  expect_equal(person(3)$c35_homeowner, 1)
  # gifts in three years are summed into one row
  expect_equal(person(7)$c35_sum_gifts, 3000)
  expect_equal(person(7)$c35_gifts_received, 1)
  expect_equal(person(60)$c35_gifts_received, 0)
  # move on 1 January at 35: the new address counts (120 m2, 1 resident)
  expect_equal(person(1)$c35_living_space_pp, 120)
  # dwelling rebuilt on 1 January at 35: the new life cycle counts (100 m2, 2 residents)
  expect_equal(person(4)$c35_living_space_pp, 50)
  # twins in one dwelling both keep their row and the same living space
  expect_equal(person(5)$c35_living_space_pp, person(6)$c35_living_space_pp)
  # every person has a living space (all dwellings are in EIGENDOMTAB and LEVCYCL)
  expect_false(any(is.na(out$c35_living_space_pp)))
})

test_that("a join that duplicates people stops the outcomes script at that block", {
  skip_if_not_installed("callr")
  skip_if_not_installed("writexl")
  d <- tempfile("synth_age35_dup_"); dir.create(d)
  s <- make_synthetic_age35(d, n = 50)
  # a cohort that already has a duplicate person must not run through silently
  cohort <- readRDS(file.path(s$loc$temp_folder, "02_predictors.rds"))
  saveRDS(bind_rows(cohort, cohort[10, ]), file.path(s$loc$temp_folder, "02_predictors.rds"))
  expect_error(run_outcomes_age35(s$cfg, s$loc), "duplicate key")
})
