# The samples of the intragenerational analysis (age35/45/55_current_address):
# the categorical outcomes, and the outcome blocks named in the configs.
root <- file.path("..", "..")
source(file.path(root, "src", "lib", "P_2_1_run_helpers.R"))
suppressPackageStartupMessages(source(file.path(root, "src", "lib", "P_2_1_current_address.R")))

test_that("add_intra_categories gives the same categories for any prefix", {
  toy <- function(prefix) {
    d <- data.frame(employed = c(1, 0, 0, 0), disability = c(0, 1, 0, 0), social_assistance = c(0, 0, 1, 0),
                    specialist_mhc = c(1, 0, 0, 0), basic_mhc = c(1, 1, 0, 0),
                    permanent_contract = c(1, 0, NA, NA), wo_attained = c(1, 0, 0, 0),
                    hbo_attained = c(1, 1, 0, 0), homeowner = c(1, 0, 1, 0))
    names(d) <- paste0(prefix, names(d))
    d$migration_background <- c("No Migration Background", "Morocco", "Other", "No Migration Background")
    add_intra_categories(d, prefix)
  }
  a <- toy("c35_"); b <- toy("c55_")
  expect_equal(as.character(a$c35_ses_ranked),
               c("Employed", "Disability Insurance", "Social Assistance Benefit", "None"))
  expect_equal(as.character(a$c35_mhc_ranked), c("Specialist", "Basic", "None", "None"))
  expect_equal(as.character(a$c35_contract_ranked),
               c("Permanent", "Fixed-term", "No Salaried Employment", "No Salaried Employment"))
  expect_equal(as.character(a$c35_education_ranked), c("WO", "HBO", "None", "None"))
  expect_equal(a$d_has_migration_background, c(0, 1, 1, 0))
  expect_true(is.factor(a$c35_homeowner))
  expect_equal(unname(as.list(a)), unname(as.list(b)))
})

test_that("the outcome blocks in the configs exist in the outcomes script", {
  script <- readLines(find_versioned(file.path(root, "src", "age35"), "03_outcomes_age35"), encoding = "UTF-8")
  blocks <- unique(regmatches(script, regexpr('(?<=run_block\\(")[a-z_]+', script, perl = TRUE)))
  for (f in list.files(file.path(root, "config"), "\\.yml$", full.names = TRUE)) {
    b <- config::get("data_preparation", file = f)$outcome_blocks
    if (!is.null(b)) expect_true(all(b %in% blocks), info = f)
  }
})

test_that("add_current_regions adds the wijk of extra_gwb_years without changing the regions", {
  source(file.path(root, "src", "lib", "P_2_1_checks.R"), local = TRUE)
  ymd8 <- function(x) as.character(x)
  # three people, each with one address at age 35; object 3 has no 2022 wijk ("----")
  cohort <- data.frame(RINPERSOONS = "R", RINPERSOON = c("1", "2", "3"),
                       birthdate = as.Date(c("1985-06-01", "1986-06-01", "1987-06-01")))
  fake <- list(
    gbaao = data.frame(RINPERSOONS = "R", RINPERSOON = c("1", "2", "3"),
                       SOORTOBJECTNUMMER = "A", RINOBJECTNUMMER = c("o1", "o2", "o3"),
                       GBADATUMAANVANGADRESHOUDING = "20000101", GBADATUMEINDEADRESHOUDING = "20991231"),
    postcode = data.frame(SOORTOBJECTNUMMER = "A", RINOBJECTNUMMER = c("o1", "o2", "o3"),
                          POSTCODENUM = c("1011", "1012", "1381"),
                          DATUMAANVPOSTCODENUMADRES = "20000101", DATUMEINDPOSTCODENUMADRES = "20991231"),
    vslgwb = data.frame(SOORTOBJECTNUMMER = "A", RINOBJECTNUMMER = c("o1", "o2", "o3"),
                        gem2025 = c("0363", "0363", "0363"), wc2025 = c("0363AA", "0363AB", "0363TB"),
                        gem2022 = c("0363", "0363", "0457"), wc2022 = c("036300", "036300", "----"))
  )
  read_cbs <- function(path, ...) fake[[path]]
  read_excel <- function(...) data.frame(`gemeenten|Code` = "0363", `COROP-gebieden|Code` = "CR23",
                                         check.names = FALSE)
  read_xlsx <- function(...) data.frame(neighborhood_code = c("WK0363AA", "WK0363AB", "WK0363TB"),
                                        neighborhood_name = c("a", "b", "Weesp"),
                                        stadsdeel_name = c("Centrum", "Centrum", "Weesp"),
                                        gebied_name = c("g1", "g1", "Weesp"))
  # the RA haven/forcats accept levels = "values" on plain columns; newer versions do not
  as_factor <- function(x, ...) if (is.data.frame(x)) x else factor(x)
  environment(add_current_regions) <- environment()
  loc <- list(data_folder = "", gbaao_data = "gbaao", postcode_data = "postcode",
              vslgwb_data = "vslgwb", corop_data = "corop", amsterdam_areas = "ams")
  cfg <- list(postcode_target_date = "31/12/2025", gwb_target_date = "31/12/2025",
              corop_target_date = "31/12/2025", amsterdam_metropolitan_area = "0363")
  file.path <- function(...) paste0(...)

  without <- suppressMessages(add_current_regions(cohort, 35, cfg, loc))
  with <- suppressMessages(add_current_regions(cohort, 35, c(cfg, list(extra_gwb_years = 2022)), loc))

  expect_false("neighborhood_code_2022" %in% names(without))
  expect_equal(with$neighborhood_code_2022, c("036300", "036300", NA))
  expect_equal(as.character(with$neighborhood_code), c("0363AA", "0363AB", "0363TB"))
  expect_equal(with[setdiff(names(with), "neighborhood_code_2022")], without)
})
