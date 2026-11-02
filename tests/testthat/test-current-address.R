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
