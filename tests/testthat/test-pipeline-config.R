# Every config lists scripts that exist, so a renamed or moved script is caught
# here instead of hours into a run in the RA.
root <- file.path("..", "..")
source(file.path(root, "src", "lib", "P_2_1_run_helpers.R"))

test_that("find_versioned finds files with and without a version prefix", {
  d <- tempfile(); dir.create(d)
  file.create(file.path(d, c("P_2_1_01_cohort_x.R", "02_predictors_x.R", "P2_1_03_outcomes_x.R")))
  expect_match(find_versioned(d, "01_cohort_x"), "P_2_1_01_cohort_x.R$")
  expect_match(find_versioned(d, "02_predictors_x"), "02_predictors_x.R$")
  expect_match(find_versioned(d, "03_outcomes_x"), "P2_1_03_outcomes_x.R$")
  expect_error(find_versioned(d, "04_missing"), "found 0")
})

configs <- list.files(file.path(root, "config"), "\\.yml$", full.names = TRUE)

test_that("every config has a pipeline whose scripts exist", {
  expect_gt(length(configs), 0)
  for (f in configs) {
    p <- config::get("pipeline", file = f)
    expect_false(is.null(p$scripts), info = f)
    for (s in c(p$scripts, p$optional_scripts)) {
      expect_length(find_versioned(file.path(root, p$script_dir), s), 1)
    }
  }
})

test_that("every cohort config has the settings the runner needs", {
  for (f in setdiff(configs, grep("jgz", configs, value = TRUE))) {
    cfg <- config::get("data_preparation", file = f)
    loc <- config::get("file_locations", file = f)
    cohort <- sub("^P_[0-9]+_[0-9]+_", "", sub("\\.yml$", "", basename(f)))
    expect_identical(cfg$cohort_name, cohort, info = f)
    expect_false(is.null(loc$temp_folder), info = f)
    expect_false(is.null(loc$output_folder), info = f)
  }
})

test_that("every R script parses", {
  scripts <- list.files(root, "\\.R$", recursive = TRUE, full.names = TRUE)
  scripts <- scripts[!grepl("renv/", scripts)]
  for (s in scripts) expect_error(parse(s, encoding = "UTF-8"), NA, info = s)
})
