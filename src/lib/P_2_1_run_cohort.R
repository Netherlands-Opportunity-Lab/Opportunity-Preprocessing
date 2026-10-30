# Kansenkaart data preparation pipeline -- run the pipeline for ONE cohort
#
# Normally started by P_2_1_run_all.R, which runs every cohort in its own R
# process (a clean session per cohort, as the old rm(list = ls()) gave). From the
# command line, in the repository root:
#
#   Rscript src/lib/P_2_1_run_cohort.R prim8_a            # all steps
#   Rscript src/lib/P_2_1_run_cohort.R prim8_a 03 04      # only steps 03 and 04
#
# Or interactively (e.g. to debug), from the repository root in a fresh session:
#
#   run_cohort <- "prim8_a"; run_steps <- "all"
#   source("src/lib/P_2_1_run_cohort.R")
#
# What it does: reads config/<cohort>.yml, runs the scripts listed under
# `pipeline` in that order, checks after every step that the cohort data has one
# row per person, and writes a log to output/logs/.
#
# (c) Netherlands Opportunity Lab 2026

run_args <- commandArgs(trailingOnly = TRUE)
if (length(run_args) > 0) {
  run_cohort <- run_args[1]
  run_steps <- if (length(run_args) > 1) run_args[-1] else "all"
}
if (!exists("run_cohort")) stop("Set run_cohort (e.g. run_cohort <- \"age35\") before sourcing this file.")
if (!exists("run_steps")) run_steps <- "all"

if (!file.exists("kansenkaart_preprocessing.Rproj")) {
  stop("Run this from the repository root (open kansenkaart_preprocessing.Rproj, or cd there).")
}
options(scipen = 999)

source(list.files("src/lib", "run_helpers\\.R$", full.names = TRUE))
source_versioned("src/lib", "checks")


#### CONFIGURATION ####
run_config_file <- find_versioned("config", run_cohort, ext = "yml")
run_pipeline <- config::get("pipeline", file = run_config_file)

if (run_cohort != "jgz") {
  # read_cbs(): reads the converted SQLite (.db) versions of the large CBS files,
  # filtered inside SQLite; a .sav path in the config falls back to read_sav().
  # See convert_files_to_sqlite/README.md
  source_versioned("src/lib", "sql_helpers")

  cfg <- config::get("data_preparation", file = run_config_file)
  loc <- config::get("file_locations",   file = run_config_file)
  for (run_dir in c(loc$temp_folder, loc$output_folder)) {
    if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
  }
  # old names, for the robustness scripts that only exist in the RA (not on GitHub)
  loc$scratch_folder <- loc$temp_folder
  excel_path <- loc$temp_folder
}
# (the JGZ scripts read config/jgz.yml themselves, via src/jgz/jgz_helpers.R)

run_scripts <- run_pipeline$scripts
if (!identical(run_steps, "all")) {
  run_candidates <- c(run_pipeline$scripts, run_pipeline$optional_scripts)
  run_scripts <- run_candidates[substr(run_candidates, 1, 2) %in% run_steps]
  if (length(run_scripts) == 0) stop("No scripts for step(s) ", paste(run_steps, collapse = ", "), " of ", run_cohort)
}


#### RUN ####
run_log <- start_run_log("output/logs", run_cohort)
withCallingHandlers({
  for (run_script in run_scripts) {
    log_step(run_log, run_script, source_versioned(run_pipeline$script_dir, run_script))

    # every cohort file has one row per person (the step has already saved its
    # output, so after a failure here the next steps can be rerun once fixed)
    if (run_cohort != "jgz" && exists("cohort_dat")) {
      check_keys(cohort_dat, c("RINPERSOONS", "RINPERSOON"), name = paste0(run_cohort, " after ", run_script))
    }
  }

  # robustness checks: only if the scripts are there (they are not on GitHub) and
  # the whole pipeline was run
  if (identical(run_steps, "all") && !is.null(run_pipeline$robustness_dir) &&
      dir.exists(run_pipeline$robustness_dir)) {
    for (run_pattern in run_pipeline$robustness_scripts) {
      log_step(run_log, paste0("robustness ", run_pattern),
               source(list.files(run_pipeline$robustness_dir, run_pattern, full.names = TRUE)))
    }
  }

}, error = function(e) log_line(run_log, "ERROR: ", conditionMessage(e)))

finish_run_log(run_log, config_files = run_config_file)
