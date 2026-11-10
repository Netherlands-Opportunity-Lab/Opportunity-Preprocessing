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
# row per person, checks the final cohort file after the last step
# (check_final_cohort(), settings under `final_checks` in the config), and writes
# a log to output/logs/.
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
run_final_checks <- config::get(file = run_config_file)$final_checks   # NULL if not set

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
# (the JGZ scripts also read their config themselves, via src/jgz/jgz_helpers.R,
# so they can be sourced on their own; the result is the same cfg and loc)

# A script name may include a folder relative to script_dir (e.g.
# "../age35/03_outcomes_age35"): the step number is the start of the file name.
run_scripts <- run_pipeline$scripts
if (!identical(run_steps, "all")) {
  run_candidates <- c(run_pipeline$scripts, run_pipeline$optional_scripts)
  run_scripts <- run_candidates[substr(basename(run_candidates), 1, 2) %in% run_steps]
  if (length(run_scripts) == 0) stop("No scripts for step(s) ", paste(run_steps, collapse = ", "), " of ", run_cohort)
}


#### RUN ####
run_log <- start_run_log("output/logs", run_cohort)
run_columns <- list()   # the columns of cohort_dat after each step (for the final checks)
withCallingHandlers({
  for (run_script in run_scripts) {
    log_step(run_log, run_script,
             source_versioned(file.path(run_pipeline$script_dir, dirname(run_script)), basename(run_script)))

    # every cohort file has one row per person (the step has already saved its
    # output, so after a failure here the next steps can be rerun once fixed)
    if (exists("cohort_dat")) {
      check_keys(cohort_dat, c("RINPERSOONS", "RINPERSOON"), name = paste0(run_cohort, " after ", run_script))
      run_columns[[run_script]] <- names(cohort_dat)
    }
  }

  # the final cohort file: only when the last step (which writes it) has run.
  # `cohort_dat` is what that step wrote to output/<cohort>_cohort.rds.
  run_last <- utils::tail(run_pipeline$scripts, 1)
  if (run_last %in% run_scripts && exists("cohort_dat")) {
    run_before <- utils::head(utils::tail(run_pipeline$scripts, 2), 1)
    log_step(run_log, "final checks", {
      run_report <- check_final_cohort(
        cohort_dat, run_final_checks, cfg, loc$temp_folder,
        columns_before = if (run_before != run_last) run_columns[[run_before]],
        name = run_cohort)
      for (run_line in run_report) log_line(run_log, run_line)
    })
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
