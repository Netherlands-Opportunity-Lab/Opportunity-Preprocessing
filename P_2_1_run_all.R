# Kansenkaart data preparation pipeline -- run everything
#
# The one script that builds all cohort files from the CBS microdata. Open
# kansenkaart_preprocessing.Rproj (so the working directory is the repository
# root) and source this file, or from the command line in the repository root:
#
#   Rscript P_2_1_run_all.R                     # every cohort in `cohorts` below
#   Rscript P_2_1_run_all.R prim8_a prim8_b     # only these cohorts
#
# Each cohort runs in its own R process (src/lib/P_2_1_run_cohort.R), so nothing
# carries over from one cohort to the next. Output:
#   output/<cohort>_cohort.rds     the cohort files (input of the analysis repos)
#   temp/<cohort>/                 intermediate files and diagnostics per step
#   output/logs/                   one log per cohort: commit, settings, R version, time per step
#
# Paths and settings are in config/<cohort>.yml. To rerun only some steps of one
# cohort, see src/lib/P_2_1_run_cohort.R.
#
# (c) Netherlands Opportunity Lab 2026


#### WHAT TO RUN ####
cohorts <- c("age16", "age21", "age35",
             "age35_current_address",   # needs age35 (input of the intragenerational analysis)
             "prim8_a", "prim8_b",
             "newborns_a_mortality", "newborns_a_perinatal",
             "newborns_b_mortality", "newborns_b_perinatal")
# The JGZ pipeline uses the JGZ files of project 8151 (L:/); run it with
#   Rscript P_2_1_run_all.R jgz

cli_cohorts <- commandArgs(trailingOnly = TRUE)
if (length(cli_cohorts) > 0) cohorts <- cli_cohorts


#### RUN ####
if (!file.exists("kansenkaart_preprocessing.Rproj")) {
  stop("Run this from the repository root (open kansenkaart_preprocessing.Rproj, or cd there).")
}

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
run_cohort_script <- list.files("src/lib", "run_cohort\\.R$", full.names = TRUE)

failed <- character(0)
for (cohort in cohorts) {
  message("\n======== ", cohort, " ========")
  status <- system2(rscript, c(shQuote(run_cohort_script), cohort))
  if (status != 0) failed <- c(failed, cohort)
}

if (length(failed) > 0) {
  stop("Failed: ", paste(failed, collapse = ", "), ". See output/logs/ and the messages above.")
}
message("\nAll cohorts done. Cohort files are in output/, logs in output/logs/.")


#### SCRIPTS FOR ROBUSTNESS CHECKS ####
# Only for the cohort they belong to, and only if the scripts are there (the
# robustness folders are not in the GitHub repository). They need the objects of
# a cohort run, so run them after P_2_1_run_cohort.R for that cohort:
#   run_cohort <- "age35"; source("src/lib/P_2_1_run_cohort.R")
#   source(list.files("src/age35/robustness/", "02a", full.names = TRUE))   # etc.
