# Kansenkaart data preparation pipeline
#
# Full cohort creation script
#
# (c) ODISSEI Social Data Science team 2025



# clean workspace
rm(list=ls())
setwd("H:/Opportunity-Preprocessing")   # repository root in the RA
options(scipen=999)



# input the desired cohort here:
# age35, age21, age16, prim8_a, prim8_b, newborns_a_mortality, newborns_a_perinatal,
# newborns_b_mortality, newborns_b_perinatal, jgz
cohort <- "age35"


if (cohort == "jgz") {

  #### JGZ ####
  # The JGZ (youth health care) preprocessing has its own steps and config:
  #   00 clean and merge the raw JGZ files -> panel (child x age)
  #   01 GBA address and postcode at each contact moment
  #   02 contact moments (cm_<visit>.rds)
  #   03 catchment-area sample for the bereik analyses
  # All paths and settings are in config/jgz.yml, read by src/jgz/jgz_helpers.R;
  # see src/jgz/README.md. Each step reads the output of the previous one from
  # disk, so a subset can be rerun, e.g. jgz_steps <- c("02", "03").
  jgz_steps <- c("00", "01", "02", "03")

  for (jgz_step in jgz_steps) {
    jgz_script <- list.files("src/jgz", paste0("^", jgz_step, "_.*\\.R$"), full.names = TRUE)
    if (length(jgz_script) != 1) stop("No (unique) script for JGZ step ", jgz_step, " in src/jgz")
    message("==== JGZ step ", jgz_step, ": ", basename(jgz_script), " ====")
    source(jgz_script)
  }

  # the JGZ outputs are now in the folders set in config/jgz.yml (file_locations)!

} else {

  #### CONFIGURATION ####
  # the config file of the cohort: config/P_2_<version>_<cohort>.yml
  cfg_file <- list.files("config", paste0("^P_2_[0-9]_", cohort, "\\.yml$"), full.names = TRUE)
  if (length(cfg_file) != 1) stop("No (unique) config file for cohort '", cohort, "' in config/")

  # load the configuration
  cfg <- config::get("data_preparation", file = cfg_file)
  loc <- config::get("file_locations",   file = cfg_file)

  # create the scratch folder
  if (!dir.exists(loc$scratch_folder)) dir.create(loc$scratch_folder)

  # diagnostics workbooks (age35: parents_zeros_negatives_missings.xlsx) go
  # to the scratch folder of the cohort, as in sqlexperiment/run_experiment.R
  excel_path <- loc$scratch_folder


  #### RUN ####
  # select cohort
  source(list.files(file.path("src", cfg$cohort_name), "01_", full.names = TRUE))

  # add predictors
  source(list.files(file.path("src", cfg$cohort_name), "02_", full.names = TRUE))

  # add outcomes
  source(list.files(file.path("src", cfg$cohort_name), "03_", full.names = TRUE))

  # post-process
  source(list.files(file.path("src", cfg$cohort_name), "04_", full.names = TRUE))

  # the pre-processed cohort data file is now available in the scratch folder!
}



#### SCRIPTS FOR ROBUSTNESS CHECKS ####
# only for the cohort they belong to, and only if the scripts are there (the
# robustness folders are not in the GitHub repository)

if (cohort == "age21" && dir.exists("src/age21/robustness")) {
  # lifecycle bias for college attendance
  source(list.files(file.path("src/age21/robustness/"), "02", full.names = TRUE))

  source(list.files(file.path("src/age21/robustness/"), "03", full.names = TRUE))
}

if (cohort == "age35" && dir.exists("src/age35/robustness")) {
  # lifecycle bias
  source(list.files(file.path("src/age35/robustness/"), "02a", full.names = TRUE))

  source(list.files(file.path("src/age35/robustness/"), "03a", full.names = TRUE))

  source(list.files(file.path("src/age35/robustness/"), "03b", full.names = TRUE))
}
