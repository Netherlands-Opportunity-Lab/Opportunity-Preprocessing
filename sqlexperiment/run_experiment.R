# Kansenkaart data preparation pipeline -- SQLite experiment
#
# Runs the pipeline of one cohort in one of two variants and logs time + memory per step:
#   variant = "sav" : the current scripts   (src/<cohort>,               config/<config of cohort>)
#   variant = "sql" : the SQLite scripts    (sqlexperiment/src/<cohort>, sqlexperiment/config/<config of cohort>)
# Config and script names may have the version prefix (P_2_<version>_, outside the
# CBS RA) or not (inside the RA): both are found.
#
# The two variants write to different scratch folders, so afterwards
# sqlexperiment/compare_experiment.R can check that the outputs are identical.
#
# Usage: set the working directory to the repository root, pick a cohort and a variant, run.
#   or from a terminal:  Rscript sqlexperiment/run_experiment.R <cohort> <variant>
# Tip: run each variant in a fresh R session, so memory numbers are comparable.
#
# (c) ODISSEI Social Data Science team 2026


# clean workspace
rm(list = ls())
# setwd("H:/Opportunity-Preprocessing")   # <- repository root in the RA
options(scipen = 999)


#### CHOOSE COHORT AND VARIANT ####
# cohorts: prim8_a, prim8_b, newborns_a_mortality, newborns_a_perinatal,
#          newborns_b_mortality, newborns_b_perinatal, age16, age21, age35
cohort  <- "prim8_a"
variant <- "sql"   # "sav" or "sql"

# command line arguments overrule the two lines above
.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) >= 1) cohort  <- .args[1]
if (length(.args) >= 2) variant <- .args[2]


#### CONFIGURATION ####
# the config of the cohort in `dir`: <cohort>.yml (name in the CBS RA) or
# P_2_<version>_<cohort>.yml (name with version prefix, outside the RA)
.find_cfg <- function(dir, cohort) {
  f <- file.path(dir, paste0(cohort, ".yml"))
  if (!file.exists(f)) f <- list.files(dir, paste0("^P_2_[0-9]_", cohort, "\\.yml$"), full.names = TRUE)
  if (length(f) != 1) stop("No (unique) config file for cohort '", cohort, "' in ", dir, "/")
  f
}
.sav_cfg_file <- .find_cfg("config", cohort)

if (variant == "sav") {
  .cfg_file   <- .sav_cfg_file
  .script_dir <- file.path("src", cohort)
} else if (variant == "sql") {
  .cfg_file   <- .find_cfg("sqlexperiment/config", cohort)
  .script_dir <- file.path("sqlexperiment/src", cohort)
  source("sqlexperiment/src/sql_helpers.R")   # read_cbs(), read_db(), find_latest_db()
} else {
  stop("variant must be 'sav' or 'sql'")
}

cfg <- config::get("data_preparation", file = .cfg_file)
loc <- config::get("file_locations",   file = .cfg_file)

# safety check: the two variants must not overwrite each other's output
if (variant == "sql") {
  .sav_cfg <- config::get("data_preparation", file = .sav_cfg_file)
  .sav_loc <- config::get("file_locations",   file = .sav_cfg_file)
  .same <- function(a, b) !is.null(a) && !is.null(b) &&
    normalizePath(a, mustWork = FALSE) == normalizePath(b, mustWork = FALSE)
  if (.same(loc$scratch_folder, .sav_loc$scratch_folder) ||
      .same(cfg$mortality_name, .sav_cfg$mortality_name) ||
      .same(cfg$perinatal_name, .sav_cfg$perinatal_name)) {
    stop("The sql config writes to the same place as the sav config; change the output paths in ", .cfg_file)
  }
}

# create the scratch folder
if (!dir.exists(loc$scratch_folder)) dir.create(loc$scratch_folder, recursive = TRUE)

# age35 writes a diagnostics .xlsx to `excel_path` (set in the master script); here: scratch
excel_path <- loc$scratch_folder


#### RUN ####
# (variables of this runner start with a dot, so the sourced scripts cannot overwrite them)
.timing <- data.frame(variant = character(0), step = character(0), seconds = numeric(0),
                      max_mem_mb = numeric(0), stringsAsFactors = FALSE)

for (.step in c("01_", "02_", "03_", "04_")) {
  # "01_..." (CBS RA) or "P_2_0_01_..." / "P2_1_01_..." (with version prefix)
  .script <- list.files(.script_dir, paste0("(^|_)", .step), full.names = TRUE)
  if (length(.script) != 1) stop("No (unique) ", .step, " script in ", .script_dir)
  message("\n=== ", cohort, " / ", variant, ": ", basename(.script), " ===")
  invisible(gc(reset = TRUE))                      # reset the 'max used' memory counter
  .t0 <- Sys.time()
  source(.script)
  .secs <- as.numeric(Sys.time() - .t0, units = "secs")
  .mem  <- sum(gc()[, 6])                          # max memory used by R during this step (Mb)
  .timing[nrow(.timing) + 1, ] <- list(variant, basename(.script), round(.secs, 1), round(.mem))
  message(sprintf("=== done in %.1f s, max memory %.0f Mb", .secs, .mem))
}

.timing <- rbind(.timing, data.frame(variant = variant, step = "TOTAL",
                                     seconds = sum(.timing$seconds), max_mem_mb = max(.timing$max_mem_mb)))
print(.timing)
write.csv(.timing, file.path(loc$scratch_folder, paste0("timing_", variant, ".csv")), row.names = FALSE)

# the pre-processed cohort data file is now available in the scratch folder!
