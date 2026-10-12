# Kansenkaart data preparation pipeline -- SQLite experiment
#
# Runs the prim8_a pipeline in one of two variants and logs time + memory per step:
#   variant = "sav" : the current scripts   (src/prim8_a,              config/P_2_1_prim8_a.yml)
#   variant = "sql" : the SQLite scripts    (sqlexperiment/src/prim8_a, sqlexperiment/config/P_2_1_prim8_a.yml)
#
# The two variants write to different scratch folders, so afterwards
# sqlexperiment/compare_prim8_a.R can check that the outputs are identical.
#
# Usage: set the working directory to the repository root, pick a variant, run.
# Tip: run each variant in a fresh R session, so memory numbers are comparable.
#
# (c) ODISSEI Social Data Science team 2026


# clean workspace
rm(list = ls())
# setwd("H:/Opportunity-Preprocessing")   # <- repository root in the RA
options(scipen = 999)


#### CHOOSE VARIANT ####
variant <- "sql"   # "sav" or "sql"


#### CONFIGURATION ####
cohort <- "prim8_a"   # used by 03_outcomes (sample size table)

if (variant == "sav") {
  cfg_file   <- "config/P_2_1_prim8_a.yml"
  script_dir <- "src/prim8_a"
} else if (variant == "sql") {
  cfg_file   <- "sqlexperiment/config/P_2_1_prim8_a.yml"
  script_dir <- "sqlexperiment/src/prim8_a"
  source("sqlexperiment/src/sql_helpers.R")   # read_db(), find_latest_db()
} else {
  stop("variant must be 'sav' or 'sql'")
}

cfg <- config::get("data_preparation", file = cfg_file)
loc <- config::get("file_locations",   file = cfg_file)

# safety check: the two variants must not overwrite each other's output
if (variant == "sql" && normalizePath(loc$scratch_folder, mustWork = FALSE) ==
    normalizePath(config::get("file_locations", file = "config/P_2_1_prim8_a.yml")$scratch_folder, mustWork = FALSE)) {
  stop("The sql config uses the same scratch folder as the sav config; change scratch_folder in ", cfg_file)
}

# create the scratch folder
if (!dir.exists(loc$scratch_folder)) dir.create(loc$scratch_folder, recursive = TRUE)


#### RUN ####
steps <- c("01_", "02_", "03_", "04_")
timing <- data.frame(variant = variant, step = character(0), seconds = numeric(0),
                     max_mem_mb = numeric(0), stringsAsFactors = FALSE)

for (s in steps) {
  script <- list.files(script_dir, s, full.names = TRUE)
  message("\n=== ", variant, ": ", basename(script), " ===")
  invisible(gc(reset = TRUE))                      # reset the 'max used' memory counter
  t0 <- Sys.time()
  source(script)
  secs <- as.numeric(Sys.time() - t0, units = "secs")
  mem  <- sum(gc()[, 6])                           # max memory used by R during this step (Mb)
  timing[nrow(timing) + 1, ] <- list(variant, basename(script), round(secs, 1), round(mem))
  message(sprintf("=== done in %.1f s, max memory %.0f Mb", secs, mem))
}

timing <- rbind(timing, data.frame(variant = variant, step = "TOTAL",
                                   seconds = sum(timing$seconds), max_mem_mb = max(timing$max_mem_mb)))
print(timing)
write.csv(timing, file.path(loc$scratch_folder, paste0("timing_", variant, ".csv")), row.names = FALSE)

# the pre-processed cohort data file is now available in the scratch folder!
