# Kansenkaart data preparation pipeline -- SQLite experiment
#
# Compares the output of the .sav pipeline with the SQLite pipeline for one cohort.
# Run both variants with sqlexperiment/run_experiment.R first.
#
# It compares every .rds file the sav variant wrote to its scratch folder with the
# file of the same name in the sql scratch folder, and checks:
#   - number of rows and columns, column names
#   - row order (same order of RINPERSOON)
#   - per column: are the values identical? (numbers with a tolerance of 1e-9;
#     factors / labelled vectors are compared on their values as text)
#   - column type and factor levels (reported separately: unused factor levels
#     can differ because the SQL version reads fewer rows; this does not change data)
#
# Prints only counts and column names, no microdata, so the output can be exported.
#
# Usage: working directory = repository root, set the cohort, run.
#   or from a terminal:  Rscript sqlexperiment/compare_experiment.R <cohort>
#
# (c) ODISSEI Social Data Science team 2026

library(tidyverse)
library(haven)

# setwd("H:/Opportunity-Preprocessing")   # <- repository root in the RA
cohort <- "prim8_a"
.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) >= 1) cohort <- .args[1]

# the config of the cohort in `dir`: <cohort>.yml (name in the CBS RA) or
# P_2_<version>_<cohort>.yml (name with version prefix, outside the RA)
find_cfg <- function(dir, cohort) {
  f <- file.path(dir, paste0(cohort, ".yml"))
  if (!file.exists(f)) f <- list.files(dir, paste0("^P_2_[0-9]_", cohort, "\\.yml$"), full.names = TRUE)
  if (length(f) != 1) stop("No (unique) config file for cohort '", cohort, "' in ", dir, "/")
  f
}
sav_dir <- config::get("file_locations", file = find_cfg("config", cohort))$scratch_folder
sql_dir <- config::get("file_locations", file = find_cfg("sqlexperiment/config", cohort))$scratch_folder

# all .rds files the sav pipeline wrote (incl. the final cohort file)
files <- sort(list.files(sav_dir, "\\.rds$"))
only_sql <- setdiff(list.files(sql_dir, "\\.rds$"), files)
if (length(files) == 0) stop("No .rds files in ", sav_dir, ": run the sav variant first")


# values as comparable plain vectors
as_plain <- function(x) {
  if (is.factor(x) || haven::is.labelled(x)) return(as.character(haven::zap_labels(x)))
  if (inherits(x, "Date") || inherits(x, "POSIXt") || inherits(x, "difftime")) return(as.numeric(x))
  as.vector(x)
}

same_values <- function(a, b) {
  a <- as_plain(a); b <- as_plain(b)
  if (is.numeric(a) && is.numeric(b)) {
    na_ok <- is.na(a) == is.na(b)
    val_ok <- is.na(a) | is.na(b) | abs(a - b) <= 1e-9 * pmax(1, abs(a))
    return(na_ok & val_ok)
  }
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & as.character(a) == as.character(b))
}

compare_file <- function(f) {
  a <- read_rds(file.path(sav_dir, f))
  b <- read_rds(file.path(sql_dir, f))
  cat("\n=====", f, "=====\n")
  if (!is.data.frame(a) || !is.data.frame(b)) {
    ok <- identical(a, b)
    cat("  (not a data frame) identical:", ok, "\n")
    return(invisible(ok))
  }
  a <- ungroup(a); b <- ungroup(b)
  cat(sprintf("rows: sav %d | sql %d    cols: sav %d | sql %d\n", nrow(a), nrow(b), ncol(a), ncol(b)))
  if (length(setdiff(names(a), names(b)))) cat("  only in sav:", setdiff(names(a), names(b)), "\n")
  if (length(setdiff(names(b), names(a)))) cat("  only in sql:", setdiff(names(b), names(a)), "\n")
  if (!identical(names(a), names(b)) && setequal(names(a), names(b))) cat("  same columns, different order\n")

  if (nrow(a) != nrow(b)) { cat("  -> DIFFERENCES FOUND (row count)\n"); return(invisible(FALSE)) }

  # row order
  if ("RINPERSOON" %in% names(a)) {
    same_order <- identical(as.character(a$RINPERSOON), as.character(b$RINPERSOON))
    cat("  same row order:", same_order, "\n")
    if (!same_order) {   # align rows to compare values anyway
      key <- intersect(c("RINPERSOONS", "RINPERSOON"), names(a))
      a <- a %>% arrange(across(all_of(key), as.character))
      b <- b %>% arrange(across(all_of(key), as.character))
    }
  }

  ok <- setequal(names(a), names(b))
  for (col in intersect(names(a), names(b))) {
    n_diff <- sum(!same_values(a[[col]], b[[col]]))
    type_a <- class(a[[col]])[1]; type_b <- class(b[[col]])[1]
    lev_diff <- is.factor(a[[col]]) && is.factor(b[[col]]) && !identical(levels(a[[col]]), levels(b[[col]]))
    if (n_diff > 0) { ok <- FALSE; cat(sprintf("  VALUES DIFFER  %-45s %d rows\n", col, n_diff)) }
    if (type_a != type_b) cat(sprintf("  type differs   %-45s sav %s | sql %s\n", col, type_a, type_b))
    if (lev_diff) cat(sprintf("  levels differ  %-45s (values identical: %s)\n", col, n_diff == 0))
  }
  cat(if (ok) "  -> all values identical\n" else "  -> DIFFERENCES FOUND\n")
  invisible(ok)
}

results <- sapply(files, function(f) {
  if (!file.exists(file.path(sql_dir, f))) {
    cat("\n=====", f, "===== missing in", sql_dir, "\n"); return(NA)
  }
  compare_file(f)
})

cat("\n\nSUMMARY", cohort, "(TRUE = identical values)\n")
print(results)
if (length(only_sql)) cat("\nOnly in the sql scratch folder:", only_sql, "\n")

# timing
t_files <- c(file.path(sav_dir, "timing_sav.csv"), file.path(sql_dir, "timing_sql.csv"))
if (all(file.exists(t_files))) {
  cat("\nTIMING\n")
  timing <- bind_rows(lapply(t_files, read.csv)) %>%
    pivot_wider(names_from = variant, values_from = c(seconds, max_mem_mb))
  print(as.data.frame(timing))
}
