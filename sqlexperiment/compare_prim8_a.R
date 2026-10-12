# Kansenkaart data preparation pipeline -- SQLite experiment
#
# Compares the output of the .sav pipeline with the SQLite pipeline for prim8_a.
# Run both variants with sqlexperiment/run_prim8_a_experiment.R first.
#
# For every scratch file it checks:
#   - number of rows and columns, column names
#   - row order (same order of RINPERSOON)
#   - per column: are the values identical? (numbers with a tolerance of 1e-9;
#     factors / labelled vectors are compared on their values as text)
#   - column type and factor levels (reported separately: unused factor levels
#     can differ because the SQL version reads fewer rows; this does not change data)
#
# Prints only counts and column names, no microdata, so the output can be exported.
#
# (c) ODISSEI Social Data Science team 2026

library(tidyverse)
library(haven)

# setwd("H:/Opportunity-Preprocessing")   # <- repository root in the RA
sav_dir <- config::get("file_locations", file = "config/P_2_1_prim8_a.yml")$scratch_folder
sql_dir <- config::get("file_locations", file = "sqlexperiment/config/P_2_1_prim8_a.yml")$scratch_folder

files <- c("01_cohort.rds", "01_sample_size.rds", "class_cohort.rds", "02_predictors.rds",
           "neighborhood_cohort.rds", "03_outcomes.rds", "03_sample_size.rds", "prim8_a_cohort.rds")


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
  a <- read_rds(file.path(sav_dir, f)) %>% ungroup()
  b <- read_rds(file.path(sql_dir, f)) %>% ungroup()
  cat("\n=====", f, "=====\n")
  cat(sprintf("rows: sav %d | sql %d    cols: sav %d | sql %d\n", nrow(a), nrow(b), ncol(a), ncol(b)))
  if (length(setdiff(names(a), names(b)))) cat("  only in sav:", setdiff(names(a), names(b)), "\n")
  if (length(setdiff(names(b), names(a)))) cat("  only in sql:", setdiff(names(b), names(a)), "\n")
  if (!identical(names(a), names(b)) && setequal(names(a), names(b))) cat("  same columns, different order\n")

  if (nrow(a) != nrow(b)) return(invisible(FALSE))

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

  ok <- TRUE
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
  if (!file.exists(file.path(sav_dir, f)) || !file.exists(file.path(sql_dir, f))) {
    cat("\n=====", f, "===== missing in one of the scratch folders\n"); return(NA)
  }
  compare_file(f)
})

cat("\n\nSUMMARY (TRUE = identical values)\n")
print(results)

# timing
t_files <- c(file.path(sav_dir, "timing_sav.csv"), file.path(sql_dir, "timing_sql.csv"))
if (all(file.exists(t_files))) {
  cat("\nTIMING\n")
  timing <- bind_rows(lapply(t_files, read.csv)) %>%
    pivot_wider(names_from = variant, values_from = c(seconds, max_mem_mb))
  print(timing)
}
