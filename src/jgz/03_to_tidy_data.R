# =============================================================================
# JGZ pilot -- 03. Gradient results from wide to long ("tidy")
#
# The gradient analyses (not in this folder) write one Excel file per outcome
# with one row per income bin (20 bins) and one column per
# contact moment x outcome x subgroup x statistic, e.g.
#   cm3_visus_boys        (estimate)
#   cm3_visus_boys_N      (number of observations)
#   cm3_visus_boysinc     (mean income in the bin)
# This script reshapes each file to one row per cm x outcome x subgroup x bin
# and writes it next to the input as <name>_tidy.xlsx.
#
#   Input/output folder: loc$gradients_folder
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(tidyr)
})

SUBGROUPS <- "all|boys|girls|native|migrant|first_born|later_born"

# Bin label -> bin number (labels look like "bin_1" ... "bin_20"; characters
# 5-6 hold the number, as in the old script)
bin_number <- function(label) {
  b <- suppressWarnings(as.integer(substr(label, 5, 6)))
  if (anyNA(b[!is.na(label)])) warning("Unexpected bin labels: ", paste(head(unique(label[is.na(b)])), collapse = ", "))
  b
}

# Reshape one gradient file.
#   outcomes: regex for the outcome part of the column names
#   style   : "suffix" -- estimate has no suffix, N as "_N", income as "inc"
#             "spraak" -- statistics as suffixes "_e", "_I", "_N"
#   widen   : put several outcomes side by side (estimate_<outcome>, ...)
#   outcome_suffix: text between outcome and subgroup (P90 files: "p90")
tidy_gradient <- function(df, outcomes = ".+", style = "suffix", widen = FALSE,
                          outcome_suffix = "") {
  names(df)[1] <- "bin"
  if (style == "suffix") {
    is_stat <- grepl("(_N|inc)$", names(df))
    names(df)[-1][!is_stat[-1]] <- paste0(names(df)[-1][!is_stat[-1]], "_estimate")
    stat_pattern <- "(estimate|N|inc)"
  } else {
    stat_pattern <- "(e|I|N)"
  }
  out <- df %>%
    pivot_longer(
      cols = -bin,
      names_to = c("cm", "outcome", "subgroup", ".value"),
      names_pattern = paste0("cm(\\d+)_(", outcomes, ")", outcome_suffix, "_(", SUBGROUPS, ")_", stat_pattern)
    ) %>%
    mutate(bin = bin_number(bin), cm = as.integer(cm))
  if (style == "spraak") out <- out %>% rename(estimate = e, inc = I)
  if (widen) {
    out <- out %>%
      pivot_wider(names_from = outcome, values_from = c(estimate, N, inc)) %>%
      arrange(cm, subgroup, bin)
  } else {
    out <- out %>% arrange(cm, outcome, subgroup, bin)
  }
  out
}

# BMI percentile files have columns bmi_cm<k>_<stat> (N, inc, p10 ... p90)
tidy_bmi_percentiles <- function(df) {
  names(df)[1] <- "bin"
  df %>%
    pivot_longer(cols = -bin, names_to = c("cm", ".value"), names_pattern = "bmi_cm(\\d+)_(.*)") %>%
    mutate(bin = bin_number(bin), cm = as.integer(cm)) %>%
    filter(!is.na(N)) %>%
    arrange(cm, bin)
}

# Apply `fun` to every input file whose name matches `pattern`
tidy_files <- function(pattern, fun) {
  files <- list.files(loc$gradients_folder, pattern = pattern, full.names = TRUE)
  files <- files[!grepl("tidy|export", basename(files)) & grepl("\\.xlsx$", files)]
  for (f in files) {
    out <- fun(read_xlsx(f, .name_repair = "minimal"))
    write_xlsx(out, sub("\\.xlsx$", "_tidy.xlsx", f))
  }
  message(sprintf("%-25s %d file(s)", pattern, length(files)))
}

tidy_files("visus",         function(d) tidy_gradient(d))
tidy_files("psychosociaal", function(d) tidy_gradient(d))
tidy_files("spraak",        function(d) tidy_gradient(d, style = "spraak"))
tidy_files("gradients_bmi", tidy_bmi_percentiles)
tidy_files("gradients_p90", function(d) {
  tidy_gradient(d, outcomes = "bmi|gewicht", outcome_suffix = "p90", widen = TRUE) %>%
    filter(!is.na(estimate_bmi) | !is.na(estimate_gewicht))
})
tidy_files("gradients_gewicht",     function(d) tidy_gradient(d, outcomes = "bmi|gewicht|lengte", widen = TRUE))
tidy_files("gradients_overgewicht", function(d) tidy_gradient(d, outcomes = "overgewicht|obesity", widen = TRUE))
