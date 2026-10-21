# JGZ pilot pipeline -- shared helpers
#
# Sourced at the top of every script in src/jgz. Loads the configuration and
# defines small helpers used in more than one script.
#
# Compatibility: CBS Remote Access runs R 3.6.2 with dplyr 1.0.2. So: no `|>`
# pipe, no `\(x)` lambdas, no dplyr::join_by()/if_any()/pick() and no
# `relationship =` argument in joins. The helpers below use base R where
# speed matters (the health file has ~1 million rows).

suppressPackageStartupMessages({
  library(dplyr)
})

if (!file.exists("config/jgz.yml")) {
  stop("config/jgz.yml not found. Run the JGZ scripts from the repository root ",
       "(setwd() to the Opportunity-Preprocessing folder first).")
}
loc <- config::get("file_locations", file = "config/jgz.yml")
cfg <- config::get("settings",       file = "config/jgz.yml")

# Paths inside the working folder
work_path <- function(...) file.path(loc$work_folder, ...)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

# Organisation codes used in the JGZ files (bestand 1-4 <-> org 6/7/10/12)
ORG_CODES  <- c(LimburgNoord = 6, ZONL = 7, Verian = 10, Icare = 12)
# Health domains (`databestand` in the health file)
DOMAIN_CODES <- c(overgewicht = 1, visus = 2, psychosociaal = 3, `spraak-taal` = 4)

# Marker for string values removed because of a genuine conflict (Stata `.r`)
REMOVED <- ".r"


# ---- Missing values ---------------------------------------------------------

# Stata semantics: a string is missing when it is "", a number when it is NA.
is_blank <- function(x) {
  if (is.character(x)) is.na(x) | x == "" else is.na(x)
}

# Report a count next to the number in the Stata log, without stopping.
check_n <- function(actual, expected, what) {
  status <- if (isTRUE(all.equal(actual, expected))) "OK" else "DIFFERS"
  message(sprintf("[%s] %s: %s (Stata log: %s)", status, what,
                  format(actual, big.mark = ","), format(expected, big.mark = ",")))
  invisible(actual)
}


# ---- Fast grouping (base R) --------------------------------------------------

# Integer group id (1..G) for every row, from one or more key columns.
# Missing key values form their own group, like in Stata.
group_id <- function(df, keys) {
  id <- rep(1L, nrow(df))
  for (k in keys) {
    code <- match(df[[k]], unique(df[[k]]))
    combined <- (as.numeric(id) - 1) * (max(code) + 1) + code
    id <- match(combined, unique(combined))
  }
  id
}

# Per-group "first non-missing value in row order" (Stata collapse (firstnm))
group_firstnm <- function(x, g, n_groups) {
  idx <- which(!is_blank(x))
  idx <- idx[!duplicated(g[idx])]
  out <- x[rep(NA_integer_, n_groups)]
  out[g[idx]] <- x[idx]
  out
}

# Per-group maximum ignoring missing (Stata collapse (max)); NA if all missing
group_max <- function(x, g, n_groups) {
  idx <- which(!is.na(x))
  o <- idx[order(g[idx], x[idx], decreasing = c(FALSE, TRUE), method = "radix")]
  o <- o[!duplicated(g[o])]
  out <- x[rep(NA_integer_, n_groups)]
  out[g[o]] <- x[o]
  out
}

# Per-group minimum of the non-missing values (strings: "" counts as missing)
group_min <- function(x, g, n_groups) {
  idx <- which(!is_blank(x))
  o <- idx[order(g[idx], x[idx], method = "radix")]
  o <- o[!duplicated(g[o])]
  out <- x[rep(NA_integer_, n_groups)]
  out[g[o]] <- x[o]
  out
}

# Collapse to one row per group: (firstnm) for `firstnm_vars`, (max) for
# `max_vars`. Variables in neither list are dropped, like Stata's collapse.
collapse_groups <- function(df, keys, firstnm_vars, max_vars) {
  g <- group_id(df, keys)
  n_groups <- max(g)
  first_row <- which(!duplicated(g))
  out <- df[first_row, keys, drop = FALSE]
  out <- out[order(g[first_row]), , drop = FALSE]
  for (v in firstnm_vars) out[[v]] <- group_firstnm(df[[v]], g, n_groups)
  for (v in max_vars)     out[[v]] <- group_max(df[[v]], g, n_groups)
  out
}

# Collapse all non-key variables: strings (firstnm), numbers (max)
collapse_all <- function(df, keys) {
  vars <- setdiff(names(df), keys)
  is_chr <- vapply(df[vars], is.character, logical(1))
  collapse_groups(df, keys, vars[is_chr], vars[!is_chr])
}


# ---- Disclosure control for the map tables (02, 05) ----------------------------

# Suppress a statistic when the cell has fewer than `min_cell` observations.
suppress_small_n <- function(value, n, min_cell = cfg$min_cell) {
  ifelse(n < min_cell, NA, value)
}

# Suppress a count (and its percentage) when the count itself or its
# complement (n - count) is below `min_cell`.
suppress_count <- function(value, count, n, min_cell = cfg$min_cell) {
  ifelse(is.na(count) | count < min_cell | (n - count) < min_cell, NA, value)
}
