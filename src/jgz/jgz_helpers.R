# JGZ preprocessing -- shared helpers
#
# Sourced at the top of every script in src/jgz. Loads the configuration and
# defines helpers used in more than one script.
#
# Compatibility: CBS Remote Access runs R 3.6.2 with dplyr 1.0.2. So: no `|>`
# pipe, no `\(x)` lambdas, no dplyr::join_by()/if_any()/pick() and no
# `relationship =` argument in joins. The helpers below use base R where
# speed matters (the health file has ~1 million rows).

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

if (!file.exists("config/jgz.yml")) {
  stop("config/jgz.yml not found. Run the JGZ scripts from the repository root ",
       "(setwd() to the Opportunity-Preprocessing folder first).")
}
loc <- config::get("file_locations", file = "config/jgz.yml")
cfg <- config::get("settings",       file = "config/jgz.yml")

# Paths inside the working folder and the CBS microdata folder
work_path <- function(...) file.path(loc$work_folder, ...)
cbs_path  <- function(...) file.path(loc$data_folder, ...)

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


# ---- Missing values and types ---------------------------------------------------

# Stata semantics: a string is missing when it is "", a number when it is NA.
is_blank <- function(x) {
  if (is.character(x)) is.na(x) | x == "" else is.na(x)
}

# RINPERSOONS etc. as plain text, whatever haven made of it
as_chr <- function(x) trimws(as.character(zap_labels(x)))

# Report a count next to the number in the Stata log, without stopping.
check_n <- function(actual, expected, what) {
  status <- if (isTRUE(all.equal(actual, expected))) "OK" else "DIFFERS"
  message(sprintf("[%s] %s: %s (Stata log: %s)", status, what,
                  format(actual, big.mark = ","), format(expected, big.mark = ",")))
  invisible(actual)
}


# ---- Dates --------------------------------------------------------------------------

ymd_date  <- function(x) as.Date(as_chr(x), format = "%Y%m%d")
# Month index = year * 12 + month - 1 (differences in months, like Stata %tm)
month_idx <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m")) - 1L


# ---- GBA addresses and postcodes (used by 01 and 03) ------------------------------------

# Address spells of the given persons (GBAADRESOBJECTBUS). Open spells end on
# 2999-12-31. Only the needed columns are read; column names may differ in
# case between file versions.
read_addresses <- function(rins) {
  cols <- "^(RINPERSOONS|RINPERSOON|GBADATUMAANVANGADRESHOUDING|GBADATUMEINDEADRESHOUDING|SOORTOBJECTNUMMER|RINOBJECTNUMMER)$"
  read_sav(cbs_path(loc$gba_adres_file), col_select = matches(cols, ignore.case = TRUE)) %>%
    rename_with(toupper) %>%
    mutate(RINPERSOON = as_chr(RINPERSOON)) %>%
    filter(RINPERSOON %in% rins) %>%
    transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON,
              start = ymd_date(GBADATUMAANVANGADRESHOUDING),
              end   = coalesce(ymd_date(GBADATUMEINDEADRESHOUDING), as.Date("2999-12-31")),
              soortobjectnummer = as_chr(SOORTOBJECTNUMMER),
              rinobjectnummer   = as_chr(RINOBJECTNUMMER))
}

# Postcode (PC4) of address objects over time (VSLPOSTCODEBUS, with validity
# dates). `objects` = paste(soortobjectnummer, rinobjectnummer).
read_postcodes <- function(objects) {
  cols <- "^(SOORTOBJECTNUMMER|RINOBJECTNUMMER|DATUMAANVPOSTCODENUMADRES|DATUMEINDPOSTCODENUMADRES|POSTCODENUM)$"
  read_sav(cbs_path(loc$vslpostcode_file), col_select = matches(cols, ignore.case = TRUE)) %>%
    rename_with(toupper) %>%
    mutate(soortobjectnummer = as_chr(SOORTOBJECTNUMMER), rinobjectnummer = as_chr(RINOBJECTNUMMER)) %>%
    filter(paste(soortobjectnummer, rinobjectnummer) %in% objects) %>%
    transmute(soortobjectnummer, rinobjectnummer,
              pc_start = ymd_date(DATUMAANVPOSTCODENUMADRES),
              pc_end   = coalesce(ymd_date(DATUMEINDPOSTCODENUMADRES), as.Date("2999-12-31")),
              # first 4 digits, also if a version holds the full postcode ("1234AB")
              postcode_4 = suppressWarnings(as.integer(substr(gsub("[^0-9]", "", as_chr(POSTCODENUM)), 1, 4))))
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
