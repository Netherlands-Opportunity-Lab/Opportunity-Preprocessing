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
ORG_CODES  <- c(LimburgNoord = 6, ZONL = 7, Verian = 10, org12 = 12)   # 4th organisation: never name it in public files
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


# ---- Reading CBS microdata: .db (SQLite) or .sav -----------------------------------

# The large CBS files are read from their SQLite versions (.db, in the
# "geconverteerde data" folder next to the catalog file), as in
# sqlexperiment/ for prim8_a: only the rows needed (filtered on IDs inside
# SQLite) and columns needed come back to R, instead of reading the whole
# .sav. Point a path in the config to the .sav instead to fall back to
# read_sav() (same result, but the whole file is read on every call).

#' Read (part of) a CBS file, .db or .sav.
#' @param file      path relative to loc$data_folder (or absolute)
#' @param cols      columns to read (NULL = all; case-insensitive; missing
#'                  columns are skipped, like any_of())
#' @param filter_in named list: column -> allowed values; each element is
#'                  applied separately, like filter(a %in% x & b %in% y)
read_cbs <- function(file, cols = NULL, filter_in = NULL) {
  path <- if (grepl("^([A-Za-z]:|/)", file)) file else cbs_path(file)
  if (grepl("\\.db$", path, ignore.case = TRUE)) {
    return(read_db(path, cols = cols, filter_in = filter_in))
  }
  dat <- if (is.null(cols)) read_sav(path) else
    read_sav(path, col_select = matches(paste0("^(", paste(cols, collapse = "|"), ")$"),
                                        ignore.case = TRUE))
  for (i in seq_along(filter_in)) {
    col <- names(dat)[toupper(names(dat)) == toupper(names(filter_in)[i])]
    dat <- dat[as_chr(dat[[col]]) %in% as.character(filter_in[[i]]), ]
  }
  dat
}

# read_db(): adapted from sqlexperiment/src/sql_helpers.R (ODISSEI Social Data
# Science team 2026). Opens the .db read-only; ID filters go into TEMP tables
# and are applied inside SQLite; rows come back in file order (ORDER BY rowid)
# and value labels from the `value_labels` table are re-attached, so the result
# looks like read_sav(). Differences: column names are matched
# case-insensitively, and it stops if an ID column comes back as a number
# (leading zeros lost).
read_db <- function(path, cols = NULL, filter_in = NULL) {
  if (!requireNamespace("RSQLite", quietly = TRUE) || !requireNamespace("DBI", quietly = TRUE)) {
    stop("Packages DBI and RSQLite are needed to read ", basename(path),
         ". Install them, or point this file to its .sav in config/jgz.yml.")
  }
  t0  <- Sys.time()
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # data table = the table that is not `value_labels` (named after the file)
  tabs <- setdiff(DBI::dbListTables(con), "value_labels")
  stem <- tools::file_path_sans_ext(basename(path))
  tab  <- if (stem %in% tabs) stem else if (length(tabs) == 1) tabs else
    stop("Cannot determine the data table in ", path, ": ", paste(tabs, collapse = ", "))
  fields <- DBI::dbListFields(con, tab)
  real_name <- function(x) fields[match(toupper(x), toupper(fields))]

  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  sel <- if (is.null(cols)) fields else na.omit(real_name(cols))
  col_sql <- paste0("t.", q(sel), collapse = ", ")

  # one TEMP table per ID filter
  where <- character()
  for (i in seq_along(filter_in)) {
    col <- real_name(names(filter_in)[i])
    if (is.na(col)) stop("Column ", names(filter_in)[i], " not in ", basename(path))
    vals <- unique(as.character(filter_in[[i]]))
    vals <- vals[!is.na(vals) & vals != ""]
    tmp  <- paste0("f", i)
    DBI::dbExecute(con, sprintf("CREATE TEMP TABLE %s (v TEXT PRIMARY KEY)", tmp))
    if (length(vals) > 0) DBI::dbAppendTable(con, tmp, data.frame(v = vals, stringsAsFactors = FALSE))
    where <- c(where, sprintf("t.%s IN (SELECT v FROM temp.%s)", q(col), tmp))
  }
  sql <- sprintf("SELECT %s FROM %s AS t%s ORDER BY t.rowid", col_sql, q(tab),
                 if (length(where)) paste0(" WHERE ", paste(where, collapse = " AND ")) else "")
  dat <- tibble::as_tibble(DBI::dbGetQuery(con, sql))

  ids <- names(dat)[grepl("^RINPERSOON(MA|PA)?$|^RINOBJECTNUMMER$", toupper(names(dat)))]
  num_ids <- ids[vapply(dat[ids], is.numeric, logical(1))]
  if (length(num_ids) > 0) {
    stop(paste(num_ids, collapse = ", "), " stored as numbers in ", basename(path),
         ": leading zeros are lost. Point this file to its .sav in config/jgz.yml.")
  }

  if ("value_labels" %in% DBI::dbListTables(con)) {
    lab <- DBI::dbReadTable(con, "value_labels")
    if (nrow(lab) > 0) {
      lc <- .label_cols(lab)
      for (col in intersect(unique(lab[[lc["var"]]]), names(dat))) {
        l <- lab[lab[[lc["var"]]] == col, ]
        codes <- l[[lc["value"]]]
        x <- dat[[col]]
        if (is.numeric(x)) codes <- as.numeric(codes) else x <- as.character(x)
        dat[[col]] <- haven::labelled(x, labels = setNames(codes, l[[lc["label"]]]))
      }
    }
  }
  message(sprintf("  read_db %-32s %10s rows  %6.1fs", tab, format(nrow(dat), big.mark = ","),
                  as.numeric(Sys.time() - t0, units = "secs")))
  dat
}

# Column names of the `value_labels` table (variable, value, label), detected
# as in sqlexperiment/; set options(sqlexp.label_cols = c(...)) if that fails.
.label_cols <- function(lab) {
  opt <- getOption("sqlexp.label_cols")
  if (!is.null(opt)) return(setNames(opt, c("var", "value", "label")))
  nms <- names(lab)
  pick <- function(candidates, exclude = character()) {
    hit <- nms[tolower(nms) %in% candidates & !(nms %in% exclude)]
    if (length(hit) == 1) hit else NA_character_
  }
  var   <- pick(c("variable", "var", "varname", "variable_name", "name", "column", "kolom"))
  value <- pick(c("value", "values", "code", "waarde", "val"), exclude = var)
  label <- pick(c("label", "labels", "value_label", "omschrijving", "lab"), exclude = c(var, value))
  if (anyNA(c(var, value, label))) {
    if (length(nms) != 3) stop("Could not recognise the columns of `value_labels` (",
                               paste(nms, collapse = ", "), "); set options(sqlexp.label_cols = ...)")
    var <- nms[1]; value <- nms[2]; label <- nms[3]
  }
  c(var = var, value = value, label = label)
}


# ---- GBA addresses and postcodes (used by 01 and 03) ------------------------------------

# Address spells of the given persons (GBAADRESOBJECTBUS). Open spells end on
# 2999-12-31. Column names may differ in case between file versions.
read_addresses <- function(rins) {
  cols <- c("RINPERSOONS", "RINPERSOON", "GBADATUMAANVANGADRESHOUDING",
            "GBADATUMEINDEADRESHOUDING", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER")
  read_cbs(loc$gba_adres_file, cols = cols, filter_in = list(RINPERSOON = rins)) %>%
    rename_with(toupper) %>%
    transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON),
              start = ymd_date(GBADATUMAANVANGADRESHOUDING),
              end   = coalesce(ymd_date(GBADATUMEINDEADRESHOUDING), as.Date("2999-12-31")),
              soortobjectnummer = as_chr(SOORTOBJECTNUMMER),
              rinobjectnummer   = as_chr(RINOBJECTNUMMER))
}

# Postcode (PC4) of address objects over time (VSLPOSTCODEBUS, with validity
# dates). `objects` = paste(soortobjectnummer, rinobjectnummer).
read_postcodes <- function(objects) {
  cols <- c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "DATUMAANVPOSTCODENUMADRES",
            "DATUMEINDPOSTCODENUMADRES", "POSTCODENUM")
  read_cbs(loc$vslpostcode_file, cols = cols,
           filter_in = list(RINOBJECTNUMMER = sub("^[^ ]* ", "", objects))) %>%
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
