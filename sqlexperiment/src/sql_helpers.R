# Kansenkaart data preparation pipeline -- SQLite experiment
#
# Helper functions to read converted CBS microdata (.db, SQLite) instead of .sav.
#
# Design goals
#   1. Same result as read_sav(): the scripts after the read stay (almost) verbatim.
#      - Value labels are taken from the `value_labels` table inside each .db and
#        re-attached as haven_labelled vectors, so haven::as_factor() keeps working
#        exactly as before (incl. which codes turn into NA with levels = "labels").
#      - Rows come back in the original file order (ORDER BY rowid). The pipeline
#        relies on row order in a few places (e.g. `RINOBJECTNUMMER[1]` in
#        summarise(), `filter(row_number() == 1)`), so this matters for equality.
#   2. Speed: only the rows we need leave the database. ID filters are written to
#      TEMP tables and applied inside SQLite (`col IN (SELECT v FROM temp.f1)`),
#      instead of loading the whole table into R and filtering there.
#      Each element of `filter_in` is applied independently, which mirrors
#      `filter(RINPERSOON %in% a & RINPERSOONS %in% b)` (NOT a pairwise match).
#   3. Read-only: the source files on G:/K: are never modified (SQLITE_RO).
#      TEMP tables live in SQLite's separate temp store, so this works on a
#      read-only file.
#
# (c) ODISSEI Social Data Science team 2026

library(DBI)
library(RSQLite)
library(haven)
library(tibble)

# print one line per read (table, rows, seconds) -- handy to find bottlenecks
if (is.null(getOption("sqlexp.verbose"))) options(sqlexp.verbose = TRUE)


#### FIND FILES ####

# Converted files live in a "geconverteerde data" folder next to the catalog file.
# Look there first, then in `dir` itself. Mirrors the old get_*_filename()
# functions: return the last file in reverse alphabetical order (= latest version).
find_latest_db <- function(dir, pattern) {
  for (d in c(file.path(dir, "geconverteerde data"), dir)) {
    fl <- list.files(path = d, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    if (length(fl) > 0) return(sort(fl, decreasing = TRUE)[1])
  }
  stop("No .db file found in '", dir, "' (or its 'geconverteerde data' folder) for pattern: ", pattern)
}


# Full path of a file in the config: relative paths are relative to data_folder (G:/),
# absolute paths (H:/..., //server/...) are used as they are. Handy for a .db you
# converted yourself that is not yet moved to its `geconverteerde data` folder.
cbs_path <- function(loc, key) {
  p <- loc[[key]]
  if (is.null(p)) stop("No '", key, "' in file_locations")
  if (grepl("^([A-Za-z]:|/|\\\\)", p)) p else file.path(loc$data_folder, p)
}


#### VALUE LABELS ####

# The converter stores labels in a long table with one row per (variable, code, label).
# We detect the column names instead of hard-coding them; if detection fails,
# set them explicitly, e.g. options(sqlexp.label_cols = c("variable", "value", "label")).
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
    if (length(nms) != 3) {
      stop("Could not recognise the columns of `value_labels` (", paste(nms, collapse = ", "),
           "). Set options(sqlexp.label_cols = c(<variable col>, <value col>, <label col>)).")
    }
    var <- nms[1]; value <- nms[2]; label <- nms[3]   # fall back to column order
  }
  c(var = var, value = value, label = label)
}

# Turn a column into a haven_labelled vector, exactly like read_sav() returns it.
.attach_labels <- function(x, codes, labels) {
  if (is.numeric(x)) codes <- as.numeric(codes) else x <- as.character(x)
  haven::labelled(x, labels = setNames(codes, labels))
}


#### READ ####

#' Read (part of) a converted CBS table.
#'
#' @param path      path to the .db file
#' @param cols      columns to read (NULL = all), like read_sav(col_select = ...)
#' @param filter_in named list: column name -> vector of allowed values.
#'                  Equivalent to filter(col %in% values) in R, but done in SQLite.
#' @param numeric   columns to convert to numeric (the converter may store
#'                  numbers as TEXT; as.numeric() is a no-op if they are REAL)
#' @param labels    re-attach value labels from the `value_labels` table
#' @param match     "all": a row must pass every filter in `filter_in` (AND, default);
#'                  "any": a row must pass at least one of them (OR), e.g.
#'                  filter(RINPERSOONMa %in% x | RINPERSOONpa %in% x)
read_db <- function(path, cols = NULL, filter_in = NULL, numeric = NULL, labels = TRUE,
                    match = c("all", "any")) {
  match <- match.arg(match)
  t0  <- Sys.time()
  con <- dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(con), add = TRUE)

  # data table = the table that is not `value_labels` (named after the source file)
  tabs <- setdiff(dbListTables(con), "value_labels")
  stem <- tools::file_path_sans_ext(basename(path))
  tab  <- if (stem %in% tabs) stem else if (length(tabs) == 1) tabs else
    stop("Cannot determine the data table in ", path, ": ", paste(tabs, collapse = ", "))

  q <- function(x) as.character(dbQuoteIdentifier(con, x))
  col_sql <- if (is.null(cols)) "t.*" else paste0("t.", q(cols), collapse = ", ")

  # one TEMP table per ID filter
  where <- character()
  for (i in seq_along(filter_in)) {
    col  <- names(filter_in)[i]
    vals <- unique(as.character(filter_in[[i]]))
    vals <- vals[!is.na(vals)]
    tmp  <- paste0("f", i)
    dbExecute(con, sprintf("CREATE TEMP TABLE %s (v TEXT PRIMARY KEY)", tmp))
    # (an unqualified name resolves to the temp schema first, so this fills temp.fN)
    if (length(vals) > 0) dbAppendTable(con, tmp, data.frame(v = vals, stringsAsFactors = FALSE))
    where <- c(where, sprintf("t.%s IN (SELECT v FROM temp.%s)", q(col), tmp))
  }

  sql <- sprintf("SELECT %s FROM %s AS t%s ORDER BY t.rowid",
                 col_sql, q(tab),
                 if (length(where)) paste0(" WHERE ", paste(where, collapse = if (match == "all") " AND " else " OR ")) else "")
  dat <- as_tibble(dbGetQuery(con, sql))

  for (col in intersect(numeric, names(dat))) dat[[col]] <- as.numeric(dat[[col]])

  if (labels && !"value_labels" %in% dbListTables(con)) {
    warning("No `value_labels` table in ", basename(path), ": columns come back without value labels, ",
            "so as_factor() calls may give other results than with read_sav(). ",
            "See convert_files_to_sqlite/savtosqlite.py for the expected format.", call. = FALSE)
  }
  if (labels && "value_labels" %in% dbListTables(con)) {
    lab <- dbReadTable(con, "value_labels")
    if (nrow(lab) > 0) {
      lc  <- .label_cols(lab)
      for (col in intersect(unique(lab[[lc["var"]]]), names(dat))) {
        l <- lab[lab[[lc["var"]]] == col, ]
        dat[[col]] <- .attach_labels(dat[[col]], l[[lc["value"]]], l[[lc["label"]]])
      }
    }
  }

  if (isTRUE(getOption("sqlexp.verbose"))) {
    message(sprintf("  read_db  %-35s %10s rows  %6.1fs", tab, format(nrow(dat), big.mark = ","),
                    as.numeric(Sys.time() - t0, units = "secs")))
  }
  dat
}


#' Read a CBS file that may or may not be converted yet.
#'
#' The scripts call read_cbs() for every file that has (or will get) a .db version.
#' What happens depends only on the path in the config:
#'   - `.db`  -> read_db(): columns and ID filters are applied inside SQLite.
#'   - other  -> read_sav(col_select = cols), then the same filters in R.
#' Both give the same rows in the same (file) order, so switching a file from .sav
#' to .db is a config change only. `numeric` is only used for .db files (in a .sav
#' the numbers are already numeric).
read_cbs <- function(path, cols = NULL, filter_in = NULL, numeric = NULL,
                     match = c("all", "any")) {
  match <- match.arg(match)
  if (tolower(tools::file_ext(path)) == "db") {
    return(read_db(path, cols = cols, filter_in = filter_in, numeric = numeric, match = match))
  }

  t0  <- Sys.time()
  dat <- if (is.null(cols)) haven::read_sav(path) else haven::read_sav(path, col_select = tidyselect::all_of(cols))
  if (length(filter_in) > 0) {
    keep <- lapply(names(filter_in), function(col) {
      vals <- unique(as.character(filter_in[[col]]))
      as.character(haven::zap_labels(dat[[col]])) %in% vals[!is.na(vals)]
    })
    dat <- dat[Reduce(if (match == "all") `&` else `|`, keep), , drop = FALSE]
  }
  if (isTRUE(getOption("sqlexp.verbose"))) {
    message(sprintf("  read_sav %-35s %10s rows  %6.1fs", basename(path), format(nrow(dat), big.mark = ","),
                    as.numeric(Sys.time() - t0, units = "secs")))
  }
  dat
}
