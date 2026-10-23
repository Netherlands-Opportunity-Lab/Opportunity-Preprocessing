# Kansenkaart data preparation pipeline -- SQLite experiment
#
# Converts one CBS .sav file to a SQLite .db in the format that read_db() /
# read_cbs() (sqlexperiment/src/sql_helpers.R) expect:
#   - one data table, named after the file (e.g. SECMBUS2024V1), with exactly the
#     same variables and rows (in the same order) as the .sav; value labels removed
#   - a table `value_labels` (variable, value, label) with all value labels, so
#     read_db() can turn the columns back into haven_labelled vectors. Without it,
#     calls like as_factor(SECM, levels = "values") give different results (or an
#     error in newer haven versions), so do not leave it out.
#   - an index on the ID columns that are present (RINPERSOON, RINOBJECTNUMMER),
#     which is what makes the filtered reads fast
#
# Note for the CBS request "Verplaatsen geconverteerde catalogusbestanden": the .db
# must have the same name as the catalog file, and the script that made it must have
# that name too and mention name, institution, project number and date. Copy this
# file to e.g. SECMBUS2024V1.R and fill in the header.
#
# Usage (in the RA):
#   source("sqlexperiment/convert_sav_to_db.R")
#   convert_sav_to_db("G:/InkomenBestedingen/SECMBUS/SECMBUS2024V1.SAV",
#                     "H:/geconverteerd/SECMBUS2024V1.db")
#
# (c) ODISSEI Social Data Science team 2026

library(DBI)
library(RSQLite)
library(haven)

convert_sav_to_db <- function(sav_path, db_path,
                              index_cols = c("RINPERSOON", "RINOBJECTNUMMER"),
                              table_name = tools::file_path_sans_ext(basename(sav_path))) {
  if (file.exists(db_path)) stop(db_path, " already exists; remove it first")
  t0  <- Sys.time()
  dat <- haven::read_sav(sav_path)

  # long table with all value labels: one row per (variable, value, label)
  labs <- do.call(rbind, lapply(names(dat), function(col) {
    l <- attr(dat[[col]], "labels", exact = TRUE)
    if (is.null(l) || length(l) == 0) return(NULL)
    data.frame(variable = col, value = as.character(unname(l)), label = names(l),
               stringsAsFactors = FALSE)
  }))
  if (is.null(labs)) labs <- data.frame(variable = character(0), value = character(0),
                                        label = character(0), stringsAsFactors = FALSE)

  # plain columns: character stays TEXT, numbers become REAL/INTEGER
  dat <- haven::zap_labels(dat)
  dat <- haven::zap_formats(dat)
  for (col in names(dat)) attr(dat[[col]], "label") <- NULL

  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con, table_name, as.data.frame(dat))
  dbWriteTable(con, "value_labels", labs)
  for (col in intersect(index_cols, names(dat))) {
    dbExecute(con, sprintf('CREATE INDEX "idx_%s_%s" ON "%s" ("%s")', table_name, col, table_name, col))
  }

  message(sprintf("%s: %s rows, %d columns, %d value labels -> %s (%.0f s)",
                  table_name, format(nrow(dat), big.mark = ","), ncol(dat), nrow(labs), db_path,
                  as.numeric(Sys.time() - t0, units = "secs")))
  invisible(db_path)
}
