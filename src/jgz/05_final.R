# =============================================================================
# JGZ pilot -- 05. Final export bundle
#
# Collects the output that may leave CBS into workbooks in loc$export_folder.
#
# Map tables (from 02). File and sheet names are kept as in the 2025 export,
# because the map scripts (pc4_map_JGZ.R) read them by name:
#   output_cases_postcode3_bmi_export.xlsx     sheets bmi_<t>, bmi_90p_<t>, ..._sex
#                                              (t = 4weeks, 8weeks, 3months, ...)
#   output_cases_postcode4_bmi_export.xlsx     same, t = 4wks, 8wks, 3months, ...
#   output_cases_postcode4_3_sdq_export.xlsx   sdq[11|14]_<t>_<pc4|pc3>
#   output_cases_postcode4_3_wiechen_export.xlsx  wiechen[_taal]_<2years|3y9m>_<pc4|pc3>
# The map tables are already suppressed in 02; here suppressed rows are
# dropped and the tables are split into these sheets.
#
# Other results (gradients from 04; proportions, bereik regressions and
# descriptives from the bereik analyses):
#   gradients_export.xlsx, proportions_export.xlsx, regressions_export.xlsx,
#   descriptives_export.xlsx
# Which organisations are exported is set in config/jgz.yml (export_orgs,
# export_pooled, export_excl). A file belongs to an organisation when its
# name contains "_<code>_" or "_<code>." (code 6, 7, 10 or 12); files with
# "excl" are the results without that organisation; files without a code are
# pooled. Sheets named org_<code> inside a workbook (e.g.
# descriptives_perorg.xlsx) are selected the same way.
#
# Changes compared with the old script (see git history):
#   - map tables are read by sheet name from the 02 workbooks (the old fixed
#     cell ranges such as A1:H250 silently cut off rows)
#   - organisation filter matches "_7_" / "_10." style names only (the old
#     pattern "(7|10)" also matched e.g. "groep7" or "2017"), and is set in
#     the config instead of fixed to 7 and 10
#   - copied files are overwritten (file.copy used to keep stale versions) and
#     only the files selected in this run are bundled
#   - sheet names are cut to Excel's 31-character limit
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(tidyr)
})

ensure_dir(loc$export_folder)
using_folder <- ensure_dir(file.path(loc$export_folder, "using data"))

EXPORT_ORGS <- unlist(cfg$export_orgs)
message("Exporting organisations: ", paste(EXPORT_ORGS, collapse = ", "),
        if (isTRUE(cfg$export_pooled)) " + pooled" else "",
        if (isTRUE(cfg$export_excl)) " + excluding-organisation results" else "")

# Organisation code in a file name ("_7_", "_10."), NA when there is none
org_in_name <- function(f) {
  m <- regmatches(basename(f), regexpr("_(6|7|10|12)(?=[_.])", basename(f), perl = TRUE))
  if (length(m) == 0) NA_real_ else as.numeric(sub("_", "", m))
}

# Should a file / sheet with this organisation code be exported?
export_this <- function(code, excl) {
  if (is.na(code)) return(isTRUE(cfg$export_pooled))
  if (excl) return(isTRUE(cfg$export_excl) && code %in% EXPORT_ORGS)
  code %in% EXPORT_ORGS
}

# Unique sheet names of at most 31 characters
sheet_names <- function(x) {
  x <- substr(x, 1, 31)
  dup <- duplicated(x) | duplicated(x, fromLast = TRUE)
  x[dup] <- paste0(substr(x[dup], 1, 27), "_", seq_len(sum(dup)))
  x
}

# keep_empty: also write sheets without rows (the map scripts expect every
# sheet to exist, even when everything in it was suppressed)
write_workbook <- function(tables, file, keep_empty = FALSE) {
  if (!keep_empty) tables <- tables[vapply(tables, nrow, integer(1)) > 0]
  nms <- sheet_names(names(tables))
  wb <- createWorkbook()
  for (i in seq_along(tables)) {
    addWorksheet(wb, nms[i])
    writeData(wb, nms[i], tables[[i]])
  }
  saveWorkbook(wb, file, overwrite = TRUE)
  message(sprintf("Written: %s (%d sheets)", file, length(tables)))
}

read_map <- function(file, sheet) read_xlsx(file.path(loc$maps_folder, file), sheet = sheet)

cms <- c("4wks", "8wks", "3months", "4months", "6months", "9months", "11months",
         "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2")

# Sheet-name spellings of the 2025 export (used by the map scripts):
# PC3 BMI sheets say "4weeks"/"8weeks", Wiechen sheets say "3y9m"
weeks_name   <- function(t) sub("^(4|8)wks$", "\\1weeks", t)
wiechen_name <- function(t) sub("^3years9m$", "3y9m", t)


# =============================================================================
# MAP TABLES
# =============================================================================

# --- BMI: one workbook per geography -------------------------------------------
for (geo in c("pc3", "pc4")) {
  area <- if (geo == "pc3") "postcode3" else "postcode"
  out <- list()
  for (t in cms) {
    tot <- read_map("jgz_maps_bmi.xlsx", paste0("bmi_", t, "_", geo))
    sex <- read_map("jgz_maps_bmi.xlsx", paste0("bmi_", t, "_", geo, "_sex"))
    tl  <- if (geo == "pc3") weeks_name(t) else t
    out[[paste0("bmi_", tl)]] <- tot %>%
      filter(!is.na(n)) %>% select(all_of(area), bmi_mean, bmi_sd, n)
    out[[paste0("bmi_90p_", tl)]] <- tot %>%
      filter(!is.na(bmi_90p)) %>% select(all_of(area), bmi_90p, bmi_90p_perc)
    out[[paste0("bmi_", tl, "_sex")]] <- sex %>%
      filter(!is.na(n)) %>% select(all_of(area), geslacht, bmi_mean, bmi_sd, n)
    out[[paste0("bmi_90p_", tl, "_sex")]] <- sex %>%
      filter(!is.na(bmi_90p)) %>% select(all_of(area), geslacht, bmi_90p, bmi_90p_perc)
  }
  geo_digits <- if (geo == "pc3") "3" else "4"
  write_workbook(out, file.path(loc$export_folder,
                                paste0("output_cases_postcode", geo_digits, "_bmi_export.xlsx")),
                 keep_empty = TRUE)
}

# --- SDQ ------------------------------------------------------------------------
out <- list()
for (t in c("groep2", "groep7", "klas2", "3years9m")) {
  for (geo in c("pc4", "pc3")) {
    area <- if (geo == "pc3") "postcode3" else "postcode"
    d <- read_map("jgz_maps_sdq.xlsx", paste0("sdq_", t, "_", geo))
    out[[paste0("sdq_", t, "_", geo)]]   <- d %>% filter(!is.na(n)) %>% select(all_of(area), sdq_mean, sdq_sd, n)
    out[[paste0("sdq11_", t, "_", geo)]] <- d %>% filter(!is.na(sdq_11_n)) %>% select(all_of(area), sdq_11_n, sdq_11_perc)
    out[[paste0("sdq14_", t, "_", geo)]] <- d %>% filter(!is.na(sdq_14_n)) %>% select(all_of(area), sdq_14_n, sdq_14_perc)
  }
}
write_workbook(out, file.path(loc$export_folder, "output_cases_postcode4_3_sdq_export.xlsx"),
               keep_empty = TRUE)

# --- Wiechen ----------------------------------------------------------------------
out <- list()
for (t in c("2years", "3years9m")) {
  tl <- wiechen_name(t)
  for (geo in c("pc4", "pc3")) {
    area <- if (geo == "pc3") "postcode3" else "postcode"
    d <- read_map("jgz_maps_wiechen.xlsx", paste0("wiechen_", t, "_", geo)) %>% filter(!is.na(n))
    out[[paste0("wiechen_taal_", tl, "_", geo)]] <- d %>%
      filter(!is.na(taal_ont_achts)) %>%
      select(all_of(area), taal_ont_achts, taal_ont_achts_perc)
    out[[paste0("wiechen_", tl, "_", geo)]] <- d %>%
      select(-taal_ont_achts, -taal_ont_achts_perc) %>%
      pivot_longer(ends_with("_perc"), names_to = "outcome", values_to = "perc") %>%
      mutate(outcome = sub("_perc$", "", outcome)) %>%
      filter(!is.na(perc))
  }
}
write_workbook(out, file.path(loc$export_folder, "output_cases_postcode4_3_wiechen_export.xlsx"),
               keep_empty = TRUE)


# =============================================================================
# OTHER RESULTS
# =============================================================================

# Select the files of the exported organisations (see header), copy them to
# "using data" and return their new paths. `pattern` picks the result type.
select_and_copy <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  keep <- vapply(files, function(f) {
    grepl("perorg", basename(f)) ||        # multi-sheet file, sheets filtered when read
      export_this(org_in_name(f), grepl("excl", basename(f)))
  }, logical(1))
  files <- files[keep]
  if (length(files) == 0) warning("No files selected in ", folder, " (pattern ", pattern, ")")
  file.copy(files, file.path(using_folder, basename(files)), overwrite = TRUE)
  file.path(using_folder, basename(files))
}

# Read all sheets of a file. Sheets named org_<code> are kept only for the
# exported organisations; in that case the sheet name is used as the name.
read_tables <- function(f) {
  sheets <- excel_sheets(f)
  is_org <- grepl("^org_\\d+$", sheets)
  if (!any(is_org)) {
    out <- list(read_excel(f, sheet = 1))
    names(out) <- basename(f)
    return(out)
  }
  codes <- as.numeric(sub("^org_", "", sheets[is_org]))
  keep <- sheets[is_org][codes %in% EXPORT_ORGS]
  out <- lapply(keep, function(sh) read_excel(f, sheet = sh))
  names(out) <- keep
  out
}

bundle <- function(files, out_file, name_fun = identity, filter_fun = identity) {
  tables <- do.call(c, lapply(files, read_tables))
  if (is.null(tables)) tables <- list()
  tables <- lapply(tables, filter_fun)
  names(tables) <- name_fun(names(tables))
  write_workbook(tables, file.path(loc$export_folder, out_file))
}

# --- Gradients (checked in 04) ---------------------------------------------------
bundle(select_and_copy(loc$gradients_folder, "^gradients_.*_tidy_export\\.xlsx$"),
       "gradients_export.xlsx",
       name_fun = function(x) sub("^gradients_(.*)_tidy_export\\.xlsx$", "\\1", x))

# --- Proportions ---------------------------------------------------------------------
bundle(select_and_copy(loc$proportions_folder, "proportions.*\\.xlsx?$"), "proportions_export.xlsx")

# --- Bereik (reach) regressions: drop rows with fewer than min_cell observations
bundle(select_and_copy(loc$bereik_folder, "^obs_.*\\.xlsx?$"), "regressions_export.xlsx",
       filter_fun = function(d) {
         # the number of observations is in `N`, or in column `c16` in older files
         n_col <- intersect(c("N", "c16"), names(d))[1]
         if (is.na(n_col)) stop("No N column (N or c16) found in a bereik file")
         d[!is.na(d[[n_col]]) & d[[n_col]] >= cfg$min_cell, ]
       })

# --- Descriptives ------------------------------------------------------------------
bundle(select_and_copy(loc$export_descriptives_folder, "descriptives.*\\.xlsx$"), "descriptives_export.xlsx")
