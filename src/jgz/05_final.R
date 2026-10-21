# =============================================================================
# JGZ pilot -- 05. Final export bundle
#
# Collects the output that may leave CBS into a few workbooks in
# loc$export_folder:
#   maps_bmi_pc3_export.xlsx, maps_bmi_pc4_export.xlsx  BMI per area (02)
#   maps_sdq_export.xlsx, maps_wiechen_export.xlsx      SDQ and Wiechen per area (02)
#   gradients_export.xlsx                               checked gradients (04)
#   proportions_export.xlsx, regressions_export.xlsx,
#   descriptives_export.xlsx                            other results
# Only organisations 7 (ZONL) and 10 (Verian) are exported.
#
# The map tables are already suppressed in 02; here suppressed rows are
# dropped and the tables are split into the sheets that go to the map makers.
#
# Changes compared with the old script (see git history):
#   - map tables are read by sheet name from the 02 workbooks (the old fixed
#     cell ranges such as A1:H250 silently cut off rows)
#   - sheet names use the contact-moment names from 02 for PC3 and PC4 alike
#     (old: "4weeks" for PC3 and "4wks" for PC4; Wiechen "3y9m" is now "3years9m")
#   - organisation filter matches "_7_" / "_10_" style names only (the old
#     pattern "(7|10)" also matched e.g. "groep7" or "2017")
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

EXPORT_ORGS <- c(7, 10)
org_pattern <- paste0("_(", paste(EXPORT_ORGS, collapse = "|"), ")(_|\\.)")

# Unique sheet names of at most 31 characters
sheet_names <- function(x) {
  x <- substr(x, 1, 31)
  dup <- duplicated(x) | duplicated(x, fromLast = TRUE)
  x[dup] <- paste0(substr(x[dup], 1, 27), "_", seq_len(sum(dup)))
  x
}

write_workbook <- function(tables, file) {
  tables <- tables[vapply(tables, nrow, integer(1)) > 0]
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
    out[[paste0("bmi_", t)]] <- tot %>%
      filter(!is.na(n)) %>% select(all_of(area), bmi_mean, bmi_sd, n)
    out[[paste0("bmi_90p_", t)]] <- tot %>%
      filter(!is.na(bmi_90p)) %>% select(all_of(area), bmi_90p, bmi_90p_perc)
    out[[paste0("bmi_", t, "_sex")]] <- sex %>%
      filter(!is.na(n)) %>% select(all_of(area), geslacht, bmi_mean, bmi_sd, n)
    out[[paste0("bmi_90p_", t, "_sex")]] <- sex %>%
      filter(!is.na(bmi_90p)) %>% select(all_of(area), geslacht, bmi_90p, bmi_90p_perc)
  }
  write_workbook(out, file.path(loc$export_folder, paste0("maps_bmi_", geo, "_export.xlsx")))
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
write_workbook(out, file.path(loc$export_folder, "maps_sdq_export.xlsx"))

# --- Wiechen ----------------------------------------------------------------------
out <- list()
for (t in c("2years", "3years9m")) {
  for (geo in c("pc4", "pc3")) {
    area <- if (geo == "pc3") "postcode3" else "postcode"
    d <- read_map("jgz_maps_wiechen.xlsx", paste0("wiechen_", t, "_", geo)) %>% filter(!is.na(n))
    out[[paste0("wiechen_taal_", t, "_", geo)]] <- d %>%
      filter(!is.na(taal_ont_achts)) %>%
      select(all_of(area), n, taal_ont_achts, taal_ont_achts_perc)
    out[[paste0("wiechen_", t, "_", geo)]] <- d %>%
      select(-taal_ont_achts, -taal_ont_achts_perc) %>%
      pivot_longer(ends_with("_perc"), names_to = "outcome", values_to = "perc") %>%
      mutate(outcome = sub("_perc$", "", outcome)) %>%
      filter(!is.na(perc))
  }
}
write_workbook(out, file.path(loc$export_folder, "maps_wiechen_export.xlsx"))


# =============================================================================
# OTHER RESULTS (organisations 7 and 10 only)
# =============================================================================

# Copy the selected files to "using data" and return their new paths
select_and_copy <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  files <- files[!grepl("excl", basename(files))]
  if (length(files) == 0) warning("No files selected in ", folder)
  file.copy(files, file.path(using_folder, basename(files)), overwrite = TRUE)
  file.path(using_folder, basename(files))
}

bundle <- function(files, out_file, read_fun = read_excel, name_fun = basename, filter_fun = identity) {
  tables <- lapply(files, function(f) filter_fun(read_fun(f)))
  names(tables) <- name_fun(files)
  write_workbook(tables, file.path(loc$export_folder, out_file))
}

# --- Gradients (checked in 04) ---------------------------------------------------
gradient_files <- select_and_copy(
  loc$gradients_folder,
  paste0("^[^ ]*_(", paste(EXPORT_ORGS, collapse = "|"), ")_tidy_export\\.xlsx$")
)
bundle(gradient_files, "gradients_export.xlsx",
       name_fun = function(f) sub("^gradients_(.*)_tidy_export\\.xlsx$", "\\1", basename(f)))

# --- Proportions ---------------------------------------------------------------------
bundle(select_and_copy(loc$proportions_folder, org_pattern), "proportions_export.xlsx")

# --- Bereik (reach) regressions: drop rows with fewer than min_cell observations
bundle(select_and_copy(loc$bereik_folder, org_pattern), "regressions_export.xlsx",
       filter_fun = function(d) {
         # the number of observations is in `N`, or in column `c16` in older files
         n_col <- intersect(c("N", "c16"), names(d))[1]
         if (is.na(n_col)) stop("No N column (N or c16) found in a bereik file")
         d[!is.na(d[[n_col]]) & d[[n_col]] >= cfg$min_cell, ]
       })

# --- Descriptives ------------------------------------------------------------------
bundle(select_and_copy(loc$export_descriptives_folder, org_pattern), "descriptives_export.xlsx")
