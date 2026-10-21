# =============================================================================
# JGZ pilot -- 04. Disclosure checks on the tidy gradient files
#
#   1. Combines the per-contact-moment speech/language (spraak) subgroup files
#      into one file (overall, per organisation and excluding each organisation)
#   2. Runs the checks from check_function.R on every tidy gradient file and
#      writes <name>_export.xlsx
#
#   Input/output folder: loc$gradients_folder (output of 03)
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")
source("src/jgz/check_function.R")
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
})

gradient_files <- function(pattern) {
  list.files(loc$gradients_folder, pattern = pattern, full.names = TRUE)
}

combine_files <- function(files, out_name) {
  if (length(files) == 0) return(invisible(NULL))
  combined <- bind_rows(lapply(files, read_xlsx))
  write_xlsx(combined, file.path(loc$gradients_folder, out_name))
  message(sprintf("Combined %d file(s) into %s", length(files), out_name))
}


# ---- 1. Combine speech/language subgroup files over contact moments -----------
combine_files(gradient_files("cm\\d+spraak_subgroups_tidy"), "gradients_spraak_subgroups_tidy.xlsx")
for (org in ORG_CODES) {
  combine_files(gradient_files(paste0("cm\\d+spraak_subgroups_", org, "_tidy")),
                paste0("gradients_spraak_subgroups_", org, "_tidy.xlsx"))
  combine_files(gradient_files(paste0("cm\\d+spraak_subgroups_excl_", org, "_tidy")),
                paste0("gradients_spraak_subgroups_excl_", org, "_tidy.xlsx"))
}


# ---- 2. Checks ---------------------------------------------------------------------
check_files <- function(pattern, check, exclude = NULL) {
  files <- gradient_files(pattern)
  files <- files[!grepl("export", basename(files))]
  if (!is.null(exclude)) files <- files[!grepl(exclude, basename(files))]
  for (f in files) {
    checked <- check(read_xlsx(f))
    write_xlsx(checked, sub("\\.xlsx$", "_export.xlsx", f))
  }
  message(sprintf("%-40s %d file(s) checked", pattern, length(files)))
}

min_cell <- cfg$min_cell

# Speech/language, psychosocial, vision (long format). The per-contact-moment
# spraak files are skipped: they are covered by the combined files above.
check_files("(spraak|visus|psychosociaal).*_tidy\\.xlsx$",
            function(d) simple_check(d, min_cell), exclude = "_cm")
# Weight-related outcomes (wide format)
check_files("gradients_overgewicht.*_tidy\\.xlsx$",
            function(d) complex_check(d, c("overgewicht", "obesity"), min_cell))
check_files("gradients_gewicht.*_tidy\\.xlsx$",
            function(d) complex_check(d, c("bmi", "gewicht", "lengte"), min_cell))
check_files("gradients_p90.*_tidy\\.xlsx$",
            function(d) complex_check(d, c("bmi", "gewicht"), min_cell))
check_files("gradients_bmi.*_tidy\\.xlsx$",
            function(d) bmi_p_check(d, min_cell))
