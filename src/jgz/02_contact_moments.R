# =============================================================================
# JGZ preprocessing -- 02. Contact moments and derived variables
#
# Assigns every row of the panel (child x age) to a scheduled JGZ visit
# ("contact moment") and adds the variables the analyses use: BMI, sex, PC3
# and PC4 of the GBA address and the target age of the visit.
#
#   Input : loc$panel_postcode_file (output of 01)
#   Output: loc$contact_moment_folder/cm_<visit>.rds (one file per visit, all
#           rows of the panel in that window)
#
# Which measurement to use when a child has several in a window is decided per
# outcome in the analyses: the one closest to `target_age`, the nominal age of
# the visit.
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")

ensure_dir(loc$contact_moment_folder)


# ---- Contact-moment windows -------------------------------------------------------
# Age windows in days, (min_age, max_age], taken over from the original script
# (exported from CBS, 21 Oct 2026). Up to 3 years they are in days; from the
# upper bound of 3y9m on, that script used months (leeftijd / 30: 60, 93, 153
# months), converted here with the same factor (60 months = 1800 days etc.).
# The bounds are irregular, lie just before the next visit and switch from days
# to months exactly where Coen's histogram/dal method does ("20210705 ANALYSIS
# GRADIENTS"): most likely the result of that method on the JGZ data, fixed in
# the script. Not documented -- to be confirmed with Bastian.
#
# target_age = nominal age of the visit; groep7 (3819) is the mean age in that
# window; klas2 has no real upper bound (27060 days = 902 months).
contact_moments <- data.frame(
  cm = c("4wks", "8wks", "3months", "4months", "6months", "9months", "11months",
         "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2"),
  min_age = c(0, 46, 79, 111, 169, 261, 320, 405, 513, 677, 1038, 1319, 1800, 2790, 4590),
  max_age = c(46, 79, 111, 169, 261, 320, 405, 513, 677, 1038, 1319, 1800, 2790, 4590, 27060),
  target_age = c(28, 56, 90, 120, 181, 273, 334, 426, 547, 730, 1095, 1368, 2190, 3819, 5140),
  stringsAsFactors = FALSE
)


# ---- Derived variables ------------------------------------------------------------
jgz <- readRDS(loc$panel_postcode_file) %>%
  mutate(
    pc4 = gba_postcode,
    pc3 = floor(gba_postcode / 10),
    # geslacht: 1 = boy, 2 = girl (checked in the data, 23 Oct 2026)
    sex = case_when(geslacht == 1 ~ "boy",
                    geslacht == 2 ~ "girl"),
    # weight in grams, length in mm (as in the BDS; Coen also divides by 1000)
    BMI = (gewicht / 1000) / (lengte / 1000)^2,
    # rows with age 0 or above the last window get NA and are not written
    cm = as.character(cut(leeftijd,
                          breaks = c(contact_moments$min_age, max(contact_moments$max_age)),
                          labels = contact_moments$cm, right = TRUE))
  ) %>%
  left_join(contact_moments %>% select(cm, target_age), by = "cm")

print(table(geslacht = jgz$geslacht, useNA = "ifany"))
if (mean(is.na(jgz$sex)) > 0.05) {
  warning("More than 5% of rows have no sex (geslacht not 1 or 2)")
}

print(table(cm = factor(jgz$cm, levels = contact_moments$cm), useNA = "ifany"))

bmi_check <- jgz %>% filter(!is.na(cm)) %>% group_by(cm) %>%
  summarise(n_bmi = sum(!is.na(BMI)), median_bmi = median(BMI, na.rm = TRUE), .groups = "drop") %>%
  arrange(match(cm, contact_moments$cm))
print(bmi_check)
if (any(bmi_check$median_bmi < 10 | bmi_check$median_bmi > 30, na.rm = TRUE)) {
  warning("Median BMI outside 10-30 for some visits: check the units of gewicht/lengte")
}


# ---- Write one file per contact moment ----------------------------------------------
for (k in contact_moments$cm) {
  saveRDS(jgz %>% filter(cm == k), file.path(loc$contact_moment_folder, paste0("cm_", k, ".rds")))
}
message("Written: ", nrow(contact_moments), " contact-moment files in ", loc$contact_moment_folder)
