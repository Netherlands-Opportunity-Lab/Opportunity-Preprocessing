# =============================================================================
# JGZ preprocessing -- 02. Contact moments and derived variables
#
# Assigns every row of the panel (child x age) to a scheduled JGZ visit
# ("contact moment") and adds the variables the analyses use: BMI, sex, PC3
# and PC4 of the GBA address, age in months, the target age of the visit.
#
#   Input : loc$panel_postcode_file (output of 01)
#   Output: loc$contact_moment_folder/cm_<visit>.rds  (one file per visit,
#           all rows of the panel in that window)
#           loc$contact_moment_folder/contact_moment_windows.xlsx (the windows
#           of both methods, for comparison)
#
# Contact moments (cfg$cm_method):
#   "fixed"     -- the age windows of the earlier script (below)
#   "histogram" -- Coen's histogram/dal method ("20210705 ANALYSIS GRADIENTS",
#                  step 1; Bastian 30 Sep 2026): windows cut at the lowest
#                  point between the peaks of the age distribution. Peaks are
#                  searched near the expected age (+-21 days; groep 2 +-100);
#                  from 3y9m on, groep 2, groep 7 and klas 2 use age in months.
# Which measurement to use when a child has several in a window is decided
# per outcome in the analyses; `target_age` is the age to be closest to: the
# nominal age (cfg$cm_target = "nominal", Bastian) or the peak (Coen, "peak").
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages(library(openxlsx))

ensure_dir(loc$contact_moment_folder)


# ---- Fixed windows ------------------------------------------------------------------
# Age windows in days, (min_age, max_age]. target_age = nominal age; groep7
# (3819) is the mean age in that window; klas2 has no real upper bound
# (27060 days = 902 months).
contact_moments <- data.frame(
  cm = c("4wks", "8wks", "3months", "4months", "6months", "9months", "11months",
         "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2"),
  min_age = c(0, 46, 79, 111, 169, 261, 320, 405, 513, 677, 1038, 1319, 1800, 2790, 4590),
  max_age = c(46, 79, 111, 169, 261, 320, 405, 513, 677, 1038, 1319, 1800, 2790, 4590, 27060),
  target_age = c(28, 56, 90, 120, 181, 273, 334, 426, 547, 730, 1095, 1368, 2190, 3819, 5140),
  stringsAsFactors = FALSE
)


# ---- Histogram windows (Coen) -----------------------------------------------------------
histogram_windows <- function(age_days, age_months) {
  peak_at <- function(n, ages, center, half) {
    idx <- which(ages >= center - half & ages <= center + half)
    top <- max(n[idx])
    mean(ages[idx][n[idx] == top])
  }
  trough_between <- function(n, ages, from, to) {
    idx <- which(ages >= from & ages <= to)
    low <- min(n[idx])
    min(ages[idx][n[idx] == low])
  }
  days <- 0:2199
  n_day <- tabulate(age_days[!is.na(age_days) & age_days >= 0 & age_days <= 2199] + 1, length(days))
  exp_day <- c(4 * 7, 8 * 7, 3 * 30, 4 * 30, 6 * 30, 9 * 30, 11 * 30, 14 * 30, 18 * 30,
               2 * 365, 3 * 365, 3 * 365 + 9 * 30, 300 * 7)
  # Coen searches the groep-2 peak value within +-100 days but its day within
  # +-21; +-100 is used for both here.
  peak_day <- vapply(seq_along(exp_day), function(i)
    peak_at(n_day, days, exp_day[i], if (i == 13) 100 else 21), numeric(1))
  cut_day <- vapply(1:12, function(i) trough_between(n_day, days, peak_day[i], peak_day[i + 1]), numeric(1))

  months <- 0:254
  n_month <- tabulate(age_months[!is.na(age_months) & age_months >= 0 & age_months <= 254] + 1, length(months))
  peak_month <- vapply(c(46, 70, 130, 170), function(e) peak_at(n_month, months, e, 10), numeric(1))
  cut_month <- vapply(1:3, function(i) trough_between(n_month, months, peak_month[i], peak_month[i + 1]), numeric(1))

  list(peak_day = peak_day[1:12], cut_day = cut_day,
       peak_month = peak_month[2:4], cut_month = cut_month)   # cut_month: after 3y9m, groep 2, groep 7
}

# Contact moment 1-15 per row, as Coen assigned cm_allocated
assign_histogram_cm <- function(age_days, age_months, w) {
  cm <- rep(NA_integer_, length(age_days))
  cm[age_days >= 0 & age_days <= w$cut_day[1]] <- 1L
  for (i in 2:12) cm[age_days > w$cut_day[i - 1] & age_days <= w$cut_day[i]] <- i
  cm[age_months > w$cut_month[1] & age_months <= w$cut_month[2] + 1] <- 13L
  cm[age_months > w$cut_month[2] & age_months <= w$cut_month[3] + 1] <- 14L
  cm[age_months > w$cut_month[3]] <- 15L
  cm
}


# ---- Derived variables ------------------------------------------------------------------
jgz <- readRDS(loc$panel_postcode_file) %>%
  mutate(
    pc4 = gba_postcode,
    pc3 = floor(gba_postcode / 10),
    sex = case_when(geslacht == cfg$sex_boy  ~ "boy",
                    geslacht == cfg$sex_girl ~ "girl"),
    # weight in grams, length in mm (as in the BDS; Coen also divides by 1000)
    BMI = (gewicht / 1000) / (lengte / 1000)^2,
    # age in calendar months, as Coen (contact month - birth month); the birth
    # date follows from the contact date and the age in days
    contact_date = as.Date(dat_a_str, format = "%Y%m%d"),
    age_months = month_idx(contact_date) - month_idx(contact_date - leeftijd),
    cm_fixed = as.character(cut(leeftijd,
                                breaks = c(contact_moments$min_age, max(contact_moments$max_age), Inf),
                                labels = c(contact_moments$cm, "older"), right = TRUE))
  )

print(table(geslacht = jgz$geslacht, useNA = "ifany"))
if (mean(is.na(jgz$sex)) > 0.05) {
  warning("More than 5% of rows have an unknown sex -- check cfg$sex_boy / cfg$sex_girl")
}


# ---- Contact moments ----------------------------------------------------------------------
hw <- histogram_windows(jgz$leeftijd, jgz$age_months)
jgz$cm_histogram <- contact_moments$cm[assign_histogram_cm(jgz$leeftijd, jgz$age_months, hw)]

window_table <- contact_moments %>%
  mutate(hist_peak = c(hw$peak_day, round(hw$peak_month * 30.4375)),
         hist_upper_day = c(hw$cut_day, rep(NA, 3)),
         hist_peak_month = c(rep(NA, 12), hw$peak_month),
         hist_upper_month = c(rep(NA, 11), hw$cut_month, NA),
         n_fixed = as.vector(table(factor(jgz$cm_fixed, levels = cm))),
         n_histogram = as.vector(table(factor(jgz$cm_histogram, levels = cm))))
print(window_table)
write.xlsx(window_table, file.path(loc$contact_moment_folder, "contact_moment_windows.xlsx"))
message("Rows assigned to a different contact moment by the two methods: ",
        sum(jgz$cm_fixed != jgz$cm_histogram, na.rm = TRUE))

cm_method <- if (is.null(cfg$cm_method)) "fixed" else cfg$cm_method
jgz$cm <- if (cm_method == "histogram") jgz$cm_histogram else jgz$cm_fixed
if (identical(cfg$cm_target, "peak")) contact_moments$target_age <- window_table$hist_peak
message("Contact moments: ", cm_method, " windows; target age: ",
        if (identical(cfg$cm_target, "peak")) "peak" else "nominal age")
jgz <- jgz %>% left_join(contact_moments %>% select(cm, target_age), by = "cm")

bmi_check <- jgz %>% group_by(cm) %>%
  summarise(n_bmi = sum(!is.na(BMI)), median_bmi = median(BMI, na.rm = TRUE), .groups = "drop") %>%
  arrange(match(cm, contact_moments$cm))
print(bmi_check)
if (any(bmi_check$median_bmi < 10 | bmi_check$median_bmi > 30, na.rm = TRUE)) {
  warning("Median BMI outside 10-30 for some visits: check the units of gewicht/lengte")
}

for (k in contact_moments$cm) {
  saveRDS(jgz %>% filter(cm == k), file.path(loc$contact_moment_folder, paste0("cm_", k, ".rds")))
}
message("Written: ", nrow(contact_moments), " contact-moment files in ", loc$contact_moment_folder)
