# =============================================================================
# JGZ pilot -- 02. Tables for the maps (BMI, Wiechen language items, SDQ)
#
# Assigns every contact moment to a scheduled JGZ visit ("contact moment",
# based on age), keeps one measurement per child per visit and aggregates
# per postcode area (PC3 and PC4, from the GBA address -- see 01).
#
#   Input : loc$panel_postcode_file (output of 01)
#   Output: loc$contact_moment_folder/cm_<visit>.rds   (one file per visit)
#           loc$maps_folder/jgz_maps_bmi.xlsx          sheets bmi_<visit>_<pc3|pc4>[_sex]
#           loc$maps_folder/jgz_maps_wiechen.xlsx      sheets wiechen_<visit>_<pc3|pc4>
#           loc$maps_folder/jgz_maps_sdq.xlsx          sheets sdq_<visit>_<pc3|pc4>
#
# Disclosure control (CBS output rules, cfg$min_cell = 10):
#   - areas with fewer than min_cell children are suppressed entirely
#   - a count (and its %) is suppressed when the count or n - count < min_cell
#   Not handled: secondary suppression (e.g. boys = total - girls). Check
#   this before requesting export.
#
# Changes compared with the old script (see git history):
#   - one function per outcome instead of 15 copied blocks
#   - 14 months: target age 426 days (was 273, copied from 9 months)
#   - top-10% BMI flag: sex coding from config (was compared against text
#     labels, so the flag was 0 for every visit except 4 weeks)
#   - BMI outliers: z-score within visit x sex (was one z-score over all ages)
#   - Wiechen: geography from one row per child (the old join on all
#     postcode combinations counted children twice)
#   - SDQ: one measurement per child per visit (was every measurement)
#   - geography always from the GBA postcode (PC4 tables used the JGZ postcode)
#   - all tables written for both PC3 and PC4, with and without sex split
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages(library(openxlsx))

ensure_dir(loc$contact_moment_folder)
ensure_dir(loc$maps_folder)


# ---- Contact moments ----------------------------------------------------------
# Age windows in days, (min_age, max_age]. The old script mixed days and
# months (leeftijd / 30); the month bounds are converted with the same factor
# (60 months = 1800 days etc.), so the windows are unchanged. target_age is
# used to pick one measurement per child when there are several in a window.
contact_moments <- data.frame(
  cm = c("4wks", "8wks", "3months", "4months", "6months", "9months", "11months",
         "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2"),
  min_age = c(0, 46, 79, 111, 169, 261, 320, 405, 513, 677, 1038, 1319, 1800, 2790, 4590),
  max_age = c(46, 79, 111, 169, 261, 320, 405, 513, 677, 1038, 1319, 1800, 2790, 4590, 27060),
  target_age = c(28, 56, 90, 120, 181, 273, 334, 426, 547, 730, 1095, 1368, 2190, 3819, 5140),
  stringsAsFactors = FALSE
)
# groep7 target (3819) is the mean age in that window; klas2 window has no
# real upper bound (27060 days = 902 months, as in the old script).


# ---- Prepare data -----------------------------------------------------------------
wiechen_items <- c("zin2w", "pop6", "noemt", "wijstaan", "zin3w", "verstaan1",
                   "spontaan", "vragen1", "verstaan2", "vragen2")

# The raw health file codes the Wiechen items as numbers (codebook: 1 = plus,
# 2 = min, 3 = M / onbekend); the old Stata panel had the text "+", "-", "M".
# Both are turned into "+", "-", "M" here, anything else becomes missing.
wiechen_code <- function(x) {
  x <- trimws(as.character(haven::zap_labels(x)))
  x <- unname(c("1" = "+", "2" = "-", "3" = "M", "+" = "+", "-" = "-", "M" = "M")[x])
  x
}

jgz <- readRDS(loc$panel_postcode_file) %>%
  mutate(
    # Wiechen items as "+" / "-" / "M"; "" and ".r" (conflict marker from 00) missing
    across(any_of(wiechen_items), wiechen_code),
    pc4 = gba_postcode,
    pc3 = floor(gba_postcode / 10),
    sex = case_when(geslacht == cfg$sex_boy  ~ "boy",
                    geslacht == cfg$sex_girl ~ "girl"),
    # weight in grams, length in mm (check the summary printed below)
    BMI = (gewicht / 1000) / (lengte / 1000)^2,
    cm  = as.character(cut(leeftijd,
                           breaks = c(contact_moments$min_age, max(contact_moments$max_age), Inf),
                           labels = c(contact_moments$cm, "older"), right = TRUE))
  ) %>%
  left_join(contact_moments %>% select(cm, target_age), by = "cm")

print(table(geslacht = jgz$geslacht, useNA = "ifany"))
if (mean(is.na(jgz$sex)) > 0.05) {
  warning("More than 5% of rows have an unknown sex -- check cfg$sex_boy / cfg$sex_girl")
}
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


# ---- Helpers ----------------------------------------------------------------------

# One measurement per child: closest to the target age, then the oldest.
one_per_child <- function(d) {
  d %>%
    mutate(dist = abs(target_age - leeftijd)) %>%
    arrange(RINPERSOONS, RINPERSOON, dist, desc(leeftijd)) %>%
    distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
    select(-dist)
}

# Apply a summary function per area, for PC3 and PC4, with and without sex.
# Returns a named list of tables: <prefix>_pc3, <prefix>_pc3_sex, ...
area_tables <- function(d, prefix, summary_fun, by_sex = TRUE) {
  out <- list()
  for (geo in c("pc3", "pc4")) {
    d_geo <- d %>% filter(!is.na(.data[[geo]])) %>% rename(area = all_of(geo))
    out[[paste0(prefix, "_", geo)]] <- summary_fun(group_by(d_geo, area)) %>%
      rename(!!(if (geo == "pc3") "postcode3" else "postcode") := area)
    if (by_sex) {
      out[[paste0(prefix, "_", geo, "_sex")]] <- summary_fun(group_by(d_geo %>% filter(!is.na(sex)), area, sex)) %>%
        rename(!!(if (geo == "pc3") "postcode3" else "postcode") := area, geslacht = sex)
    }
  }
  out
}

# Suppress rows of areas with too few children; NA n means "suppressed".
drop_small <- function(tab) {
  tab %>% mutate(n = ifelse(n < cfg$min_cell, NA, n))
}

write_sheets <- function(tables, file) {
  wb <- createWorkbook()
  for (nm in names(tables)) {
    addWorksheet(wb, nm)
    writeData(wb, nm, tables[[nm]])
  }
  saveWorkbook(wb, file, overwrite = TRUE)
  message("Written: ", file, " (", length(tables), " sheets)")
}


# =============================================================================
# BMI
# =============================================================================
bmi_summary <- function(g) {
  g %>%
    summarise(n = n(), bmi_mean = mean(BMI), bmi_sd = sd(BMI),
              bmi_90p = sum(bmi_90), .groups = "drop") %>%
    mutate(
      bmi_mean = round(suppress_small_n(bmi_mean, n), 2),
      bmi_sd   = round(suppress_small_n(bmi_sd, n), 2),
      bmi_90p_perc = round(suppress_count(bmi_90p / n * 100, bmi_90p, n), 1),
      bmi_90p      = suppress_count(bmi_90p, bmi_90p, n)
    ) %>%
    drop_small()
}

bmi_tables <- list()
for (k in contact_moments$cm) {
  d <- jgz %>%
    filter(cm == k, !is.na(BMI), !is.na(sex)) %>%
    # outliers: |z| > bmi_z_max within visit x sex
    group_by(sex) %>%
    mutate(z = (BMI - mean(BMI)) / sd(BMI)) %>%
    ungroup()
  n_out <- sum(abs(d$z) > cfg$bmi_z_max, na.rm = TRUE)
  d <- d %>%
    filter(is.na(z) | abs(z) <= cfg$bmi_z_max) %>%
    one_per_child() %>%
    # top 10% BMI within sex (90th percentile of this visit)
    group_by(sex) %>%
    mutate(bmi_90 = as.integer(BMI >= quantile(BMI, 0.9))) %>%
    ungroup()
  message(sprintf("BMI %-8s: %6d children, %d outliers removed", k, nrow(d), n_out))
  bmi_tables <- c(bmi_tables, area_tables(d, paste0("bmi_", k), bmi_summary))
}
write_sheets(bmi_tables, file.path(loc$maps_folder, "jgz_maps_bmi.xlsx"))


# =============================================================================
# WIECHEN LANGUAGE ITEMS
# =============================================================================
# Per child, the last observed value of each item within the window
# (ordered by age). "Taalontwikkeling achterstand" (taal_ont) = at least one
# item scored "-" ("onvoldoende"). Denominator: children with any item.
wiechen_children <- function(window_cms, items) {
  d <- jgz %>%
    filter(cm %in% window_cms) %>%
    filter_at(vars(all_of(items)), any_vars(!is.na(.))) %>%
    arrange(RINPERSOONS, RINPERSOON, leeftijd)
  # last observed value per item
  item_vals <- d %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    summarise(across(all_of(items), function(x) { x <- x[!is.na(x)]; if (length(x)) x[length(x)] else NA_character_ }),
              .groups = "drop")
  # geography and sex from the child's last row in the window
  geo <- d %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(RINPERSOONS, RINPERSOON, pc3, pc4, sex)
  item_vals %>%
    left_join(geo, by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(taal_ont = as.integer(rowSums(across(all_of(items), function(x) x %in% "-")) > 0))
}

wiechen_summary <- function(items) {
  function(g) {
    tab <- g %>% summarise(n = n(), taal_ont_achts = sum(taal_ont), .groups = "drop")
    counts <- g %>% summarise(across(all_of(items), list(
      vol   = function(x) sum(x %in% "+"),
      M     = function(x) sum(x %in% "M"),
      onvol = function(x) sum(x %in% "-")
    ), .names = "{.col}_{.fn}"), .groups = "drop")
    tab <- bind_cols(tab, counts %>% select(-any_of(group_vars(g))))
    count_cols <- setdiff(names(counts), group_vars(g))
    tab <- tab %>%
      mutate(taal_ont_achts_perc = round(suppress_count(taal_ont_achts / n * 100, taal_ont_achts, n), 1),
             taal_ont_achts      = suppress_count(taal_ont_achts, taal_ont_achts, n))
    for (cc in count_cols) {
      tab[[paste0(cc, "_perc")]] <- round(suppress_count(tab[[cc]] / tab$n * 100, tab[[cc]], tab$n), 1)
    }
    tab %>% select(-all_of(count_cols)) %>% drop_small()
  }
}

wiechen_sets <- list(
  `2years`   = list(window = c("18months", "2years"), items = c("zin2w", "pop6")),
  `3years9m` = list(window = c("3years", "3years9m"), items = c("vragen1", "verstaan2", "vragen2"))
)
wiechen_tables <- list()
for (k in names(wiechen_sets)) {
  s <- wiechen_sets[[k]]
  d <- wiechen_children(s$window, s$items)
  message(sprintf("Wiechen %-8s: %6d children, %.1f%% with a language delay flag",
                  k, nrow(d), 100 * mean(d$taal_ont)))
  wiechen_tables <- c(wiechen_tables,
                      area_tables(d, paste0("wiechen_", k), wiechen_summary(s$items), by_sex = FALSE))
}
write_sheets(wiechen_tables, file.path(loc$maps_folder, "jgz_maps_wiechen.xlsx"))


# =============================================================================
# SDQ (Strengths and Difficulties Questionnaire, total score)
# =============================================================================
sdq_summary <- function(g) {
  tab <- g %>% summarise(n = n(), sdq_mean = mean(sdq_t), sdq_sd = sd(sdq_t), .groups = "drop")
  for (cut_off in cfg$sdq_cutoffs) {
    cnt <- g %>% summarise(c = sum(sdq_t >= cut_off), .groups = "drop") %>% pull(c)
    tab[[paste0("sdq_", cut_off, "_perc")]] <- round(suppress_count(cnt / tab$n * 100, cnt, tab$n), 1)
    tab[[paste0("sdq_", cut_off, "_n")]]    <- suppress_count(cnt, cnt, tab$n)
  }
  tab %>%
    mutate(sdq_mean = round(suppress_small_n(sdq_mean, n), 2),
           sdq_sd   = round(suppress_small_n(sdq_sd, n), 2)) %>%
    drop_small()
}

sdq_tables <- list()
for (k in c("3years9m", "groep2", "groep7", "klas2")) {
  d <- jgz %>% filter(cm == k, !is.na(sdq_t)) %>% one_per_child()
  message(sprintf("SDQ %-8s: %6d children", k, nrow(d)))
  sdq_tables <- c(sdq_tables, area_tables(d, paste0("sdq_", k), sdq_summary, by_sex = FALSE))
}
write_sheets(sdq_tables, file.path(loc$maps_folder, "jgz_maps_sdq.xlsx"))
