# =============================================================================
# JGZ cohort -- 03. Outcomes
#
# Adds the JGZ outcomes to the cohort (one row per child), from the
# contact-moment files of 01c. The outcome list and every choice in it are in
# config/jgz.yml (data_preparation, "03_outcomes_jgz.R"); the functions are in
# src/lib/P_2_1_jgz_outcomes.R. Names: y_m<age in months>_<outcome>.
#
#   BMI          y_m<age>_bmi, _bmi_p90, _bmi_p10      m24 m36 m45 m60 m120 m162
#   Van Wiechen  y_m24_words2_can/_notseen, y_m24_bodyparts_can/_notseen,
#                y_m45_questions_can/_notseen, y_m45_intelligible,
#                y_m45_questions2_can/_notseen, y_m<age>_language_delay
#   SDQ          y_m<age>_sdq11                        m60 m120 m162
#   Visus        y_m<age>_visus                        m36 m45 m60
#   Per age      pc4_m<age>, pc3_m<age>, org_m<age>: GBA postcode and
#                organisation of the contact closest to the nominal age
#
# The bereik (take-up) outcomes are made in the catchment sample
# (config/jgz_catchment.yml), since they need children the JGZ did not see.
#
# A child without a measurement at an age has NA for that age's outcomes;
# outcomes are computed in the analysis only over the children measured.
#
#   Input : temp/jgz/02_predictors.rds, loc$contact_moment_folder/cm_<visit>.rds
#   Output: temp/jgz/03_outcomes.rds, temp/jgz/03_sample_size.rds
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")
source(list.files("src/lib", "jgz_outcomes\\.R$", full.names = TRUE))

cohort_dat  <- readRDS(file.path(loc$temp_folder, "02_predictors.rds"))
sample_size <- readRDS(file.path(loc$temp_folder, "02_sample_size.rds"))


# ---- Contact-moment rows of the children in the cohort --------------------------------
age_lists <- list(bmi = cfg$bmi_ages, wiechen = cfg$wiechen_ages, sdq = cfg$sdq_ages,
                  visus = cfg$visus_ages)
cms_needed <- unique(unlist(age_lists))

rows <- do.call(rbind, lapply(cms_needed, function(k) {
  f <- file.path(loc$contact_moment_folder, paste0("cm_", k, ".rds"))
  if (!file.exists(f)) stop("Contact-moment file not found: ", f, " (run step 01c first)")
  d <- readRDS(f)
  d$RINPERSOONS <- as_chr(d$RINPERSOONS)
  d$RINPERSOON  <- as_chr(d$RINPERSOON)
  cols <- intersect(c("RINPERSOONS", "RINPERSOON", "leeftijd", "cm", "sex", "pc3", "pc4", "org",
                      "BMI", "sdq_t", "visus_c", unlist(cfg$wiechen_items)), names(d))
  as.data.frame(d[, cols])
}))
rows <- rows[paste(rows$RINPERSOONS, rows$RINPERSOON) %in%
               paste(cohort_dat$RINPERSOONS, cohort_dat$RINPERSOON), , drop = FALSE]
message(sprintf("Contact-moment rows of cohort children: %s", format(nrow(rows), big.mark = ",")))
# visus_c is kept as text by 01a (firstnm); make sure the column exists
for (v in c("sdq_t", "visus_c")) if (!v %in% names(rows)) stop(v, " not in the contact-moment files")


# ---- Outcomes ----------------------------------------------------------------------------
target_days <- cfg$outcome_target_days

outcomes <- join_children(list(
  bmi_outcomes(rows, cfg$bmi_ages, target_days, z_max = cfg$bmi_z_max, by_sex = cfg$bmi_cutoffs_by_sex),
  wiechen_outcomes(rows, cfg$wiechen_ages, cfg$wiechen_items, target_days, no_m = unlist(cfg$wiechen_no_m)),
  sdq_outcomes(rows, cfg$sdq_ages, target_days, cutoff = cfg$sdq_cutoff, max_score = cfg$sdq_max),
  visus_outcomes(rows, cfg$visus_ages, target_days, abnormal = unlist(cfg$visus_abnormal),
                 normal = unlist(cfg$visus_normal))
))

# postcode and organisation per age, over all contact moments used for that age
age_cms <- list()
for (dom in age_lists) for (age in names(dom)) age_cms[[age]] <- unique(c(age_cms[[age]], unlist(dom[[age]])))
age_cms <- age_cms[order(as.integer(sub("^m", "", names(age_cms))))]
geography <- age_geography(rows, age_cms, target_days)

cohort_dat <- cohort_dat %>%
  mutate(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON)) %>%
  left_join(outcomes, by = c("RINPERSOONS", "RINPERSOON")) %>%
  left_join(geography, by = c("RINPERSOONS", "RINPERSOON"))
check_keys_jgz(cohort_dat, c("RINPERSOONS", "RINPERSOON"), "03 outcomes")


# ---- Overview: children and share per outcome ----------------------------------------------
y_cols <- grep("^y_m[0-9]+_", names(cohort_dat), value = TRUE)
overview <- data.frame(
  outcome  = y_cols,
  children = vapply(y_cols, function(v) sum(!is.na(cohort_dat[[v]])), numeric(1)),
  mean     = vapply(y_cols, function(v) round(mean(cohort_dat[[v]], na.rm = TRUE), 3), numeric(1)),
  row.names = NULL)
print(overview, row.names = FALSE)
write.csv(overview, file.path(loc$temp_folder, "03_outcome_overview.csv"), row.names = FALSE)


# ---- Write --------------------------------------------------------------------------------------
sample_size <- sample_size %>% mutate(n_4_outcomes = nrow(cohort_dat))
saveRDS(cohort_dat,  file.path(loc$temp_folder, "03_outcomes.rds"))
saveRDS(sample_size, file.path(loc$temp_folder, "03_sample_size.rds"))
