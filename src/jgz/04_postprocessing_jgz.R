# =============================================================================
# JGZ cohort -- 04. Post-processing
#
# Income groups per outcome age and wealth groups as in the other cohorts
# (src/newborns/04_*), then
# writes the cohort file output/jgz_cohort.rds: one row per child, with the
# predictors of 02 and the outcomes and postcodes per age of 03. This is the
# input of the JGZ map tables and gradients (Opportunity-Analysis-Map/JGZ).
#
#   Input : temp/jgz/03_outcomes.rds
#   Output: output/jgz_cohort.rds, temp/jgz/04_sample_size.rds
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")

cohort_dat  <- readRDS(file.path(loc$temp_folder, "03_outcomes.rds"))
sample_size <- readRDS(file.path(loc$temp_folder, "03_sample_size.rds"))


#### INCOME GROUP ####
# Parental income is defined per outcome age (02, since 20-11-2026):
# income_parents_perc_<age>, ranked among all children in NL reaching that age
# in the same year. The groups are made per age with the usual cut-offs:
# income_group_<age> (Low/Mid/High) and income_group_tails_<age>.
income_group_of <- function(p) factor(case_when(
  p >= .15 & p <= .35 ~ "Low",
  p >= .40 & p <= .60 ~ "Mid",
  p >= .65 & p <= .85 ~ "High",
  TRUE ~ NA_character_), levels = c("Low", "Mid", "High"))
income_tails_of <- function(p) factor(case_when(
  p >= 0   & p <= .20 ~ "Very_Low",
  p >= .80 & p <= 1   ~ "Very_High",
  TRUE ~ NA_character_), levels = c("Very_Low", "Very_High"))

income_perc_cols <- grep("^income_parents_perc_", names(cohort_dat), value = TRUE)
if (length(income_perc_cols) == 0) stop("No income_parents_perc_<age> columns: rerun step 02.")
for (pc in income_perc_cols) {
  age <- sub("^income_parents_perc_", "", pc)
  cohort_dat[[paste0("income_group_", age)]]       <- income_group_of(cohort_dat[[pc]])
  cohort_dat[[paste0("income_group_tails_", age)]] <- income_tails_of(cohort_dat[[pc]])
}


#### WEALTH GROUP ####
cohort_dat <- cohort_dat %>%
  mutate(wealth_group = factor(case_when(
    wealth_parents_perc >= .15 & wealth_parents_perc <= .35 ~ "Low",
    wealth_parents_perc >= .40 & wealth_parents_perc <= .60 ~ "Mid",
    wealth_parents_perc >= .65 & wealth_parents_perc <= .85 ~ "High",
    TRUE ~ NA_character_), levels = c("Low", "Mid", "High")),
    wealth_group_tails = factor(case_when(
      wealth_parents_perc >= 0   & wealth_parents_perc <= .20 ~ "Very_Low",
      wealth_parents_perc >= .80 & wealth_parents_perc <= 1   ~ "Very_High",
      TRUE ~ NA_character_), levels = c("Very_Low", "Very_High")))


#### WRITE ####
sample_size <- sample_size %>% mutate(n_5_final = nrow(cohort_dat))
saveRDS(sample_size, file.path(loc$temp_folder, "04_sample_size.rds"))
saveRDS(cohort_dat, file.path(loc$output_folder, paste0(cfg$cohort_name, "_cohort.rds")))
message("Written: ", file.path(loc$output_folder, paste0(cfg$cohort_name, "_cohort.rds")),
        " (", format(nrow(cohort_dat), big.mark = ","), " children)")
