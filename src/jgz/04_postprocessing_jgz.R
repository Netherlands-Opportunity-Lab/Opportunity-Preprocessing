# =============================================================================
# JGZ cohort -- 04. Post-processing
#
# Income and wealth groups as in the other cohorts (src/newborns/04_*), then
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
cohort_dat <- cohort_dat %>%
  mutate(income_group = factor(case_when(
    income_parents_perc >= .15 & income_parents_perc <= .35 ~ "Low",
    income_parents_perc >= .40 & income_parents_perc <= .60 ~ "Mid",
    income_parents_perc >= .65 & income_parents_perc <= .85 ~ "High",
    TRUE ~ NA_character_), levels = c("Low", "Mid", "High")),
    income_group_tails = factor(case_when(
      income_parents_perc >= 0   & income_parents_perc <= .20 ~ "Very_Low",
      income_parents_perc >= .80 & income_parents_perc <= 1   ~ "Very_High",
      TRUE ~ NA_character_), levels = c("Very_Low", "Very_High")))


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
