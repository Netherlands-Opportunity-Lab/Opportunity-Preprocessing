# Kansenkaart data preparation pipeline
#
# 4. Post-processing.
#   - Selecting variables of interest.
#   - Writing `scratch/kansenkaart_data.rds`.
#
# (c) ODISSEI Social Data Science team 2025



# load cohort dataset
cohort_dat <- read_rds(file.path(loc$scratch_folder, "03_outcomes.rds"))



#### INCOME GROUP ####
cohort_dat <- 
  cohort_dat %>% 
  mutate(income_group = factor(case_when(
    (income_parents_perc >= .15 & income_parents_perc <= .35) ~ "Low", 
    (income_parents_perc >= .40 & income_parents_perc <= .60) ~ "Mid", 
    (income_parents_perc >= .65 & income_parents_perc <= .85) ~ "High",
    TRUE ~ NA_character_
  ), levels = c("Low", "Mid", "High"))
  ) %>%
  mutate(income_group_tails = factor(case_when(
    (income_parents_perc >= 0   & income_parents_perc <= .20) ~ "Very_Low", 
    (income_parents_perc >= .80 & income_parents_perc <= 1) ~ "Very_High", 
    TRUE ~ NA_character_
  ), levels = c("Very_Low", "Very_High")))


#### WEALTH GROUP ####
cohort_dat <- 
  cohort_dat %>% 
  mutate(wealth_group = factor(case_when(
    (wealth_parents_perc >= .15 & wealth_parents_perc <= .35) ~ "Low", 
    (wealth_parents_perc >= .40 & wealth_parents_perc <= .60) ~ "Mid", 
    (wealth_parents_perc >= .65 & wealth_parents_perc <= .85) ~ "High",
    TRUE ~ NA_character_
  ), levels = c("Low", "Mid", "High"))
  ) %>%
  mutate(wealth_group_tails = factor(case_when(
    (wealth_parents_perc >= 0   & wealth_parents_perc <= .20) ~ "Very_Low", 
    (wealth_parents_perc >= .80 & wealth_parents_perc <= 1) ~ "Very_High", 
    TRUE ~ NA_character_
  ), levels = c("Very_Low", "Very_High")))


#### SAVE BIG2 (PERINATAL) SAMPLE ####

# The BIG2 sample excludes stillbirths and children who died within the first
# 7 days of life (perinatal_death, created in 03_outcomes). The outcomes
# c00_sga and c00_preterm_birth are kept.
sample_size <- read_rds(file.path(loc$scratch_folder, "03_sample_size.rds"))

cohort_dat <- 
  cohort_dat %>%
  filter(perinatal_death == 0) %>%
  select(-perinatal_death)

# record sample size after this additional exclusion step (Table B1, step 8)
sample_size <- sample_size %>% mutate(n_5_excl_perinatal_deaths = nrow(cohort_dat))
write_rds(sample_size, file.path(loc$scratch_folder, "04_sample_size.rds"))

output_file <- file.path(cfg$perinatal_name)
write_rds(cohort_dat, output_file)
