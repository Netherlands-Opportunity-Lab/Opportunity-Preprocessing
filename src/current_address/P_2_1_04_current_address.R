# Kansenkaart data preparation pipeline
#
# Samples of the intragenerational analysis without a parent link
# (age45_current_address, age55_current_address), step 3 of 3 (see
# 01_cohort_current_address for the steps):
#   - regions of the address at the outcome age (instead of no region at all),
#     the Amsterdam areas and the Amsterdam Metropolitan Area dummy;
#   - the categorical outcomes of the intragenerational analysis
#     (c<age>_ses_ranked, _mhc_ranked, _contract_ranked, _education_ranked);
#   - writes output/<cohort>_cohort.rds, the input of
#     Opportunity-Analysis-Intragenerational.
# The same functions as age35_current_address (src/lib/P_2_1_current_address.R).
#
# (c) Netherlands Opportunity Lab 2026


#### PACKAGES ####
library(tidyverse)
source_versioned("src/lib", "current_address")


#### LOAD THE SAMPLE ####
cohort_dat <- read_rds(file.path(loc$temp_folder, "03_outcomes.rds"))
sample_size <- read_rds(file.path(loc$temp_folder, "03_sample_size.rds"))


#### REGIONS OF THE ADDRESS AT THE OUTCOME AGE, AMSTERDAM AREAS ####
cohort_dat <- add_current_regions(cohort_dat, age = cfg$outcome_age, cfg = cfg, loc = loc)

sample_size <- sample_size %>% mutate(n_9_current_address = nrow(cohort_dat))


#### CATEGORICAL OUTCOMES FOR THE INTRAGENERATIONAL ANALYSIS ####
cohort_dat <- add_intra_categories(cohort_dat, prefix = paste0("c", cfg$outcome_age, "_"))


#### WRITE OUTPUT ####
write_rds(cohort_dat, file.path(loc$output_folder, paste0(cfg$cohort_name, "_cohort.rds")))
write_rds(sample_size, file.path(loc$temp_folder, "04_sample_size.rds"))
