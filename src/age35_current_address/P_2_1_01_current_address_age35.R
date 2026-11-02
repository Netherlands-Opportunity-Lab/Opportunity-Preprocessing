# Kansenkaart data preparation pipeline
#
# age35_current_address: the age35 sample with its regions measured at the
# address at age 35 (instead of the childhood address), plus the Amsterdam
# areas and the categorical outcomes of the intragenerational analysis.
#   - Reads output/age35_cohort.rds (run the age35 sample first).
#   - Writes output/age35_current_address_cohort.rds, the input of
#     Opportunity-Analysis-Intragenerational.
#
# Moved here from Opportunity-Analysis-Intragenerational
# (Scripts/Preprocessing/age35/A_2_0_I1_01b_preprocess_age35_sample.R) on
# 30-10-2026: assigning people to areas belongs in the preprocessing. Same
# result as the old script (checked on synthetic data). Changes:
#   - the file is written once, after the categorical outcomes (c35_*_ranked);
#   - joins go through checked_join(), which stops if an address or area table
#     has more than one row per key (the old inner_join would duplicate people);
#   - the list of Amsterdam Metropolitan Area municipalities is in the config.
# Since 02-11-2026 the work is done by the functions in
# src/lib/P_2_1_current_address.R, which age45_current_address and
# age55_current_address use as well (same output, checked on synthetic data).
#
# (c) Netherlands Opportunity Lab 2026


#### PACKAGES ####
library(tidyverse)
source_versioned("src/lib", "current_address")


#### LOAD THE SAMPLE ####
cohort_dat <- read_rds(file.path(loc$output_folder, paste0(cfg$base_cohort, "_cohort.rds")))


#### 1-3. REGIONS OF THE ADDRESS AT AGE 35, AMSTERDAM AREAS ####
cohort_dat <- add_current_regions(cohort_dat, age = cfg$current_home_age, cfg = cfg, loc = loc)


#### 4. CATEGORICAL OUTCOMES FOR THE INTRAGENERATIONAL ANALYSIS ####
cohort_dat <- add_intra_categories(cohort_dat, prefix = "c35_")


#### WRITE OUTPUT ####
write_rds(cohort_dat, file.path(loc$output_folder, paste0(cfg$cohort_name, "_cohort.rds")))
