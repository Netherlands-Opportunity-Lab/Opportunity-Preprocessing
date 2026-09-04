# Kansenkaart data preparation pipeline
#
# Full cohort creation script
#
# (c) ODISSEI Social Data Science team 2025



# clean workspace
rm(list=ls())
setwd("H:/IGM project/kansenkaart_preprocessing")
options(scipen=999)
excel_path <- 'H:/IGM project/Excel/output/'



# input the desired config file here:
# yml: age35, age21, age16, prim8_a, prim8_b, newborns_a, newborns_b
cohort <- "age35"
cfg_file <- paste0("config/", cohort, ".yml")


#### CONFIGURATION ####
# load the configuration
cfg <- config::get("data_preparation", file = cfg_file)
loc <- config::get("file_locations",   file = cfg_file)

# create the scratch folder
if (!dir.exists(loc$scratch_folder)) dir.create(loc$scratch_folder)


#### RUN ####
# select cohort
source(list.files(file.path("src", cfg$cohort_name), "01_", full.names = TRUE))

# add predictors
source(list.files(file.path("src", cfg$cohort_name), "02_", full.names = TRUE))

# add outcomes
source(list.files(file.path("src", cfg$cohort_name), "03_", full.names = TRUE))

# post-process
source(list.files(file.path("src", cfg$cohort_name), "04_", full.names = TRUE))

# the pre-processed cohort data file is now available in the scratch folder!



#### SCRIPTS FOR ROBUSTNESS CHECKS ####

# lifecycle bias for college attendance
source(list.files(file.path("src/age21/robustness/"), "02", full.names = TRUE))

source(list.files(file.path("src/age21/robustness/"), "03", full.names = TRUE))


# lifecycle bias 
source(list.files(file.path("src/age35/robustness/"), "02a", full.names = TRUE))

source(list.files(file.path("src/age35/robustness/"), "03a", full.names = TRUE))

source(list.files(file.path("src/age35/robustness/"), "03b", full.names = TRUE))

