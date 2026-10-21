library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

setwd("H:/Archive/Sonya/JGZ-Pilot-Data/Gradients Analysis Results")

#############VISUS
data <- list.files(pattern = "visus", full.names = FALSE)
data_tidy <- list.files(pattern = "tidy", full.names = FALSE)
data <- setdiff(data, data_tidy)
for (dat in data) {
  df <- read_xlsx(dat)
  tidy_data <- df %>%
    rename_with(~paste0(., "_estimate"), 
                .cols = names(.)[!grepl("(_N|inc)$", names(.))]) %>%
    rename(bin = "...1_estimate") %>%
    pivot_longer(
      cols = -"bin", 
      names_to = c("cm", "outcome", "subgroup", ".value"), 
      names_pattern = "cm(\\d+)_(.+)_(all|boys|girls|native|migrant|first_born|later_born)_(estimate|N|inc)"
    ) %>%
    mutate(
      bin = substr(bin, 5, 6),
      bin = as.integer(bin), 
      cm = as.integer(cm)
    )
  
  tidy_data <- tidy_data[order(tidy_data$cm, tidy_data$outcome, tidy_data$subgroup, tidy_data$bin),]
  new_name <- tools::file_path_sans_ext(basename(dat))
  write_xlsx(tidy_data, paste0(new_name, "_tidy.xlsx"))
}

################PSYCHOSOCIAL
data <- list.files(pattern = "psychosociaal", full.names = FALSE)
data <- setdiff(data, data_tidy)

for (dat in data) {
  df <- read_xlsx(dat)
  tidy_data <- df %>%
    rename_with(~paste0(., "_estimate"), 
                .cols = names(.)[!grepl("(_N|inc)$", names(.))]) %>%
    rename(bin = "...1_estimate") %>%
    pivot_longer(
      cols = -"bin", 
      names_to = c("cm", "outcome", "subgroup", ".value"), 
      names_pattern = "cm(\\d+)_(.+)_(all|boys|girls|native|migrant|first_born|later_born)_(estimate|N|inc)"
    ) %>%
    mutate(
      bin = substr(bin, 5, 6),
      bin = as.integer(bin), 
      cm = as.integer(cm)
    )
  
  tidy_data <- tidy_data[order(tidy_data$cm, tidy_data$outcome, tidy_data$subgroup, tidy_data$bin),]
  new_name <- tools::file_path_sans_ext(basename(dat))
  write_xlsx(tidy_data, paste0(new_name, "_tidy.xlsx"))
}

##################SPRAAKTAAL
data <- list.files(pattern = "spraak", full.names = FALSE)
data <- setdiff(data, data_tidy)

for (dat in data) {
  df <- read_xlsx(dat)
  tidy_data <- df %>%
    rename( bin = "...1") %>%
    pivot_longer(
      cols = -"bin", 
      names_to = c("cm", "outcome", "subgroup", ".value"), 
      names_pattern = "cm(\\d+)_(.+)_(all|boys|girls|native|migrant|first_born|later_born)_(e|I|N)"
    ) %>%
    mutate(
      bin = substr(bin, 5, 6),
      bin = as.integer(bin), 
      cm = as.integer(cm)
    ) %>%
    rename(
      estimate = e, 
      inc = I
    )
  
  tidy_data <- tidy_data[order(tidy_data$cm, tidy_data$outcome, tidy_data$subgroup, tidy_data$bin),]
  new_name <- tools::file_path_sans_ext(basename(dat))
  write_xlsx(tidy_data, paste0(new_name, "_tidy.xlsx"))
}

##################WEIGHT
#########BMI PERCENTILES
data <- list.files(pattern = "gradients_bmi", full.names = FALSE)
data <- setdiff(data, data_tidy)

for (dat in data) {
  df <- read_xlsx(dat)
  tidy_data <- df %>%
    rename(bin = "...1") %>%
    pivot_longer(
      cols = -"bin", 
      names_to = c("cm", ".value"), 
      names_pattern = "bmi_cm(\\d+)_(.*)"
    ) %>%
    mutate(
      bin = substr(bin, 5, 6),
      bin = as.integer(bin), 
      cm = as.integer(cm)
    ) %>%
    filter(!is.na(N))
  
  tidy_data <- tidy_data[order(tidy_data$cm, tidy_data$bin),]
  new_name <- tools::file_path_sans_ext(basename(dat))
  write_xlsx(tidy_data, paste0(new_name, "_tidy.xlsx"))
}

##################P90
data <- list.files(pattern = "gradients_p90", full.names = FALSE)
data <- setdiff(data, data_tidy)

for (dat in data) {
  df <- read_xlsx(dat)
  tidy_data <- df %>%
    rename_with(~paste0(., "_estimate"), 
                .cols = names(.)[!grepl("(_N|inc)$", names(.))]) %>%
    rename(bin = "...1_estimate") %>%
    pivot_longer(
      cols = -"bin", 
      names_to = c("cm", "outcome", "subgroup",  ".value"), 
      names_pattern = "cm(\\d+)_(bmi|gewicht)p90_(all|boys|girls|native|migrant|first_born|later_born)_(estimate|N|inc)"
    ) %>%
    pivot_wider(
      names_from = outcome, 
      values_from = c(estimate, N, inc)
    ) %>%
    mutate(
      bin = substr(bin, 5, 6),
      bin = as.integer(bin), 
      cm = as.integer(cm)
    ) %>%
    filter(!is.na(estimate_bmi) | !is.na(estimate_gewicht))
  
  tidy_data <- tidy_data[order(tidy_data$cm, tidy_data$bin),]
  new_name <- tools::file_path_sans_ext(basename(dat))
  write_xlsx(tidy_data, paste0(new_name, "_tidy.xlsx"))
}

##############GEWICHT
data <- list.files(pattern = "gradients_gewicht", full.names = FALSE)
data <- setdiff(data, data_tidy)

for (dat in data) {
  df <- read_xlsx(dat)
  tidy_data <- df %>%
    rename_with(~paste0(., "_estimate"), 
                .cols = names(.)[!grepl("(_N|inc)$", names(.))]) %>%
    rename(bin = "...1_estimate") %>%
    pivot_longer(
      cols = -"bin", 
      names_to = c("cm", "outcome", "subgroup",  ".value"), 
      names_pattern = "cm(\\d+)_(bmi|gewicht|lengte)_(all|boys|girls|native|migrant|first_born|later_born)_(estimate|N|inc)"
    ) %>%
    pivot_wider(
      names_from = outcome, 
      values_from = c(estimate, N, inc)
    ) %>%
    mutate(
      bin = substr(bin, 5, 6),
      bin = as.integer(bin), 
      cm = as.integer(cm)
    )
  
  tidy_data <- tidy_data[order(tidy_data$cm, tidy_data$bin),]
  new_name <- tools::file_path_sans_ext(basename(dat))
  write_xlsx(tidy_data, paste0(new_name, "_tidy.xlsx"))
}

##############OVERGEWICHT
data <- list.files(pattern = "gradients_overgewicht", full.names = FALSE)
data <- setdiff(data, data_tidy)


for (dat in data) {
  df <- read_xlsx(dat)
  tidy_data <- df %>%
    rename_with(~paste0(., "_estimate"), 
                .cols = names(.)[!grepl("(_N|inc)$", names(.))]) %>%
    rename(bin = "...1_estimate") %>%
    pivot_longer(
      cols = -"bin", 
      names_to = c("cm", "outcome", "subgroup",  ".value"), 
      names_pattern = "cm(\\d+)_(overgewicht|obesity)_(all|boys|girls|native|migrant|first_born|later_born)_(estimate|N|inc)"
    ) %>%
    pivot_wider(
      names_from = outcome, 
      values_from = c(estimate, N, inc)
    ) %>%
    mutate(
      bin = substr(bin, 5, 6),
      bin = as.integer(bin), 
      cm = as.integer(cm)
    )
  
  tidy_data <- tidy_data[order(tidy_data$cm, tidy_data$bin),]
  new_name <- tools::file_path_sans_ext(basename(dat))
  write_xlsx(tidy_data, paste0(new_name, "_tidy.xlsx"))
}

