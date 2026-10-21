library(readxl)
library(openxlsx)

time <- c("4weeks", "8weeks", "3months", "4months", "6months", "9months", "11months", 
          "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2")

wb <- createWorkbook()

for (t in time) {
  data <-  read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode3_bmi.xlsx", 
                      sheet = paste0("bmi_", t), range = "A1:F100")
  sex <- read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode3_bmi.xlsx", 
                    sheet = paste0("bmi_", t), range = "L1:R120")
  
  data_bmi <- data %>%
    select(postcode3, bmi_mean, bmi_sd, n) %>%
    filter(n != "NA")
  
  data_bmi_90p <- data %>%
    mutate(
      bmi_90p = as.integer(bmi_90p),
      bmi_90p = ifelse(bmi_90p < 10, NA, bmi_90p), 
      bmi_90p_perc = ifelse(bmi_90p < 10, NA, bmi_90p_perc)
    ) %>%
    select(postcode3, bmi_90p, bmi_90p_perc) %>%
    filter(!is.na(bmi_90p))
  
  sex_bmi <- sex %>%
    select(postcode3, geslacht, bmi_mean, bmi_sd, n) %>%
    filter(n != "NA")
  
  sex_bmi_90p <- sex %>%
    mutate(
      bmi_90p = as.integer(bmi_90p),
      bmi_90p = ifelse(bmi_90p < 10, NA, bmi_90p), 
      bmi_90p_perc = ifelse(bmi_90p < 10, NA, bmi_90p_perc)
    ) %>%
    select(postcode3, geslacht, bmi_90p, bmi_90p_perc) %>%
    filter(!is.na(bmi_90p))
  
  addWorksheet(wb, sheet = paste0("bmi_", t))
  writeData(wb,paste0("bmi_", t), data_bmi)
  
  addWorksheet(wb, sheet = paste0("bmi_90p_", t))
  writeData(wb,paste0("bmi_90p_", t), data_bmi_90p)
  
  addWorksheet(wb, paste0("bmi_", t, "_sex"))
  writeData(wb,paste0("bmi_", t, "_sex"), sex_bmi)
  
  addWorksheet(wb, paste0("bmi_90p_", t, "_sex"))
  writeData(wb,paste0("bmi_90p_", t, "_sex"), sex_bmi_90p)
}

saveWorkbook(wb, "H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode3_bmi_export.xlsx", overwrite = TRUE)


time <- c("4wks", "8wks", "3months", "4months", "6months", "9months", "11months", 
          "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2")

wb <- createWorkbook()

for (t in time) {
  data <-  read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_bmi.xlsx", 
                      sheet = paste0("bmi_", t), range = "A1:F100")
  sex <- read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_bmi.xlsx", 
                    sheet = paste0("bmi_", t), range = "L1:R120")
  data_bmi <- data %>%
    select(postcode, bmi_mean, bmi_sd, n) %>%
    filter(n != "NA")
  
  data_bmi_90p <- data %>%
    mutate(
      bmi_90p = as.integer(bmi_90p),
      bmi_90p = ifelse(bmi_90p < 10, NA, bmi_90p), 
      bmi_90p_perc = ifelse(bmi_90p < 10, NA, bmi_90p_perc)
    ) %>%
    select(postcode, bmi_90p, bmi_90p_perc) %>%
    filter(!is.na(bmi_90p))
  
  sex_bmi <- sex %>%
    select(postcode, geslacht, bmi_mean, bmi_sd, n) %>%
    filter(n != "NA")
  
  sex_bmi_90p <- sex %>%
    mutate(
      bmi_90p = as.integer(bmi_90p),
      bmi_90p = ifelse(bmi_90p < 10, NA, bmi_90p), 
      bmi_90p_perc = ifelse(bmi_90p < 10, NA, bmi_90p_perc)
    ) %>%
    select(postcode, geslacht, bmi_90p, bmi_90p_perc) %>%
    filter(!is.na(bmi_90p))
  
  addWorksheet(wb, sheet = paste0("bmi_", t))
  writeData(wb,paste0("bmi_", t), data_bmi)
  
  addWorksheet(wb, sheet = paste0("bmi_90p_", t))
  writeData(wb,paste0("bmi_90p_", t), data_bmi_90p)
  
  addWorksheet(wb, paste0("bmi_", t, "_sex"))
  writeData(wb,paste0("bmi_", t, "_sex"), sex_bmi)
  
  addWorksheet(wb, paste0("bmi_90p_", t, "_sex"))
  writeData(wb,paste0("bmi_90p_", t, "_sex"), sex_bmi_90p)
}

saveWorkbook(wb, "H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_bmi_export.xlsx", overwrite = TRUE)



time <- c("groep2", "groep7", "klas2", "3years9m")

wb <- createWorkbook()

for (t in time) {
  pc4 <- read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_3_sdq.xlsx", 
                    sheet = paste0("sdq_", t, "_pc4"), range = "A1:H250")
  pc3 <- read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_3_sdq.xlsx", 
                    sheet = paste0("sdq_", t, "_pc3"), range = "A1:H100")
  
  pc4_sdq <- pc4 %>%
    select(postcode, sdq_mean, sdq_sd, n) %>%
    filter(n != "NA")
  
  pc4_sdq11 <- pc4 %>%
    select(postcode, sdq_11_n, sdq_11_perc) %>%
    filter(sdq_11_n != "NA")
  
  pc4_sdq14 <- pc4 %>%
    select(postcode, sdq_14_n, sdq_14_perc) %>%
    filter(sdq_14_n != "NA")
  
  pc3_sdq <- pc3 %>%
    select(postcode3, sdq_mean, sdq_sd, n) %>%
    filter(n != "NA")
  
  pc3_sdq11 <- pc3 %>%
    select(postcode3, sdq_11_n, sdq_11_perc) %>%
    filter(sdq_11_n != "NA")
  
  pc3_sdq14 <- pc3 %>%
    select(postcode3, sdq_14_n, sdq_14_perc) %>%
    filter(sdq_14_n != "NA")
  
  addWorksheet(wb, sheet = paste0("sdq_", t, "_pc4"))
  writeData(wb,paste0("sdq_", t, "_pc4"), pc4_sdq)
  
  addWorksheet(wb, sheet = paste0("sdq11_", t, "_pc4"))
  writeData(wb,paste0("sdq11_", t, "_pc4"), pc4_sdq11)
  
  addWorksheet(wb, sheet = paste0("sdq14_", t, "_pc4"))
  writeData(wb,paste0("sdq14_", t, "_pc4"), pc4_sdq14)
  
  addWorksheet(wb, sheet = paste0("sdq_", t, "_pc3"))
  writeData(wb,paste0("sdq_", t, "_pc3"), pc3_sdq)
  
  addWorksheet(wb, sheet = paste0("sdq11_", t, "_pc3"))
  writeData(wb,paste0("sdq11_", t, "_pc3"), pc3_sdq11)
  
  addWorksheet(wb, sheet = paste0("sdq14_", t, "_pc3"))
  writeData(wb,paste0("sdq14_", t, "_pc3"), pc3_sdq14)
}

saveWorkbook(wb, "H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_3_sdq_export.xlsx", overwrite = TRUE)


time <- c("2years", "3y9m")

wb <- createWorkbook()

for (t in time) {
  pc4 <- read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_3_wiechen.xlsx", 
                    sheet = paste0("wiechen_", t, "_pc4"), range = "A1:H250")
  pc3 <- read_excel("H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_3_wiechen.xlsx", 
                    sheet = paste0("wiechen_", t, "_pc3"), range = "A1:H100")
  
  pc4_taal <- pc4 %>%
    select(postcode, taal_ont_achts, taal_ont_achts_perc) %>%
    filter(taal_ont_achts != "NA")
  
  pc4 <- pc4 %>%
    select(-c(taal_ont_achts, taal_ont_achts_perc)) %>%
    pivot_longer(
      cols = ends_with("_perc"), 
      names_to = "outcome", 
      values_to = "perc"
    ) %>%
    mutate(outcome = gsub("_perc", "", outcome)) %>%
    filter(n != "NA" & perc != "NA")
  
  pc3_taal <- pc3 %>%
    select(postcode3, taal_ont_achts, taal_ont_achts_perc) %>%
    filter(taal_ont_achts != "NA")
  
  pc3 <- pc3 %>%
    select(-c(taal_ont_achts, taal_ont_achts_perc)) %>%
    pivot_longer(
      cols = ends_with("_perc"), 
      names_to = "outcome", 
      values_to = "perc"
    ) %>%
    mutate(outcome = gsub("_perc", "", outcome)) %>%
    filter(n != "NA" & perc != "NA")
  
  addWorksheet(wb, sheet = paste0("wiechen_taal_", t, "_pc4"))
  writeData(wb,paste0("wiechen_taal_", t, "_pc4"), pc4_taal)
  
  addWorksheet(wb, sheet = paste0("wiechen_taal_", t, "_pc3"))
  writeData(wb,paste0("wiechen_taal_", t, "_pc3"), pc3_taal)
  
  addWorksheet(wb, sheet = paste0("wiechen_", t, "_pc4"))
  writeData(wb,paste0("wiechen_", t, "_pc4"), pc4)
  
  addWorksheet(wb, sheet = paste0("wiechen_", t, "_pc3"))
  writeData(wb,paste0("wiechen_", t, "_pc3"), pc3)
  
}

saveWorkbook(wb, "H:/Archive/Sonya/JGZ pilot data/maps results/output_cases_postcode4_3_wiechen_export.xlsx", overwrite = TRUE)

#### Gradients ####

# select the relevant gradients for export
files <- list.files("H:/JGZ-Pilot-Data/Gradients Analysis Results", pattern = "^[^ ]*_(7|10)_tidy_export", full.names = TRUE)
files <- files[!grepl("excl", files)]
file.copy(
  from = files, 
  to = file.path("H:/Archive/Sonya/Export JGZ/using data", basename(files))
)

gradient_files <- list.files("H:/Archive/Sonya/Export JGZ/using data", pattern = "gradients", full.names = TRUE)

wb <- createWorkbook()

for (file in gradient_files) {
  data <- read.xlsx(file)
  data <- tibble(data)
  if(nrow(data) == 0) next
  
  file_name <- basename(file)
  sheet_name <- sub("gradients_(.*)_tidy_export.xlsx", "\\1", file_name)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}


saveWorkbook(wb, "H:/Archive/Sonya/Export JGZ/gradients_export.xlsx", overwrite = TRUE)

#### Proportions ####

# select the relevant gradients for export
files <- list.files("H:/Archive/Sonya/JGZ pilot data/Proportions Results", pattern = "(7|10)", full.names = TRUE)
files <- files[!grepl("excl", files)]
file.copy(
  from = files, 
  to = file.path("H:/Archive/Sonya/Export JGZ/using data", basename(files))
)



pr_files <- list.files("H:/Archive/Sonya/Export JGZ/using data", pattern = "proportions", full.names = TRUE)

wb <- createWorkbook()

for (file in pr_files) {
  
  data <- read.xlsx(file)
  data <- tibble(data)
  if(nrow(data) == 0) next
  
  file_name <- basename(file)
  sheet_name <- file_name
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}


saveWorkbook(wb, "H:/Archive/Sonya/Export JGZ/proportions_export.xlsx", overwrite = TRUE)


#### Bereik ####

# select the relevant gradients for export
files <- list.files("H:/Archive/Sonya/JGZ pilot data/Bereik Regression Results", pattern = "(7|10)", full.names = TRUE)
files <- files[!grepl("excl", files)]
file.copy(
  from = files, 
  to = file.path("H:/Archive/Sonya/Export JGZ/using data", basename(files))
)

obs_files <- list.files("H:/Archive/Sonya/Export JGZ/using data", pattern = "obs", full.names = TRUE)

wb <- createWorkbook()

for (file in obs_files) {
  
  data <- read_xls(file)
  data <- tibble(data)
  if(nrow(data) == 0) next
  
  if ("N" %in% names(data)) {
    data <- data %>%
      filter(N >= 10) # n of observations
  } else {
    data <- data %>%
      filter(c16 >= 10) # n of observations
  }
  
  
  file_name <- basename(file)
  sheet_name <- file_name
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}


saveWorkbook(wb, "H:/Archive/Sonya/Export JGZ/regressions_export.xlsx", overwrite = TRUE)


#### Descriptives ####

# select the relevant gradients for export
files <- list.files("H:/Archive/Sonya/JGZ pilot data/Descriptives Results", pattern = "(7|10)", full.names = TRUE)
files <- files[!grepl("excl", files)]
file.copy(
  from = files, 
  to = file.path("H:/Archive/Sonya/Export JGZ/using data", basename(files))
)


d_files <- list.files("H:/Archive/Sonya/Export JGZ/using data", pattern = "descriptives", full.names = TRUE)

wb <- createWorkbook()

for (file in d_files) {
  
  data <- read.xlsx(file)
  
  file_name <- basename(file)
  sheet_name <- file_name
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}


saveWorkbook(wb, "H:/Archive/Sonya/Export JGZ/descriptives_export.xlsx", overwrite = TRUE)

