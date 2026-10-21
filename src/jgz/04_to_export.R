library(dplyr)
library(purrr)
library(readxl)
library(writexl)

setwd("H:/JGZ-Pilot-Data/Gradients Analysis Results")

#Spraaktaal 
subgroups <- list.files(pattern =".*cm\\d+spraak_subgroups_tidy.*")
comb_subgroups <- map_dfr(subgroups, read_xlsx) 
write_xlsx(comb_subgroups, "gradients_spraak_subgroups_tidy.xlsx")


for (org in c("6", "7", "10", "12")) {
  subgroups_org <- list.files(pattern = paste0(".*cm\\d+spraak_subgroups_", org, "_tidy.*"))
  subgroups_excl_org <- list.files(pattern =paste0(".*cm\\d+spraak_subgroups_excl_", org, "_tidy.*"))
  
  comb_subgroups_org <- map_dfr(subgroups_org, read_xlsx)
  comb_subgroups_excl_org <- map_dfr(subgroups_excl_org, read_xlsx) 
  
  write_xlsx(comb_subgroups_org, paste0("gradients_spraak_subgroups_", org, "_tidy.xlsx"))
  write_xlsx(comb_subgroups_excl_org, paste0("gradients_spraak_subgroups_excl_", org, "_tidy.xlsx"))
}




source("H:/JGZ-Pilot-Data/Scripts and do files/gradients/check_function.R")

#Spraktaal & Psychosociaal & Visus
files <- list.files(pattern = ".*(spraak|visus|psychosociaal).*tidy.*")
files <- files[!grepl("_cm", basename(files))] 
files <- files[!grepl("export", basename(files))]

for (file in files) {
  data <- read_xlsx(file)
  data <- simple_check(data)
  new_name <- tools::file_path_sans_ext(basename(file))
  write_xlsx(data, paste0(new_name, "_export.xlsx"))
}

#Overgewicht
files <- list.files(pattern = ".*overgewicht.*tidy.*")
files <- files[!grepl("export", basename(files))]


for (file in files) {
  data <- read_xlsx(file)
  data <- complex_check(data, c("overgewicht", "obesity"))
  new_name <- tools::file_path_sans_ext(basename(file))
  write_xlsx(data, paste0(new_name, "_export.xlsx"))
}

#Gewicht
files <- list.files(pattern = ".*gewicht.*tidy.*")
files <- files[!grepl("over", basename(files))]
files <- files[!grepl("export", basename(files))]


for (file in files) {
  data <- read_xlsx(file)
  data <- complex_check(data, c("bmi", "gewicht", "lengte"))
  new_name <- tools::file_path_sans_ext(basename(file))
  write_xlsx(data, paste0(new_name, "_export.xlsx"))
}

#P90
files <- list.files(pattern = ".*p90.*tidy.*")
files <- files[!grepl("export", basename(files))]


for (file in files) {
  data <- read_xlsx(file)
  data <- complex_check(data, c("bmi", "gewicht"))
  new_name <- tools::file_path_sans_ext(basename(file))
  write_xlsx(data, paste0(new_name, "_export.xlsx"))
}

#BMI percentiles
files <- list.files(pattern = ".*bmi.*tidy.*")
files <- files[!grepl("export", basename(files))]


for (file in files) {
  data <- read_xlsx(file)
  data <- bmi_p_check(data)
  new_name <- tools::file_path_sans_ext(basename(file))
  write_xlsx(data, paste0(new_name, "_export.xlsx"))
}
