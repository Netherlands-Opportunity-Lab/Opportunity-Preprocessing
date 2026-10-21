# Connect JGZ data to GBAADRESOBJECTBUS to get correct postal codes for each contact moment

# load the necessary libraries 
library(tidyverse)
library(haven)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readxl)
library(AGD)
library(readstata13)

setwd("H:/JGZ-Pilot-Data")

jgz_data <- read_dta("H:/Archive/JGZ for Helen and Mirthe/JGZ_panel.dta")

jgz_data <- jgz_data %>%
  mutate(dat_a = as.Date(dat_a_str, format = "%Y%m%d")) #contact moment date in a date format

#max(jgz_data$dat_a) #the latest date is in 2018 so we will use GBAADRESOBJECTBUS2018V1

gbaadres_data <- read_dta("G:/Bevolking/GBAADRESOBJECTBUS/GBAADRESOBJECT2018BUSV1.dta") 

gbaadres_data <- gbaadres_data %>%
  rename(start = GBADATUMAANVANGADRESHOUDING, 
         end = GBADATUMEINDEADRESHOUDING) %>%
  filter(RINPERSOON %in% jgz_data$RINPERSOON) %>%
  mutate(start = as.Date(start, format = "%Y%m%d"),
         end = as.Date(end, format = "%Y%m%d"))




jgz_data <- jgz_data %>%
  add_column(add_column = "rinobjectnummer")

#this loop finds corresponding address at which a person lived at the time of the contact moment
for (i in seq_len(nrow(jgz_data))) {
  person <- jgz_data$RINPERSOON[i]
  visit_date <- jgz_data$dat_a[i]
  
  person_add <- gbaadres_data[gbaadres_data$RINPERSOON == person, ]
  person_add <- person_add %>%
    mutate(rinobjectnummer = paste0(SOORTOBJECTNUMMER, RINOBJECTNUMMER)) %>%
    select(rinobjectnummer, start, end)
 
  matching_add <- person_add$rinobjectnummer[
    visit_date >= person_add$start & visit_date <= person_add$end
  ]
  
  if (length(matching_add > 0)) {
  jgz_data$rinobjectnummer[i] <- matching_add
  }
}
write_rds(jgz_data, "jgz_data_rinobjectnummer")

#load data connecting rinobjectnummer and postcodes
#pc_data <- read.dta13("G:/BouwenWonen/VSLPOSTCODEBUS/geconverteerde data/VSLPOSTCODEBUSV2020031.dta", select.cols = c("rinobjectnummer", "postcodenum"))

#pc_data <- read_dta("G:/BouwenWonen/VSLPOSTCODEBUS/geconverteerde data/VSLPOSTCODEBUSV2020031.dta", col_select = c("rinobjectnummer", "postcodenum"))


selected_dta <- jgz_data[,"rinobjectnummer"]
write_dta(selected_dta, "rin_data.dta")

matched_postcodes <- read_dta("matched_postcode.dta")

jgz_data <- jgz_data %>%
  left_join(matched_postcodes, by = "rinobjectnummer") %>%
  rename(gba_postcode = postcodenum) #to have a distinction between postode in the data and postcode infered from GBA

write_rds(jgz_data, "jgz_gba_postcode_data.rds")


