#clean workspace
rm(list=ls())
options(scipen=999)

library(haven) # read SPSS .sav files
library(dplyr) # data manipulation, equivalent of most gen/replace/merge logic
library(openxlsx)
### OPEN AND SAVE FILES IN R FORMAT ### 

# ID file
id_file <- read_spss("L:/8151mcuniekCBKV2.SAV")
saveRDS(id_file, "H:/JGZ-Pilot-Data/8151mcuniekCBKV2.rds")

# ID file for ZONL
id_zonl <- read_spss("L:/8151ZOrggroepOudeenNieuweLanduniekCBKV1.SAV")
saveRDS(id_zonl, "H:/JGZ-Pilot-Data/8151ZOrggroepOudeenNieuweLanduniekCBKV1.rds")

#Health_file
health <- read_spss("L:/8151GezondheidsbestandxV2CBKV1.SAV")
saveRDS(health, "H:/JGZ-Pilot-Data/8151GezondheidsbestandxV2CBKV1.rds")

### CLEAN ID FILE ###

# Combine original ID file with corrected ZONL id file

# (1) prepare ZONL ID file
zonl_prepared <- id_zonl %>%
  mutate(bestand = "2") %>%
  rename(KindNummer_crypt = kindnummer_crypt)
saveRDS(zonl_prepared, "H:/JGZ-Pilot-Data/81851ZONL_ID_prepared_for_cleaning.dta")
# (2) Open original ID file, drop old ZONL, append corrected ZONL
id_combined <- id_file %>%
  mutate(bestand = as.character(bestand)) %>%
  filter(bestand != "2") %>%    #drop is bestand is not 2
  bind_rows(zonl_prepared)
id_combined <- id_combined %>%
  mutate(bestand = as.numeric(bestand))  #destring bestand, replace

### GENERATE AND LABEL VARIABLES ###

#Organizations
id_combined <- id_combined %>%
  mutate(org = case_when(
    bestand == 1 ~ 6,
    bestand == 2 ~ 7,
    bestand == 3 ~ 10,
    bestand == 4 ~ 12,
    TRUE ~ NA_real_
  ))

id_combined$org <- factor(
  id_combined$org,
  levels = c(6, 7, 10, 12),
  labels = c("LimburgNoord", "ZONL", "Verian", "Icare")
)

#Onderwerp - info comes from aanleverprocedure
id_combined <- id_combined %>%
  mutate(databestand_num = case_when(
    Onderwerp == 1 ~ 1,
    Onderwerp == 4 ~ 2,
    Onderwerp == 2 ~ 3, 
    Onderwerp == 3 ~ 4,
    TRUE ~ NA_real_
  ))

id_combined$databestand <- factor(
  id_combined$databestand_num,
  levels = c(1, 2, 3, 4), 
  labels = c("overgewicht", "visus", "psychosociaal", "spraak-taal")
)


### DESCRIPTIVES: SAMPLE SIZE TRACKING THROUGH CLEANING ###

descriptive <- matrix(NA_real_, nrow = 11, ncol = 5)
colnames(descriptive) <- c("Total", "LimburgNoord", "ZONL", "Verian", "Icare")
rownames(descriptive) <- c(
  "All", "overgewicht", "visus", "psychosociaal", "spraak-taal",
  "Missing-RIN",
  "All", "overgewicht", "visus", "psychosociaal", "spraak-taal"
)

#Lines in Data Before Cleaning
descriptive[1, "Total"] <- nrow(id_combined)

org_counts <- table(id_combined$org)
descriptive[1, names(org_counts)] <- org_counts

#BY Onderwerp
for (d in 1:4) {
  sub <- id_combined %>% filter(databestand_num == d)
  descriptive[1 + d, "Total"] <- nrow(sub)
  org_n <- table(sub$org)
  descriptive[1 + d, names(org_n)] <- org_n
}
print(descriptive)


#### START OF DATA CLEANING ###

##missing observations
#RIN
missing_rin <- id_combined %>% filter(RINPERSOONS == "" & RINPERSOON == "")
descriptive[6, "Total"] <- nrow(missing_rin)
descriptive[6, names(table(missing_rin$org))] <- table(missing_rin$org)

id_combined <- id_combined %>%
  filter(!(RINPERSOONS == "" & RINPERSOON == ""))

#onderwerp
table(id_combined$org[is.na(id_combined$Onderwerp)]) #all missing onderwerp come from Icare

id_combined <- id_combined %>%
  group_by(RINPERSOONS, RINPERSOON, KindNummer_crypt, bestand, org) %>%
  mutate(
    dup_icare = if_else(org == "Icare", as.integer(n() > 1), NA_integer_)
    ) %>%
  ungroup()

table(id_combined$dup_icare[id_combined$org == "icare" & is.na(id_combined$Onderwerp)])
table(id_combined$dup_icare[id_combined$org == "icare" & !is.na(id_combined$Onderwerp)])

#Conclusion: safe to remove these 456 lines
id_combined <- id_combined %>% filter(!is.na(Onderwerp))

##duplicates
# Child IDs alone are not unique: 
sum(duplicated(id_combined$KindNummer_crypt))

#But almost unique by child ID + organization (bestand) + Onderwerp
sum(duplicated(id_combined[, c("KindNummer_crypt", "bestand", "Onderwerp")]))

id_combined <- id_combined %>% 
  group_by(KindNummer_crypt, bestand, Onderwerp) %>%
  mutate(dup = n() > 1) %>%
  ungroup() %>%
  arrange(KindNummer_crypt, bestand, Onderwerp)

View(id_combined %>% filter(dup))
# 12 obs share child ID + Bestand + Onderwerp, impossible to uniquely match on the health_file, so dropped
id_combined <- id_combined %>% filter(!dup)

### LINES IN DATA AFTER CLEANING ###

#total
descriptive[7, "Total"] <- nrow(id_combined)
org_counts_after <- table(id_combined$org)
descriptive[7, names(org_counts_after)] <- org_counts_after
print(descriptive)

for (domain in levels(id_combined$databestand)) {
  d <- id_combined %>% filter(databestand == domain)
  row_idx <- 7 + which(levels(id_combined$databestand) == domain)
  descriptive[row_idx, "Total"] <- nrow(d)
  org_n <- table(d$org)
  descriptive[row_idx, names(org_n)] <- org_n
}
print(descriptive)

#Write descriptive matrix to Excel
wb <- createWorkbook()
addWorksheet(wb, "descriptive")
writeData(wb, "descriptive", descriptive, rowNames = TRUE)
saveWorkbook(wb, "H:/JGZ-Pilot-Data/Descriptives Results/IDfile_descriptives.xlsx", overwrite = TRUE)

### SAVE CLEANED DATA FOR MERGES WITH HEALTH DATA ###

id_combined <- id_combined %>% select(-dup, -dup_icare)
#save all data
saveRDS(id_combined, "H:/JGZ-Pilot-Data/IDfile_clean_all.rds")
#save only icare overgewicht
icare_overgewicht <- id_combined %>%
  filter(org == "Icare", databestand == "overgewicht")
saveRDS(icare_overgewicht, "H:/JGZ-Pilot-Data/IDfile_clean_Icare_overgewicht.rds")

### CLEAN HEALTH FILE ###

health_clean <- health %>%
  arrange(org, KindNummer_crypt, databestand, leeftijd)

health_clean <- health_clean %>%
  mutate(gebl1 = case_when(
    gebl1 == "Afganistan" ~ "Afghanistan",
    gebl1 == "Albani?" ~ "Albanië",
    gebl1 == "Argentini?" ~ "Argentinië",
    gebl1 == "Armeni?" ~ "Armenië",
    gebl1 == "Australi?" ~ "Australië",
    gebl1 == "Azerbajdsjan" ~ "Azerbeidzjan",
    gebl1 == "Belarus(Wit-Rusland)" ~ "Belarus (Wit-Rusland)",
    gebl1 == "Belgi?" ~ "België",
    gebl1 == "Bondsrepubliek Duitsland" ~ "Duitsland",
    gebl1 == "Bosni?-Herzegovina" ~ "Bosnië-Herzegovina",
    gebl1 == "Bosni?-Herzegowina" ~ "Bosnië-Herzegovina",
    gebl1 == "Brazili?" ~ "Brazilië",
    gebl1 == "Brits Somaliland" ~ "Somalië",
    gebl1 == "Burger van Montenegro" ~ "Montenegro",
    gebl1 == "Ceylon" ~ "Sri Lanka",
    gebl1 == "Cura?ao" ~ "Curaçao",
    gebl1 == "Duitsland (oud)" ~ "Duitsland",
    gebl1 == "Ethiopi?" ~ "Ethiopië",
    gebl1 == "Etiopi?" ~ "Ethiopië",
    gebl1 == "Etiopië" ~ "Ethiopië",
    gebl1 == "Frans Guyana" ~ "Frans-Guyana",
    gebl1 == "Georgi?" ~ "Georgië",
    gebl1 == "Groot-Brittanni?" ~ "Groot-Brittannië",
    gebl1 == "Grootbrittanni?" ~ "Groot-Brittannië",
    gebl1 == "Grootbrittannië" ~ "Groot-Brittannië",
    gebl1 == "Ha?ti" ~ "Haïti",
    gebl1 == "Indonesi?" ~ "Indonesië",
    gebl1 == "Isra?l" ~ "Israël",
    gebl1 == "Itali?" ~ "Italië",
    gebl1 == "Joegoslavi?" ~ "Joegoslavië",
    gebl1 == "Jordani?" ~ "Jordanië",
    gebl1 == "Kaapverdi?" ~ "Kaapverdië",
    gebl1 == "Katar" ~ "Qatar",
    gebl1 == "Kroati?" ~ "Kroatië",
    gebl1 == "Libi?" ~ "Libië",
    gebl1 == "Macedoni?" ~ "Macedonië",
    gebl1 == "Madagascar" ~ "Madagaskar",
    gebl1 == "Maleisi?" ~ "Maleisië",
    gebl1 == "Mauritani?" ~ "Mauritanië",
    gebl1 == "Moldavi?" ~ "Moldavië",
    gebl1 == "Mongoli?" ~ "Mongolië",
    gebl1 == "Namibi?" ~ "Namibië",
    gebl1 == "Nederlands Indi?" ~ "Nederlands Indië",
    gebl1 == "NederlandsIndi?" ~ "Nederlands Indië",
    gebl1 == "Nieuw Zeeland" ~ "Nieuw-Zeeland",
    gebl1 == "Nieuwzeeland" ~ "Nieuw-Zeeland",
    gebl1 == "Noordjemen" ~ "Jemen",
    gebl1 == "Noordkorea" ~ "Noord-Korea",
    gebl1 == "Noordvi?tnam" ~ "Viëtnam",
    gebl1 == "Oekra?ne" ~ "Oekraïne",
    gebl1 == "Oostduitse" ~ "Duitse Democratische Republiek",
    gebl1 == "Roemeni?" ~ "Roemenië",
    gebl1 == "Rusland (oud)" ~ "Rusland",
    gebl1 == "Rusland(oud)" ~ "Rusland",
    gebl1 == "Saoedi-Arabi?" ~ "Saoedi-Arabië",
    gebl1 == "Saoediarabi?" ~ "Saoedi-Arabië",
    gebl1 == "Saoediarabië" ~ "Saoedi-Arabië",
    gebl1 == "Servi?" ~ "Servië",
    gebl1 == "Servi? en Montenegro" ~ "Servië en Montenegro",
    gebl1 == "Sloveni?" ~ "Slovenië",
    gebl1 == "Slovakije" ~ "Slowakije",
    gebl1 == "Somali?" ~ "Somalië",
    gebl1 == "Somalia" ~ "Somalië",
    gebl1 == "Sovjetunie" ~ "Sovjet-Unie",
    gebl1 == "Syri?" ~ "Syrië",
    gebl1 == "Tha?land" ~ "Thailand",
    gebl1 == "Tsjechi?" ~ "Tsjechië",
    gebl1 == "Tsjeschiische Republiek" ~ "Tsjechië",
    gebl1 == "Tsjechoslowakije" ~ "Tsjecho-Slowakije",
    gebl1 == "Tunesi?" ~ "Tunesië",
    gebl1 == "Vi?tnam" ~ "Viëtnam",
    gebl1 == "Za?re" ~ "Zaïre",
    gebl1 == "Zambi?" ~ "Zambia",
    gebl1 == "Zuid Korea" ~ "Zuid-Korea",
    gebl1 == "Zuid-Jemen" ~ "Jemen",
    gebl1 == "Zuidafrika" ~ "Zuid-Afrika",
    gebl1 == "Zuidjemen" ~ "Jemen",
    gebl1 == "Zuidkorea" ~ "Zuid-Korea",
    gebl1 == "Zuidvi?tnam" ~ "Viëtnam",
    gebl1 == "Zuidwest Afrika" ~ "Namibië",
    TRUE ~ gebl1
  ))

health_clean <- health_clean %>%
  mutate(gebl2 = case_when(
    gebl2 == "Afganistan" ~ "Afghanistan",
    gebl2 == "Albani?" ~ "Albanië",
    gebl2 == "Argentini?" ~ "Argentinië",
    gebl2 == "Armeni?" ~ "Armenië",
    gebl2 == "Australi?" ~ "Australië",
    gebl2 == "Azerbajdsjan" ~ "Azerbeidzjan",
    gebl2 == "Belarus(Wit-Rusland)" ~ "Belarus (Wit-Rusland)",
    gebl2 == "Belgi?" ~ "België",
    gebl2 == "Bondsrepubliek Duitsland" ~ "Duitsland",
    gebl2 == "Bosni?-Herzegovina" ~ "Bosnië-Herzegovina",
    gebl2 == "Bosni?-Herzegowina" ~ "Bosnië-Herzegovina",
    gebl2 == "Brazili?" ~ "Brazilië",
    gebl2 == "Brits Somaliland" ~ "Somalië",
    gebl2 == "Burger van Montenegro" ~ "Montenegro",
    gebl2 == "Ceylon" ~ "Sri Lanka",
    gebl2 == "Cura?ao" ~ "Curaçao",
    gebl2 == "Duitsland (oud)" ~ "Duitsland",
    gebl2 == "Ethiopi?" ~ "Ethiopië",
    gebl2 == "Etiopi?" ~ "Ethiopië",
    gebl2 == "Etiopië" ~ "Ethiopië",
    gebl2 == "Frans Guyana" ~ "Frans-Guyana",
    gebl2 == "Georgi?" ~ "Georgië",
    gebl2 == "Groot-Brittanni?" ~ "Groot-Brittannië",
    gebl2 == "Grootbrittanni?" ~ "Groot-Brittannië",
    gebl2 == "Grootbrittannië" ~ "Groot-Brittannië",
    gebl2 == "Ha?ti" ~ "Haïti",
    gebl2 == "Indonesi?" ~ "Indonesië",
    gebl2 == "Isra?l" ~ "Israël",
    gebl2 == "Itali?" ~ "Italië",
    gebl2 == "Joegoslavi?" ~ "Joegoslavië",
    gebl2 == "Jordani?" ~ "Jordanië",
    gebl2 == "Kaapverdi?" ~ "Kaapverdië",
    gebl2 == "Katar" ~ "Qatar",
    gebl2 == "Kroati?" ~ "Kroatië",
    gebl2 == "Libi?" ~ "Libië",
    gebl2 == "Macedoni?" ~ "Macedonië",
    gebl2 == "Madagascar" ~ "Madagaskar",
    gebl2 == "Maleisi?" ~ "Maleisië",
    gebl2 == "Mauritani?" ~ "Mauritanië",
    gebl2 == "Moldavi?" ~ "Moldavië",
    gebl2 == "Mongoli?" ~ "Mongolië",
    gebl2 == "Namibi?" ~ "Namibië",
    gebl2 == "Nederlands Indi?" ~ "Nederlands Indië",
    gebl2 == "NederlandsIndi?" ~ "Nederlands Indië",
    gebl2 == "Nieuw Zeeland" ~ "Nieuw-Zeeland",
    gebl2 == "Nieuwzeeland" ~ "Nieuw-Zeeland",
    gebl2 == "Noordjemen" ~ "Jemen",
    gebl2 == "Noordkorea" ~ "Noord-Korea",
    gebl2 == "Noordvi?tnam" ~ "Viëtnam",
    gebl2 == "Oekra?ne" ~ "Oekraïne",
    gebl2 == "Oostduitse" ~ "Duitse Democratische Republiek",
    gebl2 == "Roemeni?" ~ "Roemenië",
    gebl2 == "Rusland (oud)" ~ "Rusland",
    gebl2 == "Rusland(oud)" ~ "Rusland",
    gebl2 == "Saoedi-Arabi?" ~ "Saoedi-Arabië",
    gebl2 == "Saoediarabi?" ~ "Saoedi-Arabië",
    gebl2 == "Saoediarabië" ~ "Saoedi-Arabië",
    gebl2 == "Servi?" ~ "Servië",
    gebl2 == "Servi? en Montenegro" ~ "Servië en Montenegro",
    gebl2 == "Sloveni?" ~ "Slovenië",
    gebl2 == "Slovakije" ~ "Slowakije",
    gebl2 == "Somali?" ~ "Somalië",
    gebl2 == "Somalia" ~ "Somalië",
    gebl2 == "Sovjetunie" ~ "Sovjet-Unie",
    gebl2 == "Syri?" ~ "Syrië",
    gebl2 == "Tha?land" ~ "Thailand",
    gebl2 == "Tsjechi?" ~ "Tsjechië",
    gebl2 == "Tsjeschiische Republiek" ~ "Tsjechië",
    gebl2 == "Tsjechoslowakije" ~ "Tsjecho-Slowakije",
    gebl2 == "Tunesi?" ~ "Tunesië",
    gebl2 == "Vi?tnam" ~ "Viëtnam",
    gebl2 == "Za?re" ~ "Zaïre",
    gebl2 == "Zambi?" ~ "Zambia",
    gebl2 == "Zuid Korea" ~ "Zuid-Korea",
    gebl2 == "Zuid-Jemen" ~ "Jemen",
    gebl2 == "Zuidafrika" ~ "Zuid-Afrika",
    gebl2 == "Zuidjemen" ~ "Jemen",
    gebl2 == "Zuidkorea" ~ "Zuid-Korea",
    gebl2 == "Zuidvi?tnam" ~ "Viëtnam",
    gebl2 == "Zuidwest Afrika" ~ "Namibië",
    gebl2 == "Abessinië" ~ "Ethiopië",
    gebl2 == "Abessini?" ~ "Ethiopië",
    gebl2 == "Centafrika" ~ "Centraal-Afrikaanse Republiek",
    gebl2 == "Centrafrika" ~ "Centraal-Afrikaanse Republiek",
    gebl2 == "Faer?er" ~ "Faröer",
    gebl2 == "Federale Republiek Joegoslavi?" ~ "Joegoslavië",
    gebl2 == "Oekraine" ~ "Oekraïne",
    gebl2 == "Rhodesi?" ~ "Zimbabwe",
    gebl2 == "Rhodesië" ~ "Zimbabwe",
    gebl2 == "Russische Federatie" ~ "Rusland",
    TRUE ~ gebl2
  ))

### SET ALL STRINGS TO CAPITALS ONLY ###

health_clean <- health_clean %>%
  mutate(
    gem        = toupper(gem),
    woonplaats = toupper(woonplaats), #differs grom cbs script
    gebl1      = toupper(gebl1),
    gebl2      = toupper(gebl2)
  )

### DESTRING VARIABLES THAT ARE STORED AS STRING BUT ARE NUMERIC ###

notnumeric_postcode <- health_clean %>%
  filter(is.na(suppressWarnings((as.numeric(postcode)))))
health_clean <- health_clean %>%
  mutate(postcode = suppressWarnings(as.numeric(postcode)))

health_clean <- health_clean %>%
  mutate(
    geslacht = suppressWarnings(as.numeric(as.character(geslacht))),
    i_jgz    = suppressWarnings(as.numeric(as.character(i_jgz))),
    opl1     = suppressWarnings(as.numeric(as.character(opl1))),
    opl2     = suppressWarnings(as.numeric(as.character(opl2))),
    i_cm     = suppressWarnings(as.numeric(as.character(i_cm))),
    bst_nl   = suppressWarnings(as.numeric(as.character(bst_nl)))
  )

### HEALTH FILE DISCRIPTIVE: LINES IN DATA BEFORE CLEANING ###

descriptive_health <- matrix(NA_real_, nrow  = 11, ncol = 5)
colnames(descriptive_health) <- c("Total", "LimburgNoord", "ZONL", "Verian", "Icare")
rownames(descriptive_health) <- c(
  "All", "Overgewicht", "Visus", "Psychosociaal", "Spraak-taal",
  "All-Duplicates",
  "All", "Overgewicht", "Visus", "Psychosociaal", "Spraak-taal"
)
health_clean$org <- factor(
  health_clean$org,
  levels = c(6, 7, 10, 12), 
  labels = c("LimburgNoord", "ZONL", "Verian", "Icare")
)

#Total
descriptive_health[1, "Total"] <- nrow(health_clean)
org_counts_health <- table(health_clean$org)
descriptive_health[1, names(org_counts_health)] <- org_counts_health

#By Onderwerp
for (d in 1:4) {
  sub <- health_clean %>% filter(databestand == d)
  descriptive_health[1 + d, "Total"] <- nrow(sub)
  org_n <- table(sub$org)
  descriptive_health[1 + d, names(org_n)] <- org_n
}
print(descriptive_health)

#check and clean duplicates on all variables

dup_check_vars <- setdiff(names(health_clean), "dubbel")

health_clean <- health_clean %>%
  group_by(across(all_of(dup_check_vars))) %>%
  mutate(.dup_count = row_number()) %>%
  ungroup()

n_before <-  nrow(health_clean)
health_clean <- health_clean %>% filter(.dup_count == 1) %>% select(-.dup_count)
n_after <- nrow(health_clean)

message(sprintf("Dropped %d exact-duplicate rows (source do-file: 3,166)", n_before - n_after))

group_vars <- c("org", "KindNummer_crypt", "databestand", "leeftijd")
all_vars <- setdiff(names(health_clean), group_vars)

flag_switches <- function(df, var, group_vars) {
  is_num <- is.numeric(df[[var]])
  if (is_num) {
    df <- df %>%
      group_by(across(all_of(group_vars))) %>%
      arrange(.data[[var]], .by_group = TRUE) %>%
      mutate(
        .is_miss   = is.na(.data[[var]]),
        .prev_val  = lag(.data[[var]]),
        .prev_miss = lag(.is_miss),
        f   = if_else(row_number() != 1 & !.prev_miss &
                        .data[[var]] != .prev_val, 1, NA_real_),
        f_n = if_else(row_number() != 1 & !.prev_miss & !.is_miss &
                        .data[[var]] != .prev_val, 1, NA_real_)
      ) 
  } else {
          df <- df %>%
            group_by(across(all_of(group_vars))) %>%
            arrange(.data[[var]], .by_group = TRUE) %>%
            mutate(
              .is_miss    = .data[[var]] == "" | is.na(.data[[var]]),
              .prev_val  = lag(.data[[var]]),
              .prev_miss  = lag(.is_miss),
              f   = if_else(row_number() != 1 & !.is_miss &
                              .data[[var]] != .prev_val, 1, NA_real_),
              f_n = if_else(row_number() != 1 & !.is_miss & !.prev_miss &
                              .data[[var]] != .prev_val, 1, NA_real_)
            )
  }
  
  df <- df %>%
    mutate(
      max_f       = suppressWarnings(max(f, na.rm = TRUE)),
      max_f       = if_else(is.infinite(max_f), 0, max_f),
      max_f_n     = suppressWarnings(max(f_n, na.rm = TRUE)),
      max_f_n     = if_else(is.infinite(max_f_n), 0, max_f_n)
      ) %>%
    ungroup() %>%
    select(-.is_miss, -.prev_val, -.prev_miss)
  
  names(df)[names(df) == "f"]       <- paste0("f_", var)
  names(df)[names(df) == "f_n"]       <- paste0("f_n_", var)
  names(df)[names(df) == "max_f"]       <- paste0("max_f_", var)
  names(df)[names(df) == "max_f_n"]       <- paste0("max_f_n_", var)
  df
  }
  
for (v in all_vars) {
  health_clean <- flag_switches(health_clean, v, group_vars)
}

#Diagnostics matrix
variations <- matrix(NA_real_, nrow = length(all_vars), ncol = 8)
rownames(variations) <- all_vars
colnames(variations) <- c(
  "N", "Unique contact moments (N)",
  "Switches (with missings)", "CMs switches (with missings)", "Rows affected (with", 
  "Switches (without missings)", "CMs switches (without missings)", "Rows affected (without)"
)

grp_id <- health_clean %>%
  group_by(across(all_of(group_vars))) %>%
  mutate(.grp_id = cur_group_id()) %>%
  ungroup() %>%
  pull(.grp_id)

for (v in all_vars) {
  is_num <- is.numeric(health_clean[[v]])
  is_miss <- if (is_num) is.na(health_clean[[v]]) else
    (health_clean[[v]] == "" | is.na(health_clean[[v]]))
  f_v <- health_clean[[paste0("f_", v)]]
  f_n_v <- health_clean[[paste0("f_n_", v)]]
  max_f_v <- health_clean[[paste0("max_f_", v)]]
  max_f_n_v <- health_clean[[paste0("max_f_n_", v)]]
  
  variations[v, 1] <- sum(!is_miss)
  variations[v, 2] <- length(unique(grp_id[!is_miss]))
  variations[v, 3] <- sum(f_v, na.rm = TRUE)
  variations[v, 4] <- length(unique(grp_id[max_f_v == 1]))
  variations[v, 5] <- sum(max_f_v == 1, na.rm = TRUE)
  variations[v, 6] <- sum(f_n_v, na.rm = TRUE)
  variations[v, 7] <- length(unique(grp_id[max_f_n_v == 1]))
  variations[v, 8] <- sum(max_f_n_v == 1, na.rm = TRUE)
}
print(variations)

wb2 <- createWorkbook()
addWorksheet(wb2, "variations")
writeData(wb2, "variations", variations, rowNames = TRUE)
saveWorkbook(wb2, "H:/JGZ-Pilot-Data/Gezondheidsbestand_variables_within_onderwerp.xlsx", overwrite = TRUE)

saveRDS(health_clean, "H:/JGZ-Pilot-Data/Health_data_cleaning.rds")

### CREATE FRANKENSTEIN FILE ###

health_clean <- health_clean %>%
  mutate(f_frankenstein = if_else(
    if_any(all_of(paste0("max_f_n_", all_vars)), ~ . == 1), 1, NA_real_
  ))

n_flagged <- sum(health_clean$f_frankenstein == 1, na.rm = TRUE)
message(sprintf("%d rows flagged for FRANKENSTEIN file", n_flagged))

frankenstein <- health_clean %>% filter(f_frankenstein == 1)

for (v in all_vars) {
  max_f_n_v <- frankenstein[[paste0("max_f_n_", v)]]
  if (is.numeric(frankenstein[[v]])) {
    frankenstein[[v]][max_f_n_v != 1] <- NA
  } else {
    frankenstein[[v]][max_f_n_v != 1] <- ""
  }
}

frankenstein <- frankenstein %>% select(-f_frankenstein)
saveRDS(frankenstein, "H:/JGZ-Pilot-Data/FRANKENSTEIN.rds")

### CLEAN UP DATABESTAND: 5-Category VaRIABLE TREATMENT ###

cat_by <- group_vars

cat_nothing <- c(
  "postcode", "woonplaats", "gem", "st_a", "geslacht", "i_cm", "jaartal", 
  "kindmogelijknietunieknr2", "maanden", "bril", "lh_r", "lh_l", "sc_ep", 
  "sc_gp", "sc_pl", "sc_psg", "sdq_i", "zwduur", "dat_a_str"
)

cat_fill_empty <- c(
  "so_a2", "apkt_r4", "apkt_l4", "srt_o", "spontaan", "vragen1",
  "verstaan2", "vragen2", "r_gd_vve"
)

cat_dummies <- c(
  "so_a", "i_jgz", "indi", "inter", "verwijs", "opl1", "opl2", 
  "kindmogelijknietunieknr", "dubbel", "bst_nl", "taalomg", "meertaal",
  "d_vvenum"
)

cat_remove <- c(
  "lengte", "gewicht", "gebl1", "gebl2", "schoolnr", "lft2", "oog_o",
  "apk_r", "apk_l", "apkt_r3", "apkt_l3", "apkt_r5", "apkt_l5", "lc_r", "lc_l" , "visus_c",
  "scr_psp", "sdq_t", "zin2w",  "pop6", "noemt", "wijstaan", "zin3w", "verstaan1", "dat_a2_str"
)

for (v in cat_fill_empty) {
  health_clean <- health_clean %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(!!v := {
      x <- .data[[v]]
      if (is.numeric(x)) {
        first_val <- suppressWarnings(min(x, na.rm = TRUE))
        if (is.infinite(first_val)) x else rep(first_val, length(x))
      } else {
        sorted_x <- sort(x)
        rep(sorted_x[1], length(x))
      }
    }) %>%
    ungroup()
}
#Category 4
for (v in cat_dummies) {
  x <- health_clean[[v]]
  is_num <- is.numeric(x)
  miss_flag <- if (is_num) is.na(x) else (x == "" | is.na(x))
  health_clean[[paste0("d_", v, "_miss")]] <- if_else(miss_flag, 1, NA_real_)
  
  lvls <- unique(x[!miss_flag])
  for (lvl in lvls) {
    lvl_name <- gsub("[^A-Za-z0-9]", "_", as.character(lvl))
    health_clean[[paste0("d_", v, "_", lvl_name)]] <- if_else(x == lvl, 1, NA_real_)
  }
}

#Category 5
for (v in cat_remove) {
  max_f_n_v <- health_clean[[paste0("max_f_n_", v)]]
  is_num <- is.numeric(health_clean[[v]])
  if (is_num) {
    health_clean[[v]][max_f_n_v == 1] <- NA
  } else {
    health_clean[[v]][max_f_n_v == 1] <- ".r"
  }
  max_f_col <- paste0("max_f_", v)
  max_f_n_col <- paste0("max_f_n_", v)
  health_clean <- health_clean %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(!!v := {
      x <- .data[[v]]
      cond <- .data[[max_f_col]] == 1 & .data[[max_f_n_col]] != 1
      if(any(cond, na.rm = TRUE)) {
        fill_val <- if (is_num) suppressWarnings(min(x, na.rm = TRUE)) else sort(x) [length(x)]
        x[cond] <- fill_val
      }
      x
    }) %>%
    ungroup()
}

### COLLAPSE to 1 line PER org-KindNummer_rypt-databestand-leeftijd
flag_cols <- unlist(lapply(all_vars, function(v) paste0(c("f_", "max_f_", "f_n_",
                                                          "max_f_n_"), v)))
health_clean <- health_clean %>% select(-any_of(flag_cols))

collapse_vars <- setdiff(names(health_clean), group_vars)

health_clean <- health_clean %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    across(all_of(collapse_vars), function(x) {
      if (is.character(x)) {
        v <- x[!is.na(x) & x != ""]
        if (length(v)) v[1] else NA_character_
      } else {
        m <- suppressWarnings(max(x, na.rm = TRUE))
        if (is.infinite(m)) NA_real_ else m
      }
    }),
    .groups = "drop"
  )

duplicates_check <- health_clean %>%
  count(across(all_of(group_vars))) %>%
  filter(n > 1)
stopifnot(nrow(duplicates_check) == 0)

saveRDS(health_clean, "H:/JGZ-Data-Pilot/Health_by_onderwerp.rds")

### HEALTH FILE DESCRIPTIVES: LINES IN DATA AFTER CLEANING ###

descriptive_health[7, "Total"] <- nrow(health_clean)
org_counts_health_after <- table(health_clean$org)
descriptive_health[7, names(org_counts_health_after)] <- org_counts_health_after

for (d in 1:4) {
  sub <- health_clean %>% filter(databestand == levels(health_clean$databestand[d]))
  descriptive_health[7 + d, "Total"] <- nrow(sub)
  org_n <- table(sub$org)
  descriptive_health[7+d, names(org_n)] <- org_n
}
print(descriptive_health)

wb3 <- createWorkbook()
addWorksheet(wb3, "descriptive")
writeData(wb3, "descriptive", descriptive_health, rowNames = TRUE)
saveWorkbook(wb3, "H:/JGZ-Pilot-Data/HealthFile_descriptives.xlsx", overwrite = TRUE)


### Merging HEALTH and ID FILE ###

stata_merge <- function(master, using, by) {
  master <- master %>% mutate(.in_master = TRUE)
  using <- using %>% mutate(.in_using = TRUE)
  merged <- full_join(master, using, by = by, suffix = c("", ".using"))
  merged %>%
    mutate(
      .in_master = !is.na(.in_master), 
      .in_using = !is.na(.in_using),
      '_merge' = case_when(
        .in_master & !.in_using ~ 1,
        !.in_master & .in_using ~ 2,
        TRUE ~ 3
      )
    ) %>%
    select(-.in_master, -.in_using)
}

merges <- matrix(NA_real__, nrow = 3, ncol = 2)
rownames(merges) <- c("All three", "Icare-visus", "Total")
colnames(merges) <- c("Using only", "Matched")

#MERGE 1: merge on all 3 identifying variables
id_file_all <- readRDS("H:/JGZ-Pilot-Data/IDfile_clean_all.rds")

health_merge1 <- health_clean %>%
  filter(!(org == "Icare" & databestand == "visus")) %>%
  stata_merge(id_file_all, by = c("org", "KindNummer_crypt", "databestand"))

merges[1, "Using only"] <- sum(health_merge1$`_merge` == 1)
merges[1, "Matched"]    <- sum(health_merge1$`_merge` == 3)

health_merge1 <- health_merge1 %>% filter(`_merge` != 2)
saveRDS(health_merge1, "H:/JGZ-Pilot-Data/Health_ID_merge1.rds")

# MERGE3 
icare_overgewicht_ids <- readRDS("H:/JGZ-Pilot-Data/IDfile_clean_Icare_overgewicht.rds")

health_merge3 <- health_clean %>%
  filter(org == "Icare", databestand == "visus") %>%
  stata_merge(icare_overgewicht_ids, by = c("org", "KindNummer_crypt"))

merges[2, "Using only"] <- sum(health_merge3$`_merge` == 1)
merges[2, "Matched"]    <- sum(health_merge3$`merge` == 3)

health_merge3 <- health_merge3 %>% filter(`_merge` != 2)
saveRDS(health_merge3, "H:/JGZ-Pilot_Data/Health_ID_merge3.rds")

#append the two merges, the recompute the "Total row"
health_merged <- bind_rows(health_merge1, health_merge3)

merges[3, "Using only"] <- sum(health_merged$`_merge` == 1)
merge[3, "Matched"]     <- sum(health_merged$`merge` == 3)
print(merges)


wb4 <- createWorkbook()
addWorksheet(wb4, "merges")
writeData(wb4, "merges", merges, rowNames = TRUE)
saveWorkbook(wb4, "H:/JGZ-Pilot-Data/Merges.xlsx", overwrite = TRUE)

### CLEAN MERGED FILE ###
health_merged <- health_merged %>% filter(`_merge` != 1) %>% select(-`_merge`)

health_merged <- health_merged %>% relocate(RINPERSOONS, RINPERSOON)
#drop rows without RINPERSOONS
health_merged <- health_merged %>% filter(RINPERSOONS != "")
# Rename overly long dummy-variable banes
health_merged <- health_merged %>%
  rename(
    d_kmn_uniek_miss = d_kindmogelijknietunieknr_miss, 
    d_kmn_uniek_0    = d_kindmogelijknietunieknr_0,
    dm_kmn_uniek_1   = d_kindmogelijknietunieknr_1
  )

### CHECK AND CLEAN DUPLICATES ON ALL VARIABLES ###
health_merged <- health_merged %>% select(-KindNummer_crypt)

n_before_dedup <- nrow(health_merged)
health_merged <- health_merged %>% distinct(across(everything()), .keep_all = TRUE)
n_after_dedup <- nrow(health_merged)

message(sprintf("Dropped %d full-row duplicates (source do-file: 482)",
                n_before_dedup - n_after_dedup))

### SWITCH-DETECTION + VARIATIONS MATRIX -- MERGED FIle ###
merged_group_vars <- c("RINPERSOONS", "RINPERSOON", "databestand", "leeftijd")
merged_all_vars <- setdiff(names(health_merged), merged_group_vars)

message(sprintf("Variables entering merged-file switch detection: %d (source do-file: 174)",
                length(merged_all_vars)))

for (v in merged_all_vars) {
  health_merged <- flag_switches(health_merged, v, merged_group_vars)
}

variations_merged <- matrix(NA_real_, nrow = length(merged_all_vars), ncol = 8)
rownames(variations_merged) <- merged_all_vars
colnames(variations_merged) <- c(
  "N", "Unique contact moments (N)","Switches (with missings)", "CMs switches (with missings)",
  "Rows affected" "Switches (without missings)", "CMs switches (without missings)", "Rows affected"
)

grp_id_merged <- health_merged %>%
  group_by(across(all_of(merged_group_vars))) %>%
  mutate(.grp_id = cur_group_id()) %>%
  ungroup() %>%
  pull(.grp_id)

for (v in merged_all_vars) {
  is_num <- is.numeric(health_merged[[v]])
  is_miss <- if (is_num) is.na(health_merged[[v]]) else
    (health_merged[[v]] == "" | is.na(health_merged[[v]]))
  
  f_v <- health_merged[[paste0("f_", v)]]
  f_n_v <- health_merged[[paste0("f_n_", v)]]
  max_f_v <- health_merged[[paste0("max_f_", v)]]
  max_f_n_v <- health_merged[[paste0("max_f_n_", v)]]
  
  variations_merged[v, 1] <- sum(!is_miss)
  variations_merged[v, 2] <- length(unique(grp_id_merged[!is_miss]))
  variations_merged[v, 3] <- sum(f_v, na.rm = TRUE)
  variations_merged[v, 4] <- length(unique(grp_id_merged[max_f_v == 1]))
  variations_merged[v, 5] <- sum(max_f_v == 1, na.rm = TRUE)
  variations_merged[v, 6] <- sum(f_n_v, na.rm = TRUE)
  variations_merged[v, 7] <- length(unique(grp_id_merged[max_f_n_v == 1]))
  variations_merged[v, 8] <- sum(max_f_n_V == 1, na.rm = TRUE)
}

print(variations_merged)

wb5 <- createWorkbook()
addWorksheet(wb5, "variations", variations_merged, rowNames = TRUE)
writeData(wb5, "variations", variations_merged, rowNames = TRUE)
saveWorkbook(wb5, "H:/JGZ-Pilot-Data/MergedData_variables_within_onderwerp.xlsx", overwrite = TRUE)

saveRDS(health_merged, "H:/JGZ-Pilot_Data/Merged_cleaning_within_onderwerp.rds")

### VARIABLES THAT NEED TO BE CLEANED BASED ON MATRIX ###
remove_switches_only <- function(df, vars) {
  for (v in vars) {
    max_f_n_v <- df[[paste0("max_f_n_", v)]]
    if (is.numeric(df[[v]])) {
      df[[v]][max_f_n_v == 1] <- NA
    } else {
      df[[v]][max_f_n_v == 1] <- ".r"
    }
  }
  df
}

### REMOVE GENUINE CONFLICT -- FIRST PASS ###
switches_to_remove_1 <- intersect(
  c("org", "woonplaats", "gem", "gebl1", "gebl2", "postcode", "gewicht", "zwduur", "bestand"),
  names(health_merged)
)
health_merged <- remove_switches_only(health_merged, switches_to_remove_1)

### COLLPASE INTO 1 LINE PER RINPERSOONS-RINPERSOON-databestand-leeftijd ###
flag_cols_merged <- unlist(lapply(merged_all_vars, function(v) paste0(c("f_", "max_f_", "max_f_n_"), v)))
health_merged <- health_merged %>% select(-any_of(flag_cols_merged))

collapse_vars_merged <- setdiff(names(health_merged), merged_group_vars)

health_merged <- health_merged %>%
  group_by(across(all_of(merged_group_vars))) %>%
  summarise(
    across(all_of(collapse_vars_merged), function(x) {
      if (is.character(x)) {
        v <- x[!is.na(x) & x != ""]
        if (length(v)) v[1] else NA_character_
      } else {
        m <- suppressWarnings(max(x, na.rm = TRUE))
        if (is.infinite(m)) NA_real_ else m
      }
    }),
    .groups = "drop"
  )

saveRDS(health_marged, "H:/JGZ-Pilot-Data")

domain_group_vars <- c("RINPERSOONS", "RINPERSOON", "leeftijd")
domain_all_vars <- setdiff(names(health_merged), domain_group_vars)
message(sprintf("Variables entering across-domain switch detection: %d (source do-file: 172)",
                length(domain_all_vars)))

for (v in domain_all_vars) {
  health_merged <- flag_swiitches(health_merged, v, domain_group_vars)
}

variations_domain <- matrix(NA_real_, nrow = length(domain_all_vars), ncol = 8)
rownames(variations_domain) <- domain_all_vars
colnames(variations_domain) <- c(
  "N", "Unique contact moments (N)", 
  "Switches (with missing)", "CMs switches (with missings)", "Rows affected (with)",
  "Switches (without missings)", "CMs switches (without missings)", "Rows affected (without)"
)

grp_id_domain <- health_merged %>%
  group_by(across(all_of(domain_groups_vars))) %>%
  mutate(.grp_id = cur_group_id()) %>%
  ungroup() %>%
  pull(.grp_id)

for (v in domain_all_vars) {
  is_num <- is.numeric(health_merged[[v]])
  is_miss <- if (is_num) is.na(health_merged[[v]]) else
    (health_merged[[v]] == "" | is.na(health_merged[[v]]))
  f_v <- health_merged[[paste0("f_", v)]]
  f_n_v <- health_merged[[paste0("f_n_", v)]]
  max_f_v <- health_merged[[paste0("max_f_", v)]]
  max_f_n_v <- health_merged[[paste0("max_f_n_", v)]]
  
  variations_domain[v, 1] <- sum(!is_miss)
  variations_domain[v, 2]  <- length(unique(grp_id_domain[!is_miss]))
  variations_domain[v, 3] <- sum(f_v, na.rm = TRUE)
  variations_domain[v, 4] <- length(unique(grp_id_domain[max_f_v == 1]))
  variations_domain[v, 5] <- sum(max_f_v == 1, na.rm = TRUE)
  variations_domain[v, 6] <- sum(f_n_v, na.rm = TRUE)
  variations_domain[v, 7] <- length(unique(grp_id_domain[max_f_n_v == 1]))
  variations_domain[v, 8] <- sum(max_f_n_v == 1, na.rm = TRUE)
}
print(variations_domain)

wb6 <- createWorkbook()
addWorksheet(wb6, "variations")
writeData(wb6, "variations", variations_domain, rowNames = TRUE)
saveWorkbook(wb6, "H:/JGZ-Pilot-Data/MergedData_variables_across_onderwerp.xlsx", overwrite = TRUE)

saveRDS(health_merged, "H:/JGZ-Pilot-Data/Merged_data_cleaning.rds")

# Diagnostic-only duplicates check (no rows dropped by this)
dupes_across_domain <- health_merged %>%
  count(across(everythin())) %>%
  filter(n > 1)
message(springf("Full-row duplicates remainng after across-domain collapse: %d groups",
                nrow(dupes_across_domain)))

# fix of problem: same RIN has multiple lines within the same domain, even after
# the across-domain collapse
switches_to_remove_2 <- intersect(
  c("org", "databestand", "woonplaats", "gem", "gebl1", "gebl2", "postcode", "gewicht", "zwduur"),
  names(health_merged)
)
health_merged <- remove_switches_only(health_merged, switches_to_remove_2)

### COLLAPSE INTO PANEL DATA ###
domain_flag_cols <- unlist(lapply(domain_all_vars, function(v) {
  paste0(c("f_", "max_f_", "f_n_", "max_f_n_"), v)
}))
health_merged <- health_merged %>% select(-any_of(domain_flag_cols))

firstnm_vars <- c(
  "woonplaats", "gem", "gebl1", "gebl2", "schoolnr", "visus_c",
  "dat_a_str", "dat_a2_str"
)

max_vars <- c(
  "org", "postcode", "geslacht", "i_cm", "st_a", "lengte", "gewicht", "lft2", "so_a2", "jaartal", "kindmogelijknietunieknr2",
  "maanden", "bril", "oog_o", "apk_r", "apk_l", "apkt_r3", "apkt_l3", "apkt_r4", "apkt_l4", "apkt_r5", "apkt_l5", "lh_r", 
  "lh_l", "lc_r", "lc_l", "scr_psp", "sc_ep", "sc_gp", "sc_pl", "sc_ha", "sdq_t", "sc_psg", "sdq_i", "srt_o", "zin2w", "pop6", "noemt", "wijstaan",
  "zin3w", "verstaan1", "spontaan", "vragen1", "verstaan2", "vragen2", "zwduur", "r_gd_vve", "d_so_a_miss", "d_so_a_2",
  "d_so_a_3", "d_so_a_4", "d_so_a_5", "d_so_a_6", "d_so_a_7", "d_so_a_9", "d_so_a_10", "d_so_a_11", "d_so_a_12", "d_so_a_13",
  "d_so_a_14", "d_so_a_15", "d_so_a_16", "d_so_a_17", "d_so_a_19", "d_so_a_20", "d_so_a_22", "d_so_a_25", "d_i_jgz_miss",
  "d_i_jgz_1", "d_i_jgz_2", "d_i_jgz_3", "d_indi_miss", "d_indi_9", "d_indi_10", "d_indi_12", "d_indi_18",
  "d_indi_19", "d_indi_20", "d_inter_miss", "d_inter_1", "d_inter_2", "d_inter_3", "d_inter_4", "d_inter_5", "d_inter_6",
  "d_inter_7", "d_inter_98", "d_verwijs_miss", "d_verwijs_1", "d_verwijs_2", "d_verwijs_3", "d_verwijs_4", "d_verwijs_5",
  "d_verwijs_6", "d_verwijs_7", "d_verwijs_8", "d_verwijs_9", "d_verwijs_10", "d_verwijs_11", "d_verwijs_12", "d_verwijs_13",
  "d_verwijs_14", "d_verwijs_15", "d_verwijs_16", "d_verwijs_17", "d_verwijs_19", "d_verwijs_20", "d_verwijs_21", "d_verwijs_23",
  "d_verwijs_24", "d_verwijs_25", "d_verwijs_26", "d_verwijs_27", "d_verwijs_28", "d_verwijs_29", "d_verwijs_98", "d_opl1_miss",
  "d_opl1_0 d_opl1_1", "d_opl1_2", "d_opl1_3", "d_opl1_4", "d_opl1_5", "d_opl1_6", "d_opl1_7", "d_opl1_8", "d_opl1_9", "d_opl1_98",
  "d_opl2_miss", "d_opl2_0", "d_opl2_1", "d_opl2_2", "d_opl2_3", "d_opl2_4", "d_opl2_5", "d_opl2_6", "d_opl2_7", "d_opl2_8",
  "d_opl2_9", "d_opl2_98", "d_kmn_uniek_miss", "d_kmn_uniek_0", "d_kmn_uniek_1", "d_dubbel_miss", "d_dubbel_0", "d_dubbel_1",
  "d_bst_nl_miss", "d_bst_nl_1", "d_bst_nl_2", "d_taalomg_miss", "d_taalomg_1", "d_taalomg_2", "d_taalomg_3", "d_meertaal_miss",
  "d_meertaal_1", "d_meertaal_2", "d_meertaal_3", "d_d_vvenum_miss", "d_d_vvenum_1", "d_d_vvenum_2"
)

missing_firstnm <- setdiff(fristnm_vars, names(health_merged))
missing_max <- setdiff(max_vars, names(health_merged))
if (length(missing_firstnm) > 0) {
  warning("firstnm_vars not found in health_merged: ", paste(missing_firstnm, collapse = ", "))
}
if (length(missing_max) > 0) {
  warning("max_vars not found in health_merged: ", paste(missing_max, collapse = ", "))
}
firstnm_vars <- intersect(firstnm_vars, names(health_merged))
max_vars <- intersect(max_vars, names(health_merged))

panel_group_vars <- domain_group_vars #RINPERSOONS, RINPERSOON, leeftijd

JGZ_panel <- health_merged %>%
  group_by(across(all_of(panel_group_vars))) %>%
  summarise(
    across(all_of(firstnm_vars), function(x) {
      v <- x[!is.na(x) & x != ""]
      if (length(v)) v[1] else NA_character_
    }),
    across(all_of(max_vars), function(x) {
      m <- suppressWarnings(max(x, na.rm = TRUE))
      if (is.infinite(m)) NA_real_ else m
    }),
    .groups = "drop"
  )

panel_dupes <- JGZ_panel %>%
  count(across(all_of(panel_group_vars))) %>%
  filter(n > 1)
stopifnot(nrow(panel_dupes) == 0)

#sort RINPERSOONS RINPERSOON leeftid
JGZ_panel <- JGZ_panel %>% arrange(across(all_of(panel_gorup_vars)))

saveRDS(JGZ_panel, "H:/JGZ-Panel-Data/JGZ_panel.rds")