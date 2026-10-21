jgz_data <- read_rds("H:/JGZ-Pilot-Data/jgz_gba_postcode_data.rds")

# create new, accurate dummy variables for the contact moments based on the age of the children 
jgz_data <- jgz_data %>% 
  mutate(lft_maanden = leeftijd / 30, 
         cm_4wks = ifelse(leeftijd > 0 & leeftijd <= 46, 1, 0),
         cm_8wks = ifelse(leeftijd > 46 & leeftijd <= 79, 1, 0),
         cm_3months = ifelse(leeftijd > 79 & leeftijd <= 111, 1, 0),
         cm_4months = ifelse(leeftijd > 111 & leeftijd <= 169, 1, 0),
         cm_6months = ifelse(leeftijd > 169 & leeftijd <= 261, 1, 0),
         cm_9months = ifelse(leeftijd > 261 & leeftijd <= 320, 1, 0),
         cm_11months = ifelse(leeftijd > 320 & leeftijd <= 405, 1, 0),
         cm_14months = ifelse(leeftijd > 405 & leeftijd <= 513, 1, 0),
         cm_18months = ifelse(leeftijd > 513 & leeftijd <= 677, 1, 0),
         cm_2years = ifelse(leeftijd > 677 & leeftijd <= 1038, 1, 0),
         cm_3years = ifelse(leeftijd > 1038 & leeftijd <= 1319, 1, 0),
         cm_3years9m = ifelse(leeftijd > 1319 & lft_maanden <= 60, 1, 0),
         cm_groep2 = ifelse(lft_maanden > 60 & lft_maanden <= 93, 1, 0),
         cm_groep7 = ifelse(lft_maanden > 93 & lft_maanden <= 153, 1, 0),
         cm_klas2 = ifelse(lft_maanden > 153 & lft_maanden <= 902, 1, 0))


jgz_data <- jgz_data %>%
  mutate(BMI = gewicht / 1000 / (lengte / 1000)^2, 
         bmi_znl = scale(BMI))

jgz_output <- jgz_data %>% select(c(RINPERSOONS, RINPERSOON, leeftijd, woonplaats, gem, postcode, 
                                    geslacht, jaartal, org, lengte, gewicht, BMI, bmi_znl, #BMI and bmi_znl not in the data
                                    sdq_t, zin2w, pop6, noemt, wijstaan, zin3w, verstaan1, spontaan, vragen1, verstaan2, vragen2, 
                                    lft_maanden, starts_with("cm_"), gba_postcode))
jgz_output <- jgz_output %>% 
  mutate(postcode3 = strtrim(as.character(gba_postcode), 3), 
         postcode3 = as_factor(postcode3))

jgz_output <- jgz_output %>% 
  mutate(sdq_11 = ifelse(sdq_t >= 11, 1, 0),
         sdq_11 = as_factor(sdq_11),
         sdq_14 = ifelse(sdq_t >= 14, 1, 0),
         sdq_14 = as_factor(sdq_14))



cm_4wks <- jgz_output %>% filter(cm_4wks == 1)
cm_8wks <- jgz_output %>% filter(cm_8wks == 1)
cm_3months <- jgz_output %>% filter(cm_3months == 1)
cm_4months <- jgz_output %>% filter(cm_4months == 1)
cm_6months <- jgz_output %>% filter(cm_6months == 1)
cm_9months <- jgz_output %>% filter(cm_9months == 1)
cm_11months <- jgz_output %>% filter(cm_11months == 1)
cm_14months <- jgz_output %>% filter(cm_14months == 1)
cm_18months <- jgz_output %>% filter(cm_18months == 1)
cm_2years <- jgz_output %>% filter(cm_2years == 1)
cm_3years <- jgz_output %>% filter(cm_3years == 1)
cm_3years9m <- jgz_output %>% filter(cm_3years9m == 1)
cm_groep2 <- jgz_output %>% filter(cm_groep2 == 1)
cm_groep7 <- jgz_output %>% filter(cm_groep7 == 1)
cm_klas2 <- jgz_output %>% filter(cm_klas2 == 1)

write_rds(cm_4wks,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_4wks.rds" )
write_rds(cm_4wks,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_4wks.rds") 
write_rds(cm_8wks,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_8wks.rds")
write_rds(cm_3months,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_3months.rds")
write_rds(cm_4months,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_4months.rds")
write_rds(cm_6months,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_6months.rds")
write_rds(cm_9months,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_9months.rds")
write_rds(cm_11months,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_11months.rds")
write_rds(cm_14months,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_14months.rds")
write_rds(cm_18months,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_18months.rds")
write_rds(cm_2years,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_2years.rds")
write_rds(cm_3years,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_3years.rds")
write_rds(cm_3years9m,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_3years9m.rds")
write_rds(cm_groep2,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_groep2.rds")
write_rds(cm_groep7,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_groep7.rds")
write_rds(cm_klas2,"H:/Opportunity-Preprocessing/scratch/JGZ/cm_klas2.rds")

################################################################################
#### BMI

bmi_cm_4wks <- cm_4wks %>% filter(!is.na(BMI))
anyDuplicated(bmi_cm_4wks$RINPERSOON)
bmi_cm_4wks$RINPERSOON[duplicated(bmi_cm_4wks$RINPERSOON)]
summary(bmi_cm_4wks$BMI)

sum(bmi_cm_4wks$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_4wks$bmi_znl < -5, na.rm = TRUE)
sum(bmi_cm_4wks$BMI > 22, na.rm = TRUE)
sum(bmi_cm_4wks$BMI < 10, na.rm = TRUE)

bmi_cm_4wks <- bmi_cm_4wks %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_4wks <- bmi_cm_4wks %>%               # if there is two observations for one child it takes the observation closest to 4wks
  mutate(dist_4w = abs(28 - leeftijd)) %>%   # 28 days is 4 weken
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_4w) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_4wks <- bmi_cm_4wks %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_4wks <- bmi_cm_4wks %>%     # makes a dummy variable for the top 10th percentile in bmi (seperate by gender)
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == 1 & BMI >= bmi_90perc_4wks$bmi_90[1:1] | 
                           geslacht == 2 & BMI >= bmi_90perc_4wks$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_4wks <- bmi_cm_4wks %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_4wks_g <- bmi_cm_4wks %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_4wks_g, "Descriptives Results/excel-info-maps/output_bmi_cm_4wks_g.csv")

#

bmi_cm_8wks <- cm_8wks %>% filter(!is.na(BMI))
anyDuplicated(bmi_cm_8wks$RINPERSOON)
bmi_cm_8wks$RINPERSOON[duplicated(bmi_cm_8wks$RINPERSOON)]
summary(bmi_cm_8wks$BMI)

sum(bmi_cm_8wks$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_8wks$bmi_znl < -5, na.rm = TRUE)
sum(bmi_cm_8wks$BMI > 25, na.rm = TRUE)
sum(bmi_cm_8wks$BMI < 10, na.rm = TRUE)

bmi_cm_8wks <- bmi_cm_8wks %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_8wks <- bmi_cm_8wks %>% 
  mutate(dist_8w = abs(56 - leeftijd)) %>%   # 56 days is 8 weken
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_8w) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_8wks <- bmi_cm_8wks %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_8wks <- bmi_cm_8wks %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_8wks$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_8wks$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_8wks <- bmi_cm_8wks %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_8wks_g <- bmi_cm_8wks %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_8wks_g, "Descriptives Results/excel-info-maps/output_bmi_cm_8wks_g.csv")



#
bmi_cm_3months <- cm_3months %>% filter(!is.na(BMI))
anyDuplicated(bmi_cm_3months$RINPERSOON)
bmi_cm_3months$RINPERSOON[duplicated(bmi_cm_3months$RINPERSOON)]
summary(bmi_cm_3months$BMI)

sum(bmi_cm_3months$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_3months$bmi_znl < -5, na.rm = TRUE)
sum(bmi_cm_3months$BMI > 25, na.rm = TRUE)
sum(bmi_cm_3months$BMI < 10, na.rm = TRUE)

bmi_cm_3months <- bmi_cm_3months %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_3months <- bmi_cm_3months %>% 
  mutate(dist_3m = abs(90 - leeftijd)) %>%   # 90 days is 3 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_3m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_3m <- bmi_cm_3months %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_3months <- bmi_cm_3months %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_3m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_3m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_3months <- bmi_cm_3months %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_3months_g <- bmi_cm_3months %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_3months_g, "Descriptives Results/excel-info-maps/output_bmi_cm_3months_g.csv")



#
bmi_cm_4months <- cm_4months %>% filter(!is.na(BMI))
bmi_cm_4months$RINPERSOON[duplicated(bmi_cm_4months$RINPERSOON)]
summary(bmi_cm_4months$BMI)

sum(bmi_cm_4months$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_4months$bmi_znl < -5, na.rm = TRUE)
sum(bmi_cm_4months$BMI > 25, na.rm = TRUE)
sum(bmi_cm_4months$BMI < 10, na.rm = TRUE)

bmi_cm_4months <- bmi_cm_4months %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_4months <- bmi_cm_4months %>% 
  mutate(dist_4m = abs(120 - leeftijd)) %>%   # 120 days is 4 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_4m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_4m <- bmi_cm_4months %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_4months <- bmi_cm_4months %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_4m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_4m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_4months <- bmi_cm_4months %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_4months_g <- bmi_cm_4months %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_4months_g, "Descriptives Results/excel-info-maps/output_bmi_cm_4months_g.csv")


#
bmi_cm_6months <- cm_6months %>% filter(!is.na(BMI))
bmi_cm_6months$RINPERSOON[duplicated(bmi_cm_6months$RINPERSOON)]
summary(bmi_cm_6months$BMI)

sum(bmi_cm_6months$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_6months$bmi_znl < -5, na.rm = TRUE)
sum(bmi_cm_6months$BMI > 25, na.rm = TRUE)
sum(bmi_cm_6months$BMI < 10, na.rm = TRUE)

bmi_cm_6months <- bmi_cm_6months %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_6months <- bmi_cm_6months %>% 
  mutate(dist_6m = abs(181 - leeftijd)) %>%   # 181 days is 6 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_6m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_6m <- bmi_cm_6months %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_6months <- bmi_cm_6months %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_6m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_6m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_6months <- bmi_cm_6months %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_6months_g <- bmi_cm_6months %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_6months_g, "Descriptives Results/excel-info-maps/output_bmi_cm_6months_g.csv")



#
bmi_cm_9months <- cm_9months %>% filter(!is.na(BMI))
bmi_cm_9months$RINPERSOON[duplicated(bmi_cm_9months$RINPERSOON)]
summary(bmi_cm_9months$BMI)

sum(bmi_cm_9months$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_9months$bmi_znl < -5, na.rm = TRUE)
sum(bmi_cm_9months$BMI > 25, na.rm = TRUE)
sum(bmi_cm_9months$BMI < 10, na.rm = TRUE)

bmi_cm_9months <- bmi_cm_9months %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_9months <- bmi_cm_9months %>% 
  mutate(dist_9m = abs(273 - leeftijd)) %>%   # 273 days is 9 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_9m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_9m <- bmi_cm_9months %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_9months <- bmi_cm_9months %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_9m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_9m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_9months <- bmi_cm_9months %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_9months_g <- bmi_cm_9months %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_9months_g, "Descriptives Results/excel-info-maps/output_bmi_cm_9months_g.csv")


#
bmi_cm_11months <- cm_11months %>% filter(!is.na(BMI))
bmi_cm_11months$RINPERSOON[duplicated(bmi_cm_11months$RINPERSOON)]
summary(bmi_cm_11months$BMI)

sum(bmi_cm_11months$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_11months$bmi_znl < -5, na.rm = TRUE)

bmi_cm_11months <- bmi_cm_11months %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_11months <- bmi_cm_11months %>% 
  mutate(dist_11m = abs(334 - leeftijd)) %>%   # 334 days is 11 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_11m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_11m <- bmi_cm_11months %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_11months <- bmi_cm_11months %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_11m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_11m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_11months <- bmi_cm_11months %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_11months_g <- bmi_cm_11months %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_11months_g, "Descriptives Results/excel-info-maps/output_bmi_cm_11months_g.csv")



#
bmi_cm_14months <- cm_14months %>% filter(!is.na(BMI))
bmi_cm_14months$RINPERSOON[duplicated(bmi_cm_14months$RINPERSOON)]
summary(bmi_cm_14months$BMI)

sum(bmi_cm_14months$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_14months$bmi_znl < -5, na.rm = TRUE)

bmi_cm_14months <- bmi_cm_14months %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_14months <- bmi_cm_14months %>% 
  mutate(dist_14m = abs(273 - leeftijd)) %>%   # 273 days is 14 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_14m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_14m <- bmi_cm_14months %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_14months <- bmi_cm_14months %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_14m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_14m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_14months <- bmi_cm_14months %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_14months_g <- bmi_cm_14months %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_14months_g, "Descriptives Results/excel-info-maps/output_bmi_cm_14months_g.csv")

#
bmi_cm_18months <- cm_18months %>% filter(!is.na(BMI))
bmi_cm_18months$RINPERSOON[duplicated(bmi_cm_18months$RINPERSOON)]
summary(bmi_cm_18months$BMI)

sum(bmi_cm_18months$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_18months$bmi_znl < -5, na.rm = TRUE)
sum(bmi_cm_18months$BMI > 25, na.rm = TRUE)
sum(bmi_cm_18months$BMI < 10, na.rm = TRUE)

bmi_cm_18months <- bmi_cm_18months %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_18months <- bmi_cm_18months %>% 
  mutate(dist_18m = abs(547 - leeftijd)) %>%   # 547 days is 18 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_18m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_18m <- bmi_cm_18months %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_18months <- bmi_cm_18months %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_18m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_18m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_18months <- bmi_cm_18months %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_18months_g <- bmi_cm_18months %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_18months_g, "Descriptives Results/excel-info-maps/output_bmi_cm_18months_g.csv")



#
bmi_cm_2years <- cm_2years %>% filter(!is.na(BMI))
bmi_cm_2years$RINPERSOON[duplicated(bmi_cm_2years$RINPERSOON)]
summary(bmi_cm_2years$BMI)

sum(bmi_cm_2years$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_2years$bmi_znl < -5, na.rm = TRUE)

bmi_cm_2years <- bmi_cm_2years %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_2years <- bmi_cm_2years %>% 
  mutate(dist_2y = abs(730 - leeftijd)) %>%   # 730 days is 2 years
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_2y) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_2y <- bmi_cm_2years %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_2years <- bmi_cm_2years %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_2y$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_2y$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_2years <- bmi_cm_2years %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_2years_g <- bmi_cm_2years %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_2years_g, "Descriptives Results/excel-info-maps/output_bmi_cm_2years_g.csv")

#
bmi_cm_3years <- cm_3years %>% filter(!is.na(BMI))
bmi_cm_3years$RINPERSOON[duplicated(bmi_cm_3years$RINPERSOON)]
summary(bmi_cm_3years$BMI)

sum(bmi_cm_3years$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_3years$bmi_znl < -5, na.rm = TRUE)

bmi_cm_3years <- bmi_cm_3years %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_3years <- bmi_cm_3years %>% 
  mutate(dist_3y = abs(1095 - leeftijd)) %>%   # 1095 days is 3 years
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_3y) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_3y <- bmi_cm_3years %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_3years <- bmi_cm_3years %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_3y$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_3y$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_3years <- bmi_cm_3years %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_3years_g <- bmi_cm_3years %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_3years_g, "Descriptives Results/excel-info-maps/output_bmi_cm_3years_g.csv")





#
bmi_cm_3years9m <- cm_3years9m %>% filter(!is.na(BMI))
anyDuplicated(bmi_cm_3years9m$RINPERSOON)
bmi_cm_3years9m$RINPERSOON[duplicated(bmi_cm_3years9m$RINPERSOON)]

sum(bmi_cm_3years9m$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_3years9m$bmi_znl < -5, na.rm = TRUE)

bmi_cm_3years9m <- bmi_cm_3years9m %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_3years9m <- bmi_cm_3years9m %>% 
  mutate(dist_3y9m = abs(1368 - leeftijd)) %>%   # 1368 days is 3 years and 9 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_3y9m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_3years9m <- bmi_cm_3years9m %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_3years9m <- bmi_cm_3years9m %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_3years9m$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_3years9m$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_3years9m <- bmi_cm_3years9m %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_3years9m_g <- bmi_cm_3years9m %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_3years9m_g, "Descriptives Results/excel-info-maps/output_bmi_cm_3years9m_g.csv")






#
bmi_cm_groep2 <- cm_groep2 %>% filter(!is.na(BMI))
anyDuplicated(bmi_cm_groep2$RINPERSOON)
bmi_cm_groep2$RINPERSOON[duplicated(bmi_cm_groep2$RINPERSOON)]

sum(bmi_cm_groep2$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_groep2$bmi_znl < -5, na.rm = TRUE)

bmi_cm_groep2 <- bmi_cm_groep2 %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_groep2 <- bmi_cm_groep2 %>% 
  mutate(dist_gr2 = abs(2190 - leeftijd)) %>%   # 2190 days is 6 years
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_gr2) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_groep2 <- bmi_cm_groep2 %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_groep2 <- bmi_cm_groep2 %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_groep2$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_groep2$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_groep2 <- bmi_cm_groep2 %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_groep2_g <- bmi_cm_groep2 %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_groep2_g, "Descriptives Results/excel-info-maps/output_bmi_cm_groep2_g.csv")





#
bmi_cm_groep7 <- cm_groep7 %>% filter(!is.na(BMI))
anyDuplicated(bmi_cm_groep7$RINPERSOON)
bmi_cm_groep7$RINPERSOON[duplicated(bmi_cm_groep7$RINPERSOON)]

sum(bmi_cm_groep7$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_groep7$bmi_znl < -5, na.rm = TRUE)

bmi_cm_groep7 <- bmi_cm_groep7 %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_groep7 <- bmi_cm_groep7 %>% 
  mutate(dist_gr7 = abs(3819 - leeftijd)) %>%   # 3819 days is the mean age 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_gr7) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_groep7 <- bmi_cm_groep7 %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_groep7 <- bmi_cm_groep7 %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_groep7$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_groep7$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_groep7 <- bmi_cm_groep7 %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_groep7_g <- bmi_cm_groep7 %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_groep7, "Descriptives Results/excel-info-maps/output_bmi_cm_groep7_g.csv")



#
bmi_cm_klas2 <- cm_klas2 %>% filter(!is.na(BMI))
anyDuplicated(bmi_cm_klas2$RINPERSOON)
bmi_cm_klas2$RINPERSOON[duplicated(bmi_cm_klas2$RINPERSOON)]

sum(bmi_cm_klas2$bmi_znl > 5, na.rm = TRUE)
sum(bmi_cm_klas2$bmi_znl < -5, na.rm = TRUE)

bmi_cm_klas2 <- bmi_cm_klas2 %>% 
  filter(bmi_znl >= -5 & bmi_znl <= 5 | is.na(bmi_znl))

bmi_cm_klas2 <- bmi_cm_klas2 %>% 
  mutate(dist_k2 = abs(5140 - leeftijd)) %>%   # 1368 days is 3 years and 9 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(dist_k2) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd)                    #doing both steps takes longer

bmi_90perc_klas2 <- bmi_cm_klas2 %>% 
  filter(!geslacht == "onbekend") %>%
  group_by(geslacht) %>% 
  summarise(bmi_90 = quantile(BMI, probs = c(.9)))

bmi_cm_klas2 <- bmi_cm_klas2 %>% 
  filter(!geslacht == "onbekend") %>% 
  mutate(bmi_90 = ifelse(geslacht == "mannelijk" & BMI >= bmi_90perc_klas2$bmi_90[1:1] | 
                           geslacht == "vrouwelijk" & BMI >= bmi_90perc_klas2$bmi_90[2:2], 1, 0),
         bmi_90 = as_factor(bmi_90))


output_bmi_cm_klas2 <- bmi_cm_klas2 %>% group_by(postcode3) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

output_bmi_cm_klas2_g <- bmi_cm_klas2 %>% group_by(postcode3, geslacht) %>% 
  summarise(bmi_mean = round(mean(BMI, na.rm = T), 2), bmi_sd = round(sd(BMI, na.rm = T), 2), n = n(), bmi_90p = sum(bmi_90 == 1)) %>% 
  mutate(bmi_mean = ifelse(n < 10, NA, bmi_mean),
         bmi_sd = ifelse(n < 10, NA, bmi_sd),
         bmi_90p = ifelse(n < 10, NA, bmi_90p),
         bmi_90p_perc = round(bmi_90p / n * 100, 1))

write_excel_csv(output_bmi_cm_klas2, "Descriptives Results/excel-info-maps/output_bmi_cm_klas2_g.csv")


##### WIECHEN
library(magrittr)
library(data.table)

wiechen_cm <- jgz_output %>% filter(!is.na(zin2w) | !is.na(pop6) | !is.na(noemt) | !is.na(wijstaan) | !is.na(zin3w) | !is.na(verstaan1)
                                    | !is.na(spontaan) | !is.na(vragen1) | !is.na(verstaan2) | !is.na(vragen2)) %>% 
  select(-c(lengte, gewicht, BMI, bmi_znl, sdq_t, sdq_11, sdq_14)) 

wiechen_cm_2years <- wiechen_cm %>% filter((!is.na(zin2w) | !is.na(pop6)) & (cm_2years == 1 | cm_18months == 1)) 

wiechen_cm_2years_41 <- wiechen_cm_2years %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  filter(!is.na(zin2w)) %>% 
  summarise(zin2w = last(zin2w)) %>% #
  ungroup()

wiechen_cm_2years_42 <- wiechen_cm_2years %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(!is.na(pop6)) %>% 
  summarise(pop6 = last(pop6)) %>% 
  ungroup()

wiechen_cm_2years <- full_join(wiechen_cm_2years_41, wiechen_cm_2years_42)

wiechen_cm_2years <- wiechen_cm_2years %>% 
  mutate(taal_ont = ifelse(zin2w == "-" | pop6 == "-", 1, 0),
         taal_ont = as_factor(taal_ont))

round(sum(is.na(wiechen_cm_2years$taal_ont))/nrow(wiechen_cm_2years)*100, 2)
table(wiechen_cm_2years$taal_ont)
round(prop.table(table(wiechen_cm_2years$taal_ont))*100, 2)

wiechen_cm_names <- wiechen_cm %>% select(c(RINPERSOONS, RINPERSOON, postcode, postcode3, geslacht)) %>% 
  unique()

wiechen_cm_2years <- wiechen_cm_2years %>% left_join(wiechen_cm_names, by = join_by(RINPERSOONS, RINPERSOON)) 

output_wiechen_cm_2years3 <- wiechen_cm_2years %>% group_by(postcode3) %>% 
  summarise(taal_ont_achts = sum(taal_ont == 1, na.rm = T), n = n(), zin2w_vol = sum(zin2w == "+", na.rm = T),
            zin2w_M = sum(zin2w == "M", na.rm = T), zin2w_onvol = sum(zin2w == "-", na.rm = T), 
            pop6_vol = sum(pop6 == "+", na.rm = T), pop6_M = sum(pop6 == "M", na.rm = T), 
            pop6_onvol = sum(pop6 == "-", na.rm = T)) %>% 
  mutate(taal_ont_achts_perc = round(taal_ont_achts / n * 100, 1),
         zin2w_vol_perc = round(zin2w_vol / n * 100, 1),
         zin2w_M_perc = round(zin2w_M / n * 100, 1),
         zin2w_onvol_perc = round(zin2w_onvol / n * 100, 1),
         pop6_vol_perc = round(pop6_vol / n * 100, 1),
         pop6_M_perc = round(pop6_M / n * 100, 1),
         pop6_onvol_perc = round(pop6_onvol / n * 100, 1)) %>% 
  mutate(across(c(starts_with("taal_ont"), starts_with("zin2w"), starts_with("pop6")), ~ifelse(n < 10, NA, .))) %>% 
  select(-c(zin2w_vol, zin2w_M, zin2w_onvol, pop6_vol, pop6_M, pop6_onvol)) %>% 
  mutate(across(c(taal_ont_achts, taal_ont_achts_perc), ~ifelse(((taal_ont_achts_perc/100)*n < 10 | (1-(taal_ont_achts_perc/100))*n < 10), NA, .))) %>% 
  mutate(across(c(zin2w_vol_perc), ~ifelse(((zin2w_vol_perc/100)*n < 10 | (1-(zin2w_vol_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(zin2w_M_perc), ~ifelse(((zin2w_M_perc/100)*n < 10 | (1-(zin2w_M_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(zin2w_onvol_perc), ~ifelse(((zin2w_onvol_perc/100)*n < 10 | (1-(zin2w_onvol_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(pop6_vol_perc), ~ifelse(((pop6_vol_perc/100)*n < 10 | (1-(pop6_vol_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(pop6_M_perc), ~ifelse(((pop6_M_perc/100)*n < 10 | (1-(pop6_M_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(pop6_onvol_perc), ~ifelse(((pop6_onvol_perc/100)*n < 10 | (1-(pop6_onvol_perc/100))*n < 10), NA, .)))


sum(is.na(output_wiechen_cm_2years3$zin2w_vol_perc))

write_excel_csv2(output_wiechen_cm_2years3, "Descriptives Results/excel-info-maps/output_wiechen_cm_2years3.csv")


# 3 years 9 months
wiechen_cm_3years9m <- wiechen_cm %>% filter((!is.na(vragen1) | !is.na(verstaan2) | !is.na(vragen2)) 
                                             & (cm_3years9m == 1 | cm_3years == 1)) 

wiechen_cm_3years9m_48 <- wiechen_cm_3years9m %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  filter(!is.na(vragen1)) %>% 
  summarise(vragen1 = last(vragen1)) %>% #
  ungroup()

wiechen_cm_3years9m_49 <- wiechen_cm_3years9m %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(!is.na(verstaan2)) %>% 
  summarise(verstaan2 = last(verstaan2)) %>% 
  ungroup()

wiechen_cm_3years9m_50 <- wiechen_cm_3years9m %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(!is.na(vragen2)) %>% 
  summarise(vragen2 = last(vragen2)) %>% 
  ungroup()

wiechen_cm_names <- wiechen_cm %>% select(c(RINPERSOONS, RINPERSOON, postcode, postcode3, geslacht)) %>% 
  unique()

wiechen_cm_3years9m <- wiechen_cm_3years9m_48 %>% 
  full_join(wiechen_cm_3years9m_49, by = join_by(RINPERSOONS, RINPERSOON)) %>% 
  full_join(wiechen_cm_3years9m_50, by = join_by(RINPERSOONS, RINPERSOON))

sum(is.na(wiechen_cm_3years9m$vragen1))
sum(is.na(wiechen_cm_3years9m$verstaan2))
sum(is.na(wiechen_cm_3years9m$vragen2))

wiechen_cm_3years9m <- wiechen_cm_3years9m %>% 
  mutate(taal_ont = ifelse(vragen1 == "-" | verstaan2 == "-" | vragen2 == "-", 1, 0),
         taal_ont = as_factor(taal_ont))

round(sum(is.na(wiechen_cm_3years9m$taal_ont))/nrow(wiechen_cm_3years9m)*100, 2)
table(wiechen_cm_3years9m$taal_ont)
round(prop.table(table(wiechen_cm_3years9m$taal_ont))*100, 2)

wiechen_cm_3years9m <- wiechen_cm_3years9m %>% left_join(wiechen_cm_names, by = join_by(RINPERSOONS, RINPERSOON)) 

output_wiechen_cm_3years9m3 <- wiechen_cm_3years9m %>% group_by(postcode3) %>% 
  summarise(taal_ont_achts = sum(taal_ont == 1, na.rm = T), n = n(), vragen1_vol = sum(vragen1 == "+", na.rm = T),
            vragen1_M = sum(vragen1 == "M", na.rm = T), vragen1_onvol = sum(vragen1 == "-", na.rm = T), 
            verstaan2_vol = sum(verstaan2 == "+", na.rm = T), verstaan2_M = sum(verstaan2 == "M", na.rm = T), 
            verstaan2_onvol = sum(verstaan2 == "-", na.rm = T), vragen2_vol = sum(vragen2 == "+", na.rm = T),
            vragen2_M = sum(vragen2 == "M", na.rm = T), vragen2_onvol = sum(vragen2 == "-", na.rm = T),) %>% 
  mutate(taal_ont_achts_perc = round(taal_ont_achts / n * 100, 1),
         vragen1_vol_perc = round(vragen1_vol / n * 100, 1),
         vragen1_M_perc = round(vragen1_M / n * 100, 1),
         vragen1_onvol_perc = round(vragen1_onvol / n * 100, 1),
         verstaan2_vol_perc = round(verstaan2_vol / n * 100, 1),
         verstaan2_M_perc = round(verstaan2_M / n * 100, 1),
         verstaan2_onvol_perc = round(verstaan2_onvol / n * 100, 1),
         vragen2_vol_perc = round(vragen2_vol / n * 100, 1),
         vragen2_M_perc = round(vragen2_M / n * 100, 1),
         vragen2_onvol_perc = round(vragen2_onvol / n * 100, 1)) %>% 
  mutate(across(c(starts_with("taal_ont"), starts_with("vragen1"), starts_with("verstaan2"), starts_with("vragen2")), ~ifelse(n < 10, NA, .))) %>% 
  select(-c(vragen1_vol, vragen1_M, vragen1_onvol, verstaan2_vol, verstaan2_M, verstaan2_onvol, 
            vragen2_vol, vragen2_M, vragen2_onvol)) %>% 
  mutate(across(c(taal_ont_achts, taal_ont_achts_perc), ~ifelse(((taal_ont_achts_perc/100)*n < 10 | (1-(taal_ont_achts_perc/100))*n < 10), NA, .))) %>% 
  mutate(across(c(vragen1_vol_perc), ~ifelse(((vragen1_vol_perc/100)*n < 10 | (1-(vragen1_vol_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(vragen1_M_perc), ~ifelse(((vragen1_M_perc/100)*n < 10 | (1-(vragen1_M_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(vragen1_onvol_perc), ~ifelse(((vragen1_onvol_perc/100)*n < 10 | (1-(vragen1_onvol_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(verstaan2_vol_perc), ~ifelse(((verstaan2_vol_perc/100)*n < 10 | (1-(verstaan2_vol_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(verstaan2_M_perc), ~ifelse(((verstaan2_M_perc/100)*n < 10 | (1-(verstaan2_M_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(verstaan2_onvol_perc), ~ifelse(((verstaan2_onvol_perc/100)*n < 10 | (1-(verstaan2_onvol_perc/100))*n < 10), NA, .))) %>% 
  mutate(across(c(vragen2_vol_perc), ~ifelse(((vragen2_vol_perc/100)*n < 10 | (1-(vragen2_vol_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(vragen2_M_perc), ~ifelse(((vragen2_M_perc/100)*n < 10 | (1-(vragen2_M_perc/100))*n < 10), NA, .))) %>%
  mutate(across(c(vragen2_onvol_perc), ~ifelse(((vragen2_onvol_perc/100)*n < 10 | (1-(vragen2_onvol_perc/100))*n < 10), NA, .)))


#output_wiechen_cm_3years9m2 <- output_wiechen_cm_3years9m %>% 
# mutate(across(c(starts_with("vragen1"), starts_with("verstaan2"), starts_with("vragen2")), ~ifelse(. >= 90, NA, .)))

write_excel_csv2(output_wiechen_cm_3years9m3, "Descriptives Results/excel-info-maps/output_wiechen_cm_3years9m3.csv")

table(wiechen_cm_3years9m$taal_ont)
round(prop.table(table(wiechen_cm_3years9m$taal_ont))*100, 2)

table(wiechen_cm_2years$taal_ont, wiechen_cm_2years$geslacht)

table(wiechen_cm_2years$zin2w)
round(prop.table(table(wiechen_cm_2years$zin2w))*100, 2)
table(wiechen_cm_2years$pop6)
round(prop.table(table(wiechen_cm_2years$pop6))*100, 2)

test_u <- wiechen_cm_3years9m$RINPERSOON[duplicated(wiechen_cm_3years9m$RINPERSOON)]
test <- wiechen_cm_3years9m %>% filter(RINPERSOON %in% test_u)

#### SDQ 

sdq_cm_groep2 <- cm_groep2 %>% filter(!is.na(sdq_t))
sdq_cm_groep7 <- cm_groep7 %>% filter(!is.na(sdq_t))
sdq_cm_klas2 <- cm_klas2 %>% filter(!is.na(sdq_t))
sdq_cm_3years9m <- cm_3years9m %>% filter(!is.na(sdq_t))

output_sdq_cm_groep2 <- sdq_cm_groep2 %>% group_by(postcode) %>% 
  summarise(sdq_mean = mean(sdq_t, na.rm = T), sdq_sd = sd(sdq_t,na.rm = T), n = n(), 
            sdq_11_n = sum(sdq_11 == 1, na.rm = T), sdq_14_n = sum(sdq_14 == 1, na.rm = T)) %>% 
  mutate(sdq_mean = ifelse(n < 10, NA, sdq_mean),
         sdq_sd = ifelse(n < 10, NA, sdq_sd),
         sdq_11_n = ifelse(n < 10, NA, sdq_11_n),
         sdq_14_n = ifelse(n < 10, NA, sdq_14_n),
         sdq_11_perc = round(sdq_11_n / n * 100, 1),
         sdq_14_perc = round(sdq_14_n / n * 100, 1)) %>% 
  mutate(across(c(sdq_11_n, sdq_11_perc), ~ifelse(((sdq_11_perc/100)*n < 10 | (1-(sdq_11_perc/100))*n < 10), NA, .))) %>% 
  mutate(across(c(sdq_14_n, sdq_14_perc), ~ifelse(((sdq_14_perc/100)*n < 10 | (1-(sdq_14_perc/100))*n < 10), NA, .)))

output_sdq_cm_groep73 <- sdq_cm_groep7 %>% group_by(postcode3) %>% 
  summarise(sdq_mean = mean(sdq_t, na.rm = T), sdq_sd = sd(sdq_t,na.rm = T), n = n(), 
            sdq_11_n = sum(sdq_11 == 1, na.rm = T), sdq_14_n = sum(sdq_14 == 1, na.rm = T)) %>% 
  mutate(sdq_mean = ifelse(n < 10, NA, sdq_mean),
         sdq_sd = ifelse(n < 10, NA, sdq_sd),
         sdq_11_n = ifelse(n < 10, NA, sdq_11_n),
         sdq_14_n = ifelse(n < 10, NA, sdq_14_n),
         sdq_11_perc = round(sdq_11_n / n * 100, 1),
         sdq_14_perc = round(sdq_14_n / n * 100, 1)) %>% 
  mutate(across(c(sdq_11_n, sdq_11_perc), ~ifelse(((sdq_11_perc/100)*n < 10 | (1-(sdq_11_perc/100))*n < 10), NA, .))) %>% 
  mutate(across(c(sdq_14_n, sdq_14_perc), ~ifelse(((sdq_14_perc/100)*n < 10 | (1-(sdq_14_perc/100))*n < 10), NA, .)))

output_sdq_cm_klas23 <- sdq_cm_klas2 %>% group_by(postcode3) %>% 
  summarise(sdq_mean = mean(sdq_t, na.rm = T), sdq_sd = sd(sdq_t,na.rm = T), n = n(), 
            sdq_11_n = sum(sdq_11 == 1, na.rm = T), sdq_14_n = sum(sdq_14 == 1, na.rm = T)) %>% 
  mutate(sdq_mean = ifelse(n < 10, NA, sdq_mean),
         sdq_sd = ifelse(n < 10, NA, sdq_sd),
         sdq_11_n = ifelse(n < 10, NA, sdq_11_n),
         sdq_14_n = ifelse(n < 10, NA, sdq_14_n),
         sdq_11_perc = round(sdq_11_n / n * 100, 1),
         sdq_14_perc = round(sdq_14_n / n * 100, 1)) %>% 
  mutate(across(c(sdq_11_n, sdq_11_perc), ~ifelse(((sdq_11_perc/100)*n < 10 | (1-(sdq_11_perc/100))*n < 10), NA, .))) %>% 
  mutate(across(c(sdq_14_n, sdq_14_perc), ~ifelse(((sdq_14_perc/100)*n < 10 | (1-(sdq_14_perc/100))*n < 10), NA, .)))

output_sdq_cm_3years9m3 <- sdq_cm_3years9m %>% group_by(postcode3) %>% 
  summarise(sdq_mean = mean(sdq_t, na.rm = T), sdq_sd = sd(sdq_t,na.rm = T), n = n(), 
            sdq_11_n = sum(sdq_11 == 1, na.rm = T), sdq_14_n = sum(sdq_14 == 1, na.rm = T)) %>% 
  mutate(sdq_mean = ifelse(n < 10, NA, sdq_mean),
         sdq_sd = ifelse(n < 10, NA, sdq_sd),
         sdq_11_n = ifelse(n < 10, NA, sdq_11_n),
         sdq_14_n = ifelse(n < 10, NA, sdq_14_n),
         sdq_11_perc = round(sdq_11_n / n * 100, 1),
         sdq_14_perc = round(sdq_14_n / n * 100, 1)) %>% 
  mutate(across(c(sdq_11_n, sdq_11_perc), ~ifelse(((sdq_11_perc/100)*n < 10 | (1-(sdq_11_perc/100))*n < 10), NA, .))) %>% 
  mutate(across(c(sdq_14_n, sdq_14_perc), ~ifelse(((sdq_14_perc/100)*n < 10 | (1-(sdq_14_perc/100))*n < 10), NA, .)))



sdq_cm_groep2 %>% 
  group_by(geslacht) %>% 
  summarise(sdq_mean = mean(sdq_t, na.rm = T))

table(sdq_cm_groep2$sdq_11, sdq_cm_groep2$geslacht)
table(sdq_cm_groep2$sdq_14, sdq_cm_groep2$geslacht)

write_excel_csv2(output_sdq_cm_3years9m3, "Descriptives Results/excel-info-maps/output_sdq_cm_3years9m3.csv")


