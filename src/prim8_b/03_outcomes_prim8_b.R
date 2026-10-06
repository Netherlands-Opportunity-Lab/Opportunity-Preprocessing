# Kansenkaart data preparation pipeline
#
# 3. Outcome creation.
#   - Adding primary school outcomes to the cohort.
#
# (c) ODISSEI Social Data Science team 2025



#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)


#### CONFIGURATION ####
# load main cohort dataset
cohort_dat <- read_rds(file.path(loc$scratch_folder, "02_predictors.rds"))

sample_size <- read_rds(file.path(loc$scratch_folder, "02_sample_size.rds"))

# load class cohort data
class_cohort_dat <- read_rds(file.path(loc$scratch_folder, "class_cohort.rds"))

# combine the main sample and class sample
class_cohort_dat <- bind_rows(
  class_cohort_dat %>% select ("RINPERSOON", "RINPERSOONS", "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "income_parents_perc"),
  cohort_dat %>% select ("RINPERSOON", "RINPERSOONS", "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "income_parents_perc")
)

#### CLASSROOM COMPOSITION AND OTHER PRIMARY SCHOOL OUTCOMES ####

# function to get latest inschrwpo version of specified year
get_inschrwpo_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "Onderwijs/INSCHRWPOTAB"),
    pattern = paste0("INSCHRWPOTAB", year, "V[0-9]+(?i)(.sav)"), 
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


school_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(), WPOLEERJAAR = character(), 
                     WPOBRIN_crypt = character(), WPOBRINVEST = character(), WPOGROEPSGROOTTE = character(),
                     WPOREKENEN = character(), WPOTAALLV = character(), WPOTAALTV = character(),
                     WPOTOETSADVIES = character(), WPOADVIESVO = character(), 
                     WPOADVIESHERZ = character(), WPOTYPEPO = character())

for (year in seq(as.integer(cfg$primary_school_year_min), as.integer(cfg$primary_school_year_max))) {
  school_dat <- 
    # read file from disk
    read_sav(get_inschrwpo_filename(year), 
             col_select = c("RINPERSOONS", "RINPERSOON", "WPOLEERJAAR", "WPOGROEPSGROOTTE",
                            "WPOTOETSADVIES","WPOADVIESVO", "WPOADVIESHERZ", 
                            "WPOREKENEN", "WPOTAALLV", "WPOTAALTV",
                            "WPOBRIN_crypt", "WPOBRINVEST", "WPOTYPEPO")) %>% 
    mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value")) %>%
    # select only children that are in the cohort
    filter(RINPERSOON %in% cohort_dat$RINPERSOON) %>% 
    # add year
    mutate(year = year) %>% 
    # add to income children
    bind_rows(school_dat, .)
}
rm(year)

# keep final time took the test
school_dat <- school_dat %>%
  # when duplicated pick the last year
  arrange(desc(year)) %>%
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(row_number() == 1)


# add to class data
class_cohort_dat <- class_cohort_dat %>%
  left_join(school_dat, by = c("RINPERSOONS", "RINPERSOON")) %>%
  ungroup()

#rm(school_dat)


# convert NA
class_cohort_dat <- 
  class_cohort_dat %>%
  mutate(across(c("WPOREKENEN", "WPOTAALLV", "WPOTAALTV", "WPOGROEPSGROOTTE",
                  "WPOTOETSADVIES", "WPOADVIESVO", "WPOADVIESHERZ"),
                as.character)) %>%
  mutate(across(c("WPOREKENEN", "WPOTAALLV", "WPOTAALTV", "WPOGROEPSGROOTTE",
                  "WPOTOETSADVIES", "WPOADVIESVO", "WPOADVIESHERZ"),
                as.numeric))


# create outcome variables
class_cohort_dat <- 
  class_cohort_dat %>%
  mutate(
    # rekenen, lezen & taalverzorging
    math     = ifelse((WPOREKENEN == 3 | WPOREKENEN == 4), 1, 0),
    reading  = ifelse(WPOTAALLV == 4, 1, 0),
    language = ifelse(WPOTAALTV == 4, 1, 0),
    
    # cito test outcomes
    vmbo_gl_test = ifelse(WPOTOETSADVIES %in% c(42, 44, 60, 61, 70), 1, 0),
    havo_test      = ifelse(WPOTOETSADVIES %in% c(60, 61, 70), 1, 0),
    vwo_test       = ifelse(WPOTOETSADVIES == 70, 1, 0),
    
    # final high school advice 
    # replace 80 (= Geen specifiek advies mogelijk) with NA
    WPOADVIESVO = ifelse(WPOADVIESVO == 80, NA, WPOADVIESVO),
    
    # some of the advice have 0 (no final school advice). I convert them to NA for now. 
    WPOADVIESHERZ = ifelse(WPOADVIESHERZ == 0, NA, WPOADVIESHERZ),
    WPOADVIESVO = ifelse(WPOADVIESVO == 0, NA, WPOADVIESVO),
    
    # replace final school advice (wpoadviesvo) with wpoadviesherz if wpoadviesherz is not missing
    final_school_advice = ifelse(!is.na(WPOADVIESHERZ), WPOADVIESHERZ, WPOADVIESVO),
    
    vmbo_gl_final = ifelse(final_school_advice %in% c(40, 41, 42, 43, 44, 45, 50, 
                                                      51, 52, 53, 60, 61, 70), 1, 0),
    havo_final      = ifelse(final_school_advice %in% c(60, 61, 70), 1, 0),
    vwo_final       = ifelse(final_school_advice == 70, 1, 0), 
    advice_revised  = ifelse(!is.na(WPOADVIESHERZ), 1, 0), 
    # for school year 2023-2024 if WPOADVIESHERZ and WPOADVIESVO are not the same then equal to 1
    advice_revised  = ifelse(year == 2023 & WPOADVIESHERZ == WPOADVIESVO, 0, advice_revised) 
  ) 


# replace NA
class_cohort_dat <- class_cohort_dat %>%
  mutate(
    math     = ifelse(is.na(WPOREKENEN), NA, math),
    reading  = ifelse(is.na(WPOTAALLV), NA, reading),
    language = ifelse(is.na(WPOTAALTV), NA, language),
    
    # cito test outcomes
    vmbo_gl_test = ifelse(is.na(WPOTOETSADVIES), NA, vmbo_gl_test),
    havo_test      = ifelse(is.na(WPOTOETSADVIES), NA, havo_test),
    vwo_test       = ifelse(is.na(WPOTOETSADVIES), NA, vwo_test),
    
    # final school advice outcomes
    vmbo_gl_final = ifelse(is.na(final_school_advice), NA, vmbo_gl_final),
    havo_final    = ifelse(is.na(final_school_advice), NA, havo_final),
    vwo_final     = ifelse(is.na(final_school_advice), NA, vwo_final)
  )

# drop those without a test score 
class_cohort_dat <- class_cohort_dat %>%
  filter(!is.na(WPOTOETSADVIES))

# primary school ID
class_cohort_dat <- class_cohort_dat %>%
  mutate(across(c("WPOBRIN_crypt", "WPOBRINVEST"), as.character)) %>%
  mutate(school_ID = paste0(WPOBRIN_crypt, WPOBRINVEST)) %>%
  select(-c(WPOBRIN_crypt, WPOBRINVEST))


# create parents rank income outcomes
class_cohort_dat <- class_cohort_dat %>%
  mutate(
    # create dummy for below 25th
    income_below_25th = ifelse(income_parents_perc < 0.25, 1, 0),
    # create dummy for below 50th
    income_below_50th = ifelse(income_parents_perc < 0.50, 1, 0),
    # create dummy for above 75th
    income_above_75th = ifelse(income_parents_perc > 0.75, 1, 0)
  )


# create outcome for children with both parents born in a foreign country
class_cohort_dat <- class_cohort_dat %>%
  mutate(
    GBAGEBOORTELANDMOEDER = as_factor(GBAGEBOORTELANDMOEDER),
    GBAGEBOORTELANDVADER = as_factor(GBAGEBOORTELANDVADER)
  ) %>%
  mutate(
    foreign_born_parents =
      ifelse((GBAGEBOORTELANDMOEDER != "Nederland" &
                GBAGEBOORTELANDVADER != "Nederland"),  1, 0))

# CLASSROOM OUTCOMES
# 1.  class_vmbo_gl_test
# 2.  class_havo_test
# 3.  class_vwo_test
# 4.  class_foreign_born_parents
# 5.  class_income_below_25th
# 6.  class_income_below_50th
# 7.  class_income_above_75th
# 8.  class_math
# 9.  class_language
# 10. class_reading
# 11.  class_size (2014 - 2016)


# # only keep observations with more than one student in their class
# class_cohort_dat <- class_cohort_dat %>%
#   group_by(school_ID, year) %>%
#   mutate(n = n()) %>%
#   filter(n > 1)

# hold out mean function
hold_out_means <- function(x) {
  hold <- ((sum(x, na.rm = TRUE) - x) / (length(x) - 1))
  return(hold)
}

# hold out means = mean of the class without the child themselves
class_cohort_dat <-
  class_cohort_dat %>%
  group_by(school_ID, year) %>%
  mutate(
    N_students_per_school = n(),
    
    class_math = hold_out_means(math),
    class_reading = hold_out_means(reading),
    class_language = hold_out_means(language),
    
    class_foreign_born_parents = hold_out_means(foreign_born_parents),
    
    class_vmbo_gl_test = hold_out_means(vmbo_gl_test),
    class_havo_test = hold_out_means(havo_test),
    class_vwo_test = hold_out_means(vwo_test),
    
    class_income_below_25th = hold_out_means(income_below_25th),
    class_income_below_50th = hold_out_means(income_below_50th),
    class_income_above_75th = hold_out_means(income_above_75th)
  ) %>%
  rename(class_size = WPOGROEPSGROOTTE)

# select relevant variables
class_cohort_dat <-
  class_cohort_dat %>%
  select(c(RINPERSOONS, RINPERSOON, class_foreign_born_parents, year, math, 
           reading, language, vmbo_gl_test, havo_test, vwo_test, 
           final_school_advice, vmbo_gl_final, havo_final, vwo_final, advice_revised, school_ID, 
           class_vmbo_gl_test, class_havo_test, class_vwo_test,
           class_income_below_25th, class_income_below_50th, class_income_above_75th,
           class_math, class_reading, class_language, class_size))

#connect to the sample
cohort_dat <- cohort_dat %>%
 inner_join(class_cohort_dat, by = c("RINPERSOON", "RINPERSOONS"))


# record sample size
sample_size <- sample_size %>% 
  mutate(n_5_test_scores = nrow(cohort_dat))

before_neighborhood <- cohort_dat

#### POSTCODE LINK IN THE LAST (CALENDAR) YEAR TOOK THE CITO TEST OF PRIMARY SCHOOL ####
# find home
adres_dat <- read_sav(file.path(loc$data_folder, loc$gbaao_data)) %>%
  mutate(
    RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
    SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
    GBADATUMAANVANGADRESHOUDING = ymd(GBADATUMAANVANGADRESHOUDING),
    GBADATUMEINDEADRESHOUDING = ymd(GBADATUMEINDEADRESHOUDING)
  )

age_tab <- cohort_dat %>%
  select(RINPERSOONS, RINPERSOON, year) %>%
  mutate(home_address_date = ymd(paste0(year+1,"01-01"))) %>%
  select(-year)

# take the address registration on a specific date
home_tab <- adres_dat %>%
  filter(RINPERSOON %in% cohort_dat$RINPERSOON & RINPERSOONS %in% cohort_dat$RINPERSOONS) %>%
  # add home_address_date
  left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  # take addresses that are still open on home_address_date
  filter(
    GBADATUMAANVANGADRESHOUDING <= home_address_date &
      GBADATUMEINDEADRESHOUDING >= home_address_date
  ) %>%
  group_by(RINPERSOONS, RINPERSOON) %>%
  summarise(home = RINOBJECTNUMMER[1],
            type_home = SOORTOBJECTNUMMER[1]) 

## if missing first check if have an address registration 31 days after home_address_date

home_tab_missing_after <- adres_dat %>%
  filter(RINPERSOON %in% cohort_dat$RINPERSOON & RINPERSOONS %in% cohort_dat$RINPERSOONS) %>%
  # add home_address_date
  left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(pl_missing = if_else((RINPERSOON %in% home_tab$RINPERSOON), 0, 1)) %>%
  filter (pl_missing == 1) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(GBADATUMAANVANGADRESHOUDING > home_address_date) %>%
  summarize(
    # date_address is date we take the address of the child, it is the first 
    # available registered address of a child after their 21st birthday
    home = RINOBJECTNUMMER[1],
    type_home = SOORTOBJECTNUMMER[1],
    home_address_date = home_address_date[1],
    date_address = GBADATUMAANVANGADRESHOUDING[1]) %>%
  mutate(missing = if_else((date_address - home_address_date > 31), 1, 0)) %>%
  filter(missing == 0) %>%
  select(RINPERSOON, RINPERSOONS, home, type_home)

home_tab <- rbind (home_tab, home_tab_missing_after) 

#for those still missing check 31 days before home_address_date

home_tab_missing_before <- adres_dat %>%
  filter(RINPERSOON %in% cohort_dat$RINPERSOON & RINPERSOONS %in% cohort_dat$RINPERSOONS) %>%
  # add home_address_date
  left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(pl_missing = if_else((RINPERSOON %in% home_tab$RINPERSOON), 0, 1)) %>%
  filter (pl_missing == 1) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(GBADATUMEINDEADRESHOUDING < home_address_date) %>%
  arrange(desc(GBADATUMEINDEADRESHOUDING), .by_group = TRUE) %>%
  summarize(
    # date_address is date we take the address of the child, it is the first 
    # available registered address of a child after their 21st birthday
    home = RINOBJECTNUMMER[1],
    type_home = SOORTOBJECTNUMMER[1],
    home_address_date = home_address_date[1],
    date_address = GBADATUMAANVANGADRESHOUDING[1]) %>%
  mutate(missing = if_else((date_address - home_address_date < -31), 1, 0)) %>%
  filter(missing == 0) %>%
  select(RINPERSOON, RINPERSOONS, home, type_home)

home_tab <- rbind (home_tab, home_tab_missing_before) 

# clean the postcode table
vslpc_path <- file.path(loc$data_folder, loc$postcode_data)
vslpc_tab  <- read_sav(vslpc_path) %>%
  mutate(
    SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
    DATUMAANVPOSTCODENUMADRES = ymd(DATUMAANVPOSTCODENUMADRES),
    DATUMEINDPOSTCODENUMADRES = ymd(DATUMEINDPOSTCODENUMADRES),
    POSTCODENUM = ifelse(POSTCODENUM == "----", NA, POSTCODENUM)
  ) %>%
  filter(!is.na(POSTCODENUM))
  

# only consider postal codes valid on target_date and create postcode-3 level
vslpc_tab <- 
  vslpc_tab %>% 
  filter(dmy(cfg$postcode_target_date) %within% interval(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES)) %>% 
  select(SOORTOBJECTNUMMER, RINOBJECTNUMMER, POSTCODENUM)

home_tab <- home_tab %>%
  inner_join(vslpc_tab, by = c("type_home" = "SOORTOBJECTNUMMER", 
                                    "home" = "RINOBJECTNUMMER")) %>%
  rename(postcode4_grade8 = POSTCODENUM) %>%
  select(RINPERSOONS, RINPERSOON, postcode4_grade8)


# add childhood home to sample data
cohort_dat <- left_join(cohort_dat, home_tab)

# free up memory
rm(home_tab, home_tab_missing_after, home_tab_missing_before, age_tab)


#### SAVE THE NEIGHBORHOOD SAMPLE ####
neighborhood_dat <- cohort_dat

write_rds(neighborhood_dat, file.path(loc$scratch_folder, "neighborhood_cohort.rds"))


#### LIVE CONTINUOUSLY IN NL ####

# We only include children who live continuously in the Netherlands between in the calendar years of the last academic year they took a cito test
# children are only allowed to live up to 31 days not in the Netherlands

# add the year we use for residency requirement
#variable year is the calendar year in which the child started the grade in which the took the cito test the last time
child_adres_date <- cohort_dat %>%
  select(RINPERSOON, RINPERSOONS, year) %>%
  mutate(adres_year_min = year, 
         adres_year_max = year+1)  %>%
  select(-year)

# remove addresses that end before or start after the residency year
adres_tab <- adres_dat %>%
  inner_join(child_adres_date, by = c("RINPERSOONS", "RINPERSOON")) %>%
  filter(!(format(GBADATUMEINDEADRESHOUDING, "%Y") < adres_year_min),
         !(format(GBADATUMAANVANGADRESHOUDING, "%Y") > adres_year_max))


# throw out anything with an end date before start_date, and anything with a start date after end_date
# then also set the start date of everything to start_date, and the end date of everything to end_date
# then compute the timespan of each record
adres_tab <- 
  adres_tab %>%
  mutate(
    start_date  = dmy(paste0("0101", adres_year_min)),
    end_date    = dmy(paste0("3112", adres_year_max)),
    cutoff_days = as.numeric(difftime(end_date, start_date, units = "days")) - cfg$child_live_slack_days,
    recordstart = as_date(ifelse(GBADATUMAANVANGADRESHOUDING < start_date, start_date, GBADATUMAANVANGADRESHOUDING)),
    recordend   = as_date(ifelse(GBADATUMEINDEADRESHOUDING > end_date, end_date, GBADATUMEINDEADRESHOUDING)),
    timespan    = difftime(recordend, recordstart, units = "days")
  )


# group by person and sum the total number of days
# then compute whether this person lived in the Netherlands continuously
days_tab <- 
  adres_tab %>% 
  select(RINPERSOONS, RINPERSOON, timespan, cutoff_days) %>% 
  mutate(timespan = as.numeric(timespan)) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  summarize(total_days = sum(timespan),
            continuous_living = total_days >= cutoff_days) %>% 
  select(RINPERSOONS, RINPERSOON, continuous_living) %>%
  unique()


# add to cohort and filter
cohort_dat <- 
  left_join(cohort_dat, days_tab, by = c("RINPERSOONS", "RINPERSOON")) %>% 
  filter(continuous_living) %>% 
  select(-continuous_living)

rm(adres_tab, days_tab, child_adres_date)

# record sample size
sample_size <- sample_size %>% 
  mutate(n_6_child_residency = nrow(cohort_dat))

# #### CONTINUE PRIMARY SCHOOL OUTCOMES ####
# 
# # under advice and over advice outcomes
# # import under- and over advice table
# advice_tab <- read_xlsx(loc$advice_data, sheet = loc$advice_data_sheet)
# advice_mat <- as.matrix(advice_tab %>% select(-Teacher))
# 
# # change row and column names to numeric education categories
# rownames(advice_mat) <- parse_number(advice_tab$Teacher)
# colnames(advice_mat) <- parse_number(colnames(advice_tab)[-1])
# 
# #  create table with all possible combinations 
# advice_type_tab <- expand_grid(
#   final_advice = rownames(advice_mat), 
#   test_advice = colnames(advice_mat)
# ) %>% 
#   rowwise() %>% 
#   mutate(advice_type  = advice_mat[final_advice, test_advice],
#          final_advice = as.numeric(final_advice),
#          test_advice  = as.numeric(test_advice)
#   ) %>% 
#   ungroup()
# 
# 
# cohort_dat <- left_join(cohort_dat, advice_type_tab, 
#                         by = c("final_school_advice" = "final_advice",
#                                "WPOTOETSADVIES" = "test_advice")) 
# 
# rm(advice_type_tab, advice_mat, advice_tab)
# 
# 
# # create under- and over advice outcomes
# cohort_dat <- 
#   cohort_dat %>%
#   mutate(
#     under_advice = ifelse(advice_type == "under", 1, 0),
#     over_advice  = ifelse(advice_type == "over", 1, 0)
#   )
# 
# # no missings in all outcomes
# cohort_dat <- 
#   cohort_dat %>%
#   filter(
#     !(is.na(math) & is.na(reading) & is.na(language) &
#         is.na(vmbo_gl_test) & is.na(havo_test) &
#         is.na(vwo_test) & is.na(vmbo_gl_final) &
#         is.na(havo_final) & is.na(vwo_final) &
#         is.na(under_advice) & is.na(over_advice))
#   ) %>%
#   select(-c(WPOBRIN_crypt, WPOBRINVEST, WPOTYPEPO, WPOGROEPSGROOTTE, WPOREKENEN, WPOTAALLV, WPOTAALTV, 
#             WPOTOETSADVIES, WPOADVIESVO, WPOADVIESHERZ, advice_type, 
#             final_school_advice))
# 
# 
# # record sample size
# sample_size <- sample_size %>% 
#   mutate(n_5_child_outcomes = nrow(cohort_dat))


#### YOUTH HEALTH COSTS ####


# create a table with incomes at the cpi_base_year level
# first, load consumer price index data (2015 = 100)
# source: CBS statline
cpi_tab <- read_excel(loc$cpi_index_data) %>%
  mutate(year = as.numeric(year))

# set cpi_base_year = 100
cpi_tab <- cpi_tab %>%
  mutate(
    cpi = cpi / cpi_tab %>% filter(year == cfg$cpi_base_year) %>% pull(cpi) * 100)



# function to get latest inschrwpo version of specified year
get_health_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "GezondheidWelzijn/ZVWZORGKOSTENTAB", year),
    pattern = paste0("ZVWZORGKOSTEN", year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


health_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(),
                     youth_health_costs = double(), year = integer())
for (year in seq(as.integer(cfg$primary_school_year_min), as.integer(cfg$primary_school_year_max))) {

  if (year == 2020) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF")) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)

  } else if (year == 2021) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF")) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)

  } else if (year == 2022) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF", "ZVWKGGZZPMTOTAAL" )) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)
  } else if (year == 2023 | year == 2024) {
    health_tab <- read_sav(get_health_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                          "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", "ZVWKEERSTELIJNSVERBLIJF",
                                          "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", "ZVWKZIEKENVERVOER",
                                          "ZVWKBUITENLAND", "ZVWKOVERIG", "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG",
                                          "ZVWKWYKVERPLEGING", "NOPZVWKHUISARTSINSCHRIJF", "ZVWKGGZZPMTOTAAL" )) %>%
      mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values")) %>%
      # select only children
      filter(RINPERSOON %in% cohort_dat$RINPERSOON)
  }

  health_tab <- health_tab %>%
    # replace negative values with 0
    mutate(across(grep("^ZVWK", names(health_tab), value = TRUE),
                  function(x) ifelse(x < 0, 0, x))) %>%
    # sum of all healthcare costs
    mutate(
      youth_health_costs = rowSums(across(grep("^ZVWK", names(health_tab), value = TRUE)), na.rm = TRUE),
      youth_health_costs = youth_health_costs - (NOPZVWKHUISARTSINSCHRIJF + ZVWKGEBOORTEZORG),
      year = year # add year
    ) %>%
    select(RINPERSOONS, RINPERSOON, youth_health_costs, year)

  # add to health dat
  health_dat <- bind_rows(health_dat, health_tab)

}
rm(health_tab)


# deflate
health_dat <-
  health_dat %>%
  left_join(cpi_tab %>% select(year, cpi), by = "year") %>%
  mutate(youth_health_costs = youth_health_costs / (cpi / 100)) %>%
  select(-cpi)


# add to data
cohort_dat <- left_join(cohort_dat, health_dat,
                        by = c("RINPERSOONS", "RINPERSOON", "year"))

rm(health_dat, cpi_tab)

# convert NA to 0
cohort_dat <- cohort_dat %>%
  mutate(youth_health_costs = ifelse(is.na(youth_health_costs), 0, youth_health_costs))



#### YOUTH PROTECTION ####

# function to get latest jgdbeschermbus version of specified year
get_protection_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "VeiligheidRecht/JGDBESCHERMBUS"),
    pattern = paste0("JGDBESCHERM", year, "BUSV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


protection_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(),
                         year = integer())
for (year in seq(2015, as.integer(cfg$child_outcome_year_max))) {

  protection_dat <-
    read_sav(get_protection_filename(year),
             col_select = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
           year = year) %>%
    # add to protection dat
    bind_rows(protection_dat, .)
}

# use protection data 2015 dat to 2014 year since we do not have protection data 2014
protection_dat <-
  read_sav(get_protection_filename(2015),
           col_select = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
         year = 2014) %>%
  # add to protection dat
  bind_rows(protection_dat, .)


# create youth protection outcome
protection_dat <- protection_dat %>%
  mutate(youth_protection = 1) %>%
  unique()

# add to data
cohort_dat <- left_join(cohort_dat, protection_dat,
                        by = c("RINPERSOONS", "RINPERSOON", "year"))

rm(protection_dat)

# convert NA to 0
cohort_dat <-
  cohort_dat %>%
  mutate(youth_protection = ifelse(is.na(youth_protection), 0, youth_protection))

#### LIVING SPACE PER HOUSEHOLD MEMBER ####

#load home addresses
adres_tab <- read_sav(file.path(loc$data_folder, loc$gbaao_data)) %>%
  mutate(
    RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
    SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
    GBADATUMAANVANGADRESHOUDING = ymd(GBADATUMAANVANGADRESHOUDING),
    GBADATUMEINDEADRESHOUDING = ymd(GBADATUMEINDEADRESHOUDING))


# load home addresses
adres_tab <-
  adres_tab %>%
  # select only children
  filter(RINPERSOON %in% cohort_dat$RINPERSOON) %>%
  filter(!(GBADATUMEINDEADRESHOUDING < ymd(paste0(cfg$primary_school_year_min, "0101"))),
         !(GBADATUMAANVANGADRESHOUDING > ymd(paste0(cfg$primary_school_year_max, "0101"))))


# create 1 january variable
cohort_dat <- cohort_dat %>%
  mutate(january = ymd(paste0(year, "-01-01")))


# add 1 januari data to adres tab to filter for home addresses at 1 jan
adres_tab <-
  adres_tab %>%
  left_join(cohort_dat %>% select(RINPERSOONS, RINPERSOON, january)) %>%
  filter(january %within% interval(GBADATUMAANVANGADRESHOUDING, GBADATUMEINDEADRESHOUDING))


# add home addresses to data
cohort_dat <- left_join(cohort_dat, adres_tab %>%
                          select(RINPERSOONS, RINPERSOON,
                                 SOORTOBJECTNUMMER, RINOBJECTNUMMER),
                        by = c("RINPERSOONS", "RINPERSOON"))
rm(adres_tab)


# LIVING SPACE

woon_dat <-
  read_sav(file.path(loc$data_folder, loc$woon_data),
           col_select = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "VBOOPPERVLAKTE",
                          "AANVLEVCYCLWOONNIETWOON", "EINDLEVCYCLWOONNIETWOON")) %>%
  mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
         AANVLEVCYCLWOONNIETWOON = ymd(AANVLEVCYCLWOONNIETWOON),
         EINDLEVCYCLWOONNIETWOON = ifelse(EINDLEVCYCLWOONNIETWOON == "88888888",
                                          paste0(cfg$primary_school_year_max, "1231"),
                                          EINDLEVCYCLWOONNIETWOON),
         EINDLEVCYCLWOONNIETWOON = ymd(EINDLEVCYCLWOONNIETWOON)
  ) %>%
  filter(!(EINDLEVCYCLWOONNIETWOON < ymd(paste0(cfg$primary_school_year_min, "-01-01"))),
         !(AANVLEVCYCLWOONNIETWOON > ymd(paste0(cfg$primary_school_year_max, "-01-01"))))


# add 1 januari data to woon tab to filter for home addresses at 1 jan
woon_dat <-
  woon_dat %>%
  left_join(cohort_dat %>% select(SOORTOBJECTNUMMER, RINOBJECTNUMMER, january),
            by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER")) %>%
  filter(january %within% interval(AANVLEVCYCLWOONNIETWOON, EINDLEVCYCLWOONNIETWOON)) %>%
  unique()



# add living space to data
cohort_dat <- left_join(cohort_dat,
                        woon_dat %>% select(SOORTOBJECTNUMMER, RINOBJECTNUMMER, VBOOPPERVLAKTE, january),
                        by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "january"))  %>%
  mutate(VBOOPPERVLAKTE = as.numeric(VBOOPPERVLAKTE))

rm(woon_dat)



# NUMBER OF HOUSEHOLD MEMBERS


# function to get latest eigendom version of specified year
get_eigendom_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "BouwenWonen/EIGENDOMTAB"),
    pattern = paste0("EIGENDOM", year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


household_members <- tibble(SOORTOBJECTNUMMER = factor(), RINOBJECTNUMMER = character(),
                            AantalBewoners = double(), year = integer())
for (year in seq(as.integer(cfg$primary_school_year_min), as.integer(cfg$primary_school_year_max))) {

  household_members <- read_sav(get_eigendom_filename(year),
                                col_select = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER",
                                               "AantalBewoners")) %>%
    mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
           AantalBewoners = as.numeric(AantalBewoners),
           year = year) %>%
    # add to household member dat
    bind_rows(household_members, .)
}

cohort_dat <- cohort_dat %>%
  left_join(household_members, by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "year")) %>%
  mutate(AantalBewoners = as.numeric(AantalBewoners))


# create living space per household member outcome
cohort_dat <-
  cohort_dat %>%
  mutate(AantalBewoners = ifelse(AantalBewoners == 0, NA, AantalBewoners),
         living_space_pp = VBOOPPERVLAKTE / AantalBewoners) %>%
  select(-c(january, SOORTOBJECTNUMMER, RINOBJECTNUMMER,
            VBOOPPERVLAKTE, AantalBewoners))

rm(household_members)


#### NEIGHBORHOOD COMPOSITION ####

neighborhood_cohort_dat <- read_rds(file.path(loc$scratch_folder, "neighborhood_cohort.rds"))

neighborhood_cohort_dat <- neighborhood_cohort_dat %>%
  mutate(below_p25 = if_else(income_parents_perc < 0.25, 1, 0),
         below_p50 = if_else(income_parents_perc < 0.5, 1, 0),
         above_p75 = if_else(income_parents_perc > 0.75, 1, 0),
         GBAGEBOORTELANDMOEDER = as_factor(GBAGEBOORTELANDMOEDER),
         GBAGEBOORTELANDVADER = as_factor(GBAGEBOORTELANDVADER),
         foreign_born_parents = if_else((GBAGEBOORTELANDMOEDER != "Nederland" & 
                                           GBAGEBOORTELANDVADER != "Nederland"),  1, 0))

# hold out mean function
hold_out_means <- function(x) {
  hold <- ((sum(x, na.rm = TRUE) - x) / (length(x) - 1))
  return(hold)
}

# neighborhood composition at pc4 level 
neighborhood_cohort_dat <- neighborhood_cohort_dat %>%
  group_by(postcode4_grade8) %>%
  mutate(
    primary_neighborhood_income_below_25th = if_else(is.na(postcode4_grade8), NA_real_, hold_out_means(.data$below_p25)),
    primary_neighborhood_income_below_50th = if_else(is.na(postcode4_grade8), NA_real_, hold_out_means(.data$below_p50)),
    primary_neighborhood_income_above_75th = if_else(is.na(postcode4_grade8), NA_real_, hold_out_means(.data$above_p75)),
    primary_neighborhood_foreign_born_parents = if_else(is.na(postcode4_grade8), NA_real_, hold_out_means(.data$foreign_born_parents))
  ) %>% ungroup()

neighborhood_cohort_dat <- neighborhood_cohort_dat %>%
  select(c("RINPERSOONS", "RINPERSOON", contains("primary_neighborhood")))

cohort_dat <- cohort_dat %>%
  left_join(neighborhood_cohort_dat, by = c("RINPERSOONS", "RINPERSOON"))

rm(neighborhood_cohort_dat)


#### REMOVE OBSERVATIONS WITH MISSING OUTCOMES ####
outcomes <- c("class_vmbo_gl_test", "class_havo_test",
              "class_vwo_test", "class_foreign_born_parents",
              "class_income_below_25th", "class_income_below_50th",
              "class_income_above_75th",
              "primary_neighborhood_income_below_25th", 
              "primary_neighborhood_income_below_50th",
              "primary_neighborhood_income_above_75th", 
              "primary_neighborhood_foreign_born_parents")

cohort_dat <- cohort_dat %>%
  filter(rowSums(is.na(select(., all_of(outcomes)))) == 0)

# record sample size
sample_size <- sample_size %>% 
  mutate(n_7_child_outcomes = nrow(cohort_dat))


#### PREFIX ####

# add prefix to outcomes
outcomes <- c("vmbo_gl_final", "havo_final", "vwo_final", 
              "vmbo_gl_test", "havo_test", "vwo_test",
              #"over_advice", "under_advice", 
              'advice_revised',
              "math", "language", "reading",
              "youth_health_costs", "youth_protection",
              "living_space_pp","class_vmbo_gl_test", "class_havo_test",
              "class_vwo_test", "class_foreign_born_parents",
              "class_income_below_25th", "class_income_below_50th",
              "class_income_above_75th", "class_math", "class_language",
              "class_reading", "class_size", 
              "primary_neighborhood_income_below_25th", 
              "primary_neighborhood_income_below_50th",
              "primary_neighborhood_income_above_75th", 
              "primary_neighborhood_foreign_born_parents"
              )

suffix <- "c11_a_"


# rename outcomes
cohort_dat <- 
  cohort_dat %>%
  rename_with(~str_c(suffix, .), .cols = all_of(outcomes)) %>% 
  ungroup() %>%
  #remove parents birth country
  select(-c(GBAGEBOORTELANDMOEDER, GBAGEBOORTELANDVADER))


#### WRITE OUTPUT TO SCRATCH ####
write_rds(cohort_dat, file.path(loc$scratch_folder, "03_outcomes.rds"))



#write sample size reduction table to scratch
sample_size <- sample_size %>% mutate(cohort_name = cohort)
write_rds(sample_size, file.path(loc$scratch_folder, "03_sample_size.rds"))
