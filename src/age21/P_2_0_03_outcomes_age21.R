# Kansenkaart data preparation pipeline
#
# 3. Outcome creation.
#   - Adding students outcomes to the cohort.
#NOTE: outcomes using hoogsteopltab cannot be created yet
# (c) ODISSEI Social Data Science team 2025




#### PACKAGES ####
library(tidyverse)
library(lubridate)
library(haven)


#### CONFIGURATION ####
# load main cohort dataset
cohort_dat <- read_rds(file.path(loc$scratch_folder, "02_predictors.rds"))

sample_size <- read_rds(file.path(loc$scratch_folder, "02_sample_size.rds"))

# add year at which the child is a specific age to the cohort
cohort_dat <- 
  cohort_dat %>%
  mutate(
    year = year(birthdate) + cfg$outcome_age
  )


#### LIVE CONTINUOUSLY IN NL ####

# We only include children who live continuously in the Netherlands 
# children are only allowed to live up to 31 days not in the netherlands
adres_tab <- read_sav(file.path(loc$data_folder, loc$gbaao_data)) %>%
  # select only children
  filter(RINPERSOON %in% cohort_dat$RINPERSOON) %>% 
  mutate(
    RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
    SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
    GBADATUMAANVANGADRESHOUDING = ymd(GBADATUMAANVANGADRESHOUDING),
    GBADATUMEINDEADRESHOUDING = ymd(GBADATUMEINDEADRESHOUDING)
  )

# residency requirement for people between child_live_start and child_live_end
residency_tab <- 
  cohort_dat %>%
  select(RINPERSOONS, RINPERSOON, birthdate) %>%
  mutate(start_date = ymd(paste0(year(birthdate), "-01-01")) %m+% years(cfg$child_live_start), 
         end_date = ymd(paste0(year(birthdate), "-12-31")) %m+% years(cfg$child_live_end),
         cutoff_days = as.numeric(difftime(end_date, start_date, units = "days")) - 
           cfg$child_live_slack_days) %>%
  select(-birthdate)


# throw out anything with an end date before start_date, and anything with a start date after end_date
# then also set the start date of everything to start_date, and the end date of everything to end_date
# then compute the time span of each record
adres_tab <- 
  adres_tab %>% 
  inner_join(residency_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  filter(!(GBADATUMEINDEADRESHOUDING < start_date),
         !(GBADATUMAANVANGADRESHOUDING > end_date)) %>%
  mutate(
    recordstart = as_date(ifelse(GBADATUMAANVANGADRESHOUDING < start_date, start_date, GBADATUMAANVANGADRESHOUDING)),
    recordend   = as_date(ifelse(GBADATUMEINDEADRESHOUDING > end_date, end_date, GBADATUMEINDEADRESHOUDING)) ,
    timespan    = difftime(recordend, recordstart, units = "days")
  ) %>%
  select(-c(GBADATUMAANVANGADRESHOUDING, GBADATUMEINDEADRESHOUDING, 
            start_date, end_date))


# group by person and sum the total number of days
# then compute whether this person lived in the Netherlands continuously
days_tab <- 
  adres_tab %>% 
  select(RINPERSOONS, RINPERSOON, timespan, cutoff_days) %>% 
  mutate(timespan = as.numeric(timespan)) %>% 
  group_by(RINPERSOONS, RINPERSOON, cutoff_days) %>% 
  summarize(total_days = sum(timespan)) %>%  
  mutate(continuous_living = total_days >= cutoff_days) %>% 
  select(RINPERSOONS, RINPERSOON, continuous_living)

# add to cohort and filter
cohort_dat <- 
  left_join(cohort_dat, days_tab, by = c("RINPERSOONS", "RINPERSOON")) %>% 
  filter(continuous_living) %>% 
  select(-continuous_living)

rm(adres_tab, days_tab, residency_tab)

# record sample size
sample_size <- sample_size %>% 
  mutate(n_5_child_residency = nrow(cohort_dat))


#### EDUCATION ####
get_hoogstopl_filename <- function(year) {
  # get all HOOGSTEOPLTAB files with the specified year
  fl <- list.files(
    path = file.path(loc$data_folder, "Onderwijs/HOOGSTEOPLTAB", year),
    pattern = "(?i)(.sav)",
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


education_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(),
                        education_attained = factor(), education_followed = factor(),
                        year = integer())
for (year in seq(as.integer(cfg$education_year_min), as.integer(cfg$education_year_max))) {

  if (year < 2019) {
    education_dat <- read_sav(get_hoogstopl_filename(year),
                              col_select = c("RINPERSOONS", "RINPERSOON", "OPLNIVSOI2016AGG4HGMETNIRWO",
                                             "OPLNIVSOI2016AGG4HBMETNIRWO")) %>%
      rename(education_attained = OPLNIVSOI2016AGG4HBMETNIRWO,
             education_followed = OPLNIVSOI2016AGG4HGMETNIRWO) %>%
      mutate(
        RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
        education_attained = ifelse(education_attained == "----", NA, education_attained),
        education_followed = ifelse(education_followed == "----", NA, education_followed),
        year = year
      ) %>%
      # add to data
      bind_rows(education_dat, .)

  } else if (year >= 2019) {
    education_dat <- read_sav(get_hoogstopl_filename(year),
                              col_select = c("RINPERSOONS", "RINPERSOON", "OPLNIVSOI2021AGG4HBmetNIRWO",
                                             "OPLNIVSOI2021AGG4HGmetNIRWO")) %>%
      rename(education_attained = OPLNIVSOI2021AGG4HBmetNIRWO,
             education_followed = OPLNIVSOI2021AGG4HGmetNIRWO) %>%
      mutate(
        RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
        education_attained = ifelse(education_attained == "----", NA, education_attained),
        education_followed = ifelse(education_followed == "----", NA, education_followed),
        year = year
      ) %>%
      # add to data
      bind_rows(education_dat, .)
  }
}

# create education outcomes
education_dat <-
  education_dat %>%
  mutate(
    high_school_attained = ifelse(education_attained >= 2110, 1, 0),
    hbo_followed        = ifelse(education_followed >= 3110, 1, 0),
    uni_followed        = ifelse(education_followed %in% c(3113, 3212, 3213), 1, 0),

    high_school_attained = ifelse(is.na(high_school_attained), 0, high_school_attained),
    hbo_followed         = ifelse(is.na(hbo_followed), 0, hbo_followed),
    uni_followed         = ifelse(is.na(uni_followed), 0, uni_followed)
  ) %>%
  select(-c(education_attained, education_followed))

# join to cohort
cohort_dat <- cohort_dat %>%
  left_join(education_dat,
            by = c("RINPERSOONS", "RINPERSOON", "year"))

rm(education_dat)

    
     
#### LIVING AT HOME ####
household_dat <-
  read_sav(file.path(loc$data_folder, loc$household_data),
           col_select = c("RINPERSOONS", "RINPERSOON", "DATUMAANVANGHH",
                          "DATUMEINDEHH", "PLHH")) %>%
  filter(RINPERSOON %in% cohort_dat$RINPERSOON) %>%
  mutate(
    RINPERSOONS = as_factor(RINPERSOONS, levels = "value"),
    DATUMAANVANGHH = as.numeric(DATUMAANVANGHH),
    DATUMEINDEHH = as.numeric(DATUMEINDEHH),
    PLHH = as_factor(PLHH, levels = "value")
  ) %>%
  mutate(
    DATUMAANVANGHH = ymd(DATUMAANVANGHH),
    DATUMEINDEHH = ymd(DATUMEINDEHH)
  )                                                                                 

# take date at which the child is a specific age
age_tab <- 
  cohort_dat %>%
  select(RINPERSOONS, RINPERSOON, birthdate) %>%
  mutate(home_address_date = birthdate %m+% years(cfg$outcome_age)) %>%
  select(-birthdate)

# take household data at which child is a specific age
hh_tab <- 
  household_dat %>%
  left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  group_by(RINPERSOONS, RINPERSOON) %>% 
  filter(DATUMAANVANGHH <= home_address_date & 
           DATUMEINDEHH >= home_address_date) %>%
  summarize(PLHH = PLHH[1]) 

## if missing first check if have an address registration 31 days after the 21st birthday

hh_tab_missing_after <-
  household_dat %>%
  left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(pl_missing = if_else((RINPERSOON %in% hh_tab$RINPERSOON), 0, 1)) %>%
  filter (pl_missing == 1) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(DATUMAANVANGHH > home_address_date) %>%
  summarize(
    # date_21 is date we take the address of the child, it is the first 
    # available registered address of a child after their 21st birthday
    PLHH = PLHH[1],
    birthday = home_address_date[1],
    date_21 = DATUMAANVANGHH[1]) %>%
  mutate(missing = if_else((date_21 - birthday > 31), 1, 0)) %>%
  filter(missing == 0) %>%
  select(RINPERSOON, RINPERSOONS, PLHH)

hh_tab <- rbind (hh_tab, hh_tab_missing_after)

##for those still missing check 31 days before 21st birthday

hh_tab_missing_before <-
  household_dat %>%
  left_join(age_tab, by = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(pl_missing = if_else((RINPERSOON %in% hh_tab$RINPERSOON), 0, 1)) %>%
  filter (pl_missing == 1) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(DATUMEINDEHH < home_address_date) %>%
  arrange(desc(DATUMEINDEHH), .by_group = TRUE) %>%
  summarize(
    # date_21 is date we take the address of the child, it is the first 
    # available registered address of a child before their 21st birthday
    PLHH = PLHH[1],
    birthday = home_address_date[1],
    date_21 = DATUMEINDEHH[1]) %>%
  mutate(missing = if_else((date_21 - birthday < -31), 1, 0)) %>%
  filter(missing == 0) %>%
  select(RINPERSOON, RINPERSOONS, PLHH)

hh_tab <- rbind (hh_tab, hh_tab_missing_before) %>%
  mutate (PLHH = as.numeric(PLHH)) %>%
  mutate (living_with_parents = if_else(PLHH == 1, 1, 0)) %>%
  select (-PLHH)

cohort_dat <- cohort_dat %>%
  left_join(hh_tab, by = c("RINPERSOONS", "RINPERSOON"))

# free up memory 
rm(hh_tab, hh_tab_missing, age_tab, household_dat)


#### YOUNG PARENTS ####


# load kinderoudertab 
kindouder_dat <- read_sav(file.path(loc$data_folder, loc$kind_data),
                          col_select = -c("XKOPPELNUMMER")) %>%
  as_factor(only_labelled = TRUE, levels = "values") %>%
  filter((RINPERSOONMa %in% cohort_dat$RINPERSOON | RINPERSOONpa %in% cohort_dat$RINPERSOON),
         RINPERSOONS == "R", RINPERSOONSMa == "R", RINPERSOONSpa == "R") %>%
  rename(RINPERSOONS_infant = RINPERSOONS,
         RINPERSOON_infant = RINPERSOON) 

# load gba data
# voormalig birth_dat, changed to gba_dat to match cohort creation 
gba_path <- file.path(loc$data_folder, loc$gba_dat)
gba_dat <-
  read_sav(gba_path, col_select = c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTEJAAR",
                                    "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG")) %>%
  mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
         birthdate = dmy(paste(GBAGEBOORTEDAG, GBAGEBOORTEMAAND, GBAGEBOORTEJAAR, sep = "-"))) %>%
  select(-c(GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG))


# add infants birth date to cohort
kindouder_dat <-
  kindouder_dat %>%
  left_join(gba_dat, by = c("RINPERSOONS_infant" = "RINPERSOONS",
                            "RINPERSOON_infant" = "RINPERSOON")) %>%
  rename(birthdate_infant = birthdate) %>%
  select(-c(RINPERSOONS_infant, RINPERSOON_infant))

# add parental birth date
kindouder_dat <-
  kindouder_dat %>%
  left_join(gba_dat, by = c("RINPERSOONSMa" = "RINPERSOONS",
                            "RINPERSOONMa" = "RINPERSOON")) %>%
  rename(birthdate_ma = birthdate) %>%
  left_join(gba_dat, by = c("RINPERSOONSpa" = "RINPERSOONS",
                            "RINPERSOONpa" = "RINPERSOON")) %>%
  rename(birthdate_pa = birthdate)

# create one long df for each individual parent, their birthday and the birthdate of their child.
kindvader_dat <- 
  kindouder_dat %>% 
  select(RINPERSOONSpa, RINPERSOONpa, birthdate_pa, birthdate_infant) %>% 
  rename(RINPERSOONS = RINPERSOONSpa, RINPERSOON = RINPERSOONpa, 
         birthdate_parent = birthdate_pa)
kindmoeder_dat <- 
  kindouder_dat %>%
  select(RINPERSOONSMa, RINPERSOONMa, birthdate_ma, birthdate_infant) %>% 
  rename(RINPERSOONS = RINPERSOONSMa, RINPERSOON = RINPERSOONMa, 
         birthdate_parent = birthdate_ma)
kindouder_dat <- rbind(kindvader_dat, kindmoeder_dat)

# determine age at which person became a parent
kindouder_dat <-
  kindouder_dat %>%
  mutate(age_at_birth_parent = interval(birthdate_parent, birthdate_infant) / years(1)) %>%
  select(-c(birthdate_infant, birthdate_parent)) %>%
  arrange(RINPERSOON, age_at_birth_parent) %>%
  group_by(RINPERSOONS, RINPERSOON) %>%
  summarize(age_at_birth_parent = age_at_birth_parent[1])


# create young parent outcome and change to 0 if NA
cohort_dat <- cohort_dat %>%
  left_join(kindouder_dat, by = c("RINPERSOONS", "RINPERSOON")) %>% 
  mutate(young_parents = ifelse(age_at_birth_parent < 20, 1, 0),
         young_parents = ifelse(is.na(young_parents), 0, young_parents)) %>% 
  select(-age_at_birth_parent)

rm(gba_dat, kindouder_dat, kindvader_dat, kindmoeder_dat)



#### PRIMARY SCHOOL CLASS COMPOSITION ####
 # load class cohort data
 class_cohort_dat <- read_rds(file.path(loc$scratch_folder, "class_cohort.rds"))
 
 # combine the main sample and class sample
 class_cohort_dat <- bind_rows(
   class_cohort_dat %>% select ("RINPERSOON", "RINPERSOONS", "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "income_parents_perc"),
   cohort_dat %>% select ("RINPERSOON", "RINPERSOONS", "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "income_parents_perc"),
 )
 
 
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
                      WPOBRIN_crypt = character(), WPOBRINVEST = character(), WPOTYPEPO = character())
 
 for (year in seq(as.integer(cfg$primary_classroom_year_min), as.integer(cfg$primary_classroom_year_max))) {
   school_dat <-
     # read file from disk
     read_sav(get_inschrwpo_filename(year),
              col_select = c("RINPERSOONS", "RINPERSOON", "WPOLEERJAAR",
                             "WPOBRIN_crypt", "WPOBRINVEST", "WPOTYPEPO")) %>%
     mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value")) %>%
     # add year
     mutate(year = year) %>%
     # add to income children
     bind_rows(school_dat, .)
 }
 
 # keep group 8 pupils
 school_dat <- school_dat %>%
   filter(RINPERSOONS == "R") %>%
   mutate(WPOLEERJAAR = trimws(as.character(WPOLEERJAAR))) %>%
   filter(WPOLEERJAAR == "8") %>%
   select(-WPOLEERJAAR)
 
 
 # link to classroom sample
 school_dat <- school_dat %>%
   left_join(class_cohort_dat,
             by = c("RINPERSOON", "RINPERSOONS"))%>%
   # drop all children who are not in the large classroom sample
   filter(!is.na(income_parents_perc))
 
 # primary school ID
 school_dat <- school_dat %>%
   mutate(across(c("WPOBRIN_crypt", "WPOBRINVEST"), as.character)) %>%
   mutate(school_ID = paste0(WPOBRIN_crypt, WPOBRINVEST)) %>%
   select(-c(WPOBRIN_crypt, WPOBRINVEST))
 
 
 # create parents rank income outcomes
 school_dat <- school_dat %>%
   mutate(
     # create dummy for below 25th
     income_below_25th = ifelse(income_parents_perc < 0.25, 1, 0),
     # create dummy for below 50th
     income_below_50th = ifelse(income_parents_perc < 0.50, 1, 0),
     # create dummy for above 75th
     income_above_75th = ifelse(income_parents_perc > 0.75, 1, 0)
   )
 
 
 # create outcome for children with both parents born in a foreign country
 school_dat <- school_dat %>%
   mutate(
     GBAGEBOORTELANDMOEDER = as_factor(GBAGEBOORTELANDMOEDER),
     GBAGEBOORTELANDVADER = as_factor(GBAGEBOORTELANDVADER)
   ) %>%
   mutate(
     foreign_born_parents =
       ifelse((GBAGEBOORTELANDMOEDER != "Nederland" &
                 GBAGEBOORTELANDVADER != "Nederland"),  1, 0))
 
 
 # classroom outcomes: class_foreign_born_parents, class_parents_below_25, class_parents_below_50, class_parents_above_75
 
 # only keep classes with more than one student per class
 school_dat <- school_dat %>%
   group_by(school_ID, year) %>%
   mutate(n = n()) %>%
   filter(n > 1)
 
 
 # hold out mean function
 hold_out_means <- function(x) {
   hold <- ((sum(x, na.rm = TRUE) - x) / (length(x) - 1))
   return(hold)
 }
 
 
 
 # hold out means = mean of the class without the child him/herself
 school_dat <- school_dat %>%
   group_by(school_ID, year) %>%
   mutate(
     #    primary_N_students_per_school = n(),
     primary_class_foreign_born_parents = hold_out_means(foreign_born_parents),
     primary_class_income_below_25th = hold_out_means(income_below_25th),
     primary_class_income_below_50th = hold_out_means(income_below_50th),
     primary_class_income_above_75th = hold_out_means(income_above_75th)
   )
 
 
 # keep unique observations,for duplicates select the last time the child is in 8th grade
 school_dat <- school_dat %>%
   arrange(desc(year)) %>%
   group_by(RINPERSOONS, RINPERSOON) %>%
   filter(row_number() == 1)
 
 # add to outcomes to cohort
 cohort_dat <- cohort_dat %>%
   left_join (school_dat %>%
                 select(RINPERSOONS, RINPERSOON, primary_class_foreign_born_parents, primary_class_income_below_25th, primary_class_income_below_50th, primary_class_income_above_75th),
               by = c("RINPERSOONS", "RINPERSOON"))
 
 rm(school_dat)
 


#### SECONDARY SCHOOL CLASS COMPOSITION ####
#load class cohort data
class_cohort_dat <- read_rds(file.path(loc$scratch_folder, "class_cohort.rds"))

# combine the main sample and class sample
class_cohort_dat <- bind_rows(
  class_cohort_dat %>% select ("RINPERSOON", "RINPERSOONS", "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "income_parents_perc"),
  cohort_dat %>% select ("RINPERSOON", "RINPERSOONS", "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "income_parents_perc"),
)

# function to get latest ONDERWIJSINSCHRTAB version of specified year
get_school_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "Onderwijs/ONDERWIJSINSCHRTAB"),
    pattern = paste0("ONDERWIJSINSCHRTAB", year, "V[0-9]+(?i)(.sav)"), 
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


school_dat <- tibble(RINPERSOONS = factor(), RINPERSOON = character(), OPLNR = character(), VOLEERJAAR = character(), 
                     BRIN_crypt = character(), VOBRINVEST = character())

for (year in seq(as.integer(cfg$secondary_classroom_year_min),as.integer(cfg$secondary_classroom_year_max))) {
  school_dat <- 
    # read file from disk
    read_sav(get_school_filename(year), 
             col_select = c("RINPERSOONS", "RINPERSOON","OPLNR", "VOLEERJAAR", 
                            "BRIN_crypt", "VOBRINVEST")) %>% 
    mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "value")) %>%
    # add year
    mutate(year = year) %>% 
    # add to income children
    bind_rows(school_dat, .)
}

# keep those in grade 4 of secondary school 
school_dat <- school_dat %>%
  filter(VOLEERJAAR == "4", 
         RINPERSOONS == "R",
         !is.na(RINPERSOON)) 

# # find the level of secondary school 
# school_level <- read_sav(loc$opleiding_data) %>% 
#   select(OPLNR, ONDERWIJSSOORTVO)
# 
# school_dat <- school_dat %>%
#   left_join(school_level, by = "OPLNR") %>%
#   rename(school_level = ONDERWIJSSOORTVO )


# link to classroom sample
school_dat <- school_dat %>%
  left_join(class_cohort_dat, 
            by = c("RINPERSOON", "RINPERSOONS"))%>%
  # drop all children who are not in the classroom sample 
  filter(!is.na(income_parents_perc))


# generate secondary school ID
school_dat <- school_dat %>%
  mutate(across(c("BRIN_crypt", "VOBRINVEST"), as.character)) %>%
  mutate(school_ID = paste0(BRIN_crypt, VOBRINVEST)) %>%
  select(-c(BRIN_crypt, VOBRINVEST))



# create parents rank income outcomes
school_dat <- school_dat %>%
  mutate(
    # create dummy for below 25th 
    income_below_25th = ifelse(income_parents_perc < 0.25, 1, 0),
    # create dummy for below 50th 
    income_below_50th = ifelse(income_parents_perc < 0.50, 1, 0),
    # create dummy for above 75th
    income_above_75th = ifelse(income_parents_perc > 0.75, 1, 0)
  )


# create outcome for children with both parents born in a foreign country
school_dat <- school_dat %>%
  mutate(
    GBAGEBOORTELANDMOEDER = as_factor(GBAGEBOORTELANDMOEDER),
    GBAGEBOORTELANDVADER = as_factor(GBAGEBOORTELANDVADER)
  ) %>%
  mutate(
    foreign_born_parents = 
      ifelse((GBAGEBOORTELANDMOEDER != "Nederland" & 
                GBAGEBOORTELANDVADER != "Nederland"),  1, 0))


# classroom outcomes: class_foreign_born_parents, class_parents_below_25, class_parents_below_50, class_parents_above_75

#only keep classes with more than one student per class
school_dat <- school_dat %>%
  group_by(school_ID, year) %>%
  mutate(n = n()) %>%
  filter(n > 1)

# hold out mean function
hold_out_means <- function(x) {
  hold <- ((sum(x, na.rm = TRUE) - x) / (length(x) - 1))
  return(hold)
}

# hold out means = mean of the class without the child him/herself
school_dat <- school_dat %>%
  group_by(school_ID, year) %>%
  mutate(
    #    secondary_class_N_students_per_school = n(),
    secondary_class_foreign_born_parents = hold_out_means(foreign_born_parents),
    secondary_class_income_below_25th = hold_out_means(income_below_25th),
    secondary_class_income_below_50th = hold_out_means(income_below_50th),
    secondary_class_income_above_75th = hold_out_means(income_above_75th)
  ) 


# keep unique observations,for duplicates select the last time the child is in 8th grade
school_dat <- school_dat %>%
  arrange(desc(year)) %>%
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(row_number() == 1)



# add to outcomes to cohort
cohort_dat <- cohort_dat %>%
  left_join (school_dat %>% 
               select(RINPERSOONS, RINPERSOON, secondary_class_foreign_born_parents, secondary_class_income_below_25th, secondary_class_income_below_50th, secondary_class_income_above_75th),
             by = c("RINPERSOONS", "RINPERSOON"))

rm(school_dat, class_cohort_dat)

#### HEALTH COSTS ####

# function to get latest version of specified year
get_health_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, "GezondheidWelzijn/ZVWZORGKOSTENTAB", year),
    pattern = paste0("ZVWZORGKOSTEN", year), 
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}


health_tab <- tibble(RINPERSOONS = factor(), RINPERSOON = character(), 
                     pharma = integer(), specialist_mhc = integer(), 
                     basic_mhc = integer(), mhc = integer(),
                     hospital = integer(), total_health_costs = double(), 
                     year = integer())
for (year in seq(as.integer(cfg$health_year_min), as.integer(cfg$health_year_max))) {
  health_tab <- read_sav(get_health_filename(year),
                         col_select = c("RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "ZVWKMULTIDISC",
                                        "ZVWKFARMACIE", "ZVWKMONDZORG", "ZVWKZIEKENHUIS", 
                                        "ZVWKEERSTELIJNSVERBLIJF", "ZVWKPARAMEDISCH", "ZVWKHULPMIDDEL", 
                                        "ZVWKZIEKENVERVOER", "ZVWKBUITENLAND", "ZVWKOVERIG", 
                                        "ZVWKGERIATRISCH", "ZVWKGEBOORTEZORG", "ZVWKWYKVERPLEGING", 
                                        "NOPZVWKHUISARTSINSCHRIJF", "ZVWKSPECGGZ", "ZVWKGENBASGGZ")) %>%
    mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"), 
           year = year) %>%
    # select only children
    inner_join(cohort_dat %>% select(RINPERSOONS, RINPERSOON, year)) %>%
    # replace negative values with 0
    mutate(across(.cols = starts_with("ZVWK"),
                  .fns = ~ ifelse(.x < 0 , 0, .x))) %>%
    # replace NA values with 0
    mutate(across(.cols = starts_with("ZVWK"),
                  .fns = ~ ifelse(is.na(.x), 0, .x))
    ) %>%
    ungroup() %>%
    mutate(
      pharma             = ifelse(ZVWKFARMACIE > 0, 1, 0),
      basic_mhc          = ifelse((ZVWKGENBASGGZ > 0 | ZVWKSPECGGZ > 0), 1, 0),
      specialist_mhc     = ifelse(ZVWKSPECGGZ > 0, 1, 0),
      mhc                = case_when(
        is.na(basic_mhc) | (specialist_mhc) ~ NA_real_, 
        basic_mhc == 1 | specialist_mhc == 1 ~ 1, 
        TRUE ~ 0),
      hospital           = ifelse(ZVWKZIEKENHUIS > 0, 1, 0),
      total_health_costs = rowSums(across(c(ZVWKHUISARTS, ZVWKMULTIDISC, ZVWKFARMACIE, 
                                            ZVWKMONDZORG, ZVWKZIEKENHUIS, ZVWKEERSTELIJNSVERBLIJF, 
                                            ZVWKPARAMEDISCH, ZVWKHULPMIDDEL, ZVWKZIEKENVERVOER, 
                                            ZVWKBUITENLAND, ZVWKOVERIG, ZVWKGERIATRISCH, 
                                            ZVWKWYKVERPLEGING))), 
      total_health_costs = (total_health_costs - (ZVWKGEBOORTEZORG + NOPZVWKHUISARTSINSCHRIJF))
    ) %>%
    select(c("RINPERSOONS", "RINPERSOON", "pharma", "basic_mhc", "specialist_mhc",
             "mhc",  "hospital", "total_health_costs", "year")) %>%
    bind_rows(health_tab, .)
  
}

cohort_dat <- left_join(cohort_dat, health_tab,
                        by = c("RINPERSOONS", "RINPERSOON", "year"))

# # not merged
# not_merged_tab <- not_merged_func('total_health_costs')

# free up memory
rm(health_tab)


# replace NA with 0 (for those who are not merged with the zvwzorgkostentab)
cohort_dat <- cohort_dat %>%
  mutate(pharma             = ifelse(is.na(pharma), 0, pharma),
         basic_mhc          = ifelse(is.na(basic_mhc), 0, basic_mhc),
         specialist_mhc     = ifelse(is.na(specialist_mhc), 0, specialist_mhc),
         mhc                = ifelse(is.na(mhc), 0, mhc),
         hospital           = ifelse(is.na(hospital), 0, hospital),
         total_health_costs = ifelse(is.na(total_health_costs), 0, total_health_costs))


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

#Loop for postcodes at ages 1 to 21
for (age in cfg$neighborhood_composition_age_min:cfg$neighborhood_composition_age_max){
  # neighborhood composition at pc4 level 
  neighborhood_cohort_dat <- neighborhood_cohort_dat %>%
    group_by(!!sym(paste0("postcode4_age", age))) %>%
    mutate(
      !!paste0("age", age, "_neighborhood_income_below_25th") := if_else(is.na(!!sym(paste0("postcode4_age", age))), NA_real_, hold_out_means(below_p25)),
      !!paste0("age", age, "_neighborhood_income_below_50th") := if_else(is.na(!!sym(paste0("postcode4_age", age))), NA_real_, hold_out_means(below_p50)),
      !!paste0("age", age, "_neighborhood_income_above_75th") := if_else(is.na(!!sym(paste0("postcode4_age", age))), NA_real_, hold_out_means(above_p75)),
      !!paste0("age", age, "_neighborhood_foreign_born_parents") := if_else(is.na(!!sym(paste0("postcode4_age", age))), NA_real_, hold_out_means(foreign_born_parents))
    ) %>% ungroup()
}

neighborhood_cohort_dat <- neighborhood_cohort_dat %>%
  select(c("RINPERSOONS", "RINPERSOON", 
           matches(paste0("^age(", 
                          paste(cfg$neighborhood_composition_age_min:cfg$neighborhood_composition_age_max, collapse= "|"),
                          ")_neighborhood"))))

cohort_dat <- cohort_dat %>%
  left_join(neighborhood_cohort_dat, by = c("RINPERSOONS", "RINPERSOON"))

rm(neighborhood_cohort_dat)

#### REMOVE OBSERVATIONS WITH MISSING OUTCOMES ####
outcomes <- c("high_school_attained", "hbo_followed", "uni_followed")

cohort_dat <- cohort_dat %>%
  filter(rowSums(is.na(select(., all_of(outcomes)))) == 0)


#### PREFIX ####

# add prefix to outcomes
outcomes <- c("high_school_attained", "hbo_followed", "uni_followed", 
              "living_with_parents", "young_parents", 
              "total_health_costs", "basic_mhc", "specialist_mhc", 
              "mhc", "hospital", "pharma" ,
              "primary_class_foreign_born_parents", "primary_class_income_below_25th",
              "primary_class_income_below_50th", "primary_class_income_above_75th",
              "secondary_class_foreign_born_parents", "secondary_class_income_below_25th",
              "secondary_class_income_below_50th", "secondary_class_income_above_75th",
              "age1_neighborhood_income_below_25th", "age1_neighborhood_income_below_50th", "age1_neighborhood_income_above_75th",
              "age1_neighborhood_foreign_born_parents",  "age2_neighborhood_income_below_25th",     "age2_neighborhood_income_below_50th",
              "age2_neighborhood_income_above_75th",     "age2_neighborhood_foreign_born_parents",  "age3_neighborhood_income_below_25th",
              "age3_neighborhood_income_below_50th",     "age3_neighborhood_income_above_75th",     "age3_neighborhood_foreign_born_parents",
              "age4_neighborhood_income_below_25th",     "age4_neighborhood_income_below_50th",     "age4_neighborhood_income_above_75th",
              "age4_neighborhood_foreign_born_parents",  "age5_neighborhood_income_below_25th",     "age5_neighborhood_income_below_50th",
              "age5_neighborhood_income_above_75th",     "age5_neighborhood_foreign_born_parents",  "age6_neighborhood_income_below_25th",
              "age6_neighborhood_income_below_50th",     "age6_neighborhood_income_above_75th",     "age6_neighborhood_foreign_born_parents",
              "age7_neighborhood_income_below_25th",     "age7_neighborhood_income_below_50th",     "age7_neighborhood_income_above_75th",
              "age7_neighborhood_foreign_born_parents",  "age8_neighborhood_income_below_25th",     "age8_neighborhood_income_below_50th",
              "age8_neighborhood_income_above_75th",     "age8_neighborhood_foreign_born_parents",  "age9_neighborhood_income_below_25th",
              "age9_neighborhood_income_below_50th",     "age9_neighborhood_income_above_75th",     "age9_neighborhood_foreign_born_parents",
              "age10_neighborhood_income_below_25th",    "age10_neighborhood_income_below_50th",    "age10_neighborhood_income_above_75th",
              "age10_neighborhood_foreign_born_parents", "age11_neighborhood_income_below_25th",    "age11_neighborhood_income_below_50th",
              "age11_neighborhood_income_above_75th",    "age11_neighborhood_foreign_born_parents", "age12_neighborhood_income_below_25th",
              "age12_neighborhood_income_below_50th",    "age12_neighborhood_income_above_75th",    "age12_neighborhood_foreign_born_parents",
              "age13_neighborhood_income_below_25th",    "age13_neighborhood_income_below_50th",    "age13_neighborhood_income_above_75th",
              "age13_neighborhood_foreign_born_parents", "age14_neighborhood_income_below_25th",    "age14_neighborhood_income_below_50th",
              "age14_neighborhood_income_above_75th",    "age14_neighborhood_foreign_born_parents", "age15_neighborhood_income_below_25th",
              "age15_neighborhood_income_below_50th",    "age15_neighborhood_income_above_75th",    "age15_neighborhood_foreign_born_parents",
              "age16_neighborhood_income_below_25th",    "age16_neighborhood_income_below_50th",    "age16_neighborhood_income_above_75th",
              "age16_neighborhood_foreign_born_parents", "age17_neighborhood_income_below_25th",    "age17_neighborhood_income_below_50th",
              "age17_neighborhood_income_above_75th",    "age17_neighborhood_foreign_born_parents", "age18_neighborhood_income_below_25th",
              "age18_neighborhood_income_below_50th",    "age18_neighborhood_income_above_75th",    "age18_neighborhood_foreign_born_parents",
              "age19_neighborhood_income_below_25th",    "age19_neighborhood_income_below_50th",    "age19_neighborhood_income_above_75th",
              "age19_neighborhood_foreign_born_parents", "age20_neighborhood_income_below_25th",    "age20_neighborhood_income_below_50th",
              "age20_neighborhood_income_above_75th",    "age20_neighborhood_foreign_born_parents", "age21_neighborhood_income_below_25th",
              "age21_neighborhood_income_below_50th",    "age21_neighborhood_income_above_75th",    "age21_neighborhood_foreign_born_parents"
              )

suffix <- "c21_"


# rename outcomes
cohort_dat <- 
  cohort_dat %>%
  rename_with(~str_c(suffix, .), .cols = all_of(outcomes)) %>% 
  ungroup() %>%
  #remove parents birth country
  select(-c(GBAGEBOORTELANDMOEDER, GBAGEBOORTELANDVADER))

# record sample size
sample_size <- sample_size %>% 
  mutate(n_6_child_outcomes = nrow(cohort_dat))


#### WRITE OUTPUT TO SCRATCH ####
write_rds(cohort_dat, file.path(loc$scratch_folder, "03_outcomes.rds"))

#write sample size reduction table to scratch
sample_size <- sample_size %>% mutate(cohort_name = cohort)
write_rds(sample_size, file.path(loc$scratch_folder, "03_sample_size.rds"))
