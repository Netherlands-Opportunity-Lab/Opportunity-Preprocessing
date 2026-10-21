# =============================================================================
# JGZ pilot -- 06. Catchment-area sample for the bereik (reach) analyses
#
# Rebuilds Coen's Data_CatchmentArea_3pooled_withvariables.dta (Sept 2020): one
# row per child who lived in the catchment area of one JGZ organisation during
# the 2y, 3y and 3y9m contact moments, with whether the JGZ saw the child at
# those moments and with child and family characteristics. The bereik
# regression do-file ("Uni and Multi regressions Do.do") and the descriptives
# script start from this file.
#
#   Input : JGZ panel (output of 00), GBAPERSOONTAB, GBAADRESOBJECTBUS,
#           VSLPOSTCODEBUS, KINDOUDERTAB, IPI/INPATAB, INHATAB,
#           HOOGSTEOPLTAB, ZVWZORGKOSTENTAB, the PC4 -> organisation file and
#           LANDAKTUEELREF (resources/)
#   Output: loc$catch_sample_file (.dta, plus .rds) and a sample-flow table
#
# Coen's do-files are not available. The steps follow the do-file overview
# ("SAMPLE SELECTION", "PREPARE CHILD AND FAMILY CHARACTERISTICS", "ANALYSIS
# 3 CONTACT MOMENTS") and the variable list of his dataset. Variable names are
# kept so the regression do-file runs unchanged. Choices that could not be
# checked are settings in config/jgz.yml (catch_*) and marked [Cx] below:
#   [C1] windows: 2y 22-33, 3y 34-42, 3y9m 43-53 months (overall 22-53 is documented)
#   [C2] continuous residence in one organisation's area over the whole window,
#        in calendar months; spells continuous when the gap is <= 1 month
#   [C3] an area counts only between aanvang/einde (PC4 file) and not after
#        the organisation's last contact month in the JGZ data
#   [C4] observed = any JGZ panel row in the window (any organisation)
#   [C5] "cm_" variables, addresses and postcode_4 at age 36 months
#   [C6] health costs: sum of all ZVWK* columns (negatives set to 0) minus
#        GP registration fee and maternity care, as in the other pipelines of
#        this repository, floored at 0; mental care = any GGZ cost > 0
#   [C7] ETNGRP from GBAHERKOMSTGROEPERING (CBS migration background, 8 groups)
#   [C8] pa_/ma_ISCED2011LevelHB is not rebuilt (needs the OPLEIDINGSNRREF
#        reference file) and is written as an empty column; the regressions
#        use c_educ_hbo_wo, which comes from the OPLNIVSOI2016 variables
#
# Run from the repository root. Reading the full GBA address and postcode
# files takes a lot of memory; they are filtered to the relevant persons and
# addresses right after reading.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages({
  library(haven)
  library(openxlsx)
})
options(scipen = 999)


# ---- Helpers --------------------------------------------------------------------

cbs_path <- function(...) file.path(loc$data_folder, ...)

# Latest version of a yearly CBS file
latest_file <- function(folder, pattern) {
  fl <- list.files(cbs_path(folder), pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(fl) == 0) stop("No file matching ", pattern, " in ", cbs_path(folder))
  sort(fl, decreasing = TRUE)[1]
}

# RINPERSOONS etc. as plain text, whatever haven made of it
as_chr <- function(x) trimws(as.character(zap_labels(x)))

# Dates and calendar months. Month index = year * 12 + month - 1 (like Stata %tm)
ymd_date  <- function(x) as.Date(as_chr(x), format = "%Y%m%d")
month_idx <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m")) - 1L
add_months <- function(d, m) {   # same day m months later (end of month if needed)
  lt <- as.POSIXlt(d)
  first <- as.Date(sprintf("%04d-%02d-01", lt$year + 1900 + (lt$mon + m) %/% 12, (lt$mon + m) %% 12 + 1))
  last_day <- as.integer(format(as.Date(format(first + 31, "%Y-%m-01")) - 1, "%d"))
  first + pmin(lt$mday, last_day) - 1
}

n_flow <- list()
log_n <- function(step, n) {
  n_flow[[length(n_flow) + 1]] <<- data.frame(step = step, n = n)
  message(sprintf("%-60s %s", step, format(n, big.mark = ",")))
}

# Address (soortobjectnummer + rinobjectnummer) of persons on given dates
address_on <- function(persons, adres) {
  persons %>%
    inner_join(adres, by = c("RINPERSOONS", "RINPERSOON")) %>%
    filter(date >= start, date <= end) %>%
    arrange(RINPERSOONS, RINPERSOON, desc(start)) %>%
    distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
    select(RINPERSOONS, RINPERSOON, soortobjectnummer, rinobjectnummer)
}


# =============================================================================
# 1. CANDIDATE CHILDREN AND THEIR PARENTS
# =============================================================================

gba_cols <- c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTELAND", "GBAGESLACHT",
              "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "GBAAANTALOUDERSBUITENLAND",
              "GBAHERKOMSTGROEPERING", "GBAGENERATIE",
              "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG")
gba <- read_sav(cbs_path(loc$catch_gba_persoon_file), col_select = any_of(gba_cols))
herkomst_label <- as.character(as_factor(gba$GBAHERKOMSTGROEPERING, levels = "labels"))
if (all(grepl("^[0-9]+$", na.omit(herkomst_label)))) {
  warning("GBAHERKOMSTGROEPERING has no value labels: ETNGRP cannot be derived (all 7)")
}
gba <- gba %>%
  mutate(herkomst_label = herkomst_label) %>%
  mutate(across(c(RINPERSOONS, RINPERSOON), as_chr),
         birthdate = as.Date(sprintf("%s-%s-%s", as_chr(GBAGEBOORTEJAAR),
                                     as_chr(GBAGEBOORTEMAAND), as_chr(GBAGEBOORTEDAG))))

kids <- gba %>%
  filter(birthdate >= as.Date(cfg$catch_birth_date_min),
         birthdate <= as.Date(cfg$catch_birth_date_max))
log_n("Children born in the birth-date range (GBA)", nrow(kids))

kindouder <- read_sav(cbs_path(loc$catch_kindouder_file)) %>%
  mutate(across(everything(), as_chr)) %>%
  mutate(across(everything(), function(x) ifelse(x == "", NA, x)))
names(kindouder) <- sub("^RINPERSOONSMA$", "RINPERSOONSMa", sub("^RINPERSOONMA$", "RINPERSOONMa",
                    sub("^RINPERSOONSPA$", "RINPERSOONSpa", sub("^RINPERSOONPA$", "RINPERSOONpa", names(kindouder)))))

kids <- kids %>% left_join(kindouder, by = c("RINPERSOONS", "RINPERSOON"))


# =============================================================================
# 2. RESIDENCE IN A CATCHMENT AREA  [C2, C3]
# =============================================================================

# PC4 -> organisation, with coverage period in months
pc4_org <- read.csv(loc$catch_pc4_org_file, colClasses = "character") %>%
  transmute(
    org_pc4_area = as.integer(org), gemeente, postcode_4 = as.integer(postcode_4),
    cover_start = month_idx(ymd_date(aanvang)),
    cover_end   = month_idx(ymd_date(ifelse(einde %in% c(".", ""), "29991231", einde)))
  )
if (anyDuplicated(pc4_org$postcode_4)) {
  warning("PC4 in more than one row of the PC4 file: ",
          paste(unique(pc4_org$postcode_4[duplicated(pc4_org$postcode_4)]), collapse = ", "),
          " -- the first row is used")
  pc4_org <- pc4_org[!duplicated(pc4_org$postcode_4), ]
}

# End of the data delivery per organisation: last contact month in the panel
panel <- readRDS(loc$panel_file) %>%
  mutate(contact_date = as.Date(dat_a_str, format = "%Y%m%d"))
if (isTRUE(cfg$catch_use_data_end)) {
  data_end <- panel %>%
    filter(!is.na(contact_date)) %>%
    group_by(org_pc4_area = org) %>%
    summarise(data_end = max(month_idx(contact_date)), .groups = "drop")
  print(data_end)
  pc4_org <- pc4_org %>%
    left_join(data_end, by = "org_pc4_area") %>%
    mutate(cover_end = pmin(cover_end, coalesce(data_end, cover_end))) %>%
    select(-data_end)
}

# Address history of the candidate children (and later their parents)
persons <- unique(c(kids$RINPERSOON, kids$RINPERSOONpa, kids$RINPERSOONMa))
adres <- read_sav(cbs_path(loc$catch_gba_adres_file)) %>%
  rename_with(toupper) %>%
  mutate(RINPERSOON = as_chr(RINPERSOON)) %>%
  filter(RINPERSOON %in% persons) %>%
  transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON,
            start = ymd_date(GBADATUMAANVANGADRESHOUDING),
            end   = coalesce(ymd_date(GBADATUMEINDEADRESHOUDING), as.Date("2999-12-31")),
            soortobjectnummer = as_chr(SOORTOBJECTNUMMER),
            rinobjectnummer   = as_chr(RINOBJECTNUMMER))

# Postcode of each address over time (VSLPOSTCODEBUS with validity dates)
objects <- unique(paste(adres$soortobjectnummer, adres$rinobjectnummer))
postcodes <- read_sav(cbs_path(loc$catch_vslpostcode_file)) %>%
  rename_with(toupper) %>%
  mutate(soortobjectnummer = as_chr(SOORTOBJECTNUMMER), rinobjectnummer = as_chr(RINOBJECTNUMMER)) %>%
  filter(paste(soortobjectnummer, rinobjectnummer) %in% objects) %>%
  transmute(soortobjectnummer, rinobjectnummer,
            pc_start = ymd_date(DATUMAANVPOSTCODENUMADRES),
            pc_end   = coalesce(ymd_date(DATUMEINDPOSTCODENUMADRES), as.Date("2999-12-31")),
            postcode_4 = suppressWarnings(as.integer(as_chr(POSTCODENUM))))

# Child address spells x postcode periods x coverage period, in months
kid_spells <- adres %>%
  semi_join(kids, by = c("RINPERSOONS", "RINPERSOON")) %>%
  inner_join(postcodes, by = c("soortobjectnummer", "rinobjectnummer")) %>%
  mutate(s = pmax(start, pc_start), e = pmin(end, pc_end)) %>%
  filter(s <= e) %>%
  inner_join(pc4_org, by = "postcode_4") %>%
  mutate(s = pmax(month_idx(s), cover_start), e = pmin(month_idx(e), cover_end)) %>%
  filter(s <= e) %>%
  select(RINPERSOONS, RINPERSOON, org_pc4_area, s, e)

# Merge consecutive spells within the same organisation's area
residence <- kid_spells %>%
  arrange(RINPERSOONS, RINPERSOON, org_pc4_area, s) %>%
  group_by(RINPERSOONS, RINPERSOON, org_pc4_area) %>%
  mutate(prev_max_end = lag(cummax(e)),
         new_period = is.na(prev_max_end) | s > prev_max_end + cfg$catch_max_gap_months,
         period = cumsum(new_period)) %>%
  group_by(RINPERSOONS, RINPERSOON, org_pc4_area, period) %>%
  summarise(t_residence_start = min(s), t_residence_end = max(e), .groups = "drop")

# Whole window covered by one residence period in one organisation's area
windows <- cfg$catch_cm_windows
window_first <- min(unlist(windows))
window_last  <- max(unlist(windows))

sample <- kids %>%
  mutate(t_child_birth = month_idx(birthdate)) %>%
  inner_join(residence, by = c("RINPERSOONS", "RINPERSOON")) %>%
  filter(t_residence_start <= t_child_birth + window_first,
         t_residence_end   >= t_child_birth + window_last) %>%
  rename(org_catchment = org_pc4_area) %>%
  arrange(RINPERSOONS, RINPERSOON, t_residence_start) %>%
  distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
  select(-period)
log_n(sprintf("... living in one catchment area from month %d to %d", window_first, window_last),
      nrow(sample))
print(table(org_catchment = sample$org_catchment))


# =============================================================================
# 3. OBSERVED BY THE JGZ AT THE CONTACT MOMENTS  [C1, C4]
# =============================================================================

contacts <- panel %>%
  filter(!is.na(contact_date)) %>%
  select(RINPERSOONS, RINPERSOON, contact_date, leeftijd, org) %>%
  inner_join(sample %>% select(RINPERSOONS, RINPERSOON, t_child_birth, org_catchment),
             by = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(t_contact = month_idx(contact_date),
         age_month_contact = t_contact - t_child_birth)
if (isTRUE(cfg$catch_observed_same_org)) contacts <- contacts %>% filter(org == org_catchment)

for (k in names(windows)) {
  w <- windows[[k]]
  seen <- contacts %>%
    filter(age_month_contact >= w[1], age_month_contact <= w[2]) %>%
    distinct(RINPERSOONS, RINPERSOON) %>%
    mutate(seen = 1)
  sample <- sample %>%
    left_join(seen, by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(seen = coalesce(seen, 0))
  names(sample)[names(sample) == "seen"] <- paste0("d_observed_", k)
}

sample <- sample %>%
  mutate(
    n_seen = d_observed_2y + d_observed_3y + d_observed_3y9m,
    d_observed_3cms = as.numeric(n_seen > 0),     # ever seen
    d_obs_always    = as.numeric(n_seen == 3),    # seen at all three
    # obs_cat_label: 1 Never 2 Always 3 2y 4 3y 5 3y9m 6 2y-3y 7 2y-3y9m 8 3y-3y9m
    d_obs_cat = case_when(
      n_seen == 0 ~ 1, n_seen == 3 ~ 2,
      n_seen == 1 & d_observed_2y == 1   ~ 3,
      n_seen == 1 & d_observed_3y == 1   ~ 4,
      n_seen == 1 & d_observed_3y9m == 1 ~ 5,
      d_observed_3y9m == 0 ~ 6,
      d_observed_3y == 0   ~ 7,
      d_observed_2y == 0   ~ 8
    )
  ) %>%
  select(-n_seen)

# Last contact in the window (org, age, length, weight), as in Coen's file
last_contact <- contacts %>%
  filter(age_month_contact >= window_first, age_month_contact <= window_last) %>%
  arrange(RINPERSOONS, RINPERSOON, desc(contact_date)) %>%
  distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
  select(RINPERSOONS, RINPERSOON, leeftijd, org, t_contact, age_month_contact)
sample <- sample %>% left_join(last_contact, by = c("RINPERSOONS", "RINPERSOON"))
print(table(d_obs_cat = sample$d_obs_cat, org_catchment = sample$org_catchment))


# =============================================================================
# 4. CHILD AND FAMILY CHARACTERISTICS
# =============================================================================

sample <- sample %>%
  mutate(child_date_cm = add_months(birthdate, cfg$catch_reference_age_months),   # [C5]
         year_date_cm  = as.integer(format(child_date_cm, "%Y")),
         birth_year    = as.integer(format(birthdate, "%Y")))

# --- Parents: birth dates, sex; birth order and mother's age at births ----------
parent_gba <- gba %>% select(RINPERSOONS, RINPERSOON, GBAGESLACHT, birthdate)
sample <- sample %>%
  left_join(parent_gba %>% rename(RINPERSOONSMa = RINPERSOONS, RINPERSOONMa = RINPERSOON,
                                  gbageslachtmoeder = GBAGESLACHT, birthdate_ma = birthdate),
            by = c("RINPERSOONSMa", "RINPERSOONMa")) %>%
  left_join(parent_gba %>% rename(RINPERSOONSpa = RINPERSOONS, RINPERSOONpa = RINPERSOON,
                                  gbageslachtvader = GBAGESLACHT, birthdate_pa = birthdate),
            by = c("RINPERSOONSpa", "RINPERSOONpa"))

# All children of the mothers in the sample (any birth year)
siblings <- kindouder %>%
  filter(RINPERSOONMa %in% sample$RINPERSOONMa) %>%
  select(RINPERSOONSMa, RINPERSOONMa, RINPERSOONS, RINPERSOON) %>%
  inner_join(gba %>% select(RINPERSOONS, RINPERSOON, sib_birth = birthdate),
             by = c("RINPERSOONS", "RINPERSOON")) %>%
  group_by(RINPERSOONSMa, RINPERSOONMa) %>%
  mutate(birth_order = rank(sib_birth, ties.method = "min"),
         first_birth = min(sib_birth)) %>%
  ungroup()
sample <- sample %>%
  left_join(siblings %>% select(RINPERSOONS, RINPERSOON, birth_order, first_birth),
            by = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(age_birth_child    = month_idx(birthdate) - month_idx(birthdate_ma),    # months
         age_birth_first_ma = month_idx(first_birth) - month_idx(birthdate_ma))  # months

# --- Migration background (ETNGRP, 8 groups)  [C7] --------------------------------
land_ref <- read_sav(loc$catch_land_ref_file) %>% mutate(across(everything(), as_chr))
caribbean <- c("Nederlandse Antillen (oud)", "Aruba", "Cura\u00e7ao", "Bonaire",
               "Sint Maarten", "Sint Eustatius", "Saba", "Caribisch Nederland")
sample <- sample %>%
  mutate(
    LAND = herkomst_label,
    ETNGRP = case_when(
      is.na(LAND) | LAND %in% c("", "Onbekend") ~ 7,
      LAND == "Nederland" ~ 0, LAND == "Marokko" ~ 1, LAND == "Turkije" ~ 2,
      LAND == "Suriname" ~ 3, LAND %in% caribbean ~ 4,
      LAND %in% land_ref$LAND[land_ref$LANDTYPE == "NietWesters"] ~ 5,
      LAND %in% land_ref$LAND[land_ref$LANDTYPE == "Westers"] ~ 6,
      TRUE ~ 7
    )
  )
unmatched <- setdiff(unique(sample$LAND[sample$ETNGRP == 7]), c(NA, "", "Onbekend"))
if (length(unmatched) > 0) {
  message("Countries not in LANDAKTUEELREF (set to 7 Onbekend): ", paste(unmatched, collapse = ", "))
}

# --- Addresses on the reference date: child, father, mother; postcode_4 ------
ref_dates <- sample %>% select(RINPERSOONS, RINPERSOON, date = child_date_cm)
for (who in c("child", "pa", "ma")) {
  p <- switch(who,
    child = ref_dates,
    # `child` first: transmute() evaluates in order, so after
    # RINPERSOON = RINPERSOONpa the name RINPERSOON means the parent
    pa = sample %>% transmute(child = RINPERSOON, RINPERSOONS = RINPERSOONSpa,
                              RINPERSOON = RINPERSOONpa, date = child_date_cm),
    ma = sample %>% transmute(child = RINPERSOON, RINPERSOONS = RINPERSOONSMa,
                              RINPERSOON = RINPERSOONMa, date = child_date_cm))
  if (who == "child") {
    a <- address_on(p, adres)
    sample <- sample %>% left_join(a, by = c("RINPERSOONS", "RINPERSOON")) %>%
      rename(soortobjectnummer_child = soortobjectnummer, rinobjectnummer_child = rinobjectnummer)
  } else {
    a <- p %>% filter(!is.na(RINPERSOON)) %>%
      inner_join(adres, by = c("RINPERSOONS", "RINPERSOON")) %>%
      filter(date >= start, date <= end) %>%
      arrange(child, desc(start)) %>% distinct(child, .keep_all = TRUE) %>%
      select(child, soortobjectnummer, rinobjectnummer)
    names(a)[2:3] <- paste0(c("soortobjectnummer_", "rinobjectnummer_"), who)
    sample <- sample %>% left_join(a, by = c("RINPERSOON" = "child"))
  }
}
child_pc <- sample %>%
  select(RINPERSOON, soortobjectnummer = soortobjectnummer_child,
         rinobjectnummer = rinobjectnummer_child, date = child_date_cm) %>%
  inner_join(postcodes, by = c("soortobjectnummer", "rinobjectnummer")) %>%
  filter(date >= pc_start, date <= pc_end) %>%
  distinct(RINPERSOON, .keep_all = TRUE) %>%
  select(RINPERSOON, postcode_4)
sample <- sample %>%
  left_join(child_pc, by = "RINPERSOON") %>%
  mutate(soortobjectnummer = soortobjectnummer_child,   # used by the regression do-file
         rinobjectnummer   = rinobjectnummer_child)

# --- Personal income parents: birth year and reference year ------------------------
read_income <- function(year, rins) {
  if (year < 2011) {
    f <- latest_file(file.path("InkomenBestedingen/INTEGRAAL PERSOONLIJK INKOMEN", year),
                     paste0("PERSOONINK", year, "TABV[0-9]+\\.sav$"))
    d <- read_sav(f, col_select = any_of(c("RINPERSOONS", "RINPERSOON", "PERSBRUT", "PERSSOCECJ")))
    names(d) <- sub("^PERSBRUT$", "inppersbrut", sub("^PERSSOCECJ$", "inpsecj", names(d)))
  } else {
    f <- latest_file("InkomenBestedingen/INPATAB", paste0("INPA", year, "TABV[0-9]+\\.sav$"))
    d <- read_sav(f, col_select = any_of(c("RINPERSOONS", "RINPERSOON", "INPPERSBRUT", "INPSECJ")))
    names(d) <- sub("^INPPERSBRUT$", "inppersbrut", sub("^INPSECJ$", "inpsecj", names(d)))
  }
  if (!"inpsecj" %in% names(d)) {
    message("No socio-economic category in the income file for ", year)
    d$inpsecj <- NA_character_
  }
  d %>%
    mutate(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON)) %>%
    filter(RINPERSOON %in% rins) %>%
    mutate(inppersbrut = as.numeric(zap_labels(inppersbrut)),
           inppersbrut = ifelse(inppersbrut %in% c(999999999, 9999999999), NA, inppersbrut),
           inpsecj = as_chr(inpsecj),
           year = year)
}
parents <- unique(na.omit(c(sample$RINPERSOONpa, sample$RINPERSOONMa)))
income <- bind_rows(lapply(sort(unique(c(sample$birth_year, sample$year_date_cm))),
                           read_income, rins = parents))

add_income <- function(df, prefix, year_col) {
  for (who in c("pa", "ma")) {
    inc <- income %>% select(RINPERSOONS, RINPERSOON, year, inppersbrut, inpsecj)
    names(inc) <- c(paste0("RINPERSOONS", if (who == "pa") "pa" else "Ma"),
                    paste0("RINPERSOON", if (who == "pa") "pa" else "Ma"),
                    year_col, paste0(prefix, "_inppersbrut", who), paste0(prefix, "_inpsecj", who))
    df <- df %>% left_join(inc, by = names(inc)[1:3])
  }
  df
}
sample <- add_income(sample, "birth", "birth_year")
sample <- add_income(sample, "cm", "year_date_cm")

# --- Household income on the reference year (INHATAB) ---------------------------
hh <- bind_rows(lapply(sort(unique(sample$year_date_cm)), function(year) {
  koppel <- read_sav(latest_file("InkomenBestedingen/INHATAB", paste0("KOPPELPERSOONHUISHOUDEN", year))) %>%
    rename_with(toupper) %>%
    transmute(RINPERSOON = as_chr(RINPERSOON), rinpersoonshkw = as_chr(RINPERSOONSHKW),
              rinpersoonhkw = as_chr(RINPERSOONHKW)) %>%
    filter(RINPERSOON %in% sample$RINPERSOON)
  inha <- read_sav(latest_file("InkomenBestedingen/INHATAB", paste0("INHA", year, "TABV[0-9]+\\.sav$")),
                   col_select = any_of(c("RINPERSOONSHKW", "RINPERSOONHKW", "INHBRUTINKH"))) %>%
    transmute(rinpersoonshkw = as_chr(RINPERSOONSHKW), rinpersoonhkw = as_chr(RINPERSOONHKW),
              inhbrutinkh = as.numeric(INHBRUTINKH))
  koppel %>% left_join(inha, by = c("rinpersoonshkw", "rinpersoonhkw")) %>% mutate(year_date_cm = year)
}))
sample <- sample %>% left_join(hh, by = c("RINPERSOON", "year_date_cm"))

# --- Education parents (HOOGSTEOPLTAB)  [C8] -------------------------------------
edu_year <- cfg$catch_education_year
edu <- read_sav(latest_file(file.path("Onderwijs/HOOGSTEOPLTAB", edu_year),
                            paste0("HOOGSTEOPL", edu_year, "TABV[0-9]+\\.sav$")),
                col_select = c("RINPERSOONS", "RINPERSOON", any_of(cfg$catch_education_cols))) %>%
  mutate(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON)) %>%
  filter(RINPERSOON %in% parents) %>%
  mutate(across(-c(RINPERSOONS, RINPERSOON), function(x) {
    x <- as_chr(x); x[x %in% c("----", "")] <- NA
    num <- suppressWarnings(as.numeric(x))
    if (all(is.na(num) == is.na(x))) num else x
  }))
missing_cols <- setdiff(cfg$catch_education_cols, names(edu))
if (length(missing_cols) > 0) message("Not in HOOGSTEOPLTAB ", edu_year, ": ", paste(missing_cols, collapse = ", "))
for (who in c("pa", "ma")) {
  e <- edu
  names(e) <- c(paste0("RINPERSOONS", if (who == "pa") "pa" else "Ma"),
                paste0("RINPERSOON", if (who == "pa") "pa" else "Ma"),
                paste0(who, "_", names(edu)[-(1:2)]))
  sample <- sample %>% left_join(e, by = names(e)[1:2])
}
# The do-file looks for OPLNIVSOI2016AGG4HBMETNIRWO under this exact name
stopifnot(all(c("pa_OPLNIVSOI2016AGG4HBMETNIRWO", "ma_OPLNIVSOI2016AGG4HBMETNIRWO") %in% names(sample)))

# --- Health costs (ZVWZORGKOSTENTAB)  [C6] -------------------------------------------
read_health <- function(year, rins) {
  f <- latest_file(file.path("GezondheidWelzijn/ZVWZORGKOSTENTAB", year),
                   paste0("ZVWZORGKOSTEN", year, "TABV[0-9]+\\.sav$"))
  d <- read_sav(f, col_select = c("RINPERSOONS", "RINPERSOON", starts_with("ZVWK"),
                                  any_of("NOPZVWKHUISARTSINSCHRIJF"))) %>%
    zap_labels() %>%
    mutate(RINPERSOON = as_chr(RINPERSOON)) %>%
    filter(RINPERSOON %in% rins)
  cost_cols <- grep("^ZVWK", names(d), value = TRUE)
  ggz_cols  <- grep(cfg$catch_mental_care_pattern, cost_cols, value = TRUE)
  costs <- as.matrix(d[cost_cols]); costs[is.na(costs) | costs < 0] <- 0
  out <- data.frame(RINPERSOON = d$RINPERSOON, stringsAsFactors = FALSE)
  out$tot_health_exp <- pmax(0, rowSums(costs) -
    (if ("NOPZVWKHUISARTSINSCHRIJF" %in% names(d)) coalesce(as.numeric(d$NOPZVWKHUISARTSINSCHRIJF), 0) else 0) -
    (if ("ZVWKGEBOORTEZORG" %in% cost_cols) costs[, "ZVWKGEBOORTEZORG"] else 0))
  out$d_mental_care <- as.numeric(if (length(ggz_cols)) rowSums(costs[, ggz_cols, drop = FALSE]) > 0 else NA)
  out$year <- year
  out
}
health_years <- sort(unique(c(sample$year_date_cm, sample$birth_year + 2, sample$birth_year + 4)))
health <- bind_rows(lapply(health_years, read_health,
                           rins = unique(c(sample$RINPERSOON, parents))))

add_health <- function(df, id_col, year, suffix_from) {
  h <- health %>% rename(!!id_col := RINPERSOON)
  names(h)[names(h) %in% c("tot_health_exp", "d_mental_care")] <-
    paste0(suffix_from, c("tot_health_exp", "d_mental_care"))
  df %>% left_join(h, by = setNames(c(id_col, "year"), c(id_col, year)))
}
sample <- sample %>%
  mutate(year_2y = birth_year + 2, year_4y = birth_year + 4) %>%
  add_health("RINPERSOON", "year_date_cm", "child_") %>%
  left_join(health %>% transmute(RINPERSOON, year_2y = year,
                                 child_tot_health_exp_2y = tot_health_exp,
                                 child_d_mental_care_2y = d_mental_care),
            by = c("RINPERSOON", "year_2y")) %>%
  left_join(health %>% transmute(RINPERSOON, year_4y = year,
                                 child_tot_health_exp_4y = tot_health_exp,
                                 child_d_mental_care_4y = d_mental_care),
            by = c("RINPERSOON", "year_4y")) %>%
  add_health("RINPERSOONpa", "year_date_cm", "pa_") %>%
  add_health("RINPERSOONMa", "year_date_cm", "ma_") %>%
  select(-year_2y, -year_4y)


# =============================================================================
# 5. OUTPUT WITH COEN'S VARIABLE NAMES
# =============================================================================

out <- sample %>%
  transmute(
    rinpersoons = RINPERSOONS, rinpersoon = RINPERSOON,
    gbageboorteland = as_chr(GBAGEBOORTELAND), gbageslacht = as_chr(GBAGESLACHT),
    gbageboortelandmoeder = as_chr(GBAGEBOORTELANDMOEDER),
    gbageboortelandvader = as_chr(GBAGEBOORTELANDVADER),
    gbaaantaloudersbuitenland = as_chr(GBAAANTALOUDERSBUITENLAND),
    gbaherkomstgroepering = as.numeric(as_chr(GBAHERKOMSTGROEPERING)),
    gbageneratie = as_chr(GBAGENERATIE),
    gbageboortejaar = birth_year,
    gbageboortemaand = as.integer(format(birthdate, "%m")),
    gbageboortedag = format(birthdate, "%d"),
    gbageslachtmoeder = as_chr(gbageslachtmoeder), gbageslachtvader = as_chr(gbageslachtvader),
    gbageboortejaarmoeder = format(birthdate_ma, "%Y"), gbageboortemaandmoeder = format(birthdate_ma, "%m"),
    gbageboortejaarvader  = format(birthdate_pa, "%Y"), gbageboortemaandvader  = format(birthdate_pa, "%m"),
    postcode_4, t_child_birth, t_residence_start, t_residence_end,
    org_catchment,
    leeftijd, org, t_contact, age_month_contact,
    # d_obs_always is NOT included: the regression do-file creates it
    # (gen d_obs_always = (d_obs_cat==2)) and would stop if it already existed
    d_observed_3cms, d_observed_2y, d_observed_3y, d_observed_3y9m, d_obs_cat,
    rinpersoonspa = RINPERSOONSpa, rinpersoonpa = RINPERSOONpa,
    rinpersoonsma = RINPERSOONSMa, rinpersoonma = RINPERSOONMa,
    xkoppelnummer = if ("XKOPPELNUMMER" %in% names(sample)) XKOPPELNUMMER else NA_character_,
    birth_order, age_birth_child, age_birth_first_ma,
    birth_inppersbrutpa, birth_inpsecjpa, birth_inppersbrutma, birth_inpsecjma,
    child_date_cm, year_date_cm,
    cm_inppersbrutpa, cm_inpsecjpa, cm_inppersbrutma, cm_inpsecjma,
    rinpersoonshkw, rinpersoonhkw, inhbrutinkh,
    LAND, ETNGRP,
    soortobjectnummer_pa, rinobjectnummer_pa, soortobjectnummer_ma, rinobjectnummer_ma,
    child_tot_health_exp, child_d_mental_care,
    child_tot_health_exp_2y, child_d_mental_care_2y,
    child_tot_health_exp_4y, child_d_mental_care_4y,
    pa_tot_health_exp, pa_d_mental_care, ma_tot_health_exp, ma_d_mental_care,
    soortobjectnummer_child, rinobjectnummer_child, soortobjectnummer, rinobjectnummer
  ) %>%
  bind_cols(sample %>% select(starts_with("pa_OPL"), starts_with("ma_OPL"),
                              starts_with("pa_RICHT"), starts_with("ma_RICHT"),
                              starts_with("pa_GEWICHT"), starts_with("ma_GEWICHT"))) %>%
  # [C8] not rebuilt; empty so that `tab pa_ISCED2011LevelHB` in the do-file runs
  mutate(pa_ISCED2011LevelHB = NA_real_, ma_ISCED2011LevelHB = NA_real_)

# Municipality: of postcode_4 (the address at the reference age, which lies
# inside the window and therefore inside the catchment area)
out <- out %>%
  left_join(pc4_org %>% distinct(postcode_4, gemeente), by = "postcode_4")

stopifnot(!anyDuplicated(out[, c("rinpersoons", "rinpersoon")]))
log_n("Rows in the output file", nrow(out))
print(table(d_obs_cat = out$d_obs_cat, useNA = "ifany"))

# Stata value labels as in Coen's file
out$d_obs_cat <- labelled(out$d_obs_cat, c(Never = 1, Always = 2, `2y` = 3, `3y` = 4, `3y9m` = 5,
                                           `2y-3y` = 6, `2y-3y9m` = 7, `3y-3y9m` = 8))
out$ETNGRP <- labelled(out$ETNGRP, c(Autochtoon = 0, Marokko = 1, Turkije = 2, Suriname = 3,
                                     `Voormalige Nederlandse Antillen en Aruba` = 4,
                                     `Overige niet-westerse landen` = 5,
                                     `Overige westerse landen` = 6, Onbekend = 7))

ensure_dir(dirname(loc$catch_sample_file))
write_dta(out, loc$catch_sample_file)
saveRDS(out, sub("\\.dta$", ".rds", loc$catch_sample_file))
flow <- do.call(rbind, n_flow)
write.xlsx(flow, sub("\\.dta$", "_flow.xlsx", loc$catch_sample_file))
message("Written: ", loc$catch_sample_file)
