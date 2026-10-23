# =============================================================================
# JGZ preprocessing -- 03. Catchment-area sample for the bereik (reach) analyses
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
# First written without Coen's do-files, from the do-file overview and the
# variable list of his dataset. On 2026-10-23 checked against the do-files
# themselves ("20200916 SAMPLE SELECTION", "20200916 PREPARE CHILD AND FAMILY
# CHARACTERISTICS", "20200913 ANALYSIS 3 CONTACT MOMENTS"). Variable names are
# kept so the regression do-file runs unchanged. Settings in config/jgz.yml
# (catch_*), marked [Cx] below; defaults now follow Coen:
#   [C1] windows 2y 22-33, 3y 34-43, 3y9m 44-53 months (SAMPLE SELECTION)
#   [C2] continuous residence in one organisation's area over the whole
#        window, in calendar months; a spell may start in the month after the
#        previous one ended (Coen: "remove flag for records that start 1
#        month later") -- confirmed
#   [C3] area coverage: Coen uses the end date (einde) of the PC4 file but
#        not the start date (aanvang), and restricts birth cohorts per
#        organisation with a data-driven rule (see [C9]) instead of the last
#        contact month. Both alternatives remain available as settings.
#   [C4] observed = any JGZ panel row in the window, any organisation --
#        confirmed (Coen's Observed_<cm> files)
#   [C5] "cm_" variables, addresses and postcode_4 at age 36 months --
#        confirmed (ANALYSIS 3: local age_months=36). Addresses: the spell
#        that covers that calendar month; if several, the earliest (Coen)
#   [C6] health costs: Coen's sum of 16 ZVWK columns; mental care = any
#        first-line psychology, GGZ or NOP-GGZ cost (lists in the config)
#   [C7] ETNGRP: Coen merged GBAHERKOMSTGROEPERING to LANDAKTUEELREFV9
#        (variable etngrp). Used when that file is available
#        (loc$catch_land_etngrp_file); otherwise built from the country labels
#   [C8] pa_/ma_ISCED2011LevelHB rebuilt as Coen did (OPLNRHB ->
#        OPLEIDINGSNRREF -> CTOREF) when the reference files are available;
#        otherwise an empty column
#   [C9] birth cohorts per organisation (Coen, SAMPLE SELECTION): the share
#        observed per birth month is computed per organisation for the 2y and
#        the 3y9m window; the first cohort is the first month with a drop in
#        that share while it is above 80% (2y), the last cohort the last month
#        with a rise while above 80% (3y9m). For Verian the share excludes
#        Apeldoorn (not in the data); the regression do-file drops Apeldoorn.
#
# Run from the repository root. Reading the full GBA address and postcode
# files takes a lot of memory; they are filtered to the relevant persons and
# addresses right after reading.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages(library(openxlsx))
options(scipen = 999)


# ---- Helpers --------------------------------------------------------------------

# Latest version of a yearly CBS file
latest_file <- function(folder, pattern) {
  fl <- list.files(cbs_path(folder), pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(fl) == 0) stop("No file matching ", pattern, " in ", cbs_path(folder))
  sort(fl, decreasing = TRUE)[1]
}

# Same day m months later (end of month if needed)
add_months <- function(d, m) {
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

# Address spells that cover the calendar month of `date`; when several do
# (a move in that month), the earliest one -- as in Coen's PREPARE do-file
# ("order chronologically and pick the first one"). `id` names the column
# that identifies the row to attach the address to.
address_in_month <- function(persons, adres, id) {
  a <- persons %>%
    inner_join(adres, by = c("RINPERSOONS", "RINPERSOON")) %>%
    filter(month_idx(start) <= month_idx(date), month_idx(end) >= month_idx(date))
  a <- a[order(a[[id]], a$start), ]
  a <- a[!duplicated(a[[id]]), ]
  a[, c(id, "soortobjectnummer", "rinobjectnummer")]
}


# =============================================================================
# 1. CANDIDATE CHILDREN AND THEIR PARENTS
# =============================================================================

gba_cols <- c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTELAND", "GBAGESLACHT",
              "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", "GBAAANTALOUDERSBUITENLAND",
              "GBAHERKOMSTGROEPERING", "GBAGENERATIE",
              "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG")
gba <- read_sav(cbs_path(loc$gba_persoon_file), col_select = any_of(gba_cols))
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

kindouder <- read_sav(cbs_path(loc$kindouder_file)) %>%
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
# Corrections from Coen's SAMPLE SELECTION do-file: PC4 8080 is listed for both
# Verian and Icare; the Verian row is 8180 (Heerde).
for (fix in cfg$catch_pc4_corrections) {
  hit <- pc4_org$org_pc4_area == fix$org & pc4_org$postcode_4 == fix$from
  pc4_org$postcode_4[hit] <- as.integer(fix$to)
  message(sprintf("PC4 correction for org %s: %s -> %s (%d row)", fix$org, fix$from, fix$to, sum(hit)))
}
# [C3] Coen did not use the start date (aanvang) of an area
if (!isTRUE(cfg$catch_use_aanvang)) pc4_org$cover_start <- 0L
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
adres <- read_addresses(persons)

# Postcode of each address over time (VSLPOSTCODEBUS with validity dates).
# (Coen used one postcode per address object, the first registered one; the
# validity dates are more precise and only matter for renumbered addresses.)
objects <- unique(paste(adres$soortobjectnummer, adres$rinobjectnummer))
postcodes <- read_postcodes(objects)

# Child address spells x postcode periods x coverage period, in months
kid_spells <- adres %>%
  semi_join(kids, by = c("RINPERSOONS", "RINPERSOON")) %>%
  inner_join(postcodes, by = c("soortobjectnummer", "rinobjectnummer")) %>%
  mutate(s = pmax(start, pc_start), e = pmin(end, pc_end)) %>%
  filter(s <= e) %>%
  inner_join(pc4_org, by = "postcode_4") %>%
  mutate(s = pmax(month_idx(s), cover_start), e = pmin(month_idx(e), cover_end)) %>%
  filter(s <= e) %>%
  select(RINPERSOONS, RINPERSOON, org_pc4_area, gemeente, s, e)

# Merge consecutive spells within the same organisation's area  [C2]
residence <- kid_spells %>%
  arrange(RINPERSOONS, RINPERSOON, org_pc4_area, s) %>%
  group_by(RINPERSOONS, RINPERSOON, org_pc4_area) %>%
  mutate(prev_max_end = lag(cummax(e)),
         new_period = is.na(prev_max_end) | s > prev_max_end + cfg$catch_max_gap_months,
         period = cumsum(new_period)) %>%
  group_by(RINPERSOONS, RINPERSOON, org_pc4_area, period) %>%
  summarise(t_residence_start = min(s), t_residence_end = max(e),
            gemeente_window = gemeente[which.min(s)], .groups = "drop")

kids <- kids %>% mutate(t_child_birth = month_idx(birthdate))

# Children who live in one organisation's area during the whole window
# [first, last] (months of age): one row per child
in_area <- function(first, last) {
  kids %>%
    select(RINPERSOONS, RINPERSOON, t_child_birth) %>%
    inner_join(residence, by = c("RINPERSOONS", "RINPERSOON")) %>%
    filter(t_residence_start <= t_child_birth + first,
           t_residence_end   >= t_child_birth + last) %>%
    arrange(RINPERSOONS, RINPERSOON, t_residence_start) %>%
    distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
    select(RINPERSOONS, RINPERSOON, t_child_birth, org_catchment = org_pc4_area,
           gemeente_window, t_residence_start, t_residence_end)
}

# JGZ contacts (any organisation, or the catchment organisation only  [C4])
contacts_all <- panel %>%
  filter(!is.na(contact_date)) %>%
  transmute(RINPERSOONS, RINPERSOON, contact_date, leeftijd, org,
            t_contact = month_idx(contact_date))

contacts_of <- function(smp) {
  d <- contacts_all %>%
    inner_join(smp %>% select(RINPERSOONS, RINPERSOON, t_child_birth, org_catchment),
               by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(age_month_contact = t_contact - t_child_birth)
  if (isTRUE(cfg$catch_observed_same_org)) d <- d %>% filter(org == org_catchment)
  d
}

# 1/0: seen at least once in window w (months of age)
seen_in <- function(smp, w) {
  seen <- contacts_of(smp) %>%
    filter(age_month_contact >= w[1], age_month_contact <= w[2]) %>%
    distinct(RINPERSOONS, RINPERSOON) %>%
    mutate(seen = 1)
  smp %>%
    select(RINPERSOONS, RINPERSOON) %>%
    left_join(seen, by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(seen = coalesce(seen, 0))
}

windows <- cfg$catch_cm_windows
window_first <- min(unlist(windows))
window_last  <- max(unlist(windows))


# --- [C9] Birth cohorts per organisation (Coen, SAMPLE SELECTION) ------------------
# Share observed per birth month and organisation, for the children living in
# that organisation's area during the 2y window (first cohort) or the 3y9m
# window (last cohort). First cohort = first birth month where the share drops
# compared with the previous month while above the threshold; last cohort =
# last birth month where it rises while above the threshold.
cohort_limits <- NULL
if (identical(cfg$catch_cohort_rule, "coen")) {
  share_by_birth_month <- function(k) {
    w <- windows[[k]]
    smp <- in_area(w[1], w[2])
    smp <- smp %>% left_join(seen_in(smp, w), by = c("RINPERSOONS", "RINPERSOON"))
    excl <- cfg$catch_cohort_exclude
    if (!is.null(excl)) {
      smp <- smp %>% filter(!(org_catchment == excl$org & gemeente_window %in% excl$gemeente))
    }
    grid <- expand.grid(org_catchment = sort(unique(smp$org_catchment)),
                        t_child_birth = seq(min(kids$t_child_birth), max(kids$t_child_birth)))
    smp %>%
      group_by(org_catchment, t_child_birth) %>%
      summarise(n = n(), share = mean(seen), .groups = "drop") %>%
      right_join(grid, by = c("org_catchment", "t_child_birth")) %>%
      arrange(org_catchment, t_child_birth) %>%
      group_by(org_catchment) %>%
      mutate(delta = share - lag(share)) %>%
      ungroup() %>%
      mutate(window = k)
  }
  shares <- bind_rows(share_by_birth_month("2y"), share_by_birth_month("3y9m"))
  min_share <- cfg$catch_cohort_min_share
  first_c <- shares %>% filter(window == "2y", delta < 0, share > min_share) %>%
    group_by(org_catchment) %>% summarise(first_cohort = min(t_child_birth), .groups = "drop")
  last_c <- shares %>% filter(window == "3y9m", delta > 0, share > min_share) %>%
    group_by(org_catchment) %>% summarise(last_cohort = max(t_child_birth), .groups = "drop")
  cohort_limits <- full_join(first_c, last_c, by = "org_catchment")
  month_label <- function(m) ifelse(is.na(m), NA, sprintf("%04d-%02d", m %/% 12, m %% 12 + 1))
  print(cohort_limits %>% mutate(first = month_label(first_cohort), last = month_label(last_cohort)))
  # Internal diagnostic (contains small counts -- not for export)
  write.xlsx(shares %>% mutate(birth_month = month_label(t_child_birth)),
             sub("\\.dta$", "_cohort_shares.xlsx", loc$catch_sample_file))
}


# --- Sample: one organisation's area during the whole 22-53 month window -----------
sample <- in_area(window_first, window_last) %>%
  inner_join(kids %>% select(-t_child_birth), by = c("RINPERSOONS", "RINPERSOON"))
log_n(sprintf("... living in one catchment area from month %d to %d", window_first, window_last),
      nrow(sample))
if (!is.null(cohort_limits)) {
  sample <- sample %>%
    left_join(cohort_limits, by = "org_catchment") %>%
    filter(!is.na(first_cohort), !is.na(last_cohort),
           t_child_birth >= first_cohort, t_child_birth <= last_cohort) %>%
    select(-first_cohort, -last_cohort)
  log_n("... born within the organisation's cohort window [C9]", nrow(sample))
}
print(table(org_catchment = sample$org_catchment))


# =============================================================================
# 3. OBSERVED BY THE JGZ AT THE CONTACT MOMENTS  [C1, C4]
# =============================================================================

for (k in names(windows)) {
  seen <- seen_in(sample, windows[[k]])
  names(seen)[names(seen) == "seen"] <- paste0("d_observed_", k)
  sample <- sample %>% left_join(seen, by = c("RINPERSOONS", "RINPERSOON"))
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

# Last contact in the window (org, age), as in Coen's file
last_contact <- contacts_of(sample) %>%
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
  # Coen (PREPARE): order by birth year and month, birth_order = _n, so
  # children born in the same month (twins) get consecutive numbers
  mutate(sib_month = month_idx(sib_birth)) %>%
  arrange(RINPERSOONSMa, RINPERSOONMa, sib_month) %>%
  group_by(RINPERSOONSMa, RINPERSOONMa) %>%
  mutate(birth_order = row_number(),
         first_birth = min(sib_birth)) %>%
  ungroup()
sample <- sample %>%
  left_join(siblings %>% select(RINPERSOONS, RINPERSOON, birth_order, first_birth),
            by = c("RINPERSOONS", "RINPERSOON")) %>%
  mutate(age_birth_child    = month_idx(birthdate) - month_idx(birthdate_ma),    # months
         age_birth_first_ma = month_idx(first_birth) - month_idx(birthdate_ma))  # months

# --- Migration background (ETNGRP, 8 groups)  [C7] --------------------------------
# Coen: land = string(gbaherkomstgroepering) merged to LANDAKTUEELREFV9, which
# has the variable etngrp (0 Autochtoon ... 7 Onbekend). Used when available.
etngrp_file <- loc$catch_land_etngrp_file
etngrp_ref <- NULL
if (!is.null(etngrp_file) && nzchar(etngrp_file) && file.exists(etngrp_file)) {
  etngrp_ref <- (if (grepl("\\.dta$", etngrp_file, ignore.case = TRUE)) read_dta(etngrp_file) else read_sav(etngrp_file)) %>%
    rename_with(tolower)
  if (!all(c("land", "etngrp") %in% names(etngrp_ref))) {
    message("No land/etngrp columns in ", etngrp_file, "; ETNGRP built from the country labels")
    etngrp_ref <- NULL
  }
}
sample <- sample %>% mutate(LAND = herkomst_label)
if (!is.null(etngrp_ref)) {
  etngrp_ref <- etngrp_ref %>%
    transmute(land_code = as.character(suppressWarnings(as.numeric(as_chr(land)))),
              ETNGRP = suppressWarnings(as.numeric(as_chr(etngrp)))) %>%
    distinct(land_code, .keep_all = TRUE)
  sample <- sample %>%
    mutate(land_code = as.character(suppressWarnings(as.numeric(as_chr(GBAHERKOMSTGROEPERING))))) %>%
    left_join(etngrp_ref, by = "land_code") %>%
    mutate(ETNGRP = coalesce(ETNGRP, 7)) %>%
    select(-land_code)
  message("ETNGRP from ", etngrp_file)
} else {
  # Fallback: from the country label and LANDTYPE (westers / niet-westers)
  land_ref <- read_sav(loc$catch_land_ref_file) %>% mutate(across(everything(), as_chr))
  caribbean <- c("Nederlandse Antillen (oud)", "Aruba", "Cura\u00e7ao", "Bonaire",
                 "Sint Maarten", "Sint Eustatius", "Saba", "Caribisch Nederland")
  sample <- sample %>%
    mutate(
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
}

# --- Addresses in the reference month: child, father, mother; postcode_4  [C5] --
# Coen: the address spell covering the month in which the child turns 36
# months; if several, the earliest. `child` first in transmute(): after
# RINPERSOON = RINPERSOONpa the name RINPERSOON means the parent.
for (who in c("child", "pa", "ma")) {
  p <- switch(who,
    child = sample %>% transmute(child = RINPERSOON, RINPERSOONS, RINPERSOON, date = child_date_cm),
    pa    = sample %>% transmute(child = RINPERSOON, RINPERSOONS = RINPERSOONSpa,
                                 RINPERSOON = RINPERSOONpa, date = child_date_cm),
    ma    = sample %>% transmute(child = RINPERSOON, RINPERSOONS = RINPERSOONSMa,
                                 RINPERSOON = RINPERSOONMa, date = child_date_cm))
  a <- address_in_month(p %>% filter(!is.na(RINPERSOON)), adres, "child")
  names(a)[2:3] <- paste0(c("soortobjectnummer_", "rinobjectnummer_"), who)
  sample <- sample %>% left_join(a, by = c("RINPERSOON" = "child"))
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
    # IPI: the socio-economic category is PSECJ (Coen, PREPARE do-file)
    d <- read_sav(f, col_select = any_of(c("RINPERSOONS", "RINPERSOON", "PERSBRUT", "PSECJ", "PERSSOCECJ")))
    names(d) <- sub("^PERSBRUT$", "inppersbrut", sub("^(PSECJ|PERSSOCECJ)$", "inpsecj", names(d)))
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
              inhbrutinkh = as.numeric(zap_labels(INHBRUTINKH)),
              # 9999999999 = income not observed (Coen sets it to 0; NA gives
              # the same result in the regression do-file, which sets missing to 0)
              inhbrutinkh = ifelse(inhbrutinkh %in% c(999999999, 9999999999), NA, inhbrutinkh))
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

# ISCED 2011 level of the highest completed education, as Coen did:
# OPLNRHB -> OPLEIDINGSNRREF (CTO2016V) -> CTOREF (ISCED2011LevelHB)  [C8]
read_ref <- function(f) {
  d <- if (grepl("\\.dta$", f, ignore.case = TRUE)) read_dta(f) else read_sav(f)
  d %>% zap_labels() %>% rename_with(toupper) %>% mutate(across(everything(), as_chr))
}
num_key <- function(x) as.character(suppressWarnings(as.numeric(as_chr(x))))
ref_ok <- function(f) !is.null(f) && nzchar(f) && file.exists(f)
for (who in c("pa", "ma")) sample[[paste0(who, "_ISCED2011LevelHB")]] <- NA_real_
if (ref_ok(loc$catch_oplnr_ref_file) && ref_ok(loc$catch_cto_ref_file) &&
    all(paste0(c("pa", "ma"), "_OPLNRHB") %in% names(sample))) {
  oplnr_ref <- read_ref(loc$catch_oplnr_ref_file)
  cto_ref   <- read_ref(loc$catch_cto_ref_file)
  cto_col <- intersect(c("CTO2016V", "CTO"), names(oplnr_ref))[1]
  if (!"OPLNR" %in% names(oplnr_ref) || is.na(cto_col) ||
      !all(c("CTO", "ISCED2011LEVELHB") %in% names(cto_ref))) {
    message("Unexpected columns in the education reference files; ISCED left empty")
  } else {
    isced <- oplnr_ref %>%
      transmute(oplnr = num_key(OPLNR), cto = num_key(.data[[cto_col]])) %>%
      distinct(oplnr, .keep_all = TRUE) %>%
      left_join(cto_ref %>% transmute(cto = num_key(CTO),
                                      isced = suppressWarnings(as.numeric(ISCED2011LEVELHB))) %>%
                  distinct(cto, .keep_all = TRUE), by = "cto")
    for (who in c("pa", "ma")) {
      key <- num_key(sample[[paste0(who, "_OPLNRHB")]])
      sample[[paste0(who, "_ISCED2011LevelHB")]] <- isced$isced[match(key, isced$oplnr)]
    }
    message("ISCED2011LevelHB rebuilt from ", basename(loc$catch_oplnr_ref_file), " and ",
            basename(loc$catch_cto_ref_file))
  }
} else {
  message("Education reference files not found: pa_/ma_ISCED2011LevelHB left empty [C8]")
}

# --- Health costs (ZVWZORGKOSTENTAB)  [C6] -------------------------------------------
read_health <- function(year, rins) {
  f <- latest_file(file.path("GezondheidWelzijn/ZVWZORGKOSTENTAB", year),
                   paste0("ZVWZORGKOSTEN", year, "TABV[0-9]+\\.sav$"))
  # Coen (PREPARE do-file): total = sum of a fixed list of 16 ZVWK columns
  # (missing counted as 0); mental care = any of 8 columns not 0
  wanted <- toupper(unique(c(cfg$catch_health_cost_cols, cfg$catch_mental_care_cols)))
  d <- read_sav(f, col_select = matches(paste0("^(", paste(c("RINPERSOONS", "RINPERSOON", wanted),
                                                          collapse = "|"), ")$"), ignore.case = TRUE)) %>%
    rename_with(toupper) %>%
    select(RINPERSOONS, RINPERSOON, any_of(wanted)) %>%
    zap_labels() %>%
    mutate(RINPERSOON = as_chr(RINPERSOON)) %>%
    filter(RINPERSOON %in% rins)
  cost_cols <- intersect(toupper(cfg$catch_health_cost_cols), names(d))
  ggz_cols  <- intersect(toupper(cfg$catch_mental_care_cols), names(d))
  not_found <- setdiff(wanted, names(d))
  if (length(not_found) > 0) message("ZVWZORGKOSTEN ", year, " has no ", paste(not_found, collapse = ", "))
  to_matrix <- function(cols) {
    m <- as.matrix(as.data.frame(lapply(d[cols], as.numeric)))
    m[is.na(m)] <- 0
    m
  }
  out <- data.frame(RINPERSOON = d$RINPERSOON, stringsAsFactors = FALSE)
  out$tot_health_exp <- if (length(cost_cols)) rowSums(to_matrix(cost_cols)) else NA_real_
  out$d_mental_care  <- if (length(ggz_cols)) as.numeric(rowSums(to_matrix(ggz_cols) != 0) > 0) else NA_real_
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
                              starts_with("pa_GEWICHT"), starts_with("ma_GEWICHT"),
                              pa_ISCED2011LevelHB, ma_ISCED2011LevelHB))   # [C8] empty if not rebuilt

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
