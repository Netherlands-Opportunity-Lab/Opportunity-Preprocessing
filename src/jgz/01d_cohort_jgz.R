# =============================================================================
# JGZ cohort -- 01d. Cohort: one row per child, with the parent link
#
# Turns the JGZ panel into a cohort file like the other cohorts': one row per
# child (RINPERSOONS, RINPERSOON) with birth date, sex, migration background
# and the link to the parents. The predictor step that follows is the one of
# the newborn cohorts (src/newborns/02_predictors_newborns.R), so the columns
# below are named as in src/newborns/01_cohort_newborns.R; in particular the
# mother is CBKSoortNr_Moeder / Rinpersoon_Moeder (from KINDOUDERTAB here,
# from Perined there).
#
#   Input : loc$panel_file (output of 01a), GBAPERSOONTAB, KINDOUDERTAB
#   Output: temp/jgz/01_cohort.rds, temp/jgz/01_sample_size.rds
#
# Who is in the cohort: every child in the JGZ panel (any organisation, any
# contact moment) born between child_birth_date_min and child_birth_date_max,
# found in GBA with a known sex and with a mother in KINDOUDERTAB (as the
# newborns: needed for the parental income). Which outcomes a child has
# depends on the contact moments at which he/she was measured (03).
#
# The organisation is not used to select children: the export rules per
# organisation (export_orgs) are applied in the analysis, on the
# org_m<age> columns that 03 adds.
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")


# ---- 1. Children in the JGZ panel ---------------------------------------------------
cohort_dat <- readRDS(loc$panel_file) %>%
  transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON)) %>%
  filter(!is.na(RINPERSOON), RINPERSOON != "") %>%
  distinct()

sample_size <- tibble::tibble(n_0_jgz_children = nrow(cohort_dat))
message(sprintf("Children in the JGZ panel: %s", format(nrow(cohort_dat), big.mark = ",")))


# ---- 2. GBA: birth date, sex, migration background ---------------------------------
# As prep_gba() in src/newborns/01_cohort_newborns.R: origin, generation and sex
# as their labels (e.g. "Nederland", "autochtoon", "Mannen"), which is what the
# predictor step expects.
gba_cols <- c("RINPERSOONS", "RINPERSOON", "GBAGESLACHT", "GBAGEBOORTEJAAR",
              "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", "GBAGENERATIE", "GBAHERKOMSTGROEPERING")

as_label <- function(x) as.character(haven::as_factor(x, levels = "labels"))

prep_gba_jgz <- function(dat) {
  names(dat) <- toupper(names(dat))
  dat %>%
    transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON),
              birthdate = as.Date(paste(as_chr(GBAGEBOORTEJAAR), as_chr(GBAGEBOORTEMAAND),
                                        as_chr(GBAGEBOORTEDAG), sep = "-")),
              GBAGESLACHT = as_label(GBAGESLACHT),
              GBAGENERATIE = as_label(GBAGENERATIE),
              GBAHERKOMSTGROEPERING = as_label(GBAHERKOMSTGROEPERING))
}

gba_kids <- read_cbs_jgz(loc$gba_persoon_file, cols = gba_cols,
                         filter_in = list(RINPERSOON = cohort_dat$RINPERSOON)) %>%
  prep_gba_jgz()

birth_min <- as.Date(cfg$child_birth_date_min, format = "%d/%m/%Y")
birth_max <- as.Date(cfg$child_birth_date_max, format = "%d/%m/%Y")

cohort_dat <- cohort_dat %>%
  inner_join(gba_kids, by = c("RINPERSOONS", "RINPERSOON")) %>%
  filter(birthdate >= birth_min, birthdate <= birth_max) %>%
  mutate(sex = case_when(GBAGESLACHT %in% c("Mannen", "Man", "1") ~ "Men",
                         GBAGESLACHT %in% c("Vrouwen", "Vrouw", "2") ~ "Women")) %>%
  select(-GBAGESLACHT) %>%
  filter(!is.na(sex))

sample_size <- sample_size %>% mutate(n_1_in_gba_birth_range = nrow(cohort_dat))
message(sprintf("... in GBA, born %s to %s, with sex: %s", birth_min, birth_max,
                format(nrow(cohort_dat), big.mark = ",")))
rm(gba_kids)


# ---- 3. Parent link (KINDOUDERTAB) ------------------------------------------------------
kindouder <- read_cbs_jgz(loc$kindouder_file, filter_in = list(RINPERSOON = cohort_dat$RINPERSOON))
names(kindouder) <- toupper(names(kindouder))
kindouder <- kindouder %>%
  transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON),
            RINPERSOONSpa = as_chr(RINPERSOONSPA), RINPERSOONpa = as_chr(RINPERSOONPA),
            RINPERSOONSMa = as_chr(RINPERSOONSMA), RINPERSOONMa = as_chr(RINPERSOONMA)) %>%
  mutate(across(c(RINPERSOONSpa, RINPERSOONpa, RINPERSOONSMa, RINPERSOONMa),
                function(x) ifelse(x == "", NA_character_, x))) %>%
  distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE)

cohort_dat <- cohort_dat %>%
  left_join(kindouder, by = c("RINPERSOONS", "RINPERSOON")) %>%
  # the mother, under the names the predictor step uses (Perined names there)
  mutate(CBKSoortNr_Moeder = RINPERSOONSMa,
         Rinpersoon_Moeder = RINPERSOONMa) %>%
  # as the newborns: a known mother who is not a "G" (no GBA) person
  filter(!is.na(Rinpersoon_Moeder), CBKSoortNr_Moeder != "G")

sample_size <- sample_size %>% mutate(n_2_child_parent_link = nrow(cohort_dat))
message(sprintf("... with a mother in KINDOUDERTAB: %s", format(nrow(cohort_dat), big.mark = ",")))
rm(kindouder)


# ---- 4. Parents: migration background fallback and age at birth ---------------------------
gba_parents <- read_cbs_jgz(loc$gba_persoon_file, cols = gba_cols,
                            filter_in = list(RINPERSOON = c(cohort_dat$RINPERSOONMa,
                                                            cohort_dat$RINPERSOONpa))) %>%
  prep_gba_jgz()

# children without origin in GBA get their mother's (as the newborns)
cohort_dat <- cohort_dat %>%
  left_join(gba_parents %>% select(RINPERSOONS, RINPERSOON,
                                   GBAHERKOMSTGROEPERING_ma = GBAHERKOMSTGROEPERING,
                                   GBAGENERATIE_ma = GBAGENERATIE),
            by = c("CBKSoortNr_Moeder" = "RINPERSOONS", "Rinpersoon_Moeder" = "RINPERSOON")) %>%
  mutate(GBAHERKOMSTGROEPERING = ifelse(is.na(GBAHERKOMSTGROEPERING), GBAHERKOMSTGROEPERING_ma,
                                        GBAHERKOMSTGROEPERING),
         GBAGENERATIE = ifelse(is.na(GBAGENERATIE), GBAGENERATIE_ma, GBAGENERATIE)) %>%
  select(-GBAHERKOMSTGROEPERING_ma, -GBAGENERATIE_ma)

parents_birth <- gba_parents %>% select(RINPERSOONS, RINPERSOON, birthdate)
cohort_dat <- cohort_dat %>%
  left_join(parents_birth %>% rename(birthdate_ma = birthdate),
            by = c("RINPERSOONSMa" = "RINPERSOONS", "RINPERSOONMa" = "RINPERSOON")) %>%
  left_join(parents_birth %>% rename(birthdate_pa = birthdate),
            by = c("RINPERSOONSpa" = "RINPERSOONS", "RINPERSOONpa" = "RINPERSOON")) %>%
  mutate(age_ma_at_birth = as.numeric(birthdate - birthdate_ma) %/% 365.25,
         age_pa_at_birth = as.numeric(birthdate - birthdate_pa) %/% 365.25)
rm(gba_parents, parents_birth)

check_keys_jgz(cohort_dat, c("RINPERSOONS", "RINPERSOON"), "01d cohort")
print(table(`birth year` = format(cohort_dat$birthdate, "%Y")))


# ---- 5. Write ---------------------------------------------------------------------------------
saveRDS(cohort_dat, file.path(loc$temp_folder, "01_cohort.rds"))
saveRDS(sample_size, file.path(loc$temp_folder, "01_sample_size.rds"))
