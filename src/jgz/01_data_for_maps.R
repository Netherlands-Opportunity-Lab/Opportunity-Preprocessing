# =============================================================================
# JGZ pilot -- 01. Postcode at each contact moment
#
# The postcode in the JGZ health file is whatever the organisation recorded.
# For the maps we use the address the child was registered at (GBA) on the
# date of the contact moment instead.
#
#   Input : loc$panel_file (output of 00), GBAADRESOBJECTBUS,
#           loc$matched_postcode_file (rinobjectnummer -> postcode, see step 3)
#   Output: loc$panel_postcode_file -- the panel plus `rinobjectnummer` and
#           `gba_postcode` (PC4)
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages(library(haven))

jgz <- readRDS(loc$panel_file) %>%
  mutate(dat_a = as.Date(dat_a_str, format = "%Y%m%d"))   # date of contact moment

message(sprintf("Contact moments: %d, without a valid date: %d",
                nrow(jgz), sum(is.na(jgz$dat_a))))
message("Latest contact date: ", max(jgz$dat_a, na.rm = TRUE),
        " -- the GBAADRESOBJECTBUS version must cover this date")


# ---- 1. Address history of the children in the panel -----------------------
# Column names differ in case between versions of the file, so they are
# selected case-insensitively and lower-cased.
adres_cols <- "^(rinpersoons|rinpersoon|gbadatumaanvangadreshouding|gbadatumeindeadreshouding|soortobjectnummer|rinobjectnummer)$"
adres <- read_dta(loc$gbaadres_file, col_select = matches(adres_cols, ignore.case = TRUE)) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  semi_join(jgz %>% distinct(rinpersoons = RINPERSOONS, rinpersoon = RINPERSOON),
            by = c("rinpersoons", "rinpersoon")) %>%
  transmute(
    RINPERSOONS = rinpersoons,
    RINPERSOON  = rinpersoon,
    start = as.Date(as.character(gbadatumaanvangadreshouding), format = "%Y%m%d"),
    end   = as.Date(as.character(gbadatumeindeadreshouding),   format = "%Y%m%d"),
    # same key as used for the postcode lookup in step 3
    rinobjectnummer = paste0(soortobjectnummer, rinobjectnummer)
  ) %>%
  mutate(end = coalesce(end, as.Date("2999-12-31")))   # open spell = still living there


# ---- 2. Address on the date of each contact moment ---------------------------------
# Interval join: all address spells of the child, keep the one that contains
# the contact date. (Replaces the old row-by-row loop.) If spells overlap on
# that date (e.g. a move on the day itself), the most recent spell is used.
spells <- jgz %>%
  filter(!is.na(dat_a)) %>%
  select(RINPERSOONS, RINPERSOON, leeftijd, dat_a) %>%
  inner_join(adres, by = c("RINPERSOONS", "RINPERSOON")) %>%
  filter(dat_a >= start, dat_a <= end) %>%
  group_by(RINPERSOONS, RINPERSOON, leeftijd) %>%
  mutate(n_spells = n()) %>%
  slice_max(start, n = 1, with_ties = FALSE) %>%
  ungroup()

message(sprintf("Contact moments with an address: %d of %d (%.1f%%); with overlapping spells: %d",
                nrow(spells), nrow(jgz), 100 * nrow(spells) / nrow(jgz),
                sum(spells$n_spells > 1)))

jgz <- jgz %>%
  left_join(spells %>% select(RINPERSOONS, RINPERSOON, leeftijd, rinobjectnummer),
            by = c("RINPERSOONS", "RINPERSOON", "leeftijd"))


# ---- 3. Postcode of the address -----------------------------------------------------
# MANUAL STEP (unchanged from the old script): VSLPOSTCODEBUS was too large to
# read in R, so the postcode per rinobjectnummer was looked up outside R. This
# script writes the address objects needed to `rin_data.dta`; the lookup must
# produce loc$matched_postcode_file with columns `rinobjectnummer` and
# `postcodenum`.
# TODO: replace by an SQL lookup on the SQLite version of VSLPOSTCODEBUS, as
# in catchment_area_calculations.R.
write_dta(jgz %>% filter(!is.na(rinobjectnummer)) %>% distinct(rinobjectnummer),
          work_path("rin_data.dta"))

if (!file.exists(loc$matched_postcode_file)) {
  stop("Postcode lookup not found: ", loc$matched_postcode_file, "\n",
       "Match ", work_path("rin_data.dta"), " to VSLPOSTCODEBUS first (see step 3).")
}

postcodes <- read_dta(loc$matched_postcode_file) %>%
  zap_labels() %>%
  select(rinobjectnummer, postcodenum) %>%
  mutate(rinobjectnummer = as.character(rinobjectnummer)) %>%
  distinct()

# An address object with more than one postcode cannot be assigned: set to NA
# (same rule as in the catchment area calculations) instead of duplicating rows.
ambiguous <- unique(postcodes$rinobjectnummer[duplicated(postcodes$rinobjectnummer)])
if (length(ambiguous) > 0) {
  warning(length(ambiguous), " address objects have more than one postcode; set to missing.")
  postcodes <- postcodes %>% filter(!rinobjectnummer %in% ambiguous)
}

jgz <- jgz %>%
  left_join(postcodes, by = "rinobjectnummer") %>%
  rename(gba_postcode = postcodenum)   # vs. `postcode` as recorded by the JGZ

message(sprintf("Contact moments with a GBA postcode: %.1f%%",
                100 * mean(!is.na(jgz$gba_postcode))))
stopifnot(!anyDuplicated(jgz[, c("RINPERSOONS", "RINPERSOON", "leeftijd")]))

saveRDS(jgz, loc$panel_postcode_file)
