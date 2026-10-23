# =============================================================================
# JGZ preprocessing -- 01. GBA address and postcode at each contact moment
#
# The postcode in the JGZ health file is whatever the organisation recorded.
# For the maps we use the address the child was registered at (GBA) on the
# date of the contact moment instead.
#
#   Input : loc$panel_file (output of 00), GBAADRESOBJECTBUS, VSLPOSTCODEBUS
#   Output: loc$panel_postcode_file -- the panel plus the address object
#           (`soortobjectnummer`, `rinobjectnummer`) and `gba_postcode` (PC4)
#
# The postcode is taken from VSLPOSTCODEBUS with its validity dates (the
# postcode of the address on the contact date), read the same way as in 03.
# This replaces the manual lookup outside R of the earlier version.
#
# Run from the repository root.
# =============================================================================

source("src/jgz/jgz_helpers.R")

jgz <- readRDS(loc$panel_file) %>%
  mutate(dat_a = as.Date(dat_a_str, format = "%Y%m%d"))   # date of contact moment

message(sprintf("Contact moments: %d, without a valid date: %d",
                nrow(jgz), sum(is.na(jgz$dat_a))))
message("Latest contact date: ", max(jgz$dat_a, na.rm = TRUE),
        " -- the address and postcode files must cover this date")


# ---- 1. Address on the date of each contact moment ---------------------------------
# All address spells of the child; keep the one that contains the contact
# date. If spells overlap on that date (e.g. a move on the day itself), the
# most recent spell is used.
adres <- read_addresses(unique(jgz$RINPERSOON)) %>%
  semi_join(jgz %>% distinct(RINPERSOONS, RINPERSOON), by = c("RINPERSOONS", "RINPERSOON"))

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


# ---- 2. Postcode of that address on the contact date -------------------------------
postcodes <- read_postcodes(unique(paste(spells$soortobjectnummer, spells$rinobjectnummer)))

pc_on_date <- spells %>%
  select(RINPERSOONS, RINPERSOON, leeftijd, dat_a, soortobjectnummer, rinobjectnummer) %>%
  inner_join(postcodes, by = c("soortobjectnummer", "rinobjectnummer")) %>%
  filter(dat_a >= pc_start, dat_a <= pc_end) %>%
  arrange(RINPERSOONS, RINPERSOON, leeftijd, desc(pc_start)) %>%
  distinct(RINPERSOONS, RINPERSOON, leeftijd, .keep_all = TRUE) %>%
  select(RINPERSOONS, RINPERSOON, leeftijd,
         gba_postcode = postcode_4)                 # vs. `postcode` as recorded by the JGZ

spell_pc <- spells %>%
  select(RINPERSOONS, RINPERSOON, leeftijd, soortobjectnummer, rinobjectnummer) %>%
  left_join(pc_on_date, by = c("RINPERSOONS", "RINPERSOON", "leeftijd"))

jgz <- jgz %>%
  select(-dat_a) %>%
  left_join(spell_pc, by = c("RINPERSOONS", "RINPERSOON", "leeftijd"))

message(sprintf("Contact moments with a GBA postcode: %.1f%%",
                100 * mean(!is.na(jgz$gba_postcode))))
stopifnot(!anyDuplicated(jgz[, c("RINPERSOONS", "RINPERSOON", "leeftijd")]))

saveRDS(jgz, loc$panel_postcode_file)
