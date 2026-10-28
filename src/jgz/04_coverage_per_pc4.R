# =============================================================================
# JGZ preprocessing -- 04. Coverage (bereik) per PC4, to check the catchment areas
#
# Agreed with Bastian on 27-10-2026 ("Kloppen de verzorgingsgebieden?"):
#   1. compute the coverage for every PC4 that occurs in the JGZ data, per
#      contact moment, over the whole period;
#   2. list the PC4 areas with a coverage above ~20% that are NOT in the PC4
#      catchment file (Coen's list, resources/jgz_pc4gebieden.csv);
#   3. look at those areas per calendar year (catchment areas can change over
#      time: 50% may mean 0% in half of the years and 100% in the other half);
#   4. decide together whether to add them (not done here).
#
# Definitions (settings cov_* in config/jgz.yml):
#   - Children: everyone in GBA born in the birth-date range of 03
#     (catch_birth_date_min/max), in all of the Netherlands.
#   - PC4 of a child at a contact moment: the PC4 of his/her GBA address in the
#     calendar month of the nominal age (cov_reference_months: 24, 36, 45
#     months). If two address spells cover that month, the earliest one (as 03).
#   - Denominator: children living in that PC4 at that month, in a calendar
#     year with JGZ data (cov_years). No continuous-residence requirement and no
#     cohort rule, unlike 03: the point is to find areas outside the list.
#   - Numerator: of these, the children with at least one JGZ panel row in the
#     contact-moment window (catch_cm_windows, months of age), any organisation
#     -- the same "observed" rule as 03 (Coen).
#   - "PC4 in the JGZ data" = a PC4 with at least one observed child at one of
#     the contact moments.
#
#   Input : JGZ panel (00), GBAPERSOONTAB, GBAADRESOBJECTBUS, VSLPOSTCODEBUS,
#           resources/jgz_pc4gebieden.csv
#   Output: loc$coverage_file (.xlsx) with sheets
#             per_pc4        coverage per PC4 x contact moment, whole period
#             not_on_list    PC4s not on the list with coverage above the
#                            threshold at one or more contact moments
#             per_year       those PC4s per contact moment x calendar year
#             settings       the settings used
#
# INTERNAL ONLY: the sheets contain small counts and per-organisation counts
# (including org 12). Not for export.
#
# Run from the repository root, after 00 (03 is not needed):
#   setwd("H:/Opportunity-Preprocessing"); source("src/jgz/04_coverage_per_pc4.R")
# Reads the addresses of all children in the birth-date range (like 03), so it
# takes a while.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages(library(openxlsx))
options(scipen = 999)

windows  <- cfg$catch_cm_windows            # e.g. 2y = [22, 33] months of age
ref_age  <- cfg$cov_reference_months        # e.g. 2y = 24 months
stopifnot(setequal(names(windows), names(ref_age)))


# =============================================================================
# 1. CHILDREN (GBA) AND THEIR PC4 AT EACH CONTACT MOMENT
# =============================================================================

birth_years <- seq(as.integer(format(as.Date(cfg$catch_birth_date_min), "%Y")),
                   as.integer(format(as.Date(cfg$catch_birth_date_max), "%Y")))
kids <- read_cbs(loc$gba_persoon_file,
                 cols = c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTEJAAR",
                          "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG"),
                 filter_in = list(GBAGEBOORTEJAAR = birth_years)) %>%
  rename_with(toupper) %>%
  transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON),
            birthdate = as.Date(sprintf("%s-%s-%s", as_chr(GBAGEBOORTEJAAR),
                                        as_chr(GBAGEBOORTEMAAND), as_chr(GBAGEBOORTEDAG)))) %>%
  filter(!is.na(birthdate),
         birthdate >= as.Date(cfg$catch_birth_date_min),
         birthdate <= as.Date(cfg$catch_birth_date_max)) %>%
  distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
  mutate(t_birth = month_idx(birthdate))
message(sprintf("Children born %s to %s (GBA): %s", cfg$catch_birth_date_min,
                cfg$catch_birth_date_max, format(nrow(kids), big.mark = ",")))

# Address spells x postcode validity -> spells with one PC4, in months
adres <- read_addresses(unique(kids$RINPERSOON)) %>%
  semi_join(kids, by = c("RINPERSOONS", "RINPERSOON"))
postcodes <- read_postcodes(unique(paste(adres$soortobjectnummer, adres$rinobjectnummer)))
spells <- adres %>%
  inner_join(postcodes, by = c("soortobjectnummer", "rinobjectnummer")) %>%
  mutate(s = month_idx(pmax(start, pc_start)), e = month_idx(pmin(end, pc_end))) %>%
  filter(s <= e, !is.na(postcode_4)) %>%
  select(RINPERSOONS, RINPERSOON, start, s, e, postcode_4)
rm(adres, postcodes)

# PC4 of each child in month `ref` of age (earliest spell if several)
pc4_at_age <- function(ref) {
  kids %>%
    transmute(RINPERSOONS, RINPERSOON, t_birth, t_ref = t_birth + ref) %>%
    inner_join(spells, by = c("RINPERSOONS", "RINPERSOON")) %>%
    filter(s <= t_ref, e >= t_ref) %>%
    arrange(RINPERSOONS, RINPERSOON, start) %>%
    distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
    transmute(RINPERSOONS, RINPERSOON, t_birth, postcode_4,
              year = t_ref %/% 12L)     # calendar year of the reference month
}


# =============================================================================
# 2. OBSERVED BY THE JGZ IN THE CONTACT-MOMENT WINDOW (any organisation)
# =============================================================================

contacts <- readRDS(loc$panel_file) %>%
  transmute(RINPERSOONS = as_chr(RINPERSOONS), RINPERSOON = as_chr(RINPERSOON),
            org, contact_date = as.Date(dat_a_str, format = "%Y%m%d")) %>%
  filter(!is.na(contact_date)) %>%
  mutate(t_contact = month_idx(contact_date))

# Calendar years with JGZ data
cov_years <- cfg$cov_years
if (identical(cov_years, "data")) cov_years <- range(as.integer(format(contacts$contact_date, "%Y")))
cov_years <- as.integer(unlist(cov_years))
message(sprintf("Calendar years included: %d-%d", cov_years[1], cov_years[2]))

org_cols <- paste0("seen_org_", c(sort(unique(na.omit(contacts$org))), "unknown"))

# One row per child seen in window w, with 1/0 per organisation that saw
# him/her there (org missing = seen by two organisations at the same age, see
# 00 -> "unknown"). A child can count for more than one organisation.
seen_in_window <- function(w) {
  d <- contacts %>%
    inner_join(kids %>% select(RINPERSOONS, RINPERSOON, t_birth),
               by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(age_month = t_contact - t_birth) %>%
    filter(age_month >= w[1], age_month <= w[2]) %>%
    mutate(org_col = ifelse(is.na(org), "seen_org_unknown", paste0("seen_org_", org))) %>%
    distinct(RINPERSOONS, RINPERSOON, org_col)
  out <- d %>% distinct(RINPERSOONS, RINPERSOON) %>% mutate(seen = 1L)
  for (oc in org_cols) {
    hit <- d %>% filter(org_col == oc) %>% distinct(RINPERSOONS, RINPERSOON) %>% mutate(x = 1L)
    names(hit)[names(hit) == "x"] <- oc
    out <- out %>% left_join(hit, by = c("RINPERSOONS", "RINPERSOON"))
  }
  out %>% mutate(across(all_of(org_cols), function(x) coalesce(x, 0L)))
}


# =============================================================================
# 3. COVERAGE PER PC4 x CONTACT MOMENT (and per calendar year)
# =============================================================================

child_cm <- NULL
for (k in names(windows)) {
  d <- pc4_at_age(ref_age[[k]]) %>%
    filter(year >= cov_years[1], year <= cov_years[2]) %>%
    left_join(seen_in_window(windows[[k]]), by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(across(c("seen", all_of(org_cols)), function(x) coalesce(x, 0L)),
           contact_moment = k)
  message(sprintf("%-5s window %d-%d months, address at %d months: %s children, %s seen",
                  k, windows[[k]][1], windows[[k]][2], ref_age[[k]],
                  format(nrow(d), big.mark = ","), format(sum(d$seen), big.mark = ",")))
  child_cm <- bind_rows(child_cm, d %>% select(-RINPERSOONS, -RINPERSOON, -t_birth))
}

summarise_cov <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(n_children = n(), n_seen = sum(seen),
              across(all_of(org_cols), sum), .groups = "drop") %>%
    mutate(coverage = round(n_seen / n_children, 4)) %>%
    relocate(coverage, .after = n_seen)
}

per_pc4 <- summarise_cov(child_cm, postcode_4, contact_moment)

# PC4s in the JGZ data: at least one observed child at one of the contact moments
pc4_in_data <- unique(per_pc4$postcode_4[per_pc4$n_seen > 0])
per_pc4 <- per_pc4 %>% filter(postcode_4 %in% pc4_in_data)
message(sprintf("PC4 areas with at least one observed child: %d", length(pc4_in_data)))


# --- On Coen's list? (PC4 file, with Coen's corrections as in 03) -------------------
pc4_list <- read.csv(loc$catch_pc4_org_file, colClasses = "character") %>%
  transmute(org = as.integer(org), gemeente, postcode_4 = as.integer(postcode_4),
            einde = ifelse(einde %in% c(".", ""), NA, einde))
for (fix in cfg$catch_pc4_corrections) {
  hit <- pc4_list$org == fix$org & pc4_list$postcode_4 == fix$from
  pc4_list$postcode_4[hit] <- as.integer(fix$to)
}
list_info <- pc4_list %>%
  group_by(postcode_4) %>%
  summarise(list_org      = paste(sort(unique(org)), collapse = "/"),
            list_gemeente = paste(sort(unique(gemeente)), collapse = "/"),
            list_einde    = paste(sort(unique(na.omit(einde))), collapse = "/"),
            .groups = "drop")

per_pc4 <- per_pc4 %>%
  left_join(list_info, by = "postcode_4") %>%
  mutate(on_list = !is.na(list_org)) %>%
  relocate(on_list, list_org, list_gemeente, list_einde, .after = contact_moment) %>%
  arrange(postcode_4, match(contact_moment, names(windows)))

# Main organisation that saw the children (most children seen), for orientation
main_org <- function(df) {
  m <- as.matrix(df[, org_cols])
  ifelse(rowSums(m) == 0, NA, sub("^seen_org_", "", org_cols[max.col(m, ties.method = "first")]))
}
per_pc4$main_org_seen <- main_org(per_pc4)


# --- Not on the list, coverage above the threshold -------------------------------
not_on_list <- per_pc4 %>%
  filter(!on_list) %>%
  group_by(postcode_4) %>%
  filter(any(coverage > cfg$cov_flag_min_coverage & n_children >= cfg$cov_flag_min_children)) %>%
  ungroup()
flagged <- unique(not_on_list$postcode_4)
message(sprintf("PC4 areas not on the list with coverage > %.0f%% (n >= %d) at one or more contact moments: %d",
                100 * cfg$cov_flag_min_coverage, cfg$cov_flag_min_children, length(flagged)))

# wide overview: one row per PC4, coverage and n per contact moment
not_on_list_wide <- not_on_list %>%
  select(postcode_4, contact_moment, n_children, coverage, main_org_seen) %>%
  group_by(postcode_4) %>%
  mutate(main_org_seen = paste(sort(unique(na.omit(main_org_seen))), collapse = "/")) %>%
  ungroup()
wide <- not_on_list_wide %>% distinct(postcode_4, main_org_seen)
for (k in names(windows)) {
  part <- not_on_list_wide %>% filter(contact_moment == k) %>%
    select(postcode_4, n_children, coverage)
  names(part)[-1] <- paste0(names(part)[-1], "_", k)
  wide <- wide %>% left_join(part, by = "postcode_4")
}
cov_cols <- paste0("coverage_", names(windows))
wide$max_coverage <- if (nrow(wide) > 0) apply(as.matrix(wide[, cov_cols]), 1, max, na.rm = TRUE) else numeric(0)
wide <- wide %>% arrange(desc(max_coverage)) %>% relocate(max_coverage, .after = postcode_4)


# --- Those PC4s per calendar year -----------------------------------------------------
per_year <- summarise_cov(child_cm %>% filter(postcode_4 %in% flagged),
                          postcode_4, contact_moment, year) %>%
  arrange(postcode_4, match(contact_moment, names(windows)), year)
per_year$main_org_seen <- main_org(per_year)


# =============================================================================
# 4. OUTPUT (internal only)
# =============================================================================

settings <- data.frame(
  setting = c("birth dates", "calendar years", "windows (months of age)",
              "address taken at (months of age)", "observed",
              "flag: coverage above", "flag: at least n children", "PC4 file", "run on"),
  value = c(paste(cfg$catch_birth_date_min, "to", cfg$catch_birth_date_max),
            paste(cov_years, collapse = "-"),
            paste(names(windows), sapply(windows, paste, collapse = "-"), collapse = "; "),
            paste(names(ref_age), unlist(ref_age), collapse = "; "),
            "any JGZ panel row in the window, any organisation (as 03)",
            cfg$cov_flag_min_coverage, cfg$cov_flag_min_children,
            loc$catch_pc4_org_file, format(Sys.time(), "%Y-%m-%d %H:%M"))
)

ensure_dir(dirname(loc$coverage_file))
write.xlsx(list(per_pc4 = per_pc4, not_on_list = wide, per_year = per_year, settings = settings),
           loc$coverage_file, overwrite = TRUE)
message("Written: ", loc$coverage_file, " (internal: small counts, not for export)")
