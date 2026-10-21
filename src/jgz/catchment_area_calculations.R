# =============================================================================
# JGZ pilot -- Catchment area calculations
# (R translation of Stata do-file "20190310 Catchment area calculations")
#
# Goal: for every PC2 and PC3 postcode area, compute the share of
# "permanent resident" children (born 2010-2014) that appear in the JGZ
# (youth health care) data. Areas with a high share are areas where the JGZ
# data covers (nearly) the whole child population = the "catchment area".
#
#   Input : VSLPOSTCODEBUS (SQLite .db), GBAPERSOONTAB 2017,
#           GBAADRESOBJECTBUS 2017, cleaned JGZ ID file (output of 00)
#   Output: loc$catchment_output (PC3 table)
#
# Run from the repository root (paths in config/jgz.yml).
#
# KEY RESEARCH DECISIONS (inherited from the Stata code; see comments [D#]):
#   [D1] Objects (addresses) with more than one record in VSLPOSTCODEBUS are
#        dropped entirely - not deduplicated. Addresses whose postcode changed
#        over time therefore get no postcode at all.
#   [D2] JGZ ID file: one row kept per person. CHANGED: the cleaned ID file
#        from 00 is used (corrected ZONL IDs, ambiguous rows and the CBS
#        placeholder removed) instead of the raw 8151mcuniek file.
#   [D3] Population: children with birth year 2010-2014 in GBAPERSOONTAB 2017.
#   [D4] "Permanent residents" = children with exactly ONE address spell in
#        GBAADRESOBJECTBUS 2017 (never moved) ...
#   [D5] ... AND whose spell ends on 2017-12-31 (= still registered in NL at
#        end of 2017; excludes deaths/emigration). Comment in original: ~95%.
#   [D6] No check that the address spell starts at birth: children who
#        immigrated after birth and never moved within NL are included.
#   [D7] Non-numeric postcodes become NA (Stata `destring, force`); those
#        children, and children at [D1] addresses, are excluded from BOTH
#        numerator and denominator of every area.
#   [D8] "Observed by JGZ" = child appears in the JGZ ID file.
#   [D9] Catchment = observed / permanent residents per area; NA if area has
#        0 children. Areas with catchment >= 0.9 are listed (threshold 0.9).
#  [D10] Only the PC3 table is exported (PC2 is only printed), as in Stata.
#
# Changes compared with the previous R version (see git history): runs on R
# 3.6 / dplyr 1.0.2 as on CBS RA (no `|>`, no `relationship =` in joins),
# paths from config, persons identified by RINPERSOONS + RINPERSOON
# (was RINPERSOON only), [D2] as above.
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages({
  library(haven)     # read .dta files
  library(DBI)       # database interface
  library(RSQLite)   # read the SQLite .db version of VSLPOSTCODEBUS
  library(openxlsx)  # write Excel output
})

birth_years    <- 2010:2014
end_date_alive <- 20171231   # end date given to spells still running at end of 2017
catchment_cut  <- 0.9

# Stata-style `destring, force` (non-numeric -> NA, no warnings)
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Stop when a join would duplicate rows (replaces dplyr 1.1's `relationship =`)
assert_unique <- function(df, keys, what) {
  if (anyDuplicated(df[, keys])) stop(what, " is not unique by ", paste(keys, collapse = " + "))
  invisible(df)
}


# ---- 1. Postcode (PC4) per address object ---------------------------------------
# Read from the SQLite version of VSLPOSTCODEBUS. The deduplication is done in
# SQL, so only objects with a single record are loaded into R. (SQLite matches
# column names case-insensitively; the AS aliases give lower-case names in R.)
#
# [D1] Keep only objects that occur exactly once (Stata: duplicates tag ...;
#      keep if dup==0 -> drops ALL copies of a duplicated object).
#      NB: a newer file has more years of history than the 2018 file the
#      Stata code used, so more objects will have >1 record and be dropped.
#      TODO: consider taking the postcode valid on 2017-12-31 instead.
con <- dbConnect(RSQLite::SQLite(), loc$catchment_postcode_db, flags = RSQLite::SQLITE_RO)

postcode_table <- loc$catchment_postcode_table
if (!nzchar(postcode_table)) {
  tabs <- dbListTables(con)
  if (length(tabs) != 1) {
    stop("The .db holds several tables (", paste(tabs, collapse = ", "),
         "); set catchment_postcode_table in config/jgz.yml.")
  }
  postcode_table <- tabs
}

pc4_codes <- dbGetQuery(con, sprintf("
  SELECT soortobjectnummer AS soortobjectnummer,
         rinobjectnummer   AS rinobjectnummer,
         MAX(postcodenum)  AS postcodenum
  FROM %s
  GROUP BY soortobjectnummer, rinobjectnummer
  HAVING COUNT(*) = 1", dbQuoteIdentifier(con, postcode_table)))
dbDisconnect(con)

# If the .db stores rinobjectnummer as a number, leading zeros are lost and the
# merge with GBAADRESOBJECTBUS (string) may fail silently: check the match rate
# printed in step 5.
if (is.numeric(pc4_codes$rinobjectnummer)) {
  warning("rinobjectnummer is numeric in the .db - check the match rate in step 5.")
}
pc4_codes <- pc4_codes %>%
  mutate(soortobjectnummer = trimws(as.character(soortobjectnummer)),
         rinobjectnummer   = trimws(as.character(rinobjectnummer)))
cat("Address objects with a unique postcode:", nrow(pc4_codes), "\n")


# ---- 2. JGZ children: one row per person --------------------------------------------
# [D2] cleaned ID file from 00
jgz_ids <- readRDS(work_path("IDfile_clean_all.rds")) %>%
  rename_with(tolower) %>%
  transmute(rinpersoons = trimws(as.character(rinpersoons)),
            rinpersoon  = trimws(as.character(rinpersoon)),
            observed_jgz = TRUE) %>%
  distinct(rinpersoons, rinpersoon, .keep_all = TRUE)
cat("Unique persons in JGZ ID file:", nrow(jgz_ids), "\n")


# ---- 3. Children born 2010-2014 ----------------------------------------------------
kids <- read_dta(loc$catchment_persoon_file,
                 col_select = matches("^(rinpersoons|rinpersoon|gbageboortejaar)$", ignore.case = TRUE)) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  mutate(gbageboortejaar = to_num(gbageboortejaar),
         rinpersoons = trimws(as.character(rinpersoons)),
         rinpersoon  = trimws(as.character(rinpersoon))) %>%
  filter(gbageboortejaar %in% birth_years)   # [D3]
assert_unique(kids, c("rinpersoons", "rinpersoon"), "GBAPERSOONTAB")
cat("Children born", min(birth_years), "-", max(birth_years), ":", nrow(kids), "\n")


# ---- 4. Address history -> "permanent residents" -------------------------------------
adres <- read_dta(loc$catchment_adres_file,
                  col_select = matches("^(rinpersoons|rinpersoon|gbadatumeindeadreshouding|soortobjectnummer|rinobjectnummer)$",
                                       ignore.case = TRUE)) %>%
  zap_labels() %>%
  rename_with(tolower) %>%
  mutate(rinpersoons = trimws(as.character(rinpersoons)),
         rinpersoon  = trimws(as.character(rinpersoon))) %>%
  semi_join(kids, by = c("rinpersoons", "rinpersoon"))

residents <- kids %>%
  inner_join(adres, by = c("rinpersoons", "rinpersoon")) %>%
  mutate(gbadatumeindeadreshouding = to_num(gbadatumeindeadreshouding)) %>%
  add_count(rinpersoons, rinpersoon, name = "n_addr") %>%
  filter(n_addr == 1) %>%                                    # [D4] never moved
  filter(gbadatumeindeadreshouding == end_date_alive) %>%    # [D5] still registered
  select(-n_addr)
# [D6] Possible extension (not implemented): check that the spell's start date
#      (gbadatumaanvangadreshouding) is close to birth.
cat("Permanent residents:", nrow(residents), "\n")


# ---- 5. Add postcode and JGZ link ---------------------------------------------------------
assert_unique(pc4_codes, c("soortobjectnummer", "rinobjectnummer"), "VSLPOSTCODEBUS selection")
residents <- residents %>%
  mutate(soortobjectnummer = trimws(as.character(soortobjectnummer)),
         rinobjectnummer   = trimws(as.character(rinobjectnummer))) %>%
  left_join(pc4_codes, by = c("soortobjectnummer", "rinobjectnummer")) %>%
  mutate(postcode_matched = !is.na(postcodenum),
         postcodenum = to_num(postcodenum)) %>%                # [D7]
  left_join(jgz_ids, by = c("rinpersoons", "rinpersoon")) %>%
  mutate(observed_jgz = coalesce(observed_jgz, FALSE))       # [D8]

# A sudden drop vs. an earlier run usually means an ID type/format mismatch
cat("Share of permanent residents matched to a postcode object:",
    round(mean(residents$postcode_matched), 3), "\n")
cat("Share of permanent residents found in JGZ ID file:",
    round(mean(residents$observed_jgz), 3), "\n")
cat("Permanent residents without a (usable) postcode, excluded from tables:",
    sum(is.na(residents$postcodenum)), "\n")


# ---- 6. Catchment tables ---------------------------------------------------------------------
# Count per code, then join onto the full code range so empty areas still
# appear (with N = 0 and catchment NA, like Stata's missing).
catchment_table <- function(data, digits_dropped, code_range, code_name) {
  counts <- data %>%
    filter(!is.na(postcodenum)) %>%
    mutate(code = floor(postcodenum / 10^digits_dropped)) %>%   # Stata int()
    group_by(code) %>%
    summarise(n_resident = n(), n_observed = sum(observed_jgz), .groups = "drop")

  tibble(code = code_range) %>%
    left_join(counts, by = "code") %>%
    mutate(
      n_resident = coalesce(n_resident, 0L),
      n_observed = coalesce(n_observed, 0L),
      catchment  = if_else(n_resident > 0, n_observed / n_resident, NA_real_)  # [D9]
    ) %>%
    rename(!!code_name := code,
           `Permanent residents (N)` = n_resident,
           `Observed by JGZ (N)` = n_observed,
           Catchment = catchment)
}

catchment_pc2 <- catchment_table(residents, 2, 10:99,   "PC2 code")
catchment_pc3 <- catchment_table(residents, 1, 100:999, "PC3 code")
print(catchment_pc2, n = Inf)
print(catchment_pc3, n = Inf)

high_pc3 <- catchment_pc3 %>% filter(!is.na(Catchment), Catchment >= catchment_cut)
cat("PC3 areas with catchment >=", catchment_cut, ":", nrow(high_pc3), "\n")
print(high_pc3, n = Inf)


# ---- 7. Export --------------------------------------------------------------------------------
# [D10] Only PC3 is exported. NOTE: the table contains counts below 10 --
# apply the CBS output rules (small cells) before requesting export.
wb <- createWorkbook()
addWorksheet(wb, "PC3")
writeData(wb, "PC3", catchment_pc3)
# addWorksheet(wb, "PC2"); writeData(wb, "PC2", catchment_pc2)  # optional
saveWorkbook(wb, loc$catchment_output, overwrite = TRUE)
