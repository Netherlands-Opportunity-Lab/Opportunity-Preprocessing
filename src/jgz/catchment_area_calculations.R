# =============================================================================
# Catchment area calculations (R translation of Stata do-file
# "20190310 Catchment area calculations")
#
# Goal: for every PC2 and PC3 postcode area, compute the share of
# "permanent resident" children (born 2010-2014) that appear in the JGZ
# (youth health care) data. Areas with a high share are areas where the JGZ
# data covers (nearly) the whole child population = the "catchment area".
#
# Runs inside the CBS Remote Access environment. Only the paths in the
# CONFIG block need changing.
#
# KEY RESEARCH DECISIONS (inherited from the Stata code; see comments [D#]):
#   [D1] Objects (addresses) with more than one record in VSLPOSTCODEBUS are
#        dropped entirely - not deduplicated. Addresses whose postcode changed
#        over time therefore get no postcode at all.
#   [D2] JGZ ID file: one row kept per RINPERSOON; which row is arbitrary.
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
#   [D8] "Observed by JGZ" = child has a non-empty KindNummer_crypt after
#        merging the JGZ ID file on (rinpersoons, rinpersoon).
#   [D9] Catchment = observed / permanent residents per area; NA if area has
#        0 children. Areas with catchment >= 0.9 are listed (threshold 0.9).
#  [D10] Only the PC3 table is exported (PC2 is only printed), as in Stata.
# =============================================================================

library(haven)     # read .dta and .sav files
library(DBI)       # database interface
library(RSQLite)   # read the SQLite .db version of VSLPOSTCODEBUS
library(dplyr)
library(openxlsx)  # write Excel output

# ---- CONFIG -----------------------------------------------------------------
# TODO: check the folders; only the file names were given.
path_postcode   <- "G:/BouwenWonen/VSLPOSTCODEBUS/VSLPOSTCODEBUS2026031.db"
postcode_table  <- NULL   # table name inside the .db; NULL = use the only table
path_jgz_ids    <- "H:/Data files/8151mcuniekCBVK.sav"
path_persoon    <- "G:/Bevolking/GBAPERSOONTAB/2017/geconverteerde data/GBAPERSOON2017TABV1.DTA"
path_adres      <- "G:/Bevolking/GBAADRESOBJECTBUS/2017/geconverteerde data/GBAADRESOBJECT2017BUSV1.dta"
path_output     <- "H:/Coen/Output/201903/CatchmentArea.xlsx"

birth_years     <- 2010:2014
end_date_alive  <- 20171231   # end date given to spells still running at end of 2017
catchment_cut   <- 0.9

# Helper: Stata-style `destring, force` (non-numeric -> NA, no warnings)
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Helper: treat "" as missing for string identifiers
nonempty <- function(x) !is.na(x) & x != ""


# ---- 1. Postcode (PC4) per address object -----------------------------------
# Read from the SQLite version of VSLPOSTCODEBUS. The deduplication is done in
# SQL, so only objects with a single record are loaded into R.
# (SQLite matches column names case-insensitively, so this works whether the
# .db has SOORTOBJECTNUMMER or soortobjectnummer; the AS aliases make sure R
# gets lower-case names back.)
#
# [D1] Keep only objects that occur exactly once. Stata: duplicates tag ...;
#      keep if dup==0  -> drops ALL copies of a duplicated object.
#      NB: the 2026 file has ~8 more years of history than the 2018 file the
#      Stata code used, so more objects will have >1 record and be dropped.
con <- dbConnect(RSQLite::SQLite(), path_postcode, flags = RSQLite::SQLITE_RO)

if (is.null(postcode_table)) {
  tabs <- dbListTables(con)
  if (length(tabs) != 1) {
    stop("The .db holds several tables (", paste(tabs, collapse = ", "),
         "); set `postcode_table` in CONFIG.")
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
# (MAX() just returns the single value; every group has exactly one row.)

dbDisconnect(con)

# Type check: if the .db stores rinobjectnummer as a number, leading zeros are
# lost and the merge with GBAADRESOBJECTBUS (string) may fail silently.
# The postcode match rate printed in step 5 is the check for this.
if (is.numeric(pc4_codes$rinobjectnummer)) {
  warning("rinobjectnummer is numeric in the .db - check the match rate in step 5.")
}

pc4_codes <- pc4_codes |>
  mutate(
    soortobjectnummer = trimws(as.character(soortobjectnummer)),
    rinobjectnummer   = trimws(as.character(rinobjectnummer))  # Stata: recast str32
  )

cat("Address objects with a unique postcode:", nrow(pc4_codes), "\n")


# ---- 2. JGZ ID file: one row per person -------------------------------------
# Column names are lower-cased right after reading, so the code does not
# depend on the capitalisation used in the .sav file.
#
# [D2] Stata `duplicates drop RINPERSOON, force` keeps the first occurrence;
#      distinct(.keep_all = TRUE) does the same in file order. Which row is
#      kept only matters if KindNummer_crypt differs within a RINPERSOON.
jgz_ids <- read_sav(path_jgz_ids) |>
  zap_labels() |>
  rename_with(tolower) |>
  select(rinpersoons, rinpersoon, kindnummer_crypt) |>
  mutate(across(everything(), ~ trimws(as.character(.x)))) |>  # .sav strings can be space-padded
  distinct(rinpersoon, .keep_all = TRUE)

cat("Unique persons in JGZ ID file:", nrow(jgz_ids), "\n")


# ---- 3. Children born 2010-2014 ---------------------------------------------
# [D3] Missing birth years drop out automatically (Stata's extra
#      `drop if gbageboortejaar==.` was redundant).
kids <- read_dta(path_persoon,
                 col_select = c(rinpersoons, rinpersoon, gbageboortejaar)) |>
  zap_labels() |>
  mutate(gbageboortejaar = to_num(gbageboortejaar)) |>
  filter(gbageboortejaar %in% birth_years)

# Check duplicates (original: "no duplicates found")
stopifnot(!anyDuplicated(kids$rinpersoon))
cat("Children born", min(birth_years), "-", max(birth_years), ":", nrow(kids), "\n")


# ---- 4. Address history -> "permanent residents" ----------------------------
# NOTE: the Stata code merged 1:m WITHOUT dropping _merge==2, so the whole
# Dutch population stayed in memory. It still gave correct counts because
# `sum gbageboortejaar` ignores rows with a missing birth year. Here we
# restrict to the children directly (inner join) - same result, less memory.
adres <- read_dta(path_adres,
                  col_select = c(rinpersoons, rinpersoon,
                                 gbadatumeindeadreshouding,
                                 soortobjectnummer, rinobjectnummer)) |>
  zap_labels() |>
  semi_join(kids, by = c("rinpersoons", "rinpersoon"))

residents <- kids |>
  inner_join(adres, by = c("rinpersoons", "rinpersoon")) |>
  mutate(gbadatumeindeadreshouding = to_num(gbadatumeindeadreshouding)) |>
  # [D4] exactly one address record = never moved
  add_count(rinpersoon, name = "n_addr") |>
  filter(n_addr == 1) |>
  # [D5] spell runs until end of 2017 = still alive / registered
  filter(gbadatumeindeadreshouding == end_date_alive) |>
  select(-n_addr)

# [D6] Possible extension (noted in original, not implemented): check that
#      the spell's start date (gbadatumaanvangadreshouding) is close to birth.

cat("Permanent residents:", nrow(residents), "\n")


# ---- 5. Add postcode and JGZ link -------------------------------------------
residents <- residents |>
  mutate(soortobjectnummer = trimws(as.character(soortobjectnummer)),
         rinobjectnummer   = trimws(as.character(rinobjectnummer))) |>
  left_join(pc4_codes, by = c("soortobjectnummer", "rinobjectnummer"),
            relationship = "many-to-one") |>
  mutate(postcode_matched = !is.na(postcodenum)) |>
  # [D7] non-numeric / missing postcodes -> NA
  mutate(postcodenum = to_num(postcodenum)) |>
  left_join(jgz_ids, by = c("rinpersoons", "rinpersoon"),
            relationship = "one-to-one") |>
  # [D8] observed in JGZ
  mutate(observed_jgz = nonempty(kindnummer_crypt))

# Match-rate checks: a sudden drop vs. the old .dta run usually means an ID
# type/format mismatch between files rather than a real data issue.
cat("Share of permanent residents matched to a postcode object:",
    round(mean(residents$postcode_matched), 3), "\n")
cat("Share of permanent residents found in JGZ ID file:",
    round(mean(residents$observed_jgz), 3), "\n")
cat("Permanent residents without a (usable) postcode, excluded from tables:",
    sum(is.na(residents$postcodenum)), "\n")


# ---- 6. Catchment tables ----------------------------------------------------
# Stata looped over every possible code and stored N's in a matrix. Here:
# count per code, then join onto the full code range so empty areas still
# appear (with N = 0 and catchment NA, like Stata's missing).
catchment_table <- function(data, digits_dropped, code_range, code_name) {
  counts <- data |>
    filter(!is.na(postcodenum)) |>
    mutate(code = floor(postcodenum / 10^digits_dropped)) |>   # Stata int()
    group_by(code) |>
    summarise(n_resident = n(), n_observed = sum(observed_jgz), .groups = "drop")

  tibble(code = code_range) |>
    left_join(counts, by = "code") |>
    mutate(
      n_resident = coalesce(n_resident, 0L),
      n_observed = coalesce(n_observed, 0L),
      catchment  = if_else(n_resident > 0, n_observed / n_resident, NA_real_)  # [D9]
    ) |>
    rename("{code_name}" := code,
           "Permanent residents (N)" = n_resident,
           "Observed by JGZ (N)" = n_observed,
           "Catchment" = catchment)
}

catchment_pc2 <- catchment_table(residents, 2, 10:99,   "PC2 code")
catchment_pc3 <- catchment_table(residents, 1, 100:999, "PC3 code")

print(catchment_pc2, n = Inf)
print(catchment_pc3, n = Inf)

# [D9] PC3 areas with catchment >= 0.9 (Stata: tab catchment_pc31 if ...)
high_pc3 <- catchment_pc3 |> filter(!is.na(Catchment), Catchment >= catchment_cut)
cat("PC3 areas with catchment >=", catchment_cut, ":", nrow(high_pc3), "\n")
print(high_pc3, n = Inf)


# ---- 7. Export --------------------------------------------------------------
# [D10] As in Stata, only PC3 is exported. Output is .xlsx instead of .xls.
# Check CBS output rules (small cell counts) before requesting export.
wb <- createWorkbook()
addWorksheet(wb, "PC3")
writeData(wb, "PC3", catchment_pc3)
# addWorksheet(wb, "PC2"); writeData(wb, "PC2", catchment_pc2)  # optional
saveWorkbook(wb, path_output, overwrite = TRUE)

# ---- Not translated ---------------------------------------------------------
# Lines 123-124 of the do-file start a new section ("Select kids who have
# only one record AND are still alive at the end of 2017") but contain no
# code in the screenshots; the file may continue beyond line 126.
