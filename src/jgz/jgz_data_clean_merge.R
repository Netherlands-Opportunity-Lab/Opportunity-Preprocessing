# ============================================================
# JGZ DATA - CLEAN AND MERGE
# R translation of Stata do-file (original: 20220330 JGZ DATA - CLEAN AND MERGE)
# Translated incrementally, checked against source do-file line-by-line.
# ============================================================

library(haven)    # read SPSS/.dta files
library(dplyr)    # data manipulation
library(openxlsx) # write descriptive matrix to Excel

# ============================================================
# OPEN AND SAVE FILES IN R FORMAT
# ============================================================

# ID file
id_file <- read_spss("L:/8151mcuniekCBKV2.SAV")
saveRDS(id_file, "H:/Coen/JGZdata/8151mcuniekCBKV2.rds")

# ID file for ZONL
id_zonl <- read_spss("L:/8151ZOrggroepOudeenNieuweLanduniekCBKV1.SAV")
saveRDS(id_zonl, "H:/Coen/JGZdata/8151ZOrggroepOudeenNieuweLanduniekCBKV1.rds")

# Health file
health <- read_spss("L:/8151GezondheidsbestandxV2CBKV1.sav")
saveRDS(health, "H:/Coen/JGZdata/8151GezondheidsbestandxV2CBKV1.rds")


# ============================================================
# CLEAN ID FILE
# COMBINE ORIGINAL ID FILE WITH CORRECTED ZONL ID FILE
# ============================================================

# (i) Prepare ZONL ID file
zonl_prepared <- id_zonl %>%
  mutate(bestand = "2") %>%
  rename(KindNummer_crypt = kindnummer_crypt)

saveRDS(zonl_prepared, "H:/Coen/TempData/81851ZONL_ID_prepared_for_cleaning.rds")

# (ii) Open original ID file, drop old ZONL, append corrected ZONL
id_combined <- id_file %>%
  mutate(bestand = as.character(bestand)) %>%
  filter(bestand != "2") %>%
  bind_rows(zonl_prepared) %>%
  mutate(bestand = as.numeric(bestand))


# ============================================================
# GENERATE AND LABEL VARIABLES
# ============================================================

# Organizations
# CORRECTED (was mistranscribed earlier as "Venlo"): org 10 = "Verian"
id_combined <- id_combined %>%
  mutate(org = case_when(
    bestand == 1 ~ 6,
    bestand == 2 ~ 7,
    bestand == 3 ~ 10,
    bestand == 4 ~ 12,
    TRUE ~ NA_real_
  ))

id_combined$org <- factor(
  id_combined$org,
  levels = c(6, 7, 10, 12),
  labels = c("LimburgNoord", "ZONL", "Verian", "Icare")
)

# Onderwerp -> databestand (domain recoding)
# NOTE: this is a PERMUTED remapping, not identity (1->1, 4->2, 2->3, 3->4).
# Worth confirming against the "aanleverprocedure" documentation -- an
# error here would silently mislabel entire domains rather than error out.
id_combined <- id_combined %>%
  mutate(databestand_num = case_when(
    Onderwerp == 1 ~ 1,
    Onderwerp == 4 ~ 2,
    Onderwerp == 2 ~ 3,
    Onderwerp == 3 ~ 4,
    TRUE ~ NA_real_
  ))

# Keep both a numeric version (for matrix indexing, matching Stata's
# `domain` local which iterates 1-4) and a labelled factor (for display/
# analysis). This avoids the row-name mismatch bug from an earlier draft,
# where "Overgewicht" (rowname) didn't match "overgewicht" (factor level).
id_combined$databestand <- factor(
  id_combined$databestand_num,
  levels = c(1, 2, 3, 4),
  labels = c("overgewicht", "visus", "psychosociaal", "spraak-taal")
)

# set trace on / set tracedepth 1 -- Stata debugging aids, no R equivalent
# needed (use traceback() / browser() if something errors)


# ============================================================
# DESCRIPTIVES: SAMPLE SIZE TRACKING THROUGH CLEANING
# ============================================================
# Stata builds an 11x5 matrix logging N at each step (rows = All/4 domains,
# before cleaning + after; cols = Total + 4 orgs). Kept the same structure
# to cross-check against the original; row access is by NUMERIC position
# throughout (not row-name string) to avoid case-mismatch bugs.

descriptive <- matrix(NA_real_, nrow = 11, ncol = 5)
colnames(descriptive) <- c("Total", "LimburgNoord", "ZONL", "Verian", "Icare")
rownames(descriptive) <- c(
  "All", "Overgewicht", "Visus", "Psychosociaal", "Spraak-taal",
  "Missing-RIN",
  "All", "Overgewicht", "Visus", "Psychosociaal", "Spraak-taal"
)

# --- LINES IN DATA BEFORE CLEANING ------------------------------------------

# TOTAL
# Total number of observations is 321,638 -- all have a child ID from the JGZ
descriptive[1, "Total"] <- nrow(id_combined)
org_counts <- table(id_combined$org)
descriptive[1, names(org_counts)] <- org_counts

# BY ONDERWERP (domain) -- indexed numerically (row 1 = "All", so domain
# d occupies row 1+d), matching Stata's descriptive[1+`domain',1]
for (d in 1:4) {
  sub <- id_combined %>% filter(databestand_num == d)
  descriptive[1 + d, "Total"] <- nrow(sub)
  org_n <- table(sub$org)
  descriptive[1 + d, names(org_n)] <- org_n
}
print(descriptive)


# ============================================================
# START OF DATA CLEANING
# ============================================================

# --- MISSING OBSERVATIONS: RINPERSOON ---------------------------------------
# Drop observations without a RIN -- can't be linked to other CBS files.
# Stata log: 969 lines dropped, 320,669 remaining.
missing_rin <- id_combined %>% filter(RINPERSOONS == "" & RINPERSOON == "")
descriptive[6, "Total"] <- nrow(missing_rin)
descriptive[6, names(table(missing_rin$org))] <- table(missing_rin$org)

id_combined <- id_combined %>%
  filter(!(RINPERSOONS == "" & RINPERSOON == ""))
stopifnot(nrow(id_combined) == 320669)  # sanity check vs. Stata log

# --- ONDERWERP: 456 obs with child ID but no Onderwerp ----------------------
# All 456 come from one org -> Icare. Verified these are duplicates of rows
# that DO have Onderwerp filled in for Icare -- safe to drop.
#
# FIXED: dup_icare now computed directly on id_combined (matching Stata's
# `gen(dup_icare)`, which creates a column on the full dataset even though
# `duplicates tag ... if org==12` only evaluates the condition for Icare
# rows). Earlier draft mistakenly built this on a separate filtered object,
# which would have broken the later `select(-dup_icare)` step.
table(id_combined$org[is.na(id_combined$Onderwerp)])

id_combined <- id_combined %>%
  group_by(RINPERSOONS, RINPERSOON, KindNummer_crypt, bestand, org) %>%
  mutate(
    dup_icare = if_else(org == "Icare", as.integer(n() > 1), NA_integer_)
  ) %>%
  ungroup()

table(id_combined$dup_icare[id_combined$org == "Icare" & is.na(id_combined$Onderwerp)])
table(id_combined$dup_icare[id_combined$org == "Icare" & !is.na(id_combined$Onderwerp)])

# Conclusion: safe to remove these 456 lines
id_combined <- id_combined %>% filter(!is.na(Onderwerp))

# --- DUPLICATES ---------------------------------------------------------------
# Child IDs alone are not unique...
sum(duplicated(id_combined$KindNummer_crypt))

# ...but almost unique by child ID + organization (bestand) + Onderwerp
sum(duplicated(id_combined[, c("KindNummer_crypt", "bestand", "Onderwerp")]))

id_combined <- id_combined %>%
  group_by(KindNummer_crypt, bestand, Onderwerp) %>%
  mutate(dup = n() > 1) %>%
  ungroup() %>%
  arrange(KindNummer_crypt, bestand, Onderwerp)

View(id_combined %>% filter(dup))
# 12 obs (6x2) share child ID + bestand + Onderwerp but have different
# RINPERSOON -- all from bestand==1 (LimburgNoord), Onderwerp==4
# (spraak-taal). Impossible to uniquely match to the health file, so dropped.
id_combined <- id_combined %>% filter(!dup)

# Follow-up: for these 6 flagged child IDs, health file DOES have a match
# under VISUS (org ZONL / databestand visus) -- ambiguity is specific to
# the spraak-taal linkage, not a wholesale data problem.
# NOTE: original do-file lists the 6 specific crypted child IDs here as
# comments for traceability -- omitted from this script; see the source
# do-file directly if you need to reference them.
#
# Documented (not yet implemented, as far as this script shows) future fix:
# disambiguate RINPERSOON using GBA fields (gender, place of residence)
# available in the health file.
# OPEN QUESTION: was this ever implemented, or are these 12 obs permanently
# excluded? Worth a sentence in the methods write-up either way.


# --- KNOWN CBS PLACEHOLDER HASH -----------------------------------------
# One specific KindNummer_crypt value showed 13,013 "duplicates," all
# org==ZONL, databestand==overgewicht. Investigated (see "20200604 ZONL
# Overgewicht" analysis) and confirmed via correspondence with CBS contact
# (Dec 2018 / June 2020 emails): CBS generates this specific hash as a
# stand-in whenever the uploaded ID variable was empty -- i.e. it's not a
# real child, it's CBS's null-value placeholder colliding across records.
placeholder_hash <- "07a6b530b148d85858a02420de14c297"  # CBS empty-value placeholder
id_combined <- id_combined %>% filter(KindNummer_crypt != placeholder_hash)


# ============================================================
# LINES IN DATA AFTER CLEANING
# ============================================================
# NOTE: the Stata comment at this point still reads "Total ... is 321638" --
# very likely a stale copy-pasted comment from the "before cleaning" block,
# since several drops happened since then. The *code* computes the real
# post-cleaning N correctly regardless; flagging the comment text as a
# copy-paste leftover worth fixing if you tidy the do-file later.

descriptive[7, "Total"] <- nrow(id_combined)
org_counts_after <- table(id_combined$org)
descriptive[7, names(org_counts_after)] <- org_counts_after

for (d in 1:4) {
  sub <- id_combined %>% filter(databestand_num == d)
  descriptive[7 + d, "Total"] <- nrow(sub)
  org_n <- table(sub$org)
  descriptive[7 + d, names(org_n)] <- org_n
}
print(descriptive)

# Write descriptive matrix to Excel
wb <- createWorkbook()
addWorksheet(wb, "descriptive")
writeData(wb, "descriptive", descriptive, rowNames = TRUE)
saveWorkbook(wb, "H:/Coen/Output/202009/IDfile_descriptives.xlsx", overwrite = TRUE)


# ============================================================
# COUNTRY NAME CLEANING -- gebl1 AND gebl2 (Stata lines 277-451)
# ============================================================
# Mechanical find-replace, no research decisions in the base list itself.
# Run your pasted Stata "replace ... if ..." blocks through
# stata_replace_to_r_casewhen.py to regenerate the case_when() bodies
# below rather than retyping them by hand -- paste the script's output
# in place of the "..." placeholders.

gebl_recode_base <- function(x) {
  case_when(
    # --- paste output of stata_replace_to_r_casewhen.py here ---
    # (covers Stata lines 277-356 for gebl1 / 360-439 for gebl2 --
    # identical list applied to both variables)
    TRUE ~ x
  )
}

# gebl2-specific additional corrections (Stata lines 442-451) -- NOT
# present in the gebl1 list. Flagged above: worth confirming gebl1 never
# contained these raw values, rather than assuming the asymmetry is fine.
gebl2_additional_recode <- function(x) {
  case_when(
    x == "Abessinië"                        ~ "Ethiopië",
    x == "Abessini?"                        ~ "Ethiopië",
    x == "Centafrika"                       ~ "Centraal-Afrikaanse Republiek",
    x == "Centrafrika"                      ~ "Centraal-Afrikaanse Republiek",
    x == "Faer?er"                          ~ "Faröer",
    x == "Federale Republiek Joegoslavi?"   ~ "Joegoslavië",
    x == "Oekraine"                         ~ "Oekraïne",
    x == "Rhodesi?"                         ~ "Zimbabwe",
    x == "Rhodesië"                         ~ "Zimbabwe",
    x == "Russische Federatie"              ~ "Rusland",
    TRUE ~ x
  )
}

health_clean <- health_clean %>%
  mutate(
    gebl1 = gebl_recode_base(gebl1),
    gebl2 = gebl_recode_base(gebl2),
    gebl2 = gebl2_additional_recode(gebl2)  # applied only to gebl2
  )


# ============================================================
# SET ALL STRINGS TO CAPITALS ONLY (Stata lines 457-464)
# ============================================================
# NOTE: this runs AFTER the specific-value replace() cleaning above, so
# the case_when() matches are against the mixed-case originals -- if you
# ever reorder these steps, the case_when() matching will silently stop
# working since it's not case-insensitive as written.
health_clean <- health_clean %>%
  mutate(
    gem       = toupper(gem),
    woonplaats = toupper(gem),  # NOTE: source line 461 assigns FROM `gem`,
                                 # not `woonplaats`, on the right-hand side --
                                 # transcribed exactly as written; confirm
                                 # this isn't a typo in the original do-file,
                                 # since it means woonplaats gets overwritten
                                 # with (uppercased) gem's value, not its own.
    gebl1     = toupper(gebl1),
    gebl2     = toupper(gebl2)
  )


# ============================================================
# DESTRING VARIABLES THAT ARE STORED AS STRING BUT ARE NUMERIC
# (Stata lines 467-491)
# ============================================================

# --- Postcode ---------------------------------------------------------------
# QA step baked into the original do-file: flag postcodes that don't
# convert cleanly to numeric, manually inspect them (Stata's `browse`),
# then force-convert -- anything still non-numeric becomes NA silently.
# OPEN QUESTION (flagged above): how many rows were affected, and was
# anything substantively lost in the force-conversion? Worth documenting.
notnumeric_postcode <- health_clean %>%
  filter(is.na(suppressWarnings(as.numeric(postcode))))
# View(notnumeric_postcode)  # equivalent of Stata's `browse if notnumeric==1`

health_clean <- health_clean %>%
  mutate(postcode = suppressWarnings(as.numeric(postcode)))
# `destring ..., replace force` in Stata silently sets unconvertible values
# to missing -- as.numeric() does the same (with an R warning, suppressed
# here since Stata's `force` suppresses the equivalent warning too).

# --- Remaining destrings: straightforward numeric conversions --------------
# No `force`/QA step attached to these in the source -- presumably already
# clean. If any of these throw unexpected NAs after conversion, that's a
# sign the source wasn't as clean as the original do-file assumed.
health_clean <- health_clean %>%
  mutate(
    geslacht = suppressWarnings(as.numeric(geslacht)),
    i_jgz    = suppressWarnings(as.numeric(i_jgz)),
    opl1     = suppressWarnings(as.numeric(opl1)),
    opl2     = suppressWarnings(as.numeric(opl2)),
    i_cm     = suppressWarnings(as.numeric(i_cm)),
    bst_nl   = suppressWarnings(as.numeric(bst_nl))
  )


# ============================================================
# HEALTH FILE DESCRIPTIVES: LINES IN DATA BEFORE CLEANING
# ============================================================
# Second descriptives matrix, mirrors the id_combined one built earlier,
# but for health_clean. NOTE: uses `org` and `databestand` as they exist
# NATIVELY on the health file (not the versions we constructed for
# id_combined from bestand/Onderwerp) -- flagged above: confirm CBS's
# own coding for these matches the 6/7/10/12 org scheme and 1-4 domain
# scheme used elsewhere, or these counts will look wrong without erroring.
#
# Row 6 label is "All-Duplicates" here (vs. "Missing-RIN" in the id_combined
# matrix) -- signals the health-file cleaning ahead tracks duplicate rows,
# not missing RINs.

descriptive_health <- matrix(NA_real_, nrow = 11, ncol = 5)
colnames(descriptive_health) <- c("Total", "LimburgNoord", "ZONL", "Verian", "Icare")
rownames(descriptive_health) <- c(
  "All", "Overgewicht", "Visus", "Psychosociaal", "Spraak-taal",
  "All-Duplicates",
  "All", "Overgewicht", "Visus", "Psychosociaal", "Spraak-taal"
)

# TOTAL
descriptive_health[1, "Total"] <- nrow(health_clean)
org_counts_health <- table(health_clean$org)
descriptive_health[1, names(org_counts_health)] <- org_counts_health

# BY ONDERWERP (domain) -- using health file's own native `databestand`
for (d in 1:4) {
  sub <- health_clean %>% filter(databestand == d)
  descriptive_health[1 + d, "Total"] <- nrow(sub)
  org_n <- table(sub$org)
  descriptive_health[1 + d, names(org_n)] <- org_n
}
print(descriptive_health)


# ============================================================
# SAVE CLEANED DATA FOR MERGES WITH HEALTH DATA
# ============================================================

id_combined <- id_combined %>% select(-dup, -dup_icare)
# `compress` (Stata: shrink storage types) has no direct R equivalent --
# R doesn't store data the same way; skip, or use data.table::setDT() if
# memory becomes an issue with 320k+ rows.

# 1. Save all data
saveRDS(id_combined, "H:/Coen/JGZdata/IDfile_clean_all.rds")

# 2. [DISABLED IN SOURCE -- historical only, superseded by corrected ZONL
# ID data merged earlier in this script]
# zonl_visus <- id_combined %>% filter(org == "ZONL", databestand == "visus")
# saveRDS(zonl_visus, "H:/Coen/JGZdata/IDfile_clean_ZONL_visus.rds")

# 3. Save only Icare-overgewicht
icare_overgewicht <- id_combined %>%
  filter(org == "Icare", databestand == "overgewicht")
saveRDS(icare_overgewicht, "H:/Coen/JGZdata/IDfile_clean_Icare_overgewicht.rds")
# OPEN QUESTION: why does Icare-overgewicht get its own saved subset but no
# other org/domain combination does? If there's a reason (e.g. a separate
# downstream merge/analysis), worth a comment in the do-file for future-you.


# ============================================================
# CLEAN HEALTH FILE
# ============================================================

health_clean <- read_dta("H:/Coen/JGZdata/8151GezondheidsbestandxV2CBKV1.dta") %>%
  arrange(org, KindNummer_crypt, databestand, leeftijd)

# FIX (previously only described in chat, never actually applied to this
# file -- confirmed via a later RStudio error screenshot: health_clean$org
# was still raw numeric, not the labeled factor id_combined$org has).
# Applying it here, at the top of the health-file section, so it's in
# effect for every downstream step: switch-flagging, category cleanup,
# the collapse, and -- critically -- the merge with id_combined later,
# which needs both sides' org/databestand to match on the SAME
# representation. Same 6/7/10/12 and 1-4 coding confirmed via the earlier
# error output, so this should align; worth a spot-check after running
# rather than assuming.
health_clean$org <- factor(
  health_clean$org,
  levels = c(6, 7, 10, 12),
  labels = c("LimburgNoord", "ZONL", "Verian", "Icare")
)
health_clean$databestand <- factor(
  health_clean$databestand,
  levels = c(1, 2, 3, 4),
  labels = c("overgewicht", "visus", "psychosociaal", "spraak-taal")
)

# --- Clean names of countries (gebl1) -----------------------------------
# Source data has encoding-mangled Dutch diacritics (likely an SPSS/CBS
# import artifact where accented characters became "?"). Mechanical
# find-replace, not a research decision -- but the volume of corrections
# is worth noting as a data-quality issue from the original supplier.
# NOTE: table continues past what's been photographed so far (cuts off
# after Argentinië/Armenië) -- append further replace lines as you send them.
health_clean <- health_clean %>%
  mutate(gebl1 = case_when(
    gebl1 == "Afganistan"  ~ "Afghanistan",
    gebl1 == "Albani?"     ~ "Albanië",
    gebl1 == "Argentini?"  ~ "Argentinië",
    TRUE ~ gebl1
  ))


# ============================================================
# CHECK AND CLEAN DUPLICATES ON ALL VARIABLES (Stata lines 541-583)
# ============================================================
# "Fewest duplicates" check excludes `dubbel` per the source comment.
# OPEN QUESTION: what is `dubbel`? If it's itself a pre-existing
# duplicate-flag column from CBS, excluding it makes sense -- worth
# confirming before trusting the "3,166 deleted" count below.

dup_check_vars <- setdiff(names(health_clean), "dubbel")

health_clean <- health_clean %>%
  group_by(across(all_of(dup_check_vars))) %>%
  mutate(.dup_count = row_number()) %>%
  ungroup()

n_before <- nrow(health_clean)
health_clean <- health_clean %>% filter(.dup_count == 1) %>% select(-.dup_count)
n_after <- nrow(health_clean)
# Source do-file notes "3,166 observations deleted" -- sanity check:
message(sprintf("Dropped %d exact-duplicate rows (source do-file: 3,166)", n_before - n_after))


# ============================================================
# VARIABLE-LEVEL SWITCH FLAGS (Stata lines 588-745, "variations" matrix)
# ============================================================
# For each variable, within org-KindNummer_crypt-databestand-leeftijd
# ("contact moment") groups, flag whether its value SWITCHES across the
# still-duplicated rows in that group:
#   - "with missings": a switch counts even if only missing <-> a value
#   - "without missings": a switch only counts between two REAL values
#     (i.e. a genuine conflict, not just incompleteness)
#
# IMPORTANT ASYMMETRY IN THE SOURCE DO-FILE (flagged above, replicated
# here rather than silently harmonized): the numeric "with missings"
# branch requires the PREVIOUS row to be non-missing; the string branch
# instead requires the CURRENT row to be non-missing, with no check on
# the previous row at all.
#
# These flag columns are NOT temporary -- they stay in the dataset and
# get reused below for both the FRANKENSTEIN split and the 5-category
# cleanup, matching how the source do-file keeps reusing them across
# several later sections without recomputing.

group_vars <- c("org", "KindNummer_crypt", "databestand", "leeftijd")
all_vars <- setdiff(names(health_clean), group_vars)

flag_switches <- function(df, var, group_vars) {
  is_num <- is.numeric(df[[var]])

  if (is_num) {
    # Numeric: Stata sorts missings LAST
    df <- df %>%
      group_by(across(all_of(group_vars))) %>%
      arrange(.data[[var]], .by_group = TRUE) %>%
      mutate(
        .is_miss   = is.na(.data[[var]]),
        .prev_val  = lag(.data[[var]]),
        .prev_miss = lag(.is_miss),
        f   = if_else(row_number() != 1 & !.prev_miss &
                        .data[[var]] != .prev_val, 1, NA_real_),
        f_n = if_else(row_number() != 1 & !.prev_miss & !.is_miss &
                        .data[[var]] != .prev_val, 1, NA_real_)
      )
  } else {
    # String: Stata sorts missings ("") FIRST -- and per the source
    # do-file, checks CURRENT non-missing, not previous (asymmetric vs.
    # the numeric branch, kept exactly as written)
    df <- df %>%
      group_by(across(all_of(group_vars))) %>%
      arrange(.data[[var]], .by_group = TRUE) %>%
      mutate(
        .is_miss   = .data[[var]] == "" | is.na(.data[[var]]),
        .prev_val  = lag(.data[[var]]),
        .prev_miss = lag(.is_miss),
        f   = if_else(row_number() != 1 & !.is_miss &
                        .data[[var]] != .prev_val, 1, NA_real_),
        f_n = if_else(row_number() != 1 & !.is_miss & !.prev_miss &
                        .data[[var]] != .prev_val, 1, NA_real_)
      )
  }

  df <- df %>%
    mutate(
      max_f   = suppressWarnings(max(f,   na.rm = TRUE)),
      max_f   = if_else(is.infinite(max_f),   0, max_f),
      max_f_n = suppressWarnings(max(f_n, na.rm = TRUE)),
      max_f_n = if_else(is.infinite(max_f_n), 0, max_f_n)
    ) %>%
    ungroup() %>%
    select(-.is_miss, -.prev_val, -.prev_miss)

  names(df)[names(df) == "f"]       <- paste0("f_", var)
  names(df)[names(df) == "f_n"]     <- paste0("f_n_", var)
  names(df)[names(df) == "max_f"]   <- paste0("max_f_", var)
  names(df)[names(df) == "max_f_n"] <- paste0("max_f_n_", var)
  df
}

for (v in all_vars) {
  health_clean <- flag_switches(health_clean, v, group_vars)
}

# --- Diagnostics matrix (matches Stata's "variations" matrix/Excel export) --
# NOTE: computed via direct set operations on the flag columns above rather
# than replicating Stata's d_first row-marking trick -- same results, less
# code, since d_first's only purpose there was counting distinct groups.
variations <- matrix(NA_real_, nrow = length(all_vars), ncol = 8)
rownames(variations) <- all_vars
colnames(variations) <- c(
  "N", "Unique contact moments (N)",
  "Switches (with missings)", "CMs switches (with missings)", "Rows affected (with)",
  "Switches (without missings)", "CMs switches (without missings)", "Rows affected (without)"
)
# NOTE: source Stata matrix reuses the literal label "Rows affected" for
# both col5 and col8 -- renamed here to "(with)"/"(without)" since R
# matrices need unique colnames; values/meaning are unchanged.

grp_id <- health_clean %>%
  group_by(across(all_of(group_vars))) %>%
  mutate(.grp_id = cur_group_id()) %>%
  ungroup() %>%
  pull(.grp_id)

for (v in all_vars) {
  is_num <- is.numeric(health_clean[[v]])
  is_miss <- if (is_num) is.na(health_clean[[v]]) else
    (health_clean[[v]] == "" | is.na(health_clean[[v]]))

  f_v       <- health_clean[[paste0("f_", v)]]
  f_n_v     <- health_clean[[paste0("f_n_", v)]]
  max_f_v   <- health_clean[[paste0("max_f_", v)]]
  max_f_n_v <- health_clean[[paste0("max_f_n_", v)]]

  variations[v, 1] <- sum(!is_miss)
  variations[v, 2] <- length(unique(grp_id[!is_miss]))
  variations[v, 3] <- sum(f_v, na.rm = TRUE)
  variations[v, 4] <- length(unique(grp_id[max_f_v == 1]))
  variations[v, 5] <- sum(max_f_v == 1, na.rm = TRUE)
  variations[v, 6] <- sum(f_n_v, na.rm = TRUE)
  variations[v, 7] <- length(unique(grp_id[max_f_n_v == 1]))
  variations[v, 8] <- sum(max_f_n_v == 1, na.rm = TRUE)
}
print(variations)

wb2 <- createWorkbook()
addWorksheet(wb2, "variations")
writeData(wb2, "variations", variations, rowNames = TRUE)
saveWorkbook(wb2, "H:/Coen/Output/202009/Gezondheidsbestand_variables_within_onderwerp.xlsx", overwrite = TRUE)

saveRDS(health_clean, "H:/Coen/TempData/Health_data_cleaning.rds")


# ============================================================
# CREATE FRANKENSTEIN FILE (Stata lines 759-800)
# ============================================================
# Rows belonging to any contact-moment group where AT LEAST ONE variable
# has a genuine conflicting value (max_f_n_<var> == 1) get flagged and
# split into a separate file -- these are the truly ambiguous duplicate
# rows, as opposed to ones that only differ by missingness.

health_clean <- health_clean %>%
  mutate(f_frankenstein = if_else(
    if_any(all_of(paste0("max_f_n_", all_vars)), ~ . == 1), 1, NA_real_
  ))

n_flagged <- sum(health_clean$f_frankenstein == 1, na.rm = TRUE)
message(sprintf("%d rows flagged for FRANKENSTEIN file", n_flagged))

frankenstein <- health_clean %>% filter(f_frankenstein == 1)

# Blank out "fine" (non-conflicting) variables on flagged rows, keeping
# only the group-identifying variables intact
for (v in all_vars) {
  max_f_n_v <- frankenstein[[paste0("max_f_n_", v)]]
  if (is.numeric(frankenstein[[v]])) {
    frankenstein[[v]][max_f_n_v != 1] <- NA
  } else {
    frankenstein[[v]][max_f_n_v != 1] <- ""
  }
}

frankenstein <- frankenstein %>% select(-f_frankenstein)
saveRDS(frankenstein, "H:/Coen/JGZdata/FRANKENSTEIN.rds")


# ============================================================
# CLEAN UP DATABESTAND: 5-CATEGORY VARIABLE TREATMENT
# (Stata lines 805-900+)
# ============================================================
# Continues from the FULL (still-duplicated-per-contact-moment) dataset,
# NOT the FRANKENSTEIN subset. Every variable falls into one of 5
# categories based on how it behaves within a contact-moment group.

cat_by <- group_vars  # 1. group identifiers -- untouched

cat_nothing <- c(
  "postcode", "woonplaats", "gem", "st_a", "geslacht", "i_cm", "jaartal",
  "kindmogelijknietunieknr2", "maanden", "bril", "lh_r", "lh_l", "sc_ep",
  "sc_gp", "sc_pl", "sc_ha", "sc_psg", "sdq_i", "zwduur", "dat_a_str"
)  # 2. left completely as-is

cat_fill_empty <- c(
  "so_a2", "apkt_r4", "apkt_l4", "srt_o", "spontaan", "vragen1",
  "verstaan2", "vragen2", "r_gd_vve"
)  # 3. fill missing with the group's single real value

cat_dummies <- c(
  "so_a", "i_jgz", "indi", "inter", "verwijs", "opl1", "opl2",
  "kindmogelijknietunieknr", "dubbel", "bst_nl", "taalomg", "meertaal",
  "d_vvenum"
)  # 4. create dummy variables for each value + a missing-flag

cat_remove <- c(
  "lengte", "gewicht", "gebl1", "gebl2", "schoolnr", "lft2", "oog_o",
  "apk_r", "apk_l", "apkt_r3", "apkt_l3", "apkt_r5", "apkt_l5", "lc_r",
  "lc_l", "visus_c", "scr_psp", "sdq_t", "zin2w", "pop6", "noemt",
  "wijstaan", "zin3w", "verstaan1", "dat_a2_str"
)  # 5. genuine conflicts removed; missing-vs-single-value filled

# --- Category 3: fill empty values ------------------------------------------
# OPEN QUESTION (flagged above): Stata fills using sorted var[1]. For
# NUMERIC variables this correctly grabs a real value (missings sort
# last). For STRING variables, missings sort FIRST, so var[1] would be
# blank whenever any row in the group is missing -- potentially erasing
# real values instead of filling them. Confirm whether any of these 9
# variables are string-typed before trusting this step for them.
for (v in cat_fill_empty) {
  health_clean <- health_clean %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(!!v := {
      x <- .data[[v]]
      if (is.numeric(x)) {
        first_val <- suppressWarnings(min(x, na.rm = TRUE))
        if (is.infinite(first_val)) x else rep(first_val, length(x))
      } else {
        sorted_x <- sort(x)  # "" sorts first, replicated as-written
        rep(sorted_x[1], length(x))
      }
    }) %>%
    ungroup()
}

# --- Category 4: create dummies for multi-value variables -------------------
for (v in cat_dummies) {
  x <- health_clean[[v]]
  is_num <- is.numeric(x)
  miss_flag <- if (is_num) is.na(x) else (x == "" | is.na(x))
  health_clean[[paste0("d_", v, "_miss")]] <- if_else(miss_flag, 1, NA_real_)

  lvls <- unique(x[!miss_flag])
  for (lvl in lvls) {
    lvl_name <- gsub("[^A-Za-z0-9]", "_", as.character(lvl))
    health_clean[[paste0("d_", v, "_", lvl_name)]] <- if_else(x == lvl, 1, NA_real_)
  }
}

# --- Category 5: remove genuine conflicts, fill missing-only variation ------
# BUG FIX (caught by a run-time error: "gebl1 must be size 1, not
# 1079437"): max_f_v/max_f_n_v were pulled ONCE from the full, ungrouped
# health_clean before the loop below, as plain external vectors. Inside
# the grouped mutate(), dplyr automatically subsets .data columns to the
# current group's rows -- but it does NOT subset external objects like
# these captured vectors, which stayed at full dataset length. So `x`
# (from .data[[v]], correctly group-length) and `cond` (built from the
# full-length external vectors) had mismatched lengths, and `x[cond] <-
# fill_val` silently padded x out to the external vectors' full length
# instead of erroring immediately. Fix: reference max_f_/max_f_n_ via
# .data INSIDE the mutate, so they get subset to the current group too.
for (v in cat_remove) {
  max_f_n_v <- health_clean[[paste0("max_f_n_", v)]]  # full-length use below is fine -- not inside a grouped mutate
  is_num <- is.numeric(health_clean[[v]])

  # Genuine conflicts -> "removed" marker. Stata uses the extended
  # missing `.r`; R has no equivalent, so plain NA is used for numeric
  # (loses the "removed-due-to-conflict" vs. "always-missing" distinction
  # Stata's `.r` would preserve -- add a separate indicator column if you
  # need that distinction later). String columns use the literal ".r"
  # marker, matching the source do-file's own literal-string workaround.
  if (is_num) {
    health_clean[[v]][max_f_n_v == 1] <- NA
  } else {
    health_clean[[v]][max_f_n_v == 1] <- ".r"
  }

  # Missing-vs-single-real-value groups -> fill with the real value
  max_f_col   <- paste0("max_f_", v)
  max_f_n_col <- paste0("max_f_n_", v)
  health_clean <- health_clean %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(!!v := {
      x <- .data[[v]]
      cond <- .data[[max_f_col]] == 1 & .data[[max_f_n_col]] != 1
      if (any(cond, na.rm = TRUE)) {
        fill_val <- if (is_num) suppressWarnings(min(x, na.rm = TRUE)) else sort(x)[length(x)]
        x[cond] <- fill_val
      }
      x
    }) %>%
    ungroup()
}


# ============================================================
# COLLAPSE TO 1 LINE PER org-KindNummer_crypt-databestand-leeftijd
# (Stata lines 899-916)
# ============================================================
# CORRECTION: an earlier draft used distinct() as a placeholder here,
# since the actual command was cut off in the photographed range at the
# time. Now visible -- the real Stata command is a `collapse`, not a
# "keep first row": (firstnm) for string variables (first NON-MISSING
# value in the group), (max) for everything numeric, including the
# category-4 dummy variables (a 0/1/NA dummy's max across the group
# correctly reduces to "was this level ever present in this group").
# Since category 3/4/5 cleanup already equalized values within each
# group wherever the source data allowed, (max)/(firstnm) mostly just
# mechanically pick "the" value rather than aggregate meaningfully --
# but for any group where a genuine conflict was marked ".r"/NA
# (category 5) and no fill applied, the collapse output reflects that.
#
# NOTE: Stata's own (firstnm) list at this point includes `schoolnr` and
# `visus_c` as STRING variables -- worth reconciling against the
# `cat_remove` category-5 handling earlier, which treated these
# dynamically by whatever type they actually are. If they're numeric in
# your data, Stata's `firstnm` treatment there would be unusual for a
# numeric var (Stata's firstnm only applies within the string variable
# list) -- flagging as worth a type check, not silently resolved here.

flag_cols <- unlist(lapply(all_vars, function(v) paste0(c("f_", "max_f_", "f_n_", "max_f_n_"), v)))
health_clean <- health_clean %>% select(-any_of(flag_cols))

collapse_vars <- setdiff(names(health_clean), group_vars)

health_clean <- health_clean %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    across(all_of(collapse_vars), function(x) {
      if (is.character(x)) {
        # (firstnm): first non-missing, non-empty value
        v <- x[!is.na(x) & x != ""]
        if (length(v)) v[1] else NA_character_
      } else {
        # (max): ignoring missing; all-missing group -> NA (Stata's `max`
        # returns missing here too, not -Inf, so convert explicitly)
        m <- suppressWarnings(max(x, na.rm = TRUE))
        if (is.infinite(m)) NA_real_ else m
      }
    }),
    .groups = "drop"
  )

# Equivalent of Stata's `duplicates report org KindNummer_crypt databestand
# leeftijd` -- should report zero after a correct collapse; stopifnot()
# turns a silent problem into a hard error instead.
duplicates_check <- health_clean %>%
  count(across(all_of(group_vars))) %>%
  filter(n > 1)
stopifnot(nrow(duplicates_check) == 0)

saveRDS(health_clean, "H:/Coen/TempData/Health_by_onderwerp.rds")


# ============================================================
# HEALTH FILE DESCRIPTIVES: LINES IN DATA AFTER CLEANING
# (Stata lines 927-968)
# ============================================================
descriptive_health[7, "Total"] <- nrow(health_clean)
org_counts_health_after <- table(health_clean$org)
descriptive_health[7, names(org_counts_health_after)] <- org_counts_health_after

for (d in 1:4) {
  sub <- health_clean %>% filter(databestand == levels(health_clean$databestand)[d])
  descriptive_health[7 + d, "Total"] <- nrow(sub)
  org_n <- table(sub$org)
  descriptive_health[7 + d, names(org_n)] <- org_n
}
print(descriptive_health)

wb3 <- createWorkbook()
addWorksheet(wb3, "descriptive")
writeData(wb3, "descriptive", descriptive_health, rowNames = TRUE)
saveWorkbook(wb3, "H:/Coen/Output/202009/HealthFile_descriptives.xlsx", overwrite = TRUE)


# ============================================================
# MERGING HEALTH AND ID FILE (Stata lines 975-1043)
# ============================================================
# Small helper replicating Stata's `merge` _merge indicator (1 = master
# only, 2 = using only, 3 = matched) for an m:1 join -- dplyr's join
# functions don't produce this by default, so it's built manually here.
stata_merge <- function(master, using, by) {
  master <- master %>% mutate(.in_master = TRUE)
  using  <- using  %>% mutate(.in_using  = TRUE)
  merged <- full_join(master, using, by = by, suffix = c("", ".using"))
  merged %>%
    mutate(
      .in_master = !is.na(.in_master),
      .in_using  = !is.na(.in_using),
      `_merge` = case_when(
        .in_master & !.in_using ~ 1,
        !.in_master & .in_using ~ 2,
        TRUE ~ 3
      )
    ) %>%
    select(-.in_master, -.in_using)
}

merges <- matrix(NA_real_, nrow = 3, ncol = 2)
rownames(merges) <- c("All three", "Icare-visus", "Total")
colnames(merges) <- c("Using only", "Matched")

# --- MERGE 1: merge on all 3 identifying variables (org, KindNummer_crypt,
# databestand), EXCEPT Icare-visus, which is merged separately below -------
id_file_all <- readRDS("H:/Coen/JGZdata/IDfile_clean_all.rds")

health_merge1 <- health_clean %>%
  filter(!(org == "Icare" & databestand == "visus")) %>%
  stata_merge(id_file_all, by = c("org", "KindNummer_crypt", "databestand"))

merges[1, "Using only"] <- sum(health_merge1$`_merge` == 1)
merges[1, "Matched"]    <- sum(health_merge1$`_merge` == 3)

health_merge1 <- health_merge1 %>% filter(`_merge` != 2)  # keep `_merge` -- reused below
saveRDS(health_merge1, "H:/Coen/JGZdata/Health_ID_merge1.rds")

# --- MERGE 3 (named "3" for historical reasons -- an older version of the
# do-file had a separate ZONL-overgewicht step counted as "MERGE 2", since
# removed, but the numbering was never updated -- per the source comment):
# merge Icare-visus using child IDs sourced from Icare-OVERGEWICHT instead.
# THIS IS THE ANSWER to the earlier open question (decision-log #10) about
# why Icare-overgewicht was saved as its own subset: Icare's visus records
# apparently can't be reliably ID-matched directly, so they're matched via
# the same children's overgewicht records instead. -------------------------
icare_overgewicht_ids <- readRDS("H:/Coen/JGZdata/IDfile_clean_Icare_overgewicht.rds")

health_merge3 <- health_clean %>%
  filter(org == "Icare", databestand == "visus") %>%
  stata_merge(icare_overgewicht_ids, by = c("org", "KindNummer_crypt"))

merges[2, "Using only"] <- sum(health_merge3$`_merge` == 1)
merges[2, "Matched"]    <- sum(health_merge3$`_merge` == 3)

health_merge3 <- health_merge3 %>% filter(`_merge` != 2)  # keep `_merge`
saveRDS(health_merge3, "H:/Coen/JGZdata/Health_ID_merge3.rds")

# --- Append the two merges, then recompute the "Total" row -----------------
# Recomputed directly from the appended data (matching the source do-file's
# own redundant-but-deliberate recomputation) rather than summing rows 1+2
# -- kept as a genuine cross-check in case something doesn't add up.
health_merged <- bind_rows(health_merge1, health_merge3)

merges[3, "Using only"] <- sum(health_merged$`_merge` == 1)
merges[3, "Matched"]    <- sum(health_merged$`_merge` == 3)
print(merges)

# CORRECTION: an earlier draft dropped `_merge` here. The next batch of
# the do-file (CLEAN MERGED FILE section) still needs it -- `drop if
# _merge==1` removes health-only rows that never found an ID-file match
# at all. Keeping it through that step now; see below for where it's
# actually dropped.

wb4 <- createWorkbook()
addWorksheet(wb4, "merges")
writeData(wb4, "merges", merges, rowNames = TRUE)
saveWorkbook(wb4, "H:/Coen/Output/202009/Merges.xlsx", overwrite = TRUE)


# ============================================================
# CLEAN MERGED FILE (Stata lines 1046-1281+)
# ============================================================

# --- Finish the merge: drop health-only rows that never matched an ID ------
# Source comment literally says "drop variables that could not be merged"
# but the command (`drop if _merge==1`) drops ROWS, not variables --
# transcribed faithfully rather than "corrected", since that's what the
# do-file actually does.
health_merged <- health_merged %>% filter(`_merge` != 1) %>% select(-`_merge`)

# Stata's `order` is a cosmetic column-reorder with no real R equivalent
# need, but relocate() gives the same visual effect if you want it:
health_merged <- health_merged %>% relocate(RINPERSOONS, RINPERSOON)

# --- Drop rows without RINPERSOONS (can't be traced to a person) -----------
health_merged <- health_merged %>% filter(RINPERSOONS != "")

# --- Rename overly long dummy-variable names --------------------------------
# Stata has a 32-character variable-name limit; R doesn't, so this step is
# cosmetic-only here. Keeping the renames anyway for 1:1 naming parity with
# the source. NOTE: only 3 renames were visible in the photographed range
# (d_kindmogelijknietunieknr_{miss,0,1} -> d_kmn_uniek_*) -- if the do-file
# has more of these (e.g. other overlong dummy names), add them here.
health_merged <- health_merged %>%
  rename(
    d_kmn_uniek_miss = d_kindmogelijknietunieknr_miss,
    d_kmn_uniek_0     = d_kindmogelijknietunieknr_0,
    d_kmn_uniek_1     = d_kindmogelijknietunieknr_1
  )


# ============================================================
# CHECK AND CLEAN DUPLICATES ON ALL VARIABLES -- MERGED FILE
# (Stata lines 1085-1103)
# ============================================================
# IMPORTANT (per the source do-file's own comment, preserved here):
# KindNummer_crypt is dropped because it's "no longer meaningful" after
# the merge -- duplicate checking now happens on RINPERSOONS/RINPERSOON
# (the real person identifiers) instead of the org-specific child ID.
# This is a deliberate widening of what counts as "the same record":
# rows are now compared ACROSS organizations, not just within one.
health_merged <- health_merged %>% select(-KindNummer_crypt)

n_before_dedup <- nrow(health_merged)
health_merged <- health_merged %>% distinct(across(everything()), .keep_all = TRUE)
n_after_dedup <- nrow(health_merged)
# Source do-file notes "482 observations deleted" -- sanity check:
message(sprintf("Dropped %d full-row duplicates (source do-file: 482)",
                 n_before_dedup - n_after_dedup))


# ============================================================
# SWITCH-DETECTION + VARIATIONS MATRIX -- MERGED FILE
# (Stata lines 1106-1277)
# ============================================================
# STRUCTURAL ECHO of the health-file switch-detection/cleanup done
# earlier, reusing the same flag_switches() helper -- but with ONE
# deliberate, consequential change: the grouping key is now
# RINPERSOONS + RINPERSOON + databestand + leeftijd. `org` is REMOVED
# from the group entirely (contrast with group_vars used earlier, which
# included org and KindNummer_crypt).
#
# WORTH CARRYING INTO YOUR METHODS WRITE-UP -- this is the ORIGINAL
# AUTHOR's own comment in the do-file, not something this translation is
# flagging: "IMPORTANT NOTE: ... Within RINPERSOONS-RINPERSOON-
# databestand-leeftijd: select variables that are constant on all lines
# vs. those that are not. Note: Goal is to obtain a data file with max.
# 1 line per child-domain-age combination. Therefore I remove the
# organisation number from the bysort command. From the fact that I find
# switches for the organisation number I can see that for some children
# information is supplied by multiple organisations. Why? Perhaps
# because when children move their whole file moves?" -- i.e. dropping
# `org` from the group was a deliberate choice to SURFACE cross-org
# duplication, and the author flags the underlying cause as an open,
# unresolved question. Worth checking whether this was ever answered
# before finalizing your methods section.

merged_group_vars <- c("RINPERSOONS", "RINPERSOON", "databestand", "leeftijd")
merged_all_vars <- setdiff(names(health_merged), merged_group_vars)
# Source do-file sizes its matrix as J(174, 8, .) -- i.e. 174 variables at
# this point. Using length(merged_all_vars) dynamically instead of
# hardcoding 174; if it doesn't equal 174, that's worth checking rather
# than ignoring, since it would mean the variable set drifted somewhere
# upstream of this translation.
message(sprintf("Variables entering merged-file switch detection: %d (source do-file: 174)",
                 length(merged_all_vars)))

for (v in merged_all_vars) {
  health_merged <- flag_switches(health_merged, v, merged_group_vars)
}

variations_merged <- matrix(NA_real_, nrow = length(merged_all_vars), ncol = 8)
rownames(variations_merged) <- merged_all_vars
colnames(variations_merged) <- c(
  "N", "Unique contact moments (N)",
  "Switches (with missings)", "CMs switches (with missings)", "Rows affected (with)",
  "Switches (without missings)", "CMs switches (without missings)", "Rows affected (without)"
)

grp_id_merged <- health_merged %>%
  group_by(across(all_of(merged_group_vars))) %>%
  mutate(.grp_id = cur_group_id()) %>%
  ungroup() %>%
  pull(.grp_id)

for (v in merged_all_vars) {
  is_num <- is.numeric(health_merged[[v]])
  is_miss <- if (is_num) is.na(health_merged[[v]]) else
    (health_merged[[v]] == "" | is.na(health_merged[[v]]))

  f_v       <- health_merged[[paste0("f_", v)]]
  f_n_v     <- health_merged[[paste0("f_n_", v)]]
  max_f_v   <- health_merged[[paste0("max_f_", v)]]
  max_f_n_v <- health_merged[[paste0("max_f_n_", v)]]

  variations_merged[v, 1] <- sum(!is_miss)
  variations_merged[v, 2] <- length(unique(grp_id_merged[!is_miss]))
  variations_merged[v, 3] <- sum(f_v, na.rm = TRUE)
  variations_merged[v, 4] <- length(unique(grp_id_merged[max_f_v == 1]))
  variations_merged[v, 5] <- sum(max_f_v == 1, na.rm = TRUE)
  variations_merged[v, 6] <- sum(f_n_v, na.rm = TRUE)
  variations_merged[v, 7] <- length(unique(grp_id_merged[max_f_n_v == 1]))
  variations_merged[v, 8] <- sum(max_f_n_v == 1, na.rm = TRUE)
}
print(variations_merged)

wb5 <- createWorkbook()
addWorksheet(wb5, "variations")
writeData(wb5, "variations", variations_merged, rowNames = TRUE)
saveWorkbook(wb5, "H:/Coen/Output/202009/MergedData_variables_within_onderwerp.xlsx", overwrite = TRUE)

saveRDS(health_merged, "H:/Coen/TempData/Merged_cleaning_within_onderwerp.rds")

# ============================================================
# "VARIABLES THAT NEED TO BE CLEANED BASED ON MATRIX" (Stata lines
# 1281+): unlike the health-file section, this is NOT a 4-category system
# -- only a short, specific list of variables gets touched at each of the
# passes below, using a "remove genuine conflicts, no fill" pattern.
# Reusable helper for that pattern:
remove_switches_only <- function(df, vars) {
  # Stata's `replace var=.r if max_f_n_var==1` (drop genuine conflicts to
  # an extended-missing marker). The PAIRED "fill missing-vs-single-value"
  # step (`*bysort ...: replace var=var[1] if max_f_var==1 & max_f_n_var
  # !=1`) is a LITERAL COMMENT in the source at every place this pattern
  # is used below -- i.e. genuinely disabled, not just omitted from this
  # translation. So unlike the health-file category-5 pass earlier, NO
  # filling happens here: a group that only varies by missingness (not a
  # genuine conflict) is left completely untouched.
  for (v in vars) {
    max_f_n_v <- df[[paste0("max_f_n_", v)]]
    if (is.numeric(df[[v]])) {
      df[[v]][max_f_n_v == 1] <- NA
    } else {
      df[[v]][max_f_n_v == 1] <- ".r"
    }
  }
  df
}


# ============================================================
# REMOVE GENUINE CONFLICTS -- FIRST PASS (Stata lines 1297-1323)
# ============================================================
# Only these variables get resolved at this point -- everything else
# keeps whatever switches the variations_merged matrix above found. Not
# explained in the do-file why only these were chosen; worth asking
# whether the rest were judged fine, or are handled later.
switches_to_remove_1 <- intersect(
  c("org", "woonplaats", "gem", "gebl1", "gebl2", "postcode", "gewicht", "zwduur", "bestand"),
  names(health_merged)
)
health_merged <- remove_switches_only(health_merged, switches_to_remove_1)


# ============================================================
# COLLAPSE INTO 1 LINE PER RINPERSOONS-RINPERSOON-databestand-leeftijd
# (Stata lines 1326-1342)
# ============================================================
flag_cols_merged <- unlist(lapply(merged_all_vars, function(v) paste0(c("f_", "max_f_", "f_n_", "max_f_n_"), v)))
health_merged <- health_merged %>% select(-any_of(flag_cols_merged))

collapse_vars_merged <- setdiff(names(health_merged), merged_group_vars)
# `org` collapses via (max) along with everything else here -- any
# genuine RIN-databestand-leeftijd conflict in org was already marked
# NA/".r" by remove_switches_only() just above, so (max) just mechanically
# picks the surviving value (or NA if every row in the group conflicted).

health_merged <- health_merged %>%
  group_by(across(all_of(merged_group_vars))) %>%
  summarise(
    across(all_of(collapse_vars_merged), function(x) {
      if (is.character(x)) {
        v <- x[!is.na(x) & x != ""]
        if (length(v)) v[1] else NA_character_
      } else {
        m <- suppressWarnings(max(x, na.rm = TRUE))
        if (is.infinite(m)) NA_real_ else m
      }
    }),
    .groups = "drop"
  )

saveRDS(health_merged, "H:/Coen/TempData/Merged_by_onderwerp.rds")


# ============================================================
# LOOK ACROSS HEALTH DOMAINS: THIRD SWITCH-DETECTION PASS
# (Stata lines 1362-1551, do-file's own header: "BELOW FINAL STEP WHERE
# YOU LOOK ACROSS HEALTH DOMAINS")
# ============================================================
# Grouping narrows AGAIN: now just RINPERSOONS + RINPERSOON + leeftijd.
# `databestand` (the health domain -- overgewicht/visus/psychosociaal/
# spraak-taal) is REMOVED from the group and becomes just another
# variable being checked for switches -- this pass asks "does this
# child's info agree across health DOMAINS at a given age," the same way
# the previous pass asked whether it agreed across ORGANIZATIONS.
#
# order/drop-empty-RIN/rename steps shown in the source here are all
# commented out (`*drop if...`, `*rename...`) -- already done earlier in
# the pipeline, kept inactive in Stata as a historical record. No R
# equivalent needed since nothing executes.

domain_group_vars <- c("RINPERSOONS", "RINPERSOON", "leeftijd")
domain_all_vars <- setdiff(names(health_merged), domain_group_vars)
message(sprintf("Variables entering across-domain switch detection: %d (source do-file: 172)",
                 length(domain_all_vars)))

for (v in domain_all_vars) {
  health_merged <- flag_switches(health_merged, v, domain_group_vars)
}

variations_domain <- matrix(NA_real_, nrow = length(domain_all_vars), ncol = 8)
rownames(variations_domain) <- domain_all_vars
colnames(variations_domain) <- c(
  "N", "Unique contact moments (N)",
  "Switches (with missings)", "CMs switches (with missings)", "Rows affected (with)",
  "Switches (without missings)", "CMs switches (without missings)", "Rows affected (without)"
)

grp_id_domain <- health_merged %>%
  group_by(across(all_of(domain_group_vars))) %>%
  mutate(.grp_id = cur_group_id()) %>%
  ungroup() %>%
  pull(.grp_id)

for (v in domain_all_vars) {
  is_num <- is.numeric(health_merged[[v]])
  is_miss <- if (is_num) is.na(health_merged[[v]]) else
    (health_merged[[v]] == "" | is.na(health_merged[[v]]))

  f_v       <- health_merged[[paste0("f_", v)]]
  f_n_v     <- health_merged[[paste0("f_n_", v)]]
  max_f_v   <- health_merged[[paste0("max_f_", v)]]
  max_f_n_v <- health_merged[[paste0("max_f_n_", v)]]

  variations_domain[v, 1] <- sum(!is_miss)
  variations_domain[v, 2] <- length(unique(grp_id_domain[!is_miss]))
  variations_domain[v, 3] <- sum(f_v, na.rm = TRUE)
  variations_domain[v, 4] <- length(unique(grp_id_domain[max_f_v == 1]))
  variations_domain[v, 5] <- sum(max_f_v == 1, na.rm = TRUE)
  variations_domain[v, 6] <- sum(f_n_v, na.rm = TRUE)
  variations_domain[v, 7] <- length(unique(grp_id_domain[max_f_n_v == 1]))
  variations_domain[v, 8] <- sum(max_f_n_v == 1, na.rm = TRUE)
}
print(variations_domain)

wb6 <- createWorkbook()
addWorksheet(wb6, "variations")
writeData(wb6, "variations", variations_domain, rowNames = TRUE)
saveWorkbook(wb6, "H:/Coen/Output/202009/MergedData_variables_across_onderwerp.xlsx", overwrite = TRUE)

saveRDS(health_merged, "H:/Coen/TempData/Merged_data_cleaning.rds")

# --- Diagnostic-only duplicates check (no rows dropped by this) ------------
# Stata's `duplicates report <huge explicit var list>` is, in practice,
# a full-row duplicate check (the list names essentially every variable);
# replicated that way here rather than retyping ~170 names.
dupes_across_domain <- health_merged %>%
  count(across(everything())) %>%
  filter(n > 1)
message(sprintf("Full-row duplicates remaining after across-domain collapse: %d groups",
                 nrow(dupes_across_domain)))

# --- AUTHOR'S OWN NOTES on which variables show switches -------------------
# Preserved verbatim from the source do-file's comments at this point --
# this is the researcher's own assessment, not a conclusion from this
# translation:
#   *org         -- CHECK: switches in org can originate due to moves?
#   *(KindNummer_crypt already deleted -- fine: some organizations use
#     different KindNummer_crypt per domain, as instructed)
#   *databestand -- fine: we combine domains (expected/intended)
#   *woonplaats  -- CHECK - all org 10 and 12
# "org" and "woonplaats" are flagged by the ORIGINAL AUTHOR as open
# questions needing a check, not resolved as of this point in the
# do-file. Worth following up on both before treating switches in these
# two variables as understood, rather than assuming this translation
# has settled them.


# ============================================================
# "NEW PROBLEM IDENTIFIED": same RIN has multiple lines within the
# SAME domain, even after the across-domain collapse (Stata lines
# 1568-1601)
# ============================================================
# Author's own diagnostic steps: `browse if max_f_n_woonplaats==1`
# (manual inspection, no R equivalent needed) and a `duplicates report`
# by RINPERSOONS+RINPERSOON+leeftijd+databestand (diagnostic only, no
# rows dropped by the report itself).
#
# Response: reapply the SAME "remove genuine conflicts, no fill" pattern
# to a slightly different variable list -- note `databestand` IS included
# this time (it wasn't a group key in this pass, so it can itself have
# switches), while `bestand` is NOT in this second list (unlike the first
# pass above). Reuses the max_f_n_* flags computed by the across-domain
# switch-detection pass just above -- NOT recomputed.

switches_to_remove_2 <- intersect(
  c("org", "databestand", "woonplaats", "gem", "gebl1", "gebl2", "postcode", "gewicht", "zwduur"),
  names(health_merged)
)
health_merged <- remove_switches_only(health_merged, switches_to_remove_2)

# ============================================================
# NEXT STEP IN THE SOURCE DO-FILE (not yet included here)
# ============================================================
# The photographed range ends here without an explicit further collapse
# or save for THIS pass -- unclear whether a `collapse ...
# by(RINPERSOONS RINPERSOON leeftijd)` (dropping databestand from the
# group entirely, to actually resolve "same RIN has multiple lines
# within the same domain" into one row) follows, or whether that happens
# further down the do-file. Do NOT assume the pipeline ends here --
# send the next batch to continue.



# ============================================================
# COLLAPSE INTO PANEL DATA (final step of the do-file)
# (Stata lines ~1609-1638)
# ============================================================

# --- Drop auxiliary flag columns from the third (domain-level) switch
# pass -- these were only needed to drive the switches_to_remove_2 step
# just above, and Stata's do-file drops them (via one very long explicit
# `drop f_... max_f_... f_n_... max_f_n_...` line) before the final
# collapse. Built dynamically from domain_all_vars/domain_group_vars
# (already defined above), rather than retyping ~170 explicit names --
# functionally identical to the Stata line.
domain_flag_cols <- unlist(lapply(domain_all_vars, function(v) {
  paste0(c("f_", "max_f_", "f_n_", "max_f_n_"), v)
}))
health_merged <- health_merged %>% select(-any_of(domain_flag_cols))

# NOTE: the do-file's very next line --
#   *drop additional variables (Can create dummies for org and domain)
#   *drop KindNummer_crypt databestand Onderwerp bestand
# -- is commented out (leading `*`) in the source, i.e. INACTIVE. No R
# equivalent needed/applied. (It's also effectively moot: none of
# KindNummer_crypt/databestand/Onderwerp/bestand appear in the explicit
# collapse variable list below, so they get dropped as a side effect of
# the collapse regardless of whether this line were active.)

# --- Final collapse to 1 row per child per age, combining ALL health
# domains (org, databestand, Onderwerp, bestand, etc. are no longer
# group vars -- this is the last narrowing step in the whole pipeline:
# org+KindNummer_crypt+databestand+leeftijd -> RINPERSOONS+RINPERSOON+
# databestand+leeftijd -> RINPERSOONS+RINPERSOON+leeftijd).
#
# IMPORTANT DIFFERENCE from the two earlier collapses in this script:
# those used *every remaining column* dynamically (the Stata source used
# macros covering all variables at that point). THIS collapse instead
# gives an EXPLICIT, hardcoded list of variable names for (firstnm) and
# (max) -- transcribed exactly as photographed. That means Stata's
# `collapse` here silently DROPS any variable not named in either list
# (collapse only keeps by-vars + explicitly listed vars) -- replicated
# faithfully below via explicit selection rather than "everything else".
#
# FLAG: cross-checking against the flag columns generated for the
# domain-level switch-detection pass just above (domain_all_vars), a
# few variables that got their own switch-checking earlier are NOT in
# either list below, so they are dropped from the final panel data:
#   zwduur, r_gd_vve, vragen2, and the d_dubbel_*, d_inter_*,
#   d_kmn_uniek_* dummy families.
# `vragen2` in particular looks like it could be an accidental omission
# given `vragen1`, `verstaan1`, `verstaan2` ARE kept but `vragen2` is
# not (an asymmetric pair) -- worth confirming against the actual do-file
# text/output var list before treating this as intentional.

firstnm_vars <- c(
  "woonplaats", "gem", "gebl1", "gebl2", "schoolnr", "visus_c",
  "dat_a_str", "dat_a2_str"
)

max_vars <- c(
  "org", "postcode", "geslacht", "i_cm", "st_a", "lengte", "gewicht",
  "lft2", "so_a2", "jaartal", "kindmogelijknietunieknr2", "maanden",
  "bril", "oog_o", "apk_r", "apk_l", "apkt_r3", "apkt_l3", "apkt_r4",
  "apkt_l4", "apkt_r5", "apkt_l5", "lh_r", "lh_l", "lc_r", "lc_l",
  "scr_psp", "sc_ep", "sc_gp", "sc_pl", "sc_ha", "sdq_t", "sc_psg",
  "sdq_i", "srt_o", "zin2w", "pop6", "noemt", "wijstaan", "zin3w",
  "verstaan1", "spontaan", "vragen1", "verstaan2",
  "d_so_a_16", "d_so_a_17", "d_so_a_19", "d_so_a_20", "d_so_a_22", "d_so_a_25",
  "d_i_jgz_miss", "d_i_jgz_1", "d_i_jgz_2", "d_i_jgz_3",
  "d_indi_miss", "d_indi_9", "d_indi_10", "d_indi_12", "d_indi_18", "d_indi_19",
  "d_verwijs_4", "d_verwijs_5", "d_verwijs_6", "d_verwijs_7", "d_verwijs_8",
  "d_verwijs_9", "d_verwijs_10", "d_verwijs_11", "d_verwijs_12", "d_verwijs_13",
  "d_verwijs_14", "d_verwijs_15", "d_verwijs_16",
  "d_opl1_miss", "d_opl1_0", "d_opl1_1", "d_opl1_2", "d_opl1_3", "d_opl1_4",
  "d_opl1_5", "d_opl1_6", "d_opl1_7", "d_opl1_8", "d_opl1_9", "d_opl1_98",
  "d_opl2_miss", "d_opl2_0", "d_opl2_1", "d_opl2_2", "d_opl2_3",
  "d_bst_nl_1", "d_bst_nl_2",
  "d_taalomg_miss", "d_taalomg_1", "d_taalomg_2", "d_taalomg_3",
  "d_meertaal_miss", "d_meertaal_1", "d_meertaal_2", "d_meertaal_3",
  "d_d_vvenum_miss", "d_d_vvenum_1", "d_d_vvenum_2"
)

# Sanity check: flag (don't silently ignore) any listed variable that
# isn't actually present in health_merged at this point -- would signal
# either an earlier rename/drop this translation didn't anticipate, or a
# transcription slip in the variable list above.
missing_firstnm <- setdiff(firstnm_vars, names(health_merged))
missing_max <- setdiff(max_vars, names(health_merged))
if (length(missing_firstnm) > 0) {
  warning("firstnm_vars not found in health_merged: ", paste(missing_firstnm, collapse = ", "))
}
if (length(missing_max) > 0) {
  warning("max_vars not found in health_merged: ", paste(missing_max, collapse = ", "))
}
firstnm_vars <- intersect(firstnm_vars, names(health_merged))
max_vars <- intersect(max_vars, names(health_merged))

panel_group_vars <- domain_group_vars  # RINPERSOONS, RINPERSOON, leeftijd

JGZ_panel <- health_merged %>%
  group_by(across(all_of(panel_group_vars))) %>%
  summarise(
    across(all_of(firstnm_vars), function(x) {
      v <- x[!is.na(x) & x != ""]
      if (length(v)) v[1] else NA_character_
    }),
    across(all_of(max_vars), function(x) {
      m <- suppressWarnings(max(x, na.rm = TRUE))
      if (is.infinite(m)) NA_real_ else m
    }),
    .groups = "drop"
  )

# `duplicates report RINPERSOONS RINPERSOON leeftijd` in the source is
# diagnostic-only (no rows dropped). Since this is exactly the group_by
# key just collapsed on, it should be mathematically guaranteed to be
# unique -- kept as a hard stopifnot() here (as with the two earlier
# collapses in this script) so a logic error surfaces immediately rather
# than silently passing through.
panel_dupes <- JGZ_panel %>%
  count(across(all_of(panel_group_vars))) %>%
  filter(n > 1)
stopifnot(nrow(panel_dupes) == 0)

# `sort RINPERSOONS RINPERSOON leeftijd` -- no functional effect in R
# (no positional dependencies downstream); included only for parity.
JGZ_panel <- JGZ_panel %>% arrange(across(all_of(panel_group_vars)))

# `compress` -- Stata-specific storage-size optimization (shrinks numeric
# storage types to the smallest that fits). No direct R equivalent /
# not needed for an .rds file; omitted.

saveRDS(JGZ_panel, "H:/Coen/TempData/JGZ_panel.rds")

# ============================================================
# END OF DO-FILE (as far as translated) -- this appears to be the
# terminal step: the final panel dataset, one row per child (RIN) per
# age (leeftijd), combining all health domains, saved as JGZ_panel.
# No further Stata lines have been provided past this point. Confirm
# with the source whether this is indeed the end of
# "20220330 JGZ DATA - CLEAN AND MERGE", or whether further sections
# follow it.
# ============================================================
