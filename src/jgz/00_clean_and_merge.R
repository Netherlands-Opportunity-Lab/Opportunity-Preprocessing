# =============================================================================
# JGZ pilot -- 00. Clean and merge
#
# R translation of the Stata do-file "20220330 JGZ DATA - CLEAN AND MERGE".
# Builds a panel with one row per child (RINPERSOONS + RINPERSOON) per age in
# days (leeftijd), combining the four health domains of the four JGZ
# organisations.
#
#   Input : JGZ ID file, corrected ZONL ID file, JGZ health file (SPSS)
#   Output: loc$panel_file (JGZ_panel.rds), plus cleaning diagnostics
#           (descriptives, switch matrices, merge counts) in the work folder
#
# Steps
#   1. ID file: combine with corrected ZONL file, drop rows without RIN,
#      without Onderwerp, ambiguous duplicates and the CBS placeholder hash
#   2. Health file: country names, upper case, destring, exact duplicates
#   3. Within contact moment (org x child ID x domain x age): detect switches,
#      write the FRANKENSTEIN file, treat variables by category, collapse
#   4. Merge with the ID file (Icare-visus via Icare-overgewicht IDs)
#   5. Within RIN x domain x age: switches, remove conflicts, collapse
#   6. Across domains (RIN x age): switches, remove conflicts, final collapse
#
# Run from the repository root. Expected counts from the Stata log are
# printed as "[OK]" / "[DIFFERS]" so a run can be checked against Stata.
#
# Differences from the Stata do-file (deliberate, see comments [Dx]):
#   [D1] woonplaats is upper-cased from itself; the do-file has
#        `replace woonplaats = upper(gem)` (typo).
#   [D2] The gebl2-only country corrections are applied to gebl1 as well
#        (only changes values that exist in gebl1).
#   [D3] String "fill empty" variables are filled with the non-empty value
#        unless cfg$replicate_stata_string_fill is TRUE (see config/jgz.yml).
# =============================================================================

source("src/jgz/jgz_helpers.R")
suppressPackageStartupMessages({
  library(haven)
  library(openxlsx)
})
options(scipen = 999)

ensure_dir(loc$work_folder)
ensure_dir(loc$descriptives_folder)
ensure_dir(dirname(loc$panel_file))


# ---- Helpers specific to this script -------------------------------------------

# Row counts in total and per organisation (columns of the descriptives tables)
n_by_org <- function(df) {
  out <- c(Total = nrow(df), setNames(rep(0, length(ORG_CODES)), names(ORG_CODES)))
  tab <- table(factor(df$org, levels = ORG_CODES, labels = names(ORG_CODES)))
  out[names(tab)] <- as.vector(tab)
  out
}

# Five rows: all + one per domain
n_by_org_domain <- function(df) {
  rows <- lapply(DOMAIN_CODES, function(d) n_by_org(df[df$databestand %in% d, ]))
  do.call(rbind, c(list(All = n_by_org(df)), rows))
}

write_matrix <- function(m, file, sheet) {
  wb <- createWorkbook()
  addWorksheet(wb, sheet)
  writeData(wb, sheet, m, rowNames = TRUE)
  saveWorkbook(wb, file, overwrite = TRUE)
}

# Switch detection (Stata lines 588-745 and the two later passes).
#
# Within each group the do-file sorts a variable and flags a row when its value
# differs from the previous row:
#   "with missings"    -- a change between a missing and a real value counts
#   "without missings" -- only a change between two real values counts
# For a group with k distinct real values, these flags add up to
#   with missings:    (k - 1) + 1 if the group has both missing and real values
#   without missings: (k - 1)
# so they are computed directly from k. (The old R translation compared with
# `!=`, which returns NA next to a missing value; that silently dropped the
# missing<->value changes for numeric variables.)
#
# Returns the group id per row, per-group flags for every variable
# (max_f = any switch, max_f_n = a genuine conflict) and the 8-column
# "variations" matrix from the do-file.
detect_switches <- function(df, vars, keys) {
  g <- group_id(df, keys)
  n_groups <- max(g)
  group_size <- tabulate(g, n_groups)

  max_f <- list()
  max_f_n <- list()
  stats <- matrix(NA_real_, nrow = length(vars), ncol = 8, dimnames = list(vars, c(
    "N", "Unique contact moments (N)",
    "Switches (with missings)", "CMs switches (with missings)", "Rows affected (with)",
    "Switches (without missings)", "CMs switches (without missings)", "Rows affected (without)"
  )))

  for (v in vars) {
    x <- df[[v]]
    miss <- is_blank(x)
    g_real <- g[!miss]
    code <- match(x[!miss], unique(x[!miss]))
    pair <- (as.numeric(g_real) - 1) * (max(c(code, 0L)) + 1) + code
    k <- tabulate(g_real[!duplicated(pair)], n_groups)    # distinct real values
    has_real <- tabulate(g_real, n_groups) > 0
    has_miss <- tabulate(g[miss], n_groups) > 0
    mixed <- has_real & has_miss

    sw_n <- pmax(k - 1, 0)
    sw   <- sw_n + mixed
    max_f[[v]]   <- sw > 0
    max_f_n[[v]] <- sw_n > 0

    stats[v, ] <- c(
      sum(!miss), sum(has_real),
      sum(sw),   sum(max_f[[v]]),   sum(group_size[max_f[[v]]]),
      sum(sw_n), sum(max_f_n[[v]]), sum(group_size[max_f_n[[v]]])
    )
  }
  list(g = g, n_groups = n_groups, max_f = max_f, max_f_n = max_f_n, table = stats)
}

# Replace genuine conflicts by NA (numbers) or ".r" (strings) -- Stata's
# `replace var = .r if max_f_n_var == 1`. The paired "fill" step is commented
# out in the do-file at the places where this is used, so no filling here.
remove_conflicts <- function(df, vars, sw) {
  for (v in intersect(vars, names(df))) {
    conflict <- sw$max_f_n[[v]][sw$g]
    if (is.character(df[[v]])) df[[v]][conflict] <- REMOVED else df[[v]][conflict] <- NA
  }
  df
}


# =============================================================================
# 1. ID FILE
# =============================================================================

id_file <- read_spss(loc$id_file)
id_zonl <- read_spss(loc$id_file_zonl)
saveRDS(id_file, work_path("8151mcuniekCBKV2.rds"))
saveRDS(id_zonl, work_path("8151ZOrggroepOudeenNieuweLanduniekCBKV1.rds"))

# Corrected ZONL file replaces the ZONL rows (bestand 2) of the original file
zonl_prepared <- id_zonl %>%
  zap_labels() %>%
  rename(KindNummer_crypt = kindnummer_crypt) %>%
  mutate(bestand = 2)

id_combined <- id_file %>%
  zap_labels() %>%
  mutate(bestand = as.numeric(bestand)) %>%
  filter(is.na(bestand) | bestand != 2) %>%   # Stata: drop if bestand == "2"
  bind_rows(zonl_prepared)

# Organisation (bestand 1-4 -> org 6/7/10/12) and health domain. Onderwerp is
# PERMUTED onto databestand (1->1, 4->2, 2->3, 3->4) following the
# aanleverprocedure; a mistake here would mislabel whole domains.
id_combined <- id_combined %>%
  mutate(
    org = unname(ORG_CODES[bestand]),
    databestand = case_when(
      Onderwerp == 1 ~ 1,
      Onderwerp == 4 ~ 2,
      Onderwerp == 2 ~ 3,
      Onderwerp == 3 ~ 4
    )
  )
# org and databestand stay numeric (as in Stata): they are merge keys and are
# collapsed with (max) later. Labels are only added in the output tables.

descriptive <- rbind(n_by_org_domain(id_combined), `Missing-RIN` = NA)

# --- Rows without RIN cannot be linked to other CBS data ----------------------
missing_rin <- is_blank(id_combined$RINPERSOONS) & is_blank(id_combined$RINPERSOON)
descriptive["Missing-RIN", ] <- n_by_org(id_combined[missing_rin, ])
id_combined <- id_combined[!missing_rin, ]
check_n(nrow(id_combined), 320669, "ID file rows after dropping missing RIN")

# --- Rows without Onderwerp ----------------------------------------------------
# All come from Icare, and all are duplicates of Icare rows that do have an
# Onderwerp (checked in the do-file), so they can be dropped.
no_onderwerp <- is.na(id_combined$Onderwerp)
print(table(org = id_combined$org[no_onderwerp]))
id_combined <- id_combined[!no_onderwerp, ]
check_n(sum(no_onderwerp), 456, "ID rows without Onderwerp dropped")

# --- CBS placeholder hash ------------------------------------------------------------
# CBS fills an empty uploaded child ID with one fixed hash; it is not a child.
# Removed BEFORE the duplicates step below: these rows share child ID x
# organisation x Onderwerp, so otherwise they would be counted there and the
# counts would not match the cleaning flow (12 duplicates, 13,013 placeholder
# rows). The cleaned data is the same in both orders.
if (nzchar(cfg$placeholder_hash)) {
  is_placeholder <- id_combined$KindNummer_crypt %in% cfg$placeholder_hash
  id_combined <- id_combined[!is_placeholder, ]
  check_n(sum(is_placeholder), 13013, "ID rows with the CBS placeholder hash dropped (13,013 or 13,014)")
}

# --- Ambiguous duplicates ------------------------------------------------------------
# Child ID is unique within organisation x Onderwerp, except for a few rows with
# the same child ID but a different RIN. These cannot be matched to the
# health file unambiguously, so all copies are dropped.
dup_key <- id_combined[, c("KindNummer_crypt", "bestand", "Onderwerp")]
is_dup <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)
id_combined <- id_combined[!is_dup, ]
check_n(sum(is_dup), 12, "ambiguous ID duplicates dropped")

descriptive <- rbind(descriptive, n_by_org_domain(id_combined))
print(descriptive)
write_matrix(descriptive, file.path(loc$descriptives_folder, "IDfile_descriptives.xlsx"),
             "descriptive")

saveRDS(id_combined, work_path("IDfile_clean_all.rds"))
# Icare-overgewicht IDs are used to link Icare-visus (step 4)
icare_overgewicht_ids <- id_combined %>%
  filter(org == ORG_CODES[["Icare"]], databestand == DOMAIN_CODES[["overgewicht"]])
saveRDS(icare_overgewicht_ids, work_path("IDfile_clean_Icare_overgewicht.rds"))


# =============================================================================
# 2. HEALTH FILE
# =============================================================================

health <- read_spss(loc$health_file)
saveRDS(health, work_path("8151GezondheidsbestandxV2CBKV1.rds"))

health_clean <- health %>%
  zap_labels() %>%
  arrange(org, KindNummer_crypt, databestand, leeftijd)
rm(health)

stopifnot(all(health_clean$org %in% ORG_CODES),
          all(health_clean$databestand %in% DOMAIN_CODES))

# --- Country names (gebl1, gebl2) ------------------------------------------------
# Encoding-damaged and old/variant spellings -> one spelling. Accented
# characters are written as \u escapes so the file works in any encoding.
country_fix <- c(
  "Afganistan" = "Afghanistan", "Albani?" = "Albani\u00eb",
  "Argentini?" = "Argentini\u00eb", "Armeni?" = "Armeni\u00eb",
  "Australi?" = "Australi\u00eb", "Azerbajdsjan" = "Azerbeidzjan",
  "Belarus(Wit-Rusland)" = "Belarus (Wit-Rusland)", "Belgi?" = "Belgi\u00eb",
  "Bondsrepubliek Duitsland" = "Duitsland",
  "Bosni?-Herzegovina" = "Bosni\u00eb-Herzegovina",
  "Bosni?-Herzegowina" = "Bosni\u00eb-Herzegovina", "Brazili?" = "Brazili\u00eb",
  "Brits Somaliland" = "Somali\u00eb", "Burger van Montenegro" = "Montenegro",
  "Ceylon" = "Sri Lanka", "Cura?ao" = "Cura\u00e7ao", "Duitsland (oud)" = "Duitsland",
  "Ethiopi?" = "Ethiopi\u00eb", "Etiopi?" = "Ethiopi\u00eb", "Etiopi\u00eb" = "Ethiopi\u00eb",
  "Frans Guyana" = "Frans-Guyana", "Georgi?" = "Georgi\u00eb",
  "Groot-Brittanni?" = "Groot-Brittanni\u00eb", "Grootbrittanni?" = "Groot-Brittanni\u00eb",
  "Grootbrittanni\u00eb" = "Groot-Brittanni\u00eb", "Ha?ti" = "Ha\u00efti",
  "Indonesi?" = "Indonesi\u00eb", "Isra?l" = "Isra\u00ebl", "Itali?" = "Itali\u00eb",
  "Joegoslavi?" = "Joegoslavi\u00eb", "Jordani?" = "Jordani\u00eb",
  "Kaapverdi?" = "Kaapverdi\u00eb", "Katar" = "Qatar", "Kroati?" = "Kroati\u00eb",
  "Libi?" = "Libi\u00eb", "Macedoni?" = "Macedoni\u00eb", "Madagascar" = "Madagaskar",
  "Maleisi?" = "Maleisi\u00eb", "Mauritani?" = "Mauritani\u00eb",
  "Moldavi?" = "Moldavi\u00eb", "Mongoli?" = "Mongoli\u00eb", "Namibi?" = "Namibi\u00eb",
  "Nederlands Indi?" = "Nederlands Indi\u00eb", "NederlandsIndi?" = "Nederlands Indi\u00eb",
  "Nieuw Zeeland" = "Nieuw-Zeeland", "Nieuwzeeland" = "Nieuw-Zeeland",
  "Noordjemen" = "Jemen", "Noordkorea" = "Noord-Korea", "Noordvi?tnam" = "Vi\u00ebtnam",
  "Oekra?ne" = "Oekra\u00efne", "Oostduitse" = "Duitse Democratische Republiek",
  "Roemeni?" = "Roemeni\u00eb", "Rusland (oud)" = "Rusland", "Rusland(oud)" = "Rusland",
  "Saoedi-Arabi?" = "Saoedi-Arabi\u00eb", "Saoediarabi?" = "Saoedi-Arabi\u00eb",
  "Saoediarabi\u00eb" = "Saoedi-Arabi\u00eb", "Servi?" = "Servi\u00eb",
  "Servi? en Montenegro" = "Servi\u00eb en Montenegro", "Sloveni?" = "Sloveni\u00eb",
  "Slovakije" = "Slowakije", "Somali?" = "Somali\u00eb", "Somalia" = "Somali\u00eb",
  "Sovjetunie" = "Sovjet-Unie", "Syri?" = "Syri\u00eb", "Tha?land" = "Thailand",
  "Tsjechi?" = "Tsjechi\u00eb", "Tsjeschiische Republiek" = "Tsjechi\u00eb",
  "Tsjechoslowakije" = "Tsjecho-Slowakije", "Tunesi?" = "Tunesi\u00eb",
  "Vi?tnam" = "Vi\u00ebtnam", "Za?re" = "Za\u00efre", "Zambi?" = "Zambia",
  "Zuid Korea" = "Zuid-Korea", "Zuid-Jemen" = "Jemen", "Zuidafrika" = "Zuid-Afrika",
  "Zuidjemen" = "Jemen", "Zuidkorea" = "Zuid-Korea", "Zuidvi?tnam" = "Vi\u00ebtnam",
  "Zuidwest Afrika" = "Namibi\u00eb",
  # In the do-file only applied to gebl2 [D2]
  "Abessini\u00eb" = "Ethiopi\u00eb", "Abessini?" = "Ethiopi\u00eb",
  "Centafrika" = "Centraal-Afrikaanse Republiek",
  "Centrafrika" = "Centraal-Afrikaanse Republiek", "Faer?er" = "Far\u00f6er",
  "Federale Republiek Joegoslavi?" = "Joegoslavi\u00eb", "Oekraine" = "Oekra\u00efne",
  "Rhodesi?" = "Zimbabwe", "Rhodesi\u00eb" = "Zimbabwe", "Russische Federatie" = "Rusland"
)
fix_country <- function(x) {
  hit <- x %in% names(country_fix)
  x[hit] <- unname(country_fix[x[hit]])
  x
}

health_clean <- health_clean %>%
  mutate(
    gebl1      = toupper(fix_country(gebl1)),
    gebl2      = toupper(fix_country(gebl2)),
    gem        = toupper(gem),
    woonplaats = toupper(woonplaats)          # [D1]
  )

# --- Destring ----------------------------------------------------------------------
# Stata `destring, force`: values that are not numbers become missing.
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
non_numeric_pc <- !is_blank(health_clean$postcode) & is.na(to_num(health_clean$postcode))
message(sprintf("Non-numeric postcodes set to missing: %d", sum(non_numeric_pc)))
health_clean <- health_clean %>%
  mutate(across(any_of(c("postcode", "geslacht", "i_jgz", "opl1", "opl2", "i_cm", "bst_nl")),
                to_num))

descriptive_health <- rbind(n_by_org_domain(health_clean), `All-Duplicates` = NA)

# --- Exact duplicates (all variables except `dubbel`) ----------------------------------
dup_check_vars <- setdiff(names(health_clean), "dubbel")
exact_dup <- duplicated(health_clean[, dup_check_vars])
descriptive_health["All-Duplicates", ] <- n_by_org(health_clean[exact_dup, ])
health_clean <- health_clean[!exact_dup, ]
check_n(sum(exact_dup), 3166, "exact duplicate health rows dropped")


# =============================================================================
# 3. WITHIN CONTACT MOMENT: org x KindNummer_crypt x databestand x leeftijd
# =============================================================================

group_vars <- c("org", "KindNummer_crypt", "databestand", "leeftijd")
all_vars <- setdiff(names(health_clean), group_vars)

sw <- detect_switches(health_clean, all_vars, group_vars)
print(sw$table)
write_matrix(sw$table, work_path("Gezondheidsbestand_variables_within_onderwerp.xlsx"),
             "variations")

# --- FRANKENSTEIN file ----------------------------------------------------------------
# Contact moments where at least one variable has a genuine conflict. Only the
# conflicting variables are kept (the others are blanked), for inspection.
conflict_any <- Reduce(`|`, sw$max_f_n)[sw$g]
frankenstein <- health_clean[conflict_any, ]
for (v in all_vars) {
  keep <- sw$max_f_n[[v]][sw$g[conflict_any]]
  if (is.character(frankenstein[[v]])) frankenstein[[v]][!keep] <- "" else frankenstein[[v]][!keep] <- NA
}
message(sprintf("Rows written to the FRANKENSTEIN file: %d", nrow(frankenstein)))
saveRDS(frankenstein, work_path("FRANKENSTEIN.rds"))
rm(frankenstein)

# --- Treatment per variable category ------------------------------------------------
# Based on the variations matrix above (decided in the do-file):
#   1. group variables                      -- untouched
#   2. cat_nothing                          -- left as is
#   3. cat_fill_empty                       -- fill missing with the real value
#   4. cat_dummies                          -- one dummy per value + missing flag
#   5. cat_remove                           -- conflicts removed, missing filled
cat_nothing <- c(
  "postcode", "woonplaats", "gem", "st_a", "geslacht", "i_cm", "jaartal",
  "kindmogelijknietunieknr2", "maanden", "bril", "lh_r", "lh_l", "sc_ep",
  "sc_gp", "sc_pl", "sc_ha", "sc_psg", "sdq_i", "zwduur", "dat_a_str"
)
cat_fill_empty <- c(
  "so_a2", "apkt_r4", "apkt_l4", "srt_o", "spontaan", "vragen1",
  "verstaan2", "vragen2", "r_gd_vve"
)
cat_dummies <- c(
  "so_a", "i_jgz", "indi", "inter", "verwijs", "opl1", "opl2",
  "kindmogelijknietunieknr", "dubbel", "bst_nl", "taalomg", "meertaal", "d_vvenum"
)
cat_remove <- c(
  "lengte", "gewicht", "gebl1", "gebl2", "schoolnr", "lft2", "oog_o",
  "apk_r", "apk_l", "apkt_r3", "apkt_l3", "apkt_r5", "apkt_l5", "lc_r", "lc_l",
  "visus_c", "scr_psp", "sdq_t", "zin2w", "pop6", "noemt", "wijstaan", "zin3w",
  "verstaan1", "dat_a2_str"
)

uncategorised <- setdiff(all_vars, c(cat_nothing, cat_fill_empty, cat_dummies, cat_remove))
if (length(uncategorised) > 0) {
  message("Variables in no category (left as is): ", paste(uncategorised, collapse = ", "))
}
not_found <- setdiff(c(cat_nothing, cat_fill_empty, cat_dummies, cat_remove), all_vars)
if (length(not_found) > 0) {
  warning("Category variables not in the health file: ", paste(not_found, collapse = ", "))
}

# Category 3: fill. Numeric: the group's smallest value (Stata: sort, take
# var[1]). Strings: see [D3].
for (v in intersect(cat_fill_empty, names(health_clean))) {
  x <- health_clean[[v]]
  fill <- group_min(x, sw$g, sw$n_groups)[sw$g]
  has_fill <- !is_blank(fill)
  x[has_fill] <- fill[has_fill]
  if (is.character(x) && isTRUE(cfg$replicate_stata_string_fill)) {
    # Stata: sorted var[1] is "" whenever the contact moment has an empty row
    any_blank <- tabulate(sw$g[is_blank(health_clean[[v]])], sw$n_groups) > 0
    x[any_blank[sw$g]] <- ""
  }
  health_clean[[v]] <- x
}

# Category 4: dummies d_<var>_<value> (1 / missing, as in the do-file) and
# d_<var>_miss
for (v in intersect(cat_dummies, names(health_clean))) {
  x <- health_clean[[v]]
  miss <- is_blank(x)
  health_clean[[paste0("d_", v, "_miss")]] <- ifelse(miss, 1, NA_real_)
  for (lvl in sort(unique(x[!miss]))) {
    lvl_name <- gsub("[^A-Za-z0-9]", "_", as.character(lvl))
    health_clean[[paste0("d_", v, "_", lvl_name)]] <- ifelse(!miss & x == lvl, 1, NA_real_)
  }
}

# Category 5: genuine conflicts -> removed; groups that only differ by
# missingness -> filled with their single real value.
for (v in intersect(cat_remove, names(health_clean))) {
  x <- health_clean[[v]]
  conflict <- sw$max_f_n[[v]][sw$g]
  only_missing_switch <- (sw$max_f[[v]] & !sw$max_f_n[[v]])[sw$g]
  fill <- group_min(x, sw$g, sw$n_groups)[sw$g]
  x[only_missing_switch] <- fill[only_missing_switch]
  if (is.character(x)) x[conflict] <- REMOVED else x[conflict] <- NA
  health_clean[[v]] <- x
}

# --- Collapse to one row per contact moment ----------------------------------------------
health_clean <- collapse_all(health_clean, group_vars)
stopifnot(!anyDuplicated(health_clean[, group_vars]))
saveRDS(health_clean, work_path("Health_by_onderwerp.rds"))

descriptive_health <- rbind(descriptive_health, n_by_org_domain(health_clean))
print(descriptive_health)
write_matrix(descriptive_health,
             file.path(loc$descriptives_folder, "HealthFile_descriptives.xlsx"), "descriptive")


# =============================================================================
# 4. MERGE HEALTH FILE WITH ID FILE
# =============================================================================
# m:1 merge; rows of the health file without a match in the ID file are
# counted and then dropped. Columns that exist in both keep the health-file
# value (Stata default).
merge_ids <- function(master, ids, by) {
  stopifnot(!anyDuplicated(ids[, by]))
  ids <- ids[, union(by, setdiff(names(ids), names(master))), drop = FALSE]
  ids$.matched <- TRUE
  out <- left_join(master, ids, by = by)
  out$.matched <- !is.na(out$.matched)
  out
}

is_icare_visus <- health_clean$org == ORG_CODES[["Icare"]] &
  health_clean$databestand == DOMAIN_CODES[["visus"]]

# Merge 1: on organisation, child ID and domain (everything except Icare-visus)
merge1 <- merge_ids(health_clean[!is_icare_visus, ], id_combined,
                    by = c("org", "KindNummer_crypt", "databestand"))
# Merge 3: Icare-visus via the child IDs of Icare-overgewicht (the visus
# records cannot be linked directly; numbering "3" kept from the do-file)
merge3 <- merge_ids(health_clean[is_icare_visus, ], icare_overgewicht_ids,
                    by = c("org", "KindNummer_crypt"))

merges <- rbind(
  `All three`   = c(sum(!merge1$.matched), sum(merge1$.matched)),
  `Icare-visus` = c(sum(!merge3$.matched), sum(merge3$.matched))
)
merges <- rbind(merges, Total = colSums(merges))
colnames(merges) <- c("Health file only (not matched)", "Matched")
print(merges)
write_matrix(merges, work_path("Merges.xlsx"), "merges")

health_merged <- bind_rows(merge1, merge3) %>%
  filter(.matched) %>%
  select(-.matched) %>%
  filter(!is_blank(RINPERSOONS)) %>%
  relocate(RINPERSOONS, RINPERSOON) %>%
  rename(any_of(c(d_kmn_uniek_miss = "d_kindmogelijknietunieknr_miss",
                  d_kmn_uniek_0    = "d_kindmogelijknietunieknr_0",
                  d_kmn_uniek_1    = "d_kindmogelijknietunieknr_1")))
rm(merge1, merge3, health_clean)

# KindNummer_crypt is no longer meaningful (organisations use different IDs
# per domain); duplicates are now judged across organisations.
health_merged$KindNummer_crypt <- NULL
full_dup <- duplicated(health_merged)
health_merged <- health_merged[!full_dup, ]
check_n(sum(full_dup), 482, "full-row duplicates dropped after merge")


# =============================================================================
# 5. WITHIN RIN x DOMAIN x AGE
# =============================================================================
# org is not a key any more: switches in org show children whose information
# comes from more than one organisation (do-file: "perhaps because the file
# moves when a child moves?" -- open question).

merged_group_vars <- c("RINPERSOONS", "RINPERSOON", "databestand", "leeftijd")
merged_all_vars <- setdiff(names(health_merged), merged_group_vars)
check_n(length(merged_all_vars), 174, "variables in RIN x domain x age switch detection")

sw <- detect_switches(health_merged, merged_all_vars, merged_group_vars)
print(sw$table)
write_matrix(sw$table, work_path("MergedData_variables_within_onderwerp.xlsx"), "variations")

health_merged <- remove_conflicts(
  health_merged,
  c("org", "woonplaats", "gem", "gebl1", "gebl2", "postcode", "gewicht", "zwduur", "bestand"),
  sw
)
health_merged <- collapse_all(health_merged, merged_group_vars)
saveRDS(health_merged, work_path("Merged_by_onderwerp.rds"))


# =============================================================================
# 6. ACROSS DOMAINS: RIN x AGE
# =============================================================================

domain_group_vars <- c("RINPERSOONS", "RINPERSOON", "leeftijd")
domain_all_vars <- setdiff(names(health_merged), domain_group_vars)
check_n(length(domain_all_vars), 172, "variables in RIN x age switch detection")

sw <- detect_switches(health_merged, domain_all_vars, domain_group_vars)
print(sw$table)
write_matrix(sw$table, work_path("MergedData_variables_across_onderwerp.xlsx"), "variations")
message(sprintf("Full-row duplicates before the final collapse: %d",
                sum(duplicated(health_merged))))

# Do-file notes on switches at this point: org (CHECK: moves?), woonplaats
# (CHECK: all org 10 and 12), databestand (expected: domains are combined).
health_merged <- remove_conflicts(
  health_merged,
  c("org", "databestand", "woonplaats", "gem", "gebl1", "gebl2", "postcode", "gewicht", "zwduur"),
  sw
)

# --- Final collapse: explicit variable lists (Stata collapse drops the rest) ---
firstnm_vars <- c("woonplaats", "gem", "gebl1", "gebl2", "schoolnr", "visus_c",
                  "dat_a_str", "dat_a2_str")
max_vars <- c(
  "org", "postcode", "geslacht", "i_cm", "st_a", "lengte", "gewicht", "lft2",
  "so_a2", "jaartal", "kindmogelijknietunieknr2", "maanden", "bril", "oog_o",
  "apk_r", "apk_l", "apkt_r3", "apkt_l3", "apkt_r4", "apkt_l4", "apkt_r5", "apkt_l5",
  "lh_r", "lh_l", "lc_r", "lc_l", "scr_psp", "sc_ep", "sc_gp", "sc_pl", "sc_ha",
  "sdq_t", "sc_psg", "sdq_i", "srt_o", "zin2w", "pop6", "noemt", "wijstaan",
  "zin3w", "verstaan1", "spontaan", "vragen1", "verstaan2", "vragen2", "zwduur",
  "r_gd_vve",
  "d_so_a_miss", paste0("d_so_a_", c(2:7, 9:17, 19, 20, 22, 25)),
  "d_i_jgz_miss", paste0("d_i_jgz_", 1:3),
  "d_indi_miss", paste0("d_indi_", c(9, 10, 12, 18, 19, 20)),
  "d_inter_miss", paste0("d_inter_", c(1:7, 98)),
  "d_verwijs_miss", paste0("d_verwijs_", c(1:17, 19:21, 23:29, 98)),
  "d_opl1_miss", paste0("d_opl1_", c(0:9, 98)),
  "d_opl2_miss", paste0("d_opl2_", c(0:9, 98)),
  "d_kmn_uniek_miss", "d_kmn_uniek_0", "d_kmn_uniek_1",
  "d_dubbel_miss", "d_dubbel_0", "d_dubbel_1",
  "d_bst_nl_miss", "d_bst_nl_1", "d_bst_nl_2",
  "d_taalomg_miss", paste0("d_taalomg_", 1:3),
  "d_meertaal_miss", paste0("d_meertaal_", 1:3),
  "d_d_vvenum_miss", "d_d_vvenum_1", "d_d_vvenum_2"
)
# Strings in the (max) list (e.g. the Wiechen items "+", "-", "M") are kept
# with (firstnm): max() of text is not meaningful in R, and after the conflict
# removal above there is at most one value per child x age.
is_chr <- vapply(health_merged[intersect(max_vars, names(health_merged))], is.character, logical(1))
firstnm_vars <- c(firstnm_vars, names(is_chr)[is_chr])
max_vars <- setdiff(max_vars, names(is_chr)[is_chr])

for (lst in c("firstnm_vars", "max_vars")) {
  missing_vars <- setdiff(get(lst), names(health_merged))
  if (length(missing_vars) > 0) {
    message(lst, " not found in the data (skipped): ", paste(missing_vars, collapse = ", "))
  }
}
dropped_vars <- setdiff(names(health_merged), c(domain_group_vars, firstnm_vars, max_vars))
message("Variables dropped by the final collapse: ", paste(dropped_vars, collapse = ", "))

JGZ_panel <- collapse_groups(
  health_merged, domain_group_vars,
  firstnm_vars = intersect(firstnm_vars, names(health_merged)),
  max_vars     = intersect(max_vars, names(health_merged))
) %>%
  arrange(RINPERSOONS, RINPERSOON, leeftijd)
stopifnot(!anyDuplicated(JGZ_panel[, domain_group_vars]))

message(sprintf("JGZ panel: %d rows, %d children", nrow(JGZ_panel),
                nrow(distinct(JGZ_panel, RINPERSOONS, RINPERSOON))))
saveRDS(JGZ_panel, loc$panel_file)
