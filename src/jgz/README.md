# JGZ pilot pipeline

Scripts for the youth health care (JGZ) pilot data in CBS Remote Access: from
the raw JGZ files to a child x age panel, map tables per postcode area,
checked gradients and the export bundle.

All paths and settings are in `config/jgz.yml`. Run every script from the
repository root:

```r
setwd("H:/Opportunity-Preprocessing")
source("src/jgz/00_clean_and_merge.R")
```

The code is written for the CBS RA versions (R 3.6.2, dplyr 1.0.2): no `|>`,
no `join_by()`, `if_any()` or `relationship =`.

## Scripts

| Script | Input | Output | What it does |
|---|---|---|---|
| `00_clean_and_merge.R` | JGZ ID file, corrected ZONL ID file, health file | `JGZ_panel.rds` + cleaning diagnostics | Cleans the ID and health files, resolves duplicate/conflicting records (switch detection), merges health data to RIN and collapses to one row per child x age |
| `01_data_for_maps.R` | panel, GBAADRESOBJECTBUS, postcode lookup | `jgz_gba_postcode_data.rds` | Adds the GBA address and PC4 at the date of each contact moment |
| `02_create_excel_info_maps.R` | output of 01 | `jgz_maps_{bmi,wiechen,sdq}.xlsx`, `cm_<visit>.rds`, `contact_moment_windows.xlsx` | Assigns contact moments (fixed windows, or Coen's histogram/dal method via `cm_method`), one measurement per child per visit, BMI / Wiechen / SDQ per PC3 and PC4 with small-cell suppression |
| `03_to_tidy_data.R` | gradient Excel files | `*_tidy.xlsx` | Reshapes gradient output (20 income bins) from wide to long |
| `check_function.R` | – | – | Disclosure checks for gradients (used by 04) |
| `04_to_export.R` | `*_tidy.xlsx` | `*_tidy_export.xlsx` | Combines speech/language files and runs the checks |
| `05_final.R` | outputs of 02 and 04, other results | export workbooks | Bundles everything for the organisations in `export_orgs` (config). Map workbooks keep the 2025 file and sheet names that `pc4_map_JGZ.R` reads |
| `06_build_catchment_sample.R` | panel (00), GBA, KINDOUDER, income, education and health-cost files, `resources/jgz_pc4gebieden.csv` | `Data_CatchmentArea_3pooled_withvariables_R.dta` | Rebuilds Coen's catchment-area sample for the bereik analyses: children living in one organisation's area from 22 to 53 months, observed or not at 2y/3y/3y9m (`d_obs_cat`), with child and family characteristics. Variable names as in Coen's file, so the bereik regression do-file runs on it |
| `catchment_area_calculations.R` | GBA, VSLPOSTCODEBUS, cleaned ID file (00) | `CatchmentArea.xlsx` | Share of permanently resident children (born 2010-2014) covered by the JGZ data, per PC2/PC3 |
| `jgz_helpers.R` | – | – | Config loading and shared helpers (sourced by all scripts) |

Preprocessing in the narrow sense is 00, 01 and the first part of 02
(contact moments). The rest is analysis output and export.

Not in this folder: the scripts that produce the gradient, proportion and
bereik regression results that 03-05 read. The bereik analyses start from the
output of 06.

## Manual steps and open points

- **Postcode lookup (01, step 3).** `rin_data.dta` is matched to VSLPOSTCODEBUS
  outside R to produce `matched_postcode.dta`. Could be replaced by an SQL lookup
  as in the catchment script.
- **`geslacht` coding (02).** Set `sex_boy` / `sex_girl` in the config after
  checking `table(geslacht)`.
- **BMI units (02).** Assumes weight in grams and length in mm; 02 prints the
  median BMI per visit and warns if it is implausible.
- **BMI percentile check (check_function.R).** Resolved with Coen's gradient
  do-file: the p10-p90 columns hold the number of children above each
  percentile per income bin (the share in the older version), so "value below
  10" was the small-cell rule. Now applied to the count and to N - count.
- **Contact moments (02).** `cm_method: "histogram"` uses Coen's histogram/dal
  method (20210705 ANALYSIS GRADIENTS); default still `"fixed"`. Compare the
  two in `contact_moment_windows.xlsx` before switching. `cm_target` chooses
  the measurement closest to the nominal age (Bastian) or to the peak (Coen).
- **Top-10% BMI (02).** `bmi_p90_by_sex`: within sex (current) or boys and
  girls together (Coen). The flag is now `BMI > p90` (was `>=`), as in Coen's
  do-file.
- **Wiechen export (02).** All three categories (+, M, -) are exported per
  item, so one can be derived from the other two (Bastian, 20 Oct). Decide
  which two-way split is exported before the next export request.
- **Secondary suppression.** Tables are suppressed cell by cell; complementary
  cells (e.g. boys = total - girls) are not checked.
- **Catchment output** contains counts below 10; apply output rules before
  export.
- **06 is a reconstruction, now checked against Coen's do-files.** First
  rebuilt from the variable list of his dataset; on 2026-10-23 compared with
  his 2020 do-files (SAMPLE SELECTION, PREPARE CHILD AND FAMILY
  CHARACTERISTICS, ANALYSIS 3 CONTACT MOMENTS) and the `catch_*` defaults set
  to his choices: windows 22-33 / 34-43 / 44-53 months, continuity with a
  gap of at most one month, PC4 end dates without start dates, his
  data-driven birth-cohort window per organisation [C9], addresses in the
  36-month month (earliest spell), his health-cost and mental-care columns,
  birth order by month, PSECJ for 2007-2010 incomes, education file 2018, and
  ETNGRP / ISCED from his CBS reference files when they exist (paths in the
  config). Remaining differences: postcodes per address use validity dates
  (Coen: first registered postcode), and ETNGRP falls back on country labels
  if LANDAKTUEELREFV9 is not found. Compare the output with Coen's
  `H:/Archive/Coen/TempData/Data_CatchmentArea_3pooled_withvariables.dta`
  inside Remote Access (children per `org_catchment`, `d_obs_cat`
  distribution) and check the cohort windows printed by 06.
- **PC4 file.** `resources/jgz_pc4gebieden.csv` is the copy from
  `Opportunity-Analysis-Map/JGZ/resources`; the copy in "Background info
  preprocessing JGZ" on SharePoint has broken quoting. PC4 8080 is listed for
  Verian and Icare; 06 applies Coen's correction (Verian 8080 -> 8180).

## Checking a run of 00 against Stata

00 prints the counts from the Stata log next to its own, e.g.
`[OK] exact duplicate health rows dropped: 3,166 (Stata log: 3,166)`.
The variable counts (174 and 172) count the key variables too, as the
do-file's matrices do. The CBS placeholder hash is expected to drop 0 rows:
the 13,013 rows were in the old ZONL ID file, which the corrected file
replaces (do-file: "THIS ISSUE HAD BEEN SOLVED WITH THE NEW ZONL ID DATA").
Three deliberate differences can change the cleaned values (not the row
counts): `woonplaats` is no longer overwritten with `gem` (typo in the
do-file); the extra country-name corrections that the do-file applied only to
`gebl2` are applied to `gebl1` too; and string variables in the "fill empty"
category are filled with their real value. Set `replicate_stata_string_fill: TRUE` in the config to
reproduce the Stata output exactly.
