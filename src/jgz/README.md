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
| `02_create_excel_info_maps.R` | output of 01 | `jgz_maps_{bmi,wiechen,sdq}.xlsx`, `cm_<visit>.rds` | Assigns contact moments, one measurement per child per visit, BMI / Wiechen / SDQ per PC3 and PC4 with small-cell suppression |
| `03_to_tidy_data.R` | gradient Excel files | `*_tidy.xlsx` | Reshapes gradient output (20 income bins) from wide to long |
| `check_function.R` | – | – | Disclosure checks for gradients (used by 04) |
| `04_to_export.R` | `*_tidy.xlsx` | `*_tidy_export.xlsx` | Combines speech/language files and runs the checks |
| `05_final.R` | outputs of 02 and 04, other results | export workbooks | Bundles everything for organisations 7 and 10 |
| `catchment_area_calculations.R` | GBA, VSLPOSTCODEBUS, cleaned ID file (00) | `CatchmentArea.xlsx` | Share of permanently resident children (born 2010-2014) covered by the JGZ data, per PC2/PC3 |
| `jgz_helpers.R` | – | – | Config loading and shared helpers (sourced by all scripts) |

Preprocessing in the narrow sense is 00, 01 and the first part of 02
(contact moments). The rest is analysis output and export.

Not in this folder: the scripts that produce the gradient, proportion and
bereik regression results that 03-05 read.

## Manual steps and open points

- **Postcode lookup (01, step 3).** `rin_data.dta` is matched to VSLPOSTCODEBUS
  outside R to produce `matched_postcode.dta`. Could be replaced by an SQL lookup
  as in the catchment script.
- **`geslacht` coding (02).** Set `sex_boy` / `sex_girl` in the config after
  checking `table(geslacht)`.
- **BMI units (02).** Assumes weight in grams and length in mm; 02 prints the
  median BMI per visit and warns if it is implausible.
- **BMI percentile check (check_function.R).** Removes a percentile when its
  *value* is below 10, as before. Confirm whether this was meant as a
  plausibility check or as a rule on the number of observations.
- **Secondary suppression.** Tables are suppressed cell by cell; complementary
  cells (e.g. boys = total - girls) are not checked.
- **Catchment output** contains counts below 10; apply output rules before
  export.

## Checking a run of 00 against Stata

00 prints the counts from the Stata log next to its own, e.g.
`[OK] exact duplicate health rows dropped: 3,166 (Stata log: 3,166)`.
Three deliberate differences can change the cleaned values (not the row
counts): `woonplaats` is no longer overwritten with `gem` (typo in the
do-file); the extra country-name corrections that the do-file applied only to
`gebl2` are applied to `gebl1` too; and string variables in the "fill empty"
category are filled with their real value. Set `replicate_stata_string_fill: TRUE` in the config to
reproduce the Stata output exactly.
