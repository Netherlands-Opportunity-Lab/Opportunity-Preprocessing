# JGZ preprocessing

Preprocessing of the youth health care (JGZ) pilot data in CBS Remote Access:
from the raw JGZ files to a child x age panel, contact moments and the
catchment-area sample for the bereik analyses.

The analyses and the export (map tables per postcode area, gradient checks,
export bundle) are in
[Opportunity-Analysis-Map/JGZ](https://github.com/Netherlands-Opportunity-Lab/Opportunity-Analysis-Map),
next to the map, regression, descriptives and webmapper scripts.

All paths and settings are in `config/jgz.yml`. Run it like the other
cohorts, with `cohort <- "jgz"` in `P_2_0_create_cohort_data.R` (repository
root). That runs steps 00-03 in order; set `jgz_steps` there to rerun only
some of them (each step reads the previous step's output from disk, e.g.
`jgz_steps <- c("02", "03")`). Or source the scripts yourself, from the
repository root, in order:

```r
setwd("H:/Opportunity-Preprocessing")
source("src/jgz/00_clean_and_merge.R")
source("src/jgz/01_add_gba_postcode.R")
source("src/jgz/02_contact_moments.R")
source("src/jgz/03_build_catchment_sample.R")
source("src/jgz/04_coverage_per_pc4.R")   # optional: check of the catchment areas
```

The code is written for the CBS RA versions (R 3.6.2, dplyr 1.0.2): no `|>`,
no `join_by()`, `if_any()` or `relationship =`.

## Scripts

| Script | Input | Output | What it does |
|---|---|---|---|
| `00_clean_and_merge.R` | JGZ ID file, corrected ZONL ID file, health file | `JGZ_panel.rds` + cleaning diagnostics | Cleans the ID and health files, resolves duplicate/conflicting records (switch detection), merges health data to RIN and collapses to one row per child x age. R translation of Coen's do-file "JGZ DATA - CLEAN AND MERGE" |
| `01_add_gba_postcode.R` | panel (00), GBAADRESOBJECTBUS, VSLPOSTCODEBUS | `jgz_gba_postcode_data.rds` | Adds the GBA address and its PC4 on the date of each contact moment |
| `02_contact_moments.R` | output of 01 | `cm_<visit>.rds` (15 files) | Assigns each row to a contact moment (fixed age windows) and adds BMI, sex, PC3/PC4 and the target age |
| `03_build_catchment_sample.R` | panel (00), GBA, KINDOUDER, income, education and health-cost files, `resources/jgz_pc4gebieden.csv` | `Data_CatchmentArea_3pooled_withvariables_R.dta` | Coen's catchment-area sample for the bereik analyses: children living in one organisation's area from 22 to 53 months, observed or not at 2y/3y/3y9m (`d_obs_cat`), with child and family characteristics. Variable names as in Coen's file, so the bereik regression do-file runs on it |
| `04_coverage_per_pc4.R` | panel (00), GBA, GBAADRESOBJECTBUS, VSLPOSTCODEBUS, `resources/jgz_pc4gebieden.csv` | `coverage_per_pc4.xlsx` | Check of the catchment areas (Bastian, 27-10-2026): coverage per PC4 x contact moment for every PC4 in the JGZ data, the PC4s *not* in the PC4 file with coverage above 20%, and those per calendar year. Internal only (small counts) |
| `jgz_helpers.R` | – | – | Config loading and shared helpers (sourced by all scripts) |

## Decisions and open points

- **00 against Stata.** 00 prints the counts from the Stata log next to its
  own, e.g. `[OK] exact duplicate health rows dropped: 3,166 (Stata log: 3,166)`.
  The variable counts (174 and 172) count the key variables too, as the
  do-file's matrices do. The CBS placeholder hash is expected to drop 0 rows:
  the 13,013 rows were in the old ZONL ID file, which the corrected file
  replaces. Three deliberate differences can change cleaned values (not row
  counts): `woonplaats` is no longer overwritten with `gem` (typo in the
  do-file); the gebl2-only country corrections are applied to gebl1 too; and
  string "fill empty" variables are filled with their real value
  (`replicate_stata_string_fill: TRUE` reproduces Stata exactly).
- **Children seen by more than one organisation.** 00 cleans per
  organisation; after linking to RIN, records of one child at the same age
  (`leeftijd`) from different organisations get `org` = missing, and differing
  location variables and `gewicht` too (other variables: maximum, as Coen).
  At different ages each row keeps its own `org`. 01 and 02 do not use `org`.
  In 03 the organisation is where the child lives (`org_catchment`); observed
  = a contact by *any* organisation (`catch_observed_same_org: FALSE`, Coen),
  and `org` is that of the last contact. Open: how often this happens (row
  `org` in 00's switch tables). Note: the map tables (Opportunity-Analysis-Map)
  pool all organisations by GBA postcode, so their export is not limited to
  `export_orgs`.
- **`geslacht` coding (02).** 1 = boy, 2 = girl (checked in the data).
- **BMI units (02).** Weight in grams and length in mm (as in the BDS and in
  Coen's code); 02 prints the median BMI per visit and warns if implausible.
- **Contact moments (02) -- decided 23 Oct 2026.**
  - *Windows:* we use the fixed age windows from the original script; the
    option to compute them live with Coen's histogram/dal method (20210705
    ANALYSIS GRADIENTS) is removed. The fixed bounds are most likely that
    method's outcome on the JGZ data, frozen in the script (irregular bounds
    just before the next visit, switching from days to months exactly where
    his method does), so both give the same windows while fixed bounds stay
    reproducible across data deliveries. Origin still to be confirmed with
    Bastian.
  - *Several measurements in one window:* the analyses use the measurement
    closest to the nominal age of the visit (`target_age`, e.g. 1095 days for
    3 years), not the histogram peak (Coen): one rule for all organisations
    and easy to explain. Still to check in RA: how often children have several
    measurements in a window -- if these are mostly follow-up visits after an
    abnormal finding, the rule can affect prevalences.
- **03 follows Coen's do-files.** First rebuilt from the variable list of his
  dataset; on 2026-10-23 compared with his 2020 do-files (SAMPLE SELECTION,
  PREPARE CHILD AND FAMILY CHARACTERISTICS, ANALYSIS 3 CONTACT MOMENTS) and
  the `catch_*` defaults set to his choices: windows 22-33 / 34-43 / 44-53
  months, continuity with a gap of at most one month, PC4 end dates without
  start dates, his data-driven birth-cohort window per organisation [C9],
  addresses in the 36-month month (earliest spell), his health-cost and
  mental-care columns, birth order by month, PSECJ for 2007-2010 incomes,
  education file 2018, and ETNGRP / ISCED from his CBS reference files when
  they exist (paths in the config). Remaining differences: postcodes per
  address use validity dates (Coen: first registered postcode), and ETNGRP
  falls back on country labels if LANDAKTUEELREFV9 is not found. Compare the
  output with Coen's
  `H:/Archive/Coen/TempData/Data_CatchmentArea_3pooled_withvariables.dta`
  inside RA (children per `org_catchment`, `d_obs_cat` distribution) and check
  the cohort windows printed by 03. The flow table and cohort shares contain
  counts below 10: not for export.
- **Denominator of the coverage (bereik), 03.** All children in GBA who
  (1) live continuously in the PC4 catchment area of *one* organisation from
  month 22 through month 53 of age (calendar months since the birth month,
  about 1y10m-4y5m; not 18 months-4 years), with gaps between address spells
  of at most one month and the area counting until its `einde`; and (2) are
  born within that organisation's cohort window ([C9]). Whether the JGZ saw
  them plays no role in the denominator; that is the numerator (`d_obs_cat`).
  Verian's Apeldoorn is only left out when computing the cohort windows; the
  regression do-file drops it. Windows and rules in `config/jgz.yml`
  (`catch_*`).
- **Coverage per postcode area.** The earlier `catchment_area_calculations.R`
  (2019: share of never-moved children born 2010-2014 found in the JGZ ID
  file, per PC2/PC3) is removed: the official PC4 catchment file and 03 replace
  it. Coverage *within* the catchment areas comes from 03's output
  (`d_observed_*` by `postcode_4` or `org_catchment`).
- **Check of the catchment areas (04) -- agreed 27 Oct 2026.** Is Coen's PC4
  list complete? 04 computes the coverage for every PC4 where the JGZ saw at
  least one child: denominator = all children in GBA (born in the 03 range)
  whose address in the month of the nominal age (24 / 36 / 45 months,
  `cov_reference_months`) is in that PC4, in a calendar year with JGZ data;
  numerator = those with a JGZ panel row in the contact-moment window (any
  organisation, as 03). No continuity or cohort rule, unlike 03. It lists the
  PC4s not in the PC4 file with coverage above 20% (`cov_flag_min_coverage`,
  with at least `cov_flag_min_children` children) and shows them per calendar
  year, because catchment areas can change over time. Whether to add them is
  decided with Bastian. Note: from 4 years on (school groups) children are
  seen via their school, so a PC4 can have data because children go to school
  in the area; 04 only looks at 2y, 3y and 3y9m.
- **CBS file versions -- decided 23 Oct 2026.** GBAPERSOONTAB,
  GBAADRESOBJECTBUS, VSLPOSTCODEBUS and KINDOUDERTAB are the same versions as
  in prim8_a/prim8_b (2024/2024/2026031/2025; before: 2023/2023/2025031/2023),
  so the pipelines can share files. These four are read from their SQLite
  versions (`.db` in the "geconverteerde data" folders, as the other
  cohorts in `src/`) with `read_cbs()` in `jgz_helpers.R`: only the rows of the
  JGZ children, their parents, siblings and addresses are read, instead of the
  whole file. Needs the R packages DBI and RSQLite; pointing a path in the
  config to the `.sav` gives the same result with `read_sav()`. The yearly
  files (income, education, health costs) are still read as `.sav`. Newer versions include CBS corrections, so counts in 01
  and 03 can shift slightly. The CBS code lists in 03 (LANDAKTUEELREFV9,
  OPLEIDINGSNRREFV24, CTOREFV8) stay the versions Coen used, for his ETNGRP
  and ISCED coding; prim8_a uses V15 / V36 / V13.
- **PC4 file.** `resources/jgz_pc4gebieden.csv` is the copy from
  `Opportunity-Analysis-Map/JGZ/resources`; the copy in "Background info
  preprocessing JGZ" on SharePoint has broken quoting. PC4 8080 is listed for
  Verian and org12; 03 applies Coen's correction (Verian 8080 -> 8180).
