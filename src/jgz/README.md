# JGZ preprocessing

Preprocessing of the youth health care (JGZ) pilot data (project 8151) in CBS
Remote Access. Since 10-11-2026 the JGZ data are two cohorts with the same
steps as every other cohort in this repository (01 cohort, 02 predictors, 03
outcomes, 04 postprocessing), each with its own config:

| Cohort | Config | Who | Output |
|---|---|---|---|
| `jgz` | `config/jgz.yml` | children with JGZ measurements, born 2005-2018, with a mother in KINDOUDERTAB | `output/jgz_cohort.rds`: one row per child, predictors + outcomes per age (BMI, Van Wiechen, SDQ, visus) |
| `jgz_catchment` | `config/jgz_catchment.yml` | children living in one organisation's catchment area at 2y, 3y and 3y9m, seen by the JGZ or not | `output/jgz_catchment_cohort.rds` (bereik outcomes) and Coen's `Data_CatchmentArea_3pooled_withvariables_R.dta` |

Run them from the repository root, like the other cohorts:

```
Rscript P_2_1_run_all.R jgz jgz_catchment          # both, in this order (the catchment sample needs jgz's step 01a)
Rscript src/lib/P_2_1_run_cohort.R jgz 03 04       # only steps 03 and 04 of jgz
Rscript src/lib/P_2_1_run_cohort.R jgz_catchment 05   # the optional check of the catchment areas
```

In the RA `Rscript` is probably not on the PATH; from the R console use
`system2(file.path(R.home("bin"), "Rscript.exe"), c("src/lib/P_2_1_run_cohort.R", "jgz", "03", "04"))`.
Each step reads the previous step's output from disk (`temp/<cohort>/`), so a
subset can be rerun. The scripts can also be sourced one by one from the
repository root.

The analyses and the export (map tables per postcode area, gradient checks,
export bundle) are in
[Opportunity-Analysis-Map/JGZ](https://github.com/Netherlands-Opportunity-Lab/Opportunity-Analysis-Map).
They still read the contact-moment files of 01c; to do: read
`output/jgz_cohort.rds` instead, so the outcomes are defined in one place.

## Scripts: `jgz` (src/jgz)

| Step | Script | Input | Output | What it does |
|---|---|---|---|---|
| 01a | `01a_clean_and_merge.R` | JGZ ID file, corrected ZONL ID file, health file | `JGZ_panel.rds`, `JGZ_measurements_clean.rds`, `JGZ_id_link.rds` + cleaning diagnostics | Cleans the ID and health files (saved before any collapse: the measurements, one row per record, and the ID link to RIN), resolves duplicate/conflicting records (switch detection), merges health data to RIN and collapses to one row per child x age. R translation of Coen's do-file "JGZ DATA - CLEAN AND MERGE" |
| 01b | `01b_add_gba_postcode.R` | panel (01a), GBAADRESOBJECTBUS, VSLPOSTCODEBUS | `jgz_gba_postcode_data.rds` | Adds the GBA address and its PC4 on the date of each contact moment |
| 01c | `01c_contact_moments.R` | output of 01b | `cm_<visit>.rds` (15 files) | Assigns each row to a contact moment (fixed age windows) and adds BMI, sex, PC3/PC4 and the target age |
| 01d | `01d_cohort_jgz.R` | panel (01a), GBAPERSOONTAB, KINDOUDERTAB | `temp/jgz/01_cohort.rds` | One row per child: birth date, sex, origin and generation, the parent link (mother as `CBKSoortNr_Moeder` / `Rinpersoon_Moeder`) and the parents' age at birth, named as the newborn cohort file |
| 02 | `src/newborns/02_predictors_newborns.R` (shared) | 01d, income, wealth, household and education files | `temp/jgz/02_predictors.rds` | The predictor step of the newborn cohorts: parental income (mother's household income in the year before and the year of birth, ranked within birth year: `income_parents_perc`), parental wealth, migration background, household type (number of parents), parents' education. Settings in `config/jgz.yml` |
| 03 | `03_outcomes_jgz.R` (+ `src/lib/P_2_1_jgz_outcomes.R`) | 02, `cm_<visit>.rds` | `temp/jgz/03_outcomes.rds` | The JGZ outcomes per age, see below |
| 04 | `04_postprocessing_jgz.R` | 03 | `output/jgz_cohort.rds` | Income and wealth groups, as the other cohorts; writes the cohort file |
| – | `jgz_helpers.R` | – | – | Config loading and shared helpers (sourced by all JGZ scripts, also those of the catchment sample) |

## Scripts: `jgz_catchment` (src/jgz_catchment)

| Step | Script | Input | Output | What it does |
|---|---|---|---|---|
| 01 | `01_cohort_jgz_catchment.R` | panel (jgz 01a), GBA, KINDOUDER, income, education and health-cost files, `resources/jgz_pc4gebieden.csv` | `Data_CatchmentArea_3pooled_withvariables_R.dta` (+ `.rds`) | Coen's catchment-area sample for the bereik analyses: children living in one organisation's area from 22 to 53 months, observed or not at 2y/3y/3y9m (`d_obs_cat`), with child and family characteristics (step 02 is part of it). Variable names as in Coen's file, so the bereik regression do-file runs on it |
| 03 | `03_outcomes_jgz_catchment.R` | 01 | `output/jgz_catchment_cohort.rds` | The bereik outcomes under the JGZ names: `y_m24_takeup`, `y_m36_takeup`, `y_m45_takeup` (seen at 2y / 3y / 3y9m), `y_m45_takeup_always`, `y_m45_takeup_any`, all with the same denominator |
| 05 (optional) | `05_coverage_per_pc4.R` | panel (jgz 01a), GBA, GBAADRESOBJECTBUS, VSLPOSTCODEBUS, `resources/jgz_pc4gebieden.csv` | `coverage_per_pc4.xlsx` | Check of the catchment areas (Bastian, 27-10-2026): coverage per PC4 x contact moment for every PC4 in the JGZ data, the PC4s *not* in the PC4 file with coverage above 20%, and those per calendar year. Internal only (small counts) |

## Outcomes (jgz step 03)

The list follows `JGZ_outcomes_concept.xlsx` (SharePoint, "Background info
preprocessing JGZ", 10-11-2026). Every choice is a setting in `config/jgz.yml`.
Names: `y_m<age in months>_<outcome>`; file names add the version prefix
(`Y_1_0_m24_bmi_p90`). Infant contacts: 4 weeks = m1, 8 weeks = m2 (the
7.5-month visit falls in the 6-month window). School groups: groep 2 = m60,
groep 7 = m120, klas 2 = m162.

| Domain | Outcomes | Ages | Definition |
|---|---|---|---|
| BMI | `y_m<age>_bmi`, `_bmi_p90`, `_bmi_p10` | m1 m2 m3 m4 m6 m9 m11 m14 m18 m24 m36 m45 m60 m120 m162 | BMI; above the 90th / below the 10th percentile of all children measured at that age (same cut-off for every area; within sex by default, `bmi_cutoffs_by_sex`). Outliers with \|z\| > 5 (within age x sex) set to missing |
| Van Wiechen | `y_m24_words2_*` (41), `y_m24_bodyparts_*` (42), `y_m24_me_i_*` (43), `y_m24_pictures` (44), `y_m36_words3_*` (45), `y_m36_intelligible_familiar_*` (46), `y_m45_questions_*` (48), `y_m45_intelligible` (49), `y_m45_questions2_*` (50), `y_m<age>_language_delay` | m24, m36, m45 | `_can` = + or M (professional or parent), `_notseen` = - or M (not seen by the professional); items 44 and 49 have no M: `pictures` and `intelligible` = +. `language_delay` = at least one core item scored - (41+42 / 45+46 / 48+49+50), only for children with all core items scored; not Coen's overall judgement, whose share "langzaam" differs too much between organisations |
| SDQ | `y_m<age>_sdq11`; `y_m<age>_sdq` (total score) | m45 m60 m120 m162; score m60 m120 m162 | total score >= 11 (`sdq_cutoff`), and the score itself for means (not at 3y9m, `sdq_score_ages`); scores above 40 set to missing. At 3y9m partly the DMO protocol instead of the SDQ |
| Visus | `y_m<age>_visus` | m36 m45 | conclusion of the vision test (`visus_c`) = onvoldoende (`visus_abnormal`); voldoende and twijfelachtig = 0 |
| Per age | `pc4_m<age>`, `pc3_m<age>`, `org_m<age>` | all | GBA postcode and organisation of the contact closest to the nominal age, for the maps and for the export rules per organisation |

One measurement per child per age: the one closest to the nominal age
(`outcome_target_days`) among the contact moments listed for that age, per
item. Van Wiechen at m24 uses the 18-month and 2-year contacts, at m36 the 3y
contact, at m45 the 3y and 3y9m contacts (as the map tables). Items and ages
follow the filled cells of the 2019 descriptives (sheet D8,
`20190208BR Descriptives bijgeschaafd.xlsx`; checked 11-11-2026): visus is
not used at groep 2 (545 conclusions; recorded there as a Landolt-C result). Open choices for Bastian: p90/p10
within sex or not, the SDQ cut-off, the visus definition, and which of
`_can` / `_notseen` is exported (both together with N reveal all three
categories).

## Decisions and open points

- **01a against Stata.** 01a prints the counts from the Stata log next to its
  own, e.g. `[OK] exact duplicate health rows dropped: 3,166 (Stata log: 3,166)`.
  The variable counts (174 and 172) count the key variables too, as the
  do-file's matrices do. The CBS placeholder hash is expected to drop 0 rows:
  the 13,013 rows were in the old ZONL ID file, which the corrected file
  replaces. Three deliberate differences can change cleaned values (not row
  counts): `woonplaats` is no longer overwritten with `gem` (typo in the
  do-file); the gebl2-only country corrections are applied to gebl1 too; and
  string "fill empty" variables are filled with their real value
  (`replicate_stata_string_fill: TRUE` reproduces Stata exactly).
- **Children seen by more than one organisation.** 01a cleans per
  organisation; after linking to RIN, records of one child at the same age
  (`leeftijd`) from different organisations get `org` = missing, and differing
  location variables and `gewicht` too (other variables: maximum, as Coen).
  At different ages each row keeps its own `org`. 01b and 01c do not use `org`;
  03 keeps it per age (`org_m<age>`). In the catchment sample the organisation is where the child lives (`org_catchment`); observed
  = a contact by *any* organisation (`catch_observed_same_org: FALSE`, Coen),
  and `org` is that of the last contact. Open: how often this happens (row
  `org` in 01a's switch tables). Note: the map tables (Opportunity-Analysis-Map)
  pool all organisations by GBA postcode, so their export is not limited to
  `export_orgs`.
- **`geslacht` coding (01c).** 1 = boy, 2 = girl (checked in the data).
- **BMI units (01c).** Weight in grams and length in mm (as in the BDS and in
  Coen's code); 01c prints the median BMI per visit and warns if implausible.
- **Contact moments (01c) -- decided 23 Oct 2026.**
  - *Windows:* we use the fixed age windows from the original script; the
    option to compute them live with Coen's histogram/dal method (20210705
    ANALYSIS GRADIENTS) is removed. The fixed bounds are most likely that
    method's outcome on the JGZ data, frozen in the script (irregular bounds
    just before the next visit, switching from days to months exactly where
    his method does), so both give the same windows while fixed bounds stay
    reproducible across data deliveries. Origin still to be confirmed with
    Bastian.
  - *Several measurements in one window:* step 03 uses the measurement
    closest to the nominal age of the visit (`target_age`, e.g. 1095 days for
    3 years), not the histogram peak (Coen): one rule for all organisations
    and easy to explain. Still to check in RA: how often children have several
    measurements in a window -- if these are mostly follow-up visits after an
    abnormal finding, the rule can affect prevalences.
- **The catchment sample (jgz_catchment 01) follows Coen's do-files.** First rebuilt from the variable list of his
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
  comes from LANDAKTUEELREFV15 and ISCED from OPLEIDINGSNRREFV36 /
  CTOREFV13 (Coen's versions are no longer in RA), and ETNGRP falls back on country labels if that file is not found. Compare the
  output with Coen's
  `H:/Archive/Coen/TempData/Data_CatchmentArea_3pooled_withvariables.dta`
  inside RA (children per `org_catchment`, `d_obs_cat` distribution) and check
  the cohort windows printed by catchment step 01. The flow table and cohort shares contain
  counts below 10: not for export.
- **Denominator of the coverage (bereik), catchment sample.** All children in GBA who
  (1) live continuously in the PC4 catchment area of *one* organisation from
  month 22 through month 53 of age (calendar months since the birth month,
  about 1y10m-4y5m; not 18 months-4 years), with gaps between address spells
  of at most one month and the area counting until its `einde`; and (2) are
  born within that organisation's cohort window ([C9]). Whether the JGZ saw
  them plays no role in the denominator; that is the numerator (`d_obs_cat`).
  Verian's Apeldoorn is only left out when computing the cohort windows; the
  regression do-file drops it. Windows and rules in `config/jgz_catchment.yml`
  (`catch_*`).
- **Coverage per postcode area.** The earlier `catchment_area_calculations.R`
  (2019: share of never-moved children born 2010-2014 found in the JGZ ID
  file, per PC2/PC3) is removed: the official PC4 catchment file and the
  catchment sample replace it. Coverage *within* the catchment areas comes from
  `output/jgz_catchment_cohort.rds` (`y_m<age>_takeup` by `postcode_4` or `org_catchment`).
- **Check of the catchment areas (catchment 05) -- agreed 27 Oct 2026.** Is Coen's PC4
  list complete? 05 computes the coverage for every PC4 where the JGZ saw at
  least one child: denominator = all children in GBA (born in the catchment range)
  whose address in the month of the nominal age (24 / 36 / 45 months,
  `cov_reference_months`) is in that PC4, in a calendar year with JGZ data;
  numerator = those with a JGZ panel row in the contact-moment window (any
  organisation, as the catchment sample). No continuity or cohort rule, unlike that sample. It lists the
  PC4s not in the PC4 file with coverage above 20% (`cov_flag_min_coverage`,
  with at least `cov_flag_min_children` children) and shows them per calendar
  year, because catchment areas can change over time. Whether to add them is
  decided with Bastian. Note: from 4 years on (school groups) children are
  seen via their school, so a PC4 can have data because children go to school
  in the area; 05 only looks at 2y, 3y and 3y9m.
- **CBS file versions -- decided 23 Oct 2026.** GBAPERSOONTAB,
  GBAADRESOBJECTBUS, VSLPOSTCODEBUS and KINDOUDERTAB are the same versions as
  in prim8_a/prim8_b (2024/2024/2026031/2025; before: 2023/2023/2025031/2023),
  so the pipelines can share files. These four are read from their SQLite
  versions (`.db` in the "geconverteerde data" folders, as the other
  cohorts in `src/`) with `read_cbs_jgz()` in `jgz_helpers.R`: only the rows of the
  JGZ children, their parents, siblings and addresses are read, instead of the
  whole file. Needs the R packages DBI and RSQLite; pointing a path in the
  config to the `.sav` gives the same result with `read_sav()`. The yearly
  files (income, education, health costs) are still read as `.sav`. Newer versions include CBS corrections, so counts in 01b
  and the catchment sample can shift slightly. Coen's CBS code lists used there are no longer in RA,
  so newer versions are used: LANDAKTUEELREFV15 for ETNGRP (his V9;
  06-11-2026) and OPLEIDINGSNRREFV36 / CTOREFV13 for ISCED (his V24 / V8;
  09-11-2026), the same versions as prim8_b and newborns.
- **PC4 file.** `resources/jgz_pc4gebieden.csv` is the copy from
  `Opportunity-Analysis-Map/JGZ/resources`; the copy in "Background info
  preprocessing JGZ" on SharePoint has broken quoting. PC4 8080 is listed for
  Verian and org12; the catchment sample applies Coen's correction (Verian 8080 -> 8180).
