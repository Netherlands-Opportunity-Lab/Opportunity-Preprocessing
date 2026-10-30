# SQLite experiment (all cohorts)

Goal: speed up preprocessing by reading converted SQLite (`.db`) versions of the CBS
microdata instead of `.sav`, without changing the results.

## What is here

| file | what it does |
|---|---|
| `config/P_2_*_<cohort>.yml` | same settings as `config/P_2_*_<cohort>.yml`; files that have a `.db` point to it, output goes to `scratch/<cohort>_sql`. Inside the CBS RA the configs and scripts have no `P_2_*` prefix (`config/<cohort>.yml`, `01_...R`); the runner and the compare script find both |
| `src/sql_helpers.R` | `read_cbs()`, `read_db()`, `cbs_path()`, `find_latest_db()` |
| `src/<cohort>/*.R` | the four scripts per cohort; only the reads of converted files changed (marked `[SQL]`) |
| `run_experiment.R` | runs the `sav` or `sql` variant of one cohort and logs time and memory per script |
| `compare_experiment.R` | checks that both variants produce identical scratch files (prints counts only) |
| `../convert_files_to_sqlite/savtosqlite.py` | converts `.sav` files to `.db` in the format `read_db()` expects, with an index on RINPERSOON / RINOBJECTNUMMER (Python, pyreadstat) |

Cohorts: `prim8_a`, `prim8_b`, `newborns_a_mortality`, `newborns_a_perinatal`,
`newborns_b_mortality`, `newborns_b_perinatal`, `age16`, `age21`, `age35`.

## How to run (in the RA, working directory = repository root)

1. `run_experiment.R` with `cohort <- "age35"` and `variant <- "sav"` (fresh R session)
2. `run_experiment.R` with the same cohort and `variant <- "sql"` (fresh R session)
3. `compare_experiment.R` with the same cohort: every file should say `all values identical`,
   and it prints the timing side by side

From a terminal: `Rscript sqlexperiment/run_experiment.R age35 sav`, then `... age35 sql`,
then `Rscript sqlexperiment/compare_experiment.R age35`.

## Which files are read from `.db`

Converted so far: GBAPERSOON2024TABV3, GBAADRESOBJECT2024BUSV1, KINDOUDER2025TABV2,
GBAHUISHOUDENS2024BUSV1, VSLPOSTCODEBUSV2026031, VSLGWB2025TAB03V1,
LEVCYCLWOONNIETWOONBUSV2025123, and for age35 SECMBUS2024V1 and SPOLISBUS (per year).

The age cohorts use older versions of three of these files. Those stay `.sav` in the sql
configs, so that both variants read exactly the same data:

| cohort | read from `.db` | still `.sav` (other version than the `.db`) |
|---|---|---|
| prim8_a, prim8_b | GBAPERSOON, GBAADRESOBJECT, KINDOUDER, GBAHUISHOUDENS, VSLPOSTCODE, VSLGWB, LEVCYCLWOONNIETWOON | – |
| newborns (all four) | GBAPERSOON, GBAADRESOBJECT, KINDOUDER, GBAHUISHOUDENS, VSLPOSTCODE, VSLGWB | – |
| age16 | GBAPERSOON, GBAADRESOBJECT, GBAHUISHOUDENS, VSLGWB | KINDOUDER2024TABV1, VSLPOSTCODEBUSV2025031, LEVCYCLWOONNIETWOONBUSV2024122 |
| age21 | GBAPERSOON, GBAADRESOBJECT, GBAHUISHOUDENS, VSLGWB | KINDOUDER2024TABV1, VSLPOSTCODEBUSV2025031 |
| age35 | GBAPERSOON (02 only), GBAADRESOBJECT, GBAHUISHOUDENS, VSLGWB, **SECMBUS**, **SPOLISBUS** (each year that has a `.db`) | KINDOUDER2024TABV1, VSLPOSTCODEBUSV2025031, LEVCYCLWOONNIETWOONBUSV2024122, GBAPERSOON2018TABV2 (hard-coded in 01) |

All reads of these files go through `read_cbs()`, which looks at the path in the config:
`.db` → filtered inside SQLite; `.sav` → `read_sav()` plus the same filter in R. So moving
a cohort to a newer (converted) version, or converting one of the older versions, is a
config change only. (Moving to a newer version does change the results, also for the
`sav` pipeline; that is a separate decision.)

All other yearly files (IPI/INPA, IHI/INHA, VEH, KOPPELTABEL, HOOGSTEOPL, INSCHRWPO,
ZVWZORGKOSTEN, JGDBESCHERM, EIGENDOM, SCH, DO/DOODOORZ/GBAOVERLIJDEN) and the reference
files on K: are still read with `read_sav()`, with the original code.

### SECMBUS (age35)

The sql config expects `InkomenBestedingen/SECMBUS/geconverteerde data/SECMBUS2024V1.db`.
Until CBS has moved your converted file there, set `secm_data` in
`sqlexperiment/config/P_2_0_age35.yml` to its full path (e.g. `H:/.../SECMBUS2024V1.db`);
`cbs_path()` accepts both. Convert it with `convert_files_to_sqlite/savtosqlite.py` so it
has the `value_labels` table and the RINPERSOON index (see below).

### SPOLISBUS (age35)

For each year, step 03 first looks for a `.db` with exactly the name of the latest
`SPOLISBUS<year>V<n>.sav` (so the same version): in `spolis_db_folder` from the sql config
(e.g. the folder on H: where `savtosqlite.py` wrote them), then in `Spolis/SPOLISBUS/geconverteerde data`.
Years without such a `.db` are read from the `.sav`, so you can convert year by year.
No script in the Opportunity Lab repos reads POLISBUS (the files before SPOLIS).

## How `read_cbs()` / `read_db()` work

- Opens the `.db` read-only and filters inside SQLite: the IDs the script needs
  (cohort children, parents, their dwellings) go into a TEMP table, and only matching
  rows come back to R. `read_sav()` always loaded the whole file.
- Every filter mirrors what the original script does with the table afterwards (a
  `filter(RINPERSOON %in% ...)`, or a join/lookup on those IDs only), so it never drops a
  row that could reach the output. The reason is written next to each `[SQL]` change.
- `filter_in` with several columns means AND; `match = "any"` means OR (used once, for
  `RINPERSOONMa %in% x | RINPERSOONpa %in% x` in age21).
- Returns rows in file order (`ORDER BY rowid`), because the scripts rely on row
  order in a few places (`RINOBJECTNUMMER[1]`, `row_number() == 1`).
- Re-attaches the value labels from the `value_labels` table as `haven_labelled`,
  so `as_factor()` gives the same factors as with `read_sav()`.
- `numeric =` converts columns stored as TEXT (income, wealth, health costs) to numbers.

## Assumptions about the converted files

- One `.db` per catalog file, in a `geconverteerde data` folder next to the catalog
  file (the paths are in the config).
- Data table named after the file; labels in `value_labels` (variable, value, label).
  The column names are detected automatically; set
  `options(sqlexp.label_cols = c(...))` if that fails.
- **The `value_labels` table is needed.** Without it the columns come back unlabelled,
  and `as_factor(x, levels = "values")` then gives other factors (newer haven versions
  even stop with an error). `read_db()` warns when the table is missing.
  `convert_files_to_sqlite/savtosqlite.py` writes this format.

## Tested

On the synthetic data (`werkeur/concept/mock_data/synthetic`), with the eight files above
converted with `savtosqlite.py` (a full `read_db()` of each `.db` is identical to
`read_sav()`; SPOLISBUS 2020–2024 included), and the RA folder layout (`G:/`, `K:/`, `resources/`) mimicked: for all nine
cohorts every scratch file of the `sql` variant is identical to the `sav` variant
(values, row order, column types, factor levels). The timings on the synthetic data say
little (the files are tiny); the real test is the run in the RA.

Two cohorts first did not run to the end on the synthetic data, in **both** variants,
because of bugs in `src/`. These are now fixed in `src/` and in the copies here:
- age16, `01_cohort`: the age-12/16 loop filtered on `recordstart`/`recordend` (not columns
  of GBAADRESOBJECT) → now `GBADATUMAANVANGADRESHOUDING`/`GBADATUMEINDEADRESHOUDING`, as in
  age21; and it created `new_postcode4_age*` while the next line and `03_outcomes` use
  `postcode4_age*` → renamed to `postcode4_age*`.
- age35, `03_outcomes`: the "delete observations if outcomes are missing" filter ran on a data
  frame still grouped by `birth_year` (size mismatch error) → `ungroup()` first.
