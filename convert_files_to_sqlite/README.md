# Reading CBS files from SQLite (.db)

The preprocessing scripts in `src/` read the large CBS files from converted SQLite (`.db`)
versions instead of `.sav`. Only the rows a script needs leave the database, which cuts
the running time and memory use a lot, without changing the results.

| file | what it does |
|---|---|
| `src/sql_helpers.R` | `read_cbs()`, `read_db()`, `cbs_path()`, `find_latest_db()`; sourced by `P_2_0_create_cohort_data.R` |
| `config/P_2_*_<cohort>.yml` | files that have a `.db` point to it; the others are still `.sav` |
| `convert_files_to_sqlite/savtosqlite.py` | converts `.sav` files to `.db` in the format `read_db()` expects, with an index on RINPERSOON / RINOBJECTNUMMER (Python, pyreadstat) |

Reads of converted files are marked `[SQL]` in the scripts, with the reason for each filter.
Needs the R packages DBI and RSQLite (both available in the CBS RA).

The scripts as they were before this change (everything read with `read_sav()`) are in
the git history: commit `c019c13` (the last commit before the switch).

## Which files are read from `.db`

Converted so far: GBAPERSOON2024TABV3, GBAADRESOBJECT2024BUSV1, KINDOUDER2025TABV2,
GBAHUISHOUDENS2024BUSV1, VSLPOSTCODEBUSV2026031, VSLGWB2025TAB03V1,
LEVCYCLWOONNIETWOONBUSV2025123, and for age35 SECMBUS2024V1 and SPOLISBUS (per year).

The age cohorts use older versions of three of these files. Those stay `.sav` in the
configs:

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
config change only. (Moving to a newer version does change the results; that is a
separate decision.)

All other yearly files (IPI/INPA, IHI/INHA, VEH, KOPPELTABEL, HOOGSTEOPL, INSCHRWPO,
ZVWZORGKOSTEN, JGDBESCHERM, EIGENDOM, SCH, DO/DOODOORZ/GBAOVERLIJDEN) and the reference
files on K: are still read with `read_sav()`.

The JGZ pipeline (`src/jgz/`) has its own copy of `read_cbs()` in `jgz_helpers.R`.

### SECMBUS (age35)

`config/P_2_0_age35.yml` expects `InkomenBestedingen/SECMBUS/geconverteerde data/SECMBUS2024V1.db`.
Until CBS has moved your converted file there, set `secm_data` to its full path
(e.g. `H:/.../SECMBUS2024V1.db`); `cbs_path()` accepts both. Convert it with
`savtosqlite.py` so it has the `value_labels` table and the RINPERSOON index (see below).

### SPOLISBUS (age35)

For each year, step 03 first looks for a `.db` with exactly the name of the latest
`SPOLISBUS<year>V<n>.sav` (so the same version): in `spolis_db_folder` from the config
(e.g. the folder on H: where `savtosqlite.py` wrote them), then in `Spolis/SPOLISBUS/geconverteerde data`.
Years without such a `.db` are read from the `.sav`, so you can convert year by year.

## How `read_cbs()` / `read_db()` work

- Opens the `.db` read-only and filters inside SQLite: the IDs the script needs
  (cohort children, parents, their dwellings) go into a TEMP table, and only matching
  rows come back to R. `read_sav()` always loaded the whole file.
- Every filter mirrors what the script does with the table afterwards (a
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
  `savtosqlite.py` writes this format.

## Tested

On the synthetic data (`werkeur/concept/mock_data/synthetic`), with the files above
converted with `savtosqlite.py` (a full `read_db()` of each `.db` is identical to
`read_sav()`; SPOLISBUS 2020–2024 included), and the RA folder layout (`G:/`, `K:/`,
`resources/`) mimicked: for all nine cohorts every scratch file of the SQLite scripts was
identical to that of the `.sav` scripts (values, row order, column types, factor levels).
