# SQLite experiment (prim8_a)

Goal: speed up preprocessing by reading converted SQLite (`.db`) versions of the CBS
microdata instead of `.sav`, without changing the results.

## What is here

| file | what it does |
|---|---|
| `config/P_2_1_prim8_a.yml` | same settings as `config/P_2_1_prim8_a.yml`; the 7 converted files point to their `.db`, output to `scratch/prim8_a_sql` |
| `src/sql_helpers.R` | `read_db()` and `find_latest_db()` |
| `src/prim8_a/P2_1_0*.R` | the four prim8_a scripts; only the reads of the 7 converted files changed (marked `[SQL]`) |
| `run_prim8_a_experiment.R` | runs the `sav` or `sql` variant and logs time and memory per script |
| `compare_prim8_a.R` | checks that both variants produce identical scratch files (prints counts only) |

## How to run (in the RA, working directory = repository root)

1. `run_prim8_a_experiment.R` with `variant <- "sav"` (fresh R session)
2. `run_prim8_a_experiment.R` with `variant <- "sql"` (fresh R session)
3. `compare_prim8_a.R`: all files should say `all values identical`, and it prints the timing side by side

## Which files are read from `.db`

For now only these 7 files are converted and read with `read_db()`:
GBAPERSOON2024TABV3, GBAADRESOBJECT2024BUSV1, KINDOUDER2025TABV2, GBAHUISHOUDENS2024BUSV1,
VSLPOSTCODEBUSV2026031, VSLGWB2025TAB03V1, LEVCYCLWOONNIETWOONBUSV2025123.

All yearly files (IPI/INPA, VEH, KOPPELPERSOONHUISHOUDEN, HOOGSTEOPL, INSCHRWPO,
ZVWZORGKOSTEN, JGDBESCHERM, EIGENDOM) and the reference files on K: (OPLEIDINGSNRREF,
CTOREF) are still read with `read_sav()`, with the original code. To switch one of them
later: replace its `read_sav(path, col_select = ...)` with
`read_db(path, cols = ..., filter_in = ..., numeric = ...)` and point its
`get_*_filename()` to the `.db` (`find_latest_db()` in `src/sql_helpers.R` does that).

## How `read_db()` works

- Opens the `.db` read-only and filters inside SQLite: the IDs the script needs
  (cohort children, parents, their dwellings) go into a TEMP table, and only matching
  rows come back to R. `read_sav()` always loaded the whole file.
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

## Tested

On the synthetic data (`werkeur/concept/mock_data`) with the same 7 files converted
to `.db` and the rest as `.sav`: all eight scratch files are identical to the `.sav`
pipeline (values, row order, column types).
