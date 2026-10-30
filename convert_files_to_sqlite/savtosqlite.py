"""
Convert CBS .sav files to SQLite (.db) for the preprocessing pipeline (see README.md here).

Each .db gets the format that read_db() / read_cbs() in src/sql_helpers.R
expect:
  - one data table, named after the file (e.g. POLISBUS2006V3), with the same variables
    and rows, in the same order, as the .sav
  - an index on RINPERSOON and/or RINOBJECTNUMMER (if present): this is what makes the
    filtered reads fast. Add other ID columns to INDEX_COLS if you filter on them
    (e.g. RINPERSOONMa / RINPERSOONpa for KINDOUDER, RINPERSOONHKW for VEH)
  - a table `value_labels` (variable, code, label) with all value labels, so read_db()
    can turn the columns back into labelled vectors. Without it, as_factor() calls in the
    scripts give other results (or an error in newer haven versions).

Usage (in the RA): fill in FILES and OUT_DIR, then run
  python convert_files_to_sqlite/savtosqlite.py

Notes
  - An existing .db with the same name in OUT_DIR is deleted first (re-running would
    otherwise append all rows a second time).
  - Columns with an SPSS date format become datetimes in pyreadstat (haven gives Date).
    Most CBS date variables are text ("20190101"), so this rarely matters; set
    DISABLE_DATETIME_CONVERSION = True to keep the raw values.
  - For the CBS request "Verplaatsen geconverteerde catalogusbestanden" the .db must have
    the same name as the catalog file, and the script must have that name too and mention
    name, institution, project number and date.

(c) ODISSEI Social Data Science team 2026
"""

import os
import sqlite3

import pandas as pd
import pyreadstat

FILES = [
    "G:/Polis/POLISBUS/POLISBUS2006V3.sav",
    "G:/Polis/POLISBUS/POLISBUS2007V3.sav",
    "G:/Polis/POLISBUS/POLISBUS2008V3.sav",
    "G:/Polis/POLISBUS/POLISBUS2009V3.sav",
]
OUT_DIR = r"H:\Opportunity-Preprocessing\convert_files_to_sqlite"
INDEX_COLS = ("RINPERSOON", "RINOBJECTNUMMER")
CHUNKSIZE = 200_000
DISABLE_DATETIME_CONVERSION = False


def convert(src, db, index_cols=INDEX_COLS, chunksize=CHUNKSIZE):
    table = os.path.splitext(os.path.basename(src))[0]
    if os.path.exists(db):
        os.remove(db)

    con = sqlite3.connect(db)
    con.execute("PRAGMA journal_mode=OFF")   # fast bulk load; if it crashes, just run again
    con.execute("PRAGMA synchronous=OFF")

    # metadata (column names, value labels) first, so it is also there for an empty file
    _, meta = pyreadstat.read_sav(src, metadataonly=True)

    # 1. data, in chunks (rows are appended in file order)
    n = 0
    with con:
        for df, _ in pyreadstat.read_file_in_chunks(
                pyreadstat.read_sav, src, chunksize=chunksize,
                disable_datetime_conversion=DISABLE_DATETIME_CONVERSION):
            df.to_sql(table, con, if_exists="append", index=False)
            n += len(df)
        if n == 0:   # empty file: still create the (empty) table
            pd.DataFrame(columns=meta.column_names).to_sql(table, con, index=False)

    # 2. indexes, after all rows are in (faster than building them during the load)
    for col in index_cols:
        if col in meta.column_names:
            con.execute(f'CREATE INDEX IF NOT EXISTS "idx_{table}_{col}" ON "{table}" ("{col}")')
    con.commit()

    # 3. value labels: one row per (variable, code, label)
    labels = [(var, code, lab)
              for var, lblset in meta.variable_to_label.items()
              for code, lab in meta.value_labels.get(lblset, {}).items()]
    pd.DataFrame(labels, columns=["variable", "code", "label"]).to_sql(
        "value_labels", con, if_exists="replace", index=False)

    con.close()
    print(f"done {table}: {n:,} rows, {len(labels)} value labels -> {db}")


if __name__ == "__main__":
    for src in FILES:
        table = os.path.splitext(os.path.basename(src))[0]
        convert(src, os.path.join(OUT_DIR, table + ".db"))
