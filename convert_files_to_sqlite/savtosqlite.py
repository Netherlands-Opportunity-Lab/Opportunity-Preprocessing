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

Progress
  - Every step is written with a timestamp to the screen and to `savtosqlite.log` in
    OUT_DIR (appended, written immediately), so after reconnecting to the RA you can see
    how far it got, or why it stopped. One line per chunk: rows so far, % of the file,
    and the seconds spent reading and writing that chunk.
  - If the read time per chunk keeps going up, that is expected: pyreadstat reads each
    chunk with row_offset, and for a compressed .sav it has to unpack all rows before
    that offset again. The total extra work grows with the number of chunks, so for
    large files (SPOLISBUS) use a large CHUNKSIZE.

Chunk size and memory
  - CHUNKSIZE = 1,000,000 rows (was 200,000): 5x fewer chunks, so 5x fewer re-reads.
  - Memory per chunk is roughly, per million rows, 8 MB per numeric column and ~60 MB
    per text column. The log shows the actual size of the first chunk. If you get a
    MemoryError, lower CHUNKSIZE; if there is memory to spare, raise it.

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
import time
import traceback
from datetime import datetime

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
CHUNKSIZE = 1_000_000   # rows per chunk; see "Chunk size and memory" above
DISABLE_DATETIME_CONVERSION = False
LOG_FILE = os.path.join(OUT_DIR, "savtosqlite.log")


def log(msg):
    """Print msg with a timestamp and append it to LOG_FILE right away."""
    line = f"{datetime.now():%Y-%m-%d %H:%M:%S}  {msg}"
    print(line, flush=True)
    # open/close per line, so the line is on disk even if the session ends
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def fmt_secs(s):
    return f"{int(s // 3600)}h{int(s % 3600 // 60):02d}m{int(s % 60):02d}s"


def convert(src, db, index_cols=INDEX_COLS, chunksize=CHUNKSIZE):
    table = os.path.splitext(os.path.basename(src))[0]
    t_start = time.time()
    if os.path.exists(db):
        os.remove(db)

    con = sqlite3.connect(db)
    con.execute("PRAGMA journal_mode=OFF")   # fast bulk load; if it crashes, just run again
    con.execute("PRAGMA synchronous=OFF")

    # metadata (column names, value labels) first, so it is also there for an empty file
    _, meta = pyreadstat.read_sav(src, metadataonly=True)
    n_total = meta.number_rows   # can be None if the .sav header does not store it
    log(f"start {table}: {n_total if n_total is None else f'{n_total:,}'} rows, "
        f"{meta.number_columns} columns, {os.path.getsize(src) / 1e9:.1f} GB, "
        f"chunksize {chunksize:,} -> {db}")

    # 1. data, in chunks (rows are appended in file order)
    n = 0
    i = 0
    with con:
        t_chunk = time.time()
        for df, _ in pyreadstat.read_file_in_chunks(
                pyreadstat.read_sav, src, chunksize=chunksize,
                disable_datetime_conversion=DISABLE_DATETIME_CONVERSION):
            # time since the previous chunk was written = reading this chunk
            t_read = time.time() - t_chunk
            if i == 0:
                log(f"  chunk 1 uses {df.memory_usage(deep=True).sum() / 1e9:.2f} GB in memory")
            t0 = time.time()
            df.to_sql(table, con, if_exists="append", index=False)
            t_write = time.time() - t0
            n += len(df)
            i += 1
            pct = f" ({n / n_total:.0%})" if n_total else ""
            log(f"  chunk {i}: {n:,} rows{pct} | read {t_read:.0f}s, write {t_write:.0f}s"
                f" | total {fmt_secs(time.time() - t_start)}")
            t_chunk = time.time()
        if n == 0:   # empty file: still create the (empty) table
            pd.DataFrame(columns=meta.column_names).to_sql(table, con, index=False)

    # 2. indexes, after all rows are in (faster than building them during the load)
    for col in index_cols:
        if col in meta.column_names:
            log(f"  index on {col} ...")
            t0 = time.time()
            con.execute(f'CREATE INDEX IF NOT EXISTS "idx_{table}_{col}" ON "{table}" ("{col}")')
            log(f"  index on {col} done in {fmt_secs(time.time() - t0)}")
    con.commit()

    # 3. value labels: one row per (variable, code, label)
    labels = [(var, code, lab)
              for var, lblset in meta.variable_to_label.items()
              for code, lab in meta.value_labels.get(lblset, {}).items()]
    pd.DataFrame(labels, columns=["variable", "code", "label"]).to_sql(
        "value_labels", con, if_exists="replace", index=False)

    con.close()
    log(f"done {table}: {n:,} rows, {len(labels)} value labels "
        f"in {fmt_secs(time.time() - t_start)} -> {db}")


if __name__ == "__main__":
    log(f"=== run started: {len(FILES)} file(s), chunksize {CHUNKSIZE:,} ===")
    for src in FILES:
        table = os.path.splitext(os.path.basename(src))[0]
        try:
            convert(src, os.path.join(OUT_DIR, table + ".db"))
        except BaseException:   # also Ctrl+C / MemoryError: write why it stopped
            log(f"FAILED {table}:\n{traceback.format_exc()}")
            raise
    log("=== run finished ===")
