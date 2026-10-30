# Plan: shared, normalised intermediate tables (not implemented yet)

Status: open. Written 30-10-2026 when the repository was reorganised along the
rules of Gentzkow & Shapiro, *Code and Data for the Social Sciences* (chapter 5,
"Keys"). Remove this file once it is done or dropped.

## The problem
Every sample builds the same person-level facts from scratch inside its own
`02_predictors` script: parental income and wealth per year, the parents'
residency spells, household links, the region of an address. The code is
repeated in every sample folder (`age16`, `age21`, `age35`, `prim8`, `newborns`),
the results are joined into one wide `cohort_dat` and then removed with `rm()`.
A fix to, say, the income definition has to be made in five places, and nothing
of it can be reused by another sample or project.

## The proposal
A step `00_build_tables` (its own folder, `src/tables/`) that writes keyed,
normalised tables once per CBS file version, e.g. as tables in one SQLite file:

| Table | Key | Content |
|---|---|---|
| `person_income_year` | RINPERSOONS, RINPERSOON, year | personal income (IPI/INPA), deflated, with the 999999999/negative rules applied |
| `household_wealth_year` | RINPERSOONS, RINPERSOON, year | household wealth via the koppeltabel (VEH), deflated |
| `address_spell` | RINPERSOONS, RINPERSOON, start date | GBA address spells |
| `region_address` | RINOBJECTNUMMER, region year | PC4, PC3, municipality, wijk, COROP, Amsterdam stadsdeel/gebied |

The sample scripts then only select and aggregate (average over the sample's
years, percentile ranks within the sample) and join with `checked_join()`.

## Why it is not done yet
It changes how every sample's predictors are computed, and the result can only
be checked inside the CBS RA. The plan for doing it safely:

1. Pick one sample (prim8_a) and build `person_income_year` and
   `household_wealth_year` from its current code.
2. In the RA, run old and new code side by side and compare the
   `02_predictors.rds` files (same rows, identical values).
3. Only then move the other samples over, one at a time, with the same comparison.
