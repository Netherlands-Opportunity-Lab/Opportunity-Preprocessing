# Kansenkaart data preparation pipeline -- data checks
#
# Checks that stop the pipeline with a clear message instead of letting a
# problem pass silently (Gentzkow & Shapiro, "Code and Data for the Social
# Sciences": keys, and "check for errors").
#
#   check_keys()    the key columns of a table are present, non-missing and unique
#   checked_join()  a left/inner join that first checks the join type (1:1 or m:1)
#                   and reports how many rows did not match
#   check_final_cohort()  the final cohort file has the columns, number of rows
#                   and value ranges it should have (settings: `final_checks` in
#                   config/<cohort>.yml)
#
# Written for the CBS RA package versions (dplyr 1.0.2): no join_by(), no
# `relationship =` argument.
#
# (c) Netherlands Opportunity Lab 2026

library(dplyr)


# Stop unless `keys` identify the rows of `dat`: every key column exists, has no
# missing values and every combination occurs once. Returns `dat` invisibly, so it
# can be used in a pipe: dat %>% check_keys(c("RINPERSOONS", "RINPERSOON")).
check_keys <- function(dat, keys, name = deparse(substitute(dat))) {
  absent <- setdiff(keys, names(dat))
  if (length(absent) > 0) {
    stop(name, ": key column(s) not found: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  key_dat <- as.data.frame(dat)[, keys, drop = FALSE]
  n_missing <- sum(!stats::complete.cases(key_dat))
  n_duplicate <- sum(duplicated(key_dat))
  if (n_missing > 0 || n_duplicate > 0) {
    stop(sprintf("%s: key (%s) is not valid: %d row(s) with a missing key value, %d duplicate key(s)",
                 name, paste(keys, collapse = ", "), n_missing, n_duplicate), call. = FALSE)
  }
  invisible(dat)
}


# Join `y` onto `x` after checking the join type:
#   "m:1"  every row of x matches at most one row of y (y unique on `by`)
#   "1:1"  both x and y are unique on `by`
# `by` as in dplyr: c("a", "b") or c("a" = "b"). Prints how many rows of x found no
# match in y, so a join that silently loses or never fills rows becomes visible.
checked_join <- function(x, y, by, type = c("left", "inner"), relationship = c("m:1", "1:1"),
                         name = paste(deparse(substitute(x)), "+", deparse(substitute(y)))) {
  type <- match.arg(type)
  relationship <- match.arg(relationship)
  by_x <- if (is.null(names(by))) by else ifelse(names(by) == "", by, names(by))
  by_y <- unname(by)

  check_keys(y, by_y, name = paste0(name, " (right-hand table)"))
  if (relationship == "1:1") check_keys(x, by_x, name = paste0(name, " (left-hand table)"))

  n_unmatched <- nrow(anti_join(x, y, by = by))
  message(sprintf("%s: %s join on %s, %d of %d rows without a match",
                  name, type, paste(by_x, collapse = ", "), n_unmatched, nrow(x)))

  if (type == "left") left_join(x, y, by = by) else inner_join(x, y, by = by)
}


# Checks on the final cohort file (output/<cohort>_cohort.rds). P_2_1_run_cohort.R
# runs them on `cohort_dat` after the last step of a cohort, so a wrong cohort file
# stops the run in the RA instead of being found later by whoever analyses it.
# What to check is set per cohort under `final_checks` in config/<cohort>.yml:
#
#   final_checks:
#     expected_columns: [...]       must be in the file (its outcomes)
#     forbidden_columns: [...]      must not be in it (helper columns, outcomes
#                                   that belong to the other sample)
#     dropped_in_last_step: [...]   the only columns the last step may drop
#     ranges:                       column: [setting_min, setting_max], the names of
#       birthdate: [child_birth_date_min, child_birth_date_max]   settings under
#                                   data_preparation (dates as "dd/mm/yyyy")
#
# The checks:
#   1. the last step drops no columns other than `dropped_in_last_step` (needs
#      `columns_before`, the columns before that step; skipped when it ran alone)
#   2. every expected column is present and no forbidden column is
#   3. the number of rows equals the last count (n_...) in the most recent
#      <step>_sample_size.rds in `temp_folder`, so the file and the sample size
#      table come from the same run
#   4. the values of the `ranges` columns lie within the settings; missing values
#      are reported but are not an error
#
# Stops with one message that lists every failed check. Otherwise returns, invisibly,
# a few lines for the run log.
check_final_cohort <- function(dat, checks, cfg, temp_folder, columns_before = NULL,
                               name = "cohort") {
  problems <- character(0)
  report <- character(0)

  # 1. columns dropped in the last step
  if (is.null(columns_before)) {
    report <- c(report, "column comparison skipped: the step before the last one did not run in this session")
  } else {
    dropped <- setdiff(columns_before, names(dat))
    unexpected <- setdiff(dropped, checks$dropped_in_last_step)
    if (length(unexpected) > 0) {
      problems <- c(problems, paste0("the last step dropped column(s) it should keep: ",
                                     paste(unexpected, collapse = ", ")))
    }
    added <- setdiff(names(dat), columns_before)
    report <- c(report, paste0("last step dropped: ", paste_or_none(dropped),
                               "; added: ", paste_or_none(added)))
  }

  # 2. expected and forbidden columns
  absent <- setdiff(checks$expected_columns, names(dat))
  if (length(absent) > 0) {
    problems <- c(problems, paste0("expected column(s) missing: ", paste(absent, collapse = ", ")))
  }
  present <- intersect(checks$forbidden_columns, names(dat))
  if (length(present) > 0) {
    problems <- c(problems, paste0("forbidden column(s) present: ", paste(present, collapse = ", ")))
  }

  # 3. number of rows against the sample size table
  size_files <- list.files(temp_folder, "sample_size\\.rds$", full.names = TRUE)
  if (length(size_files) == 0) {
    report <- c(report, paste0("row count not checked: no *_sample_size.rds in ", temp_folder))
  } else {
    size_file <- size_files[which.max(file.mtime(size_files))]
    sizes <- readRDS(size_file)
    n_col <- utils::tail(grep("^n_", names(sizes), value = TRUE), 1)
    n_expected <- if (length(n_col) == 1) sizes[[n_col]][1] else NA
    if (!isTRUE(nrow(dat) == n_expected)) {
      problems <- c(problems, sprintf("%d rows, but %s in %s is %s", nrow(dat),
                                      n_col, basename(size_file), n_expected))
    } else {
      report <- c(report, sprintf("%d rows = %s in %s", nrow(dat), n_col, basename(size_file)))
    }
  }

  # 4. value ranges
  for (col in names(checks$ranges)) {
    settings <- unlist(checks$ranges[[col]])
    limits <- lapply(settings, function(s) cfg[[s]])
    if (length(settings) != 2 || any(vapply(limits, is.null, logical(1)))) {
      problems <- c(problems, paste0("range of ", col, ": [", paste(settings, collapse = ", "),
                                     "] are not two settings in data_preparation"))
      next
    }
    if (!col %in% names(dat)) {
      problems <- c(problems, paste0("range of ", col, ": column not found"))
      next
    }
    x <- dat[[col]]
    if (inherits(x, c("Date", "POSIXt"))) {
      x <- as.Date(x)
      limits <- lapply(limits, config_date)
    } else {
      # plain numbers (also for labelled or factor columns read from CBS files)
      x <- if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(unclass(x))
      limits <- lapply(limits, as.numeric)
    }
    n_outside <- sum(x < limits[[1]] | x > limits[[2]], na.rm = TRUE)
    n_missing <- sum(is.na(x))
    if (n_outside > 0) {
      problems <- c(problems, sprintf("%s: %d value(s) outside [%s, %s]", col, n_outside,
                                      format(limits[[1]]), format(limits[[2]])))
    } else {
      report <- c(report, sprintf("%s within [%s, %s], %d missing", col,
                                  format(limits[[1]]), format(limits[[2]]), n_missing))
    }
  }

  if (length(problems) > 0) {
    stop(name, ": the final cohort file failed ", length(problems), " check(s):\n  - ",
         paste(problems, collapse = "\n  - "), call. = FALSE)
  }
  invisible(paste0(name, " final check: ", report))
}

paste_or_none <- function(x) if (length(x) == 0) "none" else paste(x, collapse = ", ")

# a date setting from a config: "dd/mm/yyyy" (cohort configs) or "yyyy-mm-dd"
config_date <- function(x) {
  d <- as.Date(x, format = "%d/%m/%Y")
  if (is.na(d)) d <- as.Date(x, format = "%Y-%m-%d")
  if (is.na(d)) stop("not a date: ", x, call. = FALSE)
  d
}
