# Kansenkaart data preparation pipeline -- data checks
#
# Checks that stop the pipeline with a clear message instead of letting a
# problem pass silently (Gentzkow & Shapiro, "Code and Data for the Social
# Sciences": keys, and "check for errors").
#
#   check_keys()    the key columns of a table are present, non-missing and unique
#   checked_join()  a left/inner join that first checks the join type (1:1 or m:1)
#                   and reports how many rows did not match
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
