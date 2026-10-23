# =============================================================================
# JGZ pilot -- disclosure checks for the gradient output (used by 04)
#
# A gradient is one outcome for one contact moment (cm) and subgroup over the
# 20 parental-income bins. A gradient is only exported when:
#   1. every bin has N >= min_cell
#   2. for proportions: every bin has N * p >= min_cell and N * (1 - p) >= min_cell
#   3. all 20 bins are present (otherwise a suppressed bin could be derived)
# Checks are applied per gradient: a gradient fails as a whole.
#
# Changes compared with the old version (see git history):
#   - complex_check() suppresses per outcome. The old version dropped whole
#     rows when one outcome failed, which also removed the other outcomes
#     (e.g. weight and length disappeared when BMI was suppressed), and it
#     added a stray text column `estimate` to the export.
#   - proportions are recognised by 0 <= estimate <= 1 (was only <= 1).
# =============================================================================

N_BINS <- 20
BIN_LABELS <- paste0(seq(0, 95, 5), "% - ", seq(5, 100, 5), "%")

label_bins <- function(data) {
  if ("bin" %in% names(data)) data$bin <- BIN_LABELS[as.integer(data$bin)]
  data
}

is_proportion <- function(estimate) {
  e <- estimate[!is.na(estimate)]
  length(e) > 0 && all(e >= 0 & e <= 1)
}

# Which rows belong to a gradient that passes checks 1 and 2.
# `keys` identify a gradient; returns a logical vector over the rows.
gradient_passes <- function(data, keys, estimate, N, min_cell = 10) {
  ok_row <- !is.na(estimate) & !is.na(N) & N >= min_cell
  if (is_proportion(estimate)) {
    ok_row <- ok_row & N * estimate >= min_cell & N * (1 - estimate) >= min_cell
  }
  g <- interaction(data[keys], drop = TRUE)
  # every row of the gradient must be ok, and all bins must be present
  all_ok <- tapply(ok_row, g, all)
  n_bins <- tapply(!is.na(estimate), g, sum)
  (all_ok & n_bins >= N_BINS)[as.integer(g)]
}

# Long data: cm, outcome, subgroup, bin, estimate, N, inc
simple_check <- function(data, min_cell = 10) {
  keep <- gradient_passes(data, c("cm", "outcome", "subgroup"), data$estimate, data$N, min_cell)
  label_bins(data[keep & !is.na(data$estimate), ])
}

# Wide data: cm, subgroup, bin, estimate_<o>, N_<o>, inc_<o> per outcome o
complex_check <- function(data, outcomes, min_cell = 10) {
  for (o in outcomes) {
    est <- paste0("estimate_", o); n <- paste0("N_", o); inc <- paste0("inc_", o)
    if (!est %in% names(data)) next
    keep <- gradient_passes(data, c("cm", "subgroup"), data[[est]], data[[n]], min_cell)
    for (col in intersect(c(est, n, inc), names(data))) data[[col]][!keep] <- NA
  }
  est_cols <- intersect(paste0("estimate_", outcomes), names(data))
  data <- data[rowSums(!is.na(data[est_cols])) > 0, ]
  data <- data[, colSums(!is.na(data)) > 0, drop = FALSE]   # drop empty columns
  label_bins(data)
}

# BMI percentile gradients: cm, bin, N, inc, p10, p25, p50, p75, p90
bmi_p_check <- function(data, min_cell = 10) {
  pcols <- intersect(c("p10", "p25", "p50", "p75", "p90"), names(data))
  # 1 + 3: every bin N >= min_cell and all bins present, per cm
  keep <- gradient_passes(data, "cm", data$N, data$N, min_cell)
  data <- data[keep, ]
  # The p10 ... p90 columns are not BMI values: in Coen's gradient do-file
  # ("20210713 ANALYSIS GRADIENTS - cases") they hold the NUMBER of children
  # in the bin whose BMI is above that percentile (r(N) of bmi_cm<k>_p<p>==1,
  # only filled when >= 10); in the earlier version the share. So the old
  # rule "value below 10" was the small-cell rule on these counts. Applied
  # here to the count and its complement (N - count); shares are converted
  # to counts first. A percentile column fails for a cm as a whole.
  for (p in pcols) {
    v <- data[[p]]
    count <- if (is_proportion(v)) v * data$N else v
    bad <- is.na(count) | count < min_cell | (data$N - count) < min_cell
    fail <- tapply(bad, data$cm, any)
    data[[p]][data$cm %in% names(fail)[fail %in% TRUE]] <- NA
  }
  data <- data[, colSums(!is.na(data)) > 0, drop = FALSE]
  label_bins(data)
}
