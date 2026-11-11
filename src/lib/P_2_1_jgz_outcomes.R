# JGZ outcomes -- functions used by src/jgz/03_outcomes_jgz.R
#
# Each function takes the contact-moment rows of the JGZ cohort (the cm_<visit>
# files of 01c, bound together: one row per child x age in days, with `cm`,
# `leeftijd`, `sex`, `pc3`, `pc4`, `org` and the measurements) and returns one
# row per child with the outcome columns y_m<age>_<outcome>. Settings come
# from config/jgz.yml (data_preparation). Written for R 3.6.2 / dplyr 1.0.2
# and later (see src/jgz/jgz_helpers.R).
#
# One rule for every outcome (decided 23-10-2026): per child and age, the
# measurement closest to the nominal age (target days) among the contact
# moments listed for that age; on a tie the older measurement. When an age
# combines several items (Van Wiechen), each item gets its own closest
# measurement.
#
# Tested in tests/testthat/test-jgz-outcomes.R (no CBS data needed).

suppressPackageStartupMessages(library(dplyr))

KEYS_JGZ <- c("RINPERSOONS", "RINPERSOON")

#' Value of `var` per child: the non-missing measurement closest to `target`
#' (days) among the rows of the contact moments `cms`.
#' @return data frame: RINPERSOONS, RINPERSOON, value, leeftijd (and the extra
#'   columns in `keep`, taken from the same row)
closest_measurement <- function(rows, var, cms, target, keep = character(0)) {
  d <- rows[rows$cm %in% cms & !is.na(rows[[var]]), c(KEYS_JGZ, "leeftijd", var, keep), drop = FALSE]
  if (is.character(d[[var]])) d <- d[d[[var]] != "", , drop = FALSE]
  d$dist <- abs(d$leeftijd - target)
  d <- d[order(d$RINPERSOONS, d$RINPERSOON, d$dist, -d$leeftijd), , drop = FALSE]
  d <- d[!duplicated(d[, KEYS_JGZ]), , drop = FALSE]
  d$dist <- NULL
  names(d)[names(d) == var] <- "value"
  rownames(d) <- NULL
  d
}

# Join a list of child-level tables (each with KEYS_JGZ) into one
join_children <- function(tables) {
  tables <- tables[vapply(tables, nrow, integer(1)) > 0]
  if (length(tables) == 0) return(NULL)
  Reduce(function(a, b) full_join(a, b, by = KEYS_JGZ), tables)
}


# ---- Postcode and organisation per age -------------------------------------------------

#' pc4_m<age>, pc3_m<age> and org_m<age>: from the child's row closest to the
#' nominal age among all contact moments used for that age (any outcome).
#' The map tables use pc4/pc3 of the GBA address on the contact date (01b);
#' org is the organisation of that contact (missing when two organisations
#' recorded the same child on the same day, see 01a).
age_geography <- function(rows, age_cms, target_days) {
  out <- lapply(names(age_cms), function(age) {
    d <- rows[rows$cm %in% age_cms[[age]], , drop = FALSE]
    d$any_row <- 1L
    g <- closest_measurement(d, "any_row", age_cms[[age]], target_days[[age]], keep = c("pc4", "pc3", "org"))
    g <- g[, c(KEYS_JGZ, "pc4", "pc3", "org")]
    names(g) <- c(KEYS_JGZ, paste0(c("pc4_", "pc3_", "org_"), age))
    g
  })
  join_children(out)
}


# ---- BMI ----------------------------------------------------------------------------------

#' y_m<age>_bmi, y_m<age>_bmi_p90 (BMI above the 90th percentile) and
#' y_m<age>_bmi_p10 (below the 10th percentile) per age.
#' Outliers (|z| > z_max, z within age x sex) are set to missing first.
#' The cut-offs are computed over all children with a BMI at that age, within
#' sex (by_sex = TRUE) or boys and girls together.
bmi_outcomes <- function(rows, ages, target_days, z_max = 5, by_sex = TRUE) {
  out <- lapply(names(ages), function(age) {
    d <- closest_measurement(rows, "BMI", ages[[age]], target_days[[age]], keep = "sex")
    if (nrow(d) == 0) return(d[, KEYS_JGZ])
    d <- d %>%
      group_by(sex) %>%
      mutate(z = (value - mean(value)) / sd(value)) %>%
      ungroup() %>%
      mutate(value = ifelse(!is.na(z) & abs(z) > z_max, NA_real_, value))
    n_out <- sum(is.na(d$value))
    if (n_out > 0) message(sprintf("BMI %s: %d outlier(s) (|z| > %s) set to missing", age, n_out, z_max))
    grp <- if (isTRUE(by_sex)) d$sex else rep("all", nrow(d))
    p90 <- ave(d$value, grp, FUN = function(x) stats::quantile(x, 0.9, na.rm = TRUE))
    p10 <- ave(d$value, grp, FUN = function(x) stats::quantile(x, 0.1, na.rm = TRUE))
    res <- data.frame(d[, KEYS_JGZ],
                      bmi = d$value,
                      bmi_p90 = as.integer(d$value > p90),
                      bmi_p10 = as.integer(d$value < p10),
                      stringsAsFactors = FALSE)
    names(res)[-(1:2)] <- paste0("y_", age, "_", c("bmi", "bmi_p90", "bmi_p10"))
    res
  })
  join_children(out)
}


# ---- Van Wiechen language items -------------------------------------------------------------

#' The raw health file codes the items as numbers (1 = +, 2 = -, 3 = M); older
#' panels hold the text "+", "-", "M". Anything else (e.g. ".r", the marker of
#' a removed conflict) becomes missing. Same coding as the map tables.
wiechen_code <- function(x) {
  x <- trimws(as.character(haven::zap_labels(x)))
  unname(c("1" = "+", "2" = "-", "3" = "M", "+" = "+", "-" = "-", "M" = "M")[x])
}

#' Per age and item: y_m<age>_<item>_can (+ or M) and y_m<age>_<item>_notseen
#' (- or M); for items in `no_m`: y_m<age>_<item> (+). Plus
#' y_m<age>_language_delay = at least one of the core items of that age scored
#' - (M, reported only by the parent, is not a delay).
#' @param items named list per age: outcome name -> raw variable, e.g.
#'   list(m24 = list(words2 = "zin2w", bodyparts = "pop6"))
#' @param delay_items named list per age: the core items (outcome names) that
#'   define language_delay; NULL = all items of that age
#' @param delay_complete TRUE: language_delay only for children with ALL core
#'   items of that age scored (missing otherwise), so every child is judged on
#'   the same items (decided 11-11-2026); FALSE: for children with at least one
#'   core item scored
wiechen_outcomes <- function(rows, ages, items, target_days, no_m = character(0),
                             delay_items = NULL, delay_complete = FALSE) {
  out <- lapply(names(ages), function(age) {
    per_item <- lapply(names(items[[age]]), function(item) {
      var <- items[[age]][[item]]
      if (!var %in% names(rows)) stop("Van Wiechen variable ", var, " not in the contact-moment files")
      r <- rows[, c(KEYS_JGZ, "cm", "leeftijd")]
      r$code <- wiechen_code(rows[[var]])
      d <- closest_measurement(r, "code", ages[[age]], target_days[[age]])
      res <- d[, KEYS_JGZ]
      if (item %in% no_m) {
        res[[paste0("y_", age, "_", item)]] <- as.integer(d$value == "+")
      } else {
        res[[paste0("y_", age, "_", item, "_can")]]     <- as.integer(d$value %in% c("+", "M"))
        res[[paste0("y_", age, "_", item, "_notseen")]] <- as.integer(d$value %in% c("-", "M"))
      }
      res[[paste0(".min_", item)]] <- as.integer(d$value == "-")
      res
    })
    res <- join_children(per_item)
    if (is.null(res)) return(data.frame(RINPERSOONS = character(0), RINPERSOON = character(0)))
    min_cols <- grep("^\\.min_", names(res), value = TRUE)
    core <- if (is.null(delay_items[[age]])) names(items[[age]]) else unlist(delay_items[[age]])
    if (!all(core %in% names(items[[age]]))) {
      stop("language_delay items for ", age, " not in wiechen_items: ",
           paste(setdiff(core, names(items[[age]])), collapse = ", "))
    }
    m <- as.matrix(res[, paste0(".min_", core), drop = FALSE])
    n_scored <- rowSums(!is.na(m))
    enough <- if (isTRUE(delay_complete)) n_scored == length(core) else n_scored > 0
    res[[paste0("y_", age, "_language_delay")]] <-
      ifelse(enough, as.integer(rowSums(m == 1, na.rm = TRUE) > 0), NA_integer_)
    res[, setdiff(names(res), min_cols)]
  })
  join_children(out)
}


# ---- SDQ ----------------------------------------------------------------------------------------

#' y_m<age>_sdq<cutoff> = SDQ total score >= cutoff. Scores above `max_score`
#' (impossible; the range is 0-40) are treated as missing before choosing the
#' measurement.
sdq_outcomes <- function(rows, ages, target_days, cutoff = 11, max_score = 40) {
  r <- rows[, c(KEYS_JGZ, "cm", "leeftijd")]
  r$sdq <- suppressWarnings(as.numeric(as.character(haven::zap_labels(rows$sdq_t))))
  n_bad <- sum(!is.na(r$sdq) & (r$sdq < 0 | r$sdq > max_score))
  if (n_bad > 0) message(sprintf("SDQ: %d score(s) outside 0-%s set to missing", n_bad, max_score))
  r$sdq[!is.na(r$sdq) & (r$sdq < 0 | r$sdq > max_score)] <- NA
  out <- lapply(names(ages), function(age) {
    d <- closest_measurement(r, "sdq", ages[[age]], target_days[[age]])
    res <- d[, KEYS_JGZ]
    res[[paste0("y_", age, "_sdq", cutoff)]] <- as.integer(d$value >= cutoff)
    res
  })
  join_children(out)
}


# ---- Visus ---------------------------------------------------------------------------------------

#' y_m<age>_visus = 1 when the conclusion of the vision test (visus_c) is in
#' `abnormal`, 0 when it is in `normal`, missing otherwise. Codes are compared
#' as lower-case text, so both the code ("3") and the label ("onvoldoende") work.
visus_outcomes <- function(rows, ages, target_days, abnormal = c("3", "onvoldoende"),
                           normal = c("1", "voldoende")) {
  r <- rows[, c(KEYS_JGZ, "cm", "leeftijd")]
  v <- tolower(trimws(as.character(haven::zap_labels(rows$visus_c))))
  r$visus <- ifelse(v %in% tolower(abnormal), 1L, ifelse(v %in% tolower(normal), 0L, NA_integer_))
  out <- lapply(names(ages), function(age) {
    d <- closest_measurement(r, "visus", ages[[age]], target_days[[age]])
    res <- d[, KEYS_JGZ]
    res[[paste0("y_", age, "_visus")]] <- d$value
    res
  })
  join_children(out)
}
