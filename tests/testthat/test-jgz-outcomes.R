# JGZ outcomes (src/lib/P_2_1_jgz_outcomes.R), used by src/jgz/03_outcomes_jgz.R.
# Toy contact-moment rows, no CBS data needed.
root <- file.path("..", "..")
suppressPackageStartupMessages(library(dplyr))
source(file.path(root, "src", "lib", "P_2_1_jgz_outcomes.R"))

target <- list(m24 = 730, m36 = 1095, m45 = 1368, m60 = 2190)

# one row per child x age in days, as the cm_<visit> files of 01c
row <- function(rin, cm, leeftijd, ...) {
  data.frame(RINPERSOONS = "R", RINPERSOON = rin, cm = cm, leeftijd = leeftijd,
             sex = "boy", pc3 = 101, pc4 = 1011, org = 7, ..., stringsAsFactors = FALSE)
}

test_that("closest_measurement takes the non-missing value closest to the nominal age", {
  rows <- bind_rows(
    row("a", "18months", 560, BMI = 16),
    row("a", "2years", 700, BMI = 17),
    row("a", "2years", 770, BMI = 18),        # 40 days off: 700 is 30 days off
    row("b", "2years", 760, BMI = NA),        # missing value is skipped
    row("b", "18months", 600, BMI = 15),
    row("c", "2years", 700, BMI = 20),
    row("c", "2years", 760, BMI = 21))        # tie (30 days): the older one
  out <- closest_measurement(rows, "BMI", c("18months", "2years"), 730)
  expect_equal(out$value[out$RINPERSOON == "a"], 17)
  expect_equal(out$value[out$RINPERSOON == "b"], 15)
  expect_equal(out$value[out$RINPERSOON == "c"], 21)
  # only the listed contact moments
  out2 <- closest_measurement(rows, "BMI", "2years", 730)
  expect_false("b" %in% out2$RINPERSOON)
})

test_that("bmi_outcomes: p90/p10 flags over all children at that age, outliers dropped", {
  set.seed(1)
  rows <- row(paste0("k", 1:200), "2years", 730, BMI = c(seq(14, 19, length.out = 199), 60))
  rows$sex <- rep(c("boy", "girl"), 100)
  out <- bmi_outcomes(rows, list(m24 = "2years"), target, z_max = 5, by_sex = FALSE)
  expect_true(all(c("y_m24_bmi", "y_m24_bmi_p90", "y_m24_bmi_p10") %in% names(out)))
  expect_true(is.na(out$y_m24_bmi[out$RINPERSOON == "k200"]))        # the 60 is an outlier
  expect_equal(mean(out$y_m24_bmi_p90, na.rm = TRUE), 0.1, tolerance = 0.01)
  expect_equal(mean(out$y_m24_bmi_p10, na.rm = TRUE), 0.1, tolerance = 0.01)
  by_sex <- bmi_outcomes(rows, list(m24 = "2years"), target, by_sex = TRUE)
  expect_equal(nrow(by_sex), 200)
})

test_that("wiechen_outcomes: _can = + or M, _notseen = - or M, no-M items, language delay", {
  rows <- bind_rows(
    row("a", "2years", 730, zin2w = "1", pop6 = "3"),   # + and M
    row("b", "2years", 730, zin2w = "2", pop6 = "1"),   # - and +
    row("c", "18months", 550, zin2w = NA, pop6 = "2"),  # only pop6 (-), at 18 months
    row("d", "2years", 730, zin2w = ".r", pop6 = NA),   # removed conflict: missing
    row("e", "3years9m", 1368, zin2w = NA, pop6 = NA, verstaan2 = "1", vragen1 = "3"))
  out <- wiechen_outcomes(rows,
                          ages = list(m24 = c("18months", "2years"), m45 = c("3years", "3years9m")),
                          items = list(m24 = list(words2 = "zin2w", bodyparts = "pop6"),
                                       m45 = list(questions = "vragen1", intelligible = "verstaan2")),
                          target_days = target, no_m = "intelligible")
  get <- function(rin, col) out[[col]][out$RINPERSOON == rin]
  expect_equal(get("a", "y_m24_words2_can"), 1L);     expect_equal(get("a", "y_m24_words2_notseen"), 0L)
  expect_equal(get("a", "y_m24_bodyparts_can"), 1L);  expect_equal(get("a", "y_m24_bodyparts_notseen"), 1L)
  expect_equal(get("b", "y_m24_words2_can"), 0L);     expect_equal(get("b", "y_m24_language_delay"), 1L)
  expect_equal(get("a", "y_m24_language_delay"), 0L)
  expect_true(is.na(get("c", "y_m24_words2_can")));   expect_equal(get("c", "y_m24_language_delay"), 1L)
  expect_false("d" %in% out$RINPERSOON)               # no valid item at all
  expect_equal(get("e", "y_m45_intelligible"), 1L)
  expect_false("y_m45_intelligible_can" %in% names(out))
  expect_equal(get("e", "y_m45_questions_can"), 1L);  expect_equal(get("e", "y_m45_questions_notseen"), 1L)
})

test_that("language_delay: only core items, and only when all core items are scored", {
  rows <- bind_rows(
    row("a", "2years", 730, zin2w = "1", pop6 = "1", noemt = "2"),   # extra item -: not core
    row("b", "2years", 730, zin2w = "2", pop6 = "1", noemt = "1"),   # core item -
    row("c", "2years", 730, zin2w = "2", pop6 = NA,  noemt = "1"),   # core item missing
    row("d", "2years", 730, zin2w = "3", pop6 = "1", noemt = "1"))   # M is not a delay
  args <- list(rows = rows, ages = list(m24 = "2years"),
               items = list(m24 = list(words2 = "zin2w", bodyparts = "pop6", me_i = "noemt")),
               target_days = target, delay_items = list(m24 = c("words2", "bodyparts")))
  complete <- do.call(wiechen_outcomes, c(args, delay_complete = TRUE))
  get <- function(out, rin) out$y_m24_language_delay[out$RINPERSOON == rin]
  expect_equal(get(complete, "a"), 0L)
  expect_equal(get(complete, "b"), 1L)
  expect_true(is.na(get(complete, "c")))
  expect_equal(get(complete, "d"), 0L)
  any_scored <- do.call(wiechen_outcomes, c(args, delay_complete = FALSE))
  expect_equal(get(any_scored, "c"), 1L)
  expect_error(wiechen_outcomes(rows, list(m24 = "2years"), list(m24 = list(words2 = "zin2w")), target,
                                delay_items = list(m24 = "nope")), "not in wiechen_items")
})

test_that("sdq_outcomes: score >= cut-off, impossible scores dropped", {
  rows <- bind_rows(row("a", "groep2", 2190, sdq_t = 11), row("b", "groep2", 2190, sdq_t = 10),
                    row("c", "groep2", 2190, sdq_t = 117), row("c", "groep2", 2300, sdq_t = 12))
  out <- sdq_outcomes(rows, list(m60 = "groep2"), target, cutoff = 11, max_score = 40)
  expect_equal(out$y_m60_sdq11[order(out$RINPERSOON)], c(1L, 0L, 1L))   # c: 117 ignored, 12 used
  expect_false("y_m60_sdq" %in% names(out))
  with_score <- sdq_outcomes(rows, list(m60 = "groep2"), target, score_ages = "m60")
  expect_equal(with_score$y_m60_sdq[order(with_score$RINPERSOON)], c(11, 10, 12))
})

test_that("visus_outcomes: codes and labels, unknown values missing", {
  rows <- bind_rows(row("a", "3years", 1095, visus_c = "3"), row("b", "3years", 1095, visus_c = "voldoende"),
                    row("c", "3years", 1095, visus_c = "2"), row("d", "3years", 1095, visus_c = "9"))
  out <- visus_outcomes(rows, list(m36 = "3years"), target,
                        abnormal = c("3", "onvoldoende"), normal = c("1", "2", "voldoende", "twijfelachtig"))
  expect_equal(out$y_m36_visus[order(out$RINPERSOON)], c(1L, 0L, 0L))
  expect_false("d" %in% out$RINPERSOON)
})

test_that("age_geography: postcode and organisation of the contact closest to the nominal age", {
  rows <- bind_rows(row("a", "18months", 550), row("a", "2years", 740))
  rows$pc4 <- c(1011, 2022); rows$org <- c(6, 7)
  out <- age_geography(rows, list(m24 = c("18months", "2years")), target)
  expect_equal(out$pc4_m24, 2022); expect_equal(out$org_m24, 7)
})

test_that("config/jgz.yml lists outcomes the functions can make", {
  cfg <- config::get("data_preparation", file = file.path(root, "config", "jgz.yml"))
  ages <- unique(c(names(cfg$bmi_ages), names(cfg$wiechen_ages), names(cfg$sdq_ages), names(cfg$visus_ages)))
  expect_true(all(ages %in% names(cfg$outcome_target_days)))
  expect_setequal(names(cfg$wiechen_items), names(cfg$wiechen_ages))
  for (a in names(cfg$language_delay_items)) {
    expect_true(all(unlist(cfg$language_delay_items[[a]]) %in% names(cfg$wiechen_items[[a]])), info = a)
  }
  checks <- config::get(file = file.path(root, "config", "jgz.yml"))$final_checks
  y_expected <- grep("^y_", checks$expected_columns, value = TRUE)
  made <- c(as.vector(outer(names(cfg$bmi_ages), c("bmi", "bmi_p90", "bmi_p10"), function(a, o) paste0("y_", a, "_", o))),
            unlist(lapply(names(cfg$wiechen_items), function(a) {
              its <- names(cfg$wiechen_items[[a]])
              c(paste0("y_", a, "_language_delay"),
                unlist(lapply(its, function(i) if (i %in% unlist(cfg$wiechen_no_m)) paste0("y_", a, "_", i)
                              else paste0("y_", a, "_", i, c("_can", "_notseen")))))
            })),
            paste0("y_", names(cfg$sdq_ages), "_sdq", cfg$sdq_cutoff),
            paste0("y_", unlist(cfg$sdq_score_ages), "_sdq"),
            paste0("y_", names(cfg$visus_ages), "_visus"))
  expect_true(all(y_expected %in% made), info = paste(setdiff(y_expected, made), collapse = ", "))
})
