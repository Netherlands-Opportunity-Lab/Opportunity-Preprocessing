# =============================================================================
# JGZ catchment sample -- 03. Bereik (take-up) outcomes
#
# Turns the catchment sample of 01 (Coen's dataset, one row per child living
# in an organisation's catchment area at 2y, 3y and 3y9m) into a cohort file
# with the bereik outcomes under the JGZ names (27-10-2026: per contact moment,
# plus "always" and "at least once", all with the same denominator):
#
#   y_m24_takeup, y_m36_takeup, y_m45_takeup   seen by the JGZ at 2y / 3y / 3y9m
#   y_m45_takeup_always                        seen at all three
#   y_m45_takeup_any                           seen at least once
#
# "Seen" = any JGZ panel row in the contact-moment window, by any organisation
# (setting catch_observed_same_org). Bereik is about where the child lives
# (org_catchment, postcode_4), not which organisation saw the child.
#
#   Input : the .rds copy of loc$catch_sample_file (output of 01)
#   Output: output/jgz_catchment_cohort.rds, temp/jgz_catchment/03_sample_size.rds
#
# Run from the repository root.
# =============================================================================

jgz_config_file <- if (exists("run_config_file")) run_config_file else "config/jgz_catchment.yml"
source("src/jgz/jgz_helpers.R")

catch <- readRDS(sub("\\.dta$", ".rds", loc$catch_sample_file))

cohort_dat <- catch %>%
  mutate(RINPERSOONS = as_chr(rinpersoons), RINPERSOON = as_chr(rinpersoon)) %>%
  select(RINPERSOONS, RINPERSOON, everything(), -rinpersoons, -rinpersoon)

for (age in names(cfg$takeup_ages)) {
  src_col <- paste0("d_observed_", cfg$takeup_ages[[age]])
  if (!src_col %in% names(cohort_dat)) stop(src_col, " not in the catchment sample")
  cohort_dat[[paste0("y_", age, "_takeup")]] <- as.integer(haven::zap_labels(cohort_dat[[src_col]]))
}
take_cols <- paste0("y_", names(cfg$takeup_ages), "_takeup")
n_seen <- rowSums(as.matrix(cohort_dat[, take_cols]))
last_age <- utils::tail(names(cfg$takeup_ages), 1)
cohort_dat[[paste0("y_", last_age, "_takeup_always")]] <- as.integer(n_seen == length(take_cols))
cohort_dat[[paste0("y_", last_age, "_takeup_any")]]    <- as.integer(n_seen > 0)

# same definition as Coen's d_observed_3cms (ever seen) -- a check, not a choice
stopifnot(all(cohort_dat[[paste0("y_", last_age, "_takeup_any")]] ==
                as.integer(haven::zap_labels(cohort_dat$d_observed_3cms)), na.rm = TRUE))
check_keys_jgz(cohort_dat, c("RINPERSOONS", "RINPERSOON"), "catchment outcomes")

print(round(colMeans(cohort_dat[, grep("^y_m", names(cohort_dat), value = TRUE)], na.rm = TRUE), 3))
print(table(org_catchment = cohort_dat$org_catchment, useNA = "ifany"))

sample_size <- tibble::tibble(n_1_catchment_sample = nrow(cohort_dat))
saveRDS(sample_size, file.path(loc$temp_folder, "03_sample_size.rds"))
saveRDS(cohort_dat, file.path(loc$output_folder, paste0(cfg$cohort_name, "_cohort.rds")))
message("Written: ", file.path(loc$output_folder, paste0(cfg$cohort_name, "_cohort.rds")))
