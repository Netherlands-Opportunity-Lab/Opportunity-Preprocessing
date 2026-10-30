# Run the tests from the repository root:  Rscript tests/run_tests.R
# They need no CBS data, so they also run outside the RA. Run them before every
# commit that touches src/ or config/.
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
