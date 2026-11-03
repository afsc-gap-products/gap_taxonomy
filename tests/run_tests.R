## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## run_tests.R -- test runner for the GAP_TAXONOMY pipeline
##
##   source("tests/run_tests.R")
##
##   run_gap_tax_tests()                                  # 1. offline unit tests (no database)
##   run_gap_tax_tests(oracle = TRUE,                     # 2. + Oracle integration tests:
##                     scratch_schema = "MY_USER")        #    scratch-schema audit/merge tests
##                                                        #    + read-only production audit checks
##   run_gap_tax_tests(contract = TRUE)                   # 3. data contract tests on the objects
##                                                        #    created by steps 01-04 in THIS session
##
## Scratch tests create tables named ZZT<random>_* in `scratch_schema` and drop
## them afterwards. They refuse to run in GAP_TAXONOMY / RACE_DATA / GAP_PRODUCTS.
## The production audit checks are read-only.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(here)
library(testthat)

run_gap_tax_tests <- function(oracle = FALSE, scratch_schema = NULL, contract = FALSE) {
  dir <- here::here("tests")
  filters <- "^(delta|sql-builders|registry|functions|qaqc|audit-ddl)$"
  if (oracle) filters <- c(filters, "^(audit-scratch-oracle|audit-production)$")
  if (contract) filters <- c(filters, "^pipeline-outputs$")

  if (oracle) {
    Sys.setenv(GAP_TAX_TEST_ORACLE = "true")
    if (!is.null(scratch_schema)) Sys.setenv(GAP_TAX_TEST_SCHEMA = scratch_schema)
  }
  testthat::test_dir(dir,
    filter = paste(filters, collapse = "|"),
    reporter = "progress", stop_on_failure = FALSE
  )
}
