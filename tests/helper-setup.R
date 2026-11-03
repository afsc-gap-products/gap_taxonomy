## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## helper-setup.R -- loaded automatically before the tests
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(purrr)
})

## The pipeline utilities are sourced into the GLOBAL environment (that is where
## the pipeline itself defines them), but only if they are not there already, so
## a live session (e.g. a cfg you have modified) is never overwritten.
if (!exists("tax_registry", envir = globalenv())) {
  source(here::here("code", "utils", "config.R"), local = FALSE)
}
if (!exists("standardize_names", envir = globalenv())) {
  source(here::here("code", "utils", "functions.R"), local = FALSE)
}
if (!exists("prepare_delta", envir = globalenv())) {
  source(here::here("code", "utils", "oracle_helpers.R"), local = FALSE)
}

## ---- stubbing -------------------------------------------------------------
## Temporarily replace global functions (used to test SQL generation offline).
with_stubs <- function(stubs, code) {
  old <- mget(names(stubs), envir = globalenv(), inherits = FALSE)
  list2env(stubs, envir = globalenv())
  on.exit(list2env(old, envir = globalenv()), add = TRUE)
  force(code)
}

## ---- Oracle test plumbing -------------------------------------------------
skip_if_no_oracle <- function() {
  if (!identical(tolower(Sys.getenv("GAP_TAX_TEST_ORACLE")), "true")) {
    skip("Oracle tests are off (set GAP_TAX_TEST_ORACLE=true).")
  }
}

#' Scratch schema for tests that CREATE/DROP tables and triggers.
oracle_test_schema <- function() {
  s <- toupper(Sys.getenv("GAP_TAX_TEST_SCHEMA"))
  if (!nzchar(s)) skip("Set GAP_TAX_TEST_SCHEMA to a scratch schema you can create tables/triggers in.")
  if (s %in% c("GAP_TAXONOMY", "RACE_DATA", "GAP_PRODUCTS")) {
    stop("Refusing to run scratch tests in production schema ", s)
  }
  assert_schema(s)
}

.tc <- new.env()
test_channel <- function() {
  ch <- if (!is.null(.tc$ch)) .tc$ch else get0("channel", envir = globalenv())
  .tc$ch <- oracle_connect(ch)
  .tc$ch
}

ora_q <- function(channel, sql) {
  res <- RODBC::sqlQuery(channel, sql, stringsAsFactors = FALSE)
  if (!is.data.frame(res)) stop("Query failed:\n", paste(res, collapse = "\n"), "\n", sql)
  res
}

sql_lit <- function(x) {
  if (is.na(x)) {
    "NULL"
  } else if (is.numeric(x)) {
    format(x, scientific = FALSE, trim = TRUE)
  } else {
    sprintf("'%s'", gsub("'", "''", as.character(x)))
  }
}

ora_insert <- function(channel, schema, table, df) {
  for (i in seq_len(nrow(df))) {
    vals <- vapply(df, function(col) sql_lit(col[[i]]), character(1))
    run_sql(channel, sprintf(
      "INSERT INTO %s.%s (%s) VALUES (%s)",
      schema, table, paste(names(df), collapse = ", "), paste(vals, collapse = ", ")
    ), "test insert")
  }
  invisible(TRUE)
}

#' Generate a unique tag for scratch objects and drop everything carrying it
#' (tables, AUDIT_ tables, STAGE_ tables, backups) when the calling test ends.
local_scratch <- function(channel, schema, env = parent.frame()) {
  tag <- paste0("ZZT", paste(sample(LETTERS, 5), collapse = ""))
  withr::defer(drop_scratch(channel, schema, tag), envir = env)
  tag
}

drop_scratch <- function(channel, schema, tag) {
  nm <- try(ora_q(channel, sprintf(
    "SELECT table_name FROM all_tables WHERE owner = '%1$s'
       AND (table_name LIKE '%2$s%%' OR table_name LIKE 'AUDIT_%2$s%%' OR table_name LIKE 'STAGE_%2$s%%')",
    schema, tag
  )), silent = TRUE)
  if (inherits(nm, "try-error")) {
    return(invisible(FALSE))
  }
  for (t in nm$TABLE_NAME) {
    try(RODBC::sqlQuery(channel, sprintf("DROP TABLE %s.%s CASCADE CONSTRAINTS PURGE", schema, t)), silent = TRUE)
  }
  invisible(TRUE)
}

#' Rows the audit tables should contain after a run, computed from the payloads.
expected_audit_counts <- function(payloads) {
  purrr::map_dfr(payloads, function(p) {
    ops <- table(p$data$OPERATION)
    if (length(ops) == 0) {
      return(NULL)
    }
    tibble::tibble(
      TABLE_NAME = toupper(p$table),
      OPERATION_TYPE = names(ops),
      NUMBER_RECS = as.integer(ops)
    )
  })
}
