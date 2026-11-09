## sync_grants(): registry-driven GRANT statements (offline; run_sql is stubbed)

capture_grants <- function(registry) {
  sent <- character()
  with_stubs(
    list(run_sql = function(channel, sql, what = "SQL statement") {
      sent <<- c(sent, sql)
      invisible(TRUE)
    }),
    suppressMessages(sync_grants("fake", registry))
  )
  sent
}
need_sync_grants <- function() {
  if (!exists("sync_grants", envir = globalenv())) skip("sync_grants() is not defined in oracle_helpers.R")
}
reg1 <- function(...) {
  list(taxonomic_classification = list(schema = "GAP_TAXONOMY", grants = list(...)))
}

test_that("a grant WITH GRANT OPTION produces the exact statement", {
  need_sync_grants()
  sql <- capture_grants(reg1(list(privilege = "SELECT", to = "GAP_PRODUCTS", with_grant_option = TRUE)))
  expect_equal(sql, "GRANT SELECT ON GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION TO GAP_PRODUCTS WITH GRANT OPTION")
})

test_that("without the option there is no WITH GRANT OPTION suffix", {
  need_sync_grants()
  sql <- capture_grants(reg1(list(privilege = "SELECT", to = "GAP_PRODUCTS")))
  expect_equal(sql, "GRANT SELECT ON GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION TO GAP_PRODUCTS")
})

test_that("tables without grants send nothing; multiple grants are sent in order", {
  need_sync_grants()
  expect_length(capture_grants(list(taxon_group_stanzas = list(schema = "GAP_TAXONOMY"))), 0)
  sql <- capture_grants(reg1(
    list(privilege = "SELECT", to = "GAP_PRODUCTS"),
    list(privilege = "SELECT", to = "OTHER_USER")
  ))
  expect_length(sql, 2)
  expect_match(sql[1], "TO GAP_PRODUCTS$")
  expect_match(sql[2], "TO OTHER_USER$")
})

test_that("a malformed grantee is rejected before any SQL is sent", {
  need_sync_grants()
  sent <- character()
  expect_error(
    with_stubs(
      list(run_sql = function(channel, sql, what = "") sent <<- c(sent, sql)),
      sync_grants("fake", reg1(list(privilege = "SELECT", to = "x; DROP TABLE y")))
    )
  )
  expect_length(sent, 0)
})

test_that("config.R grants the classification table to GAP_PRODUCTS with grant option (if configured)", {
  g <- tax_registry$taxonomic_classification$grants
  if (is.null(g)) skip("no grants configured on taxonomic_classification in config.R")
  expect_true(any(vapply(g, function(x) {
    identical(toupper(x$to), "GAP_PRODUCTS") && identical(toupper(x$privilege), "SELECT") &&
      isTRUE(x$with_grant_option)
  }, logical(1))))
})