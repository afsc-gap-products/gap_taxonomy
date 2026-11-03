## Data CONTRACT tests: invariants the objects created by steps 01-04 must satisfy
## BEFORE anything is sent to Oracle. Run them in the same R session after
## sourcing 01-04 (they look the objects up in the global environment and skip
## anything that does not exist yet):
##
##   source("tests/run_tests.R"); run_gap_tax_tests(contract = TRUE)

g <- function(x) get(x, envir = globalenv())
need <- function(...) {
  nm <- c(...)
  ok <- vapply(nm, function(x) exists(x, envir = globalenv(), inherits = FALSE), logical(1))
  if (!all(ok)) skip(paste("Run steps 01-04 first; missing:", paste(nm[!ok], collapse = ", ")))
}
na_eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)

# ---- 01: taxonomic_classification_new ---------------------------------------
test_that("classification: keys are unique and required columns exist", {
  need("taxonomic_classification_new")
  x <- g("taxonomic_classification_new")
  expect_true(all(c("species_code", "species_name", "active", "database", "database_id", "id_rank") %in% names(x)))
  expect_false(anyNA(x$species_code))
  expect_equal(nrow(x), nrow(dplyr::distinct(x, species_code, database, database_id)))
})

test_that("classification: exactly one active row per species_code, flags are 0/1", {
  need("taxonomic_classification_new")
  x <- g("taxonomic_classification_new")
  expect_true(all(x$active %in% c(0, 1)))
  n_active <- x %>%
    dplyr::group_by(species_code) %>%
    dplyr::summarise(n = sum(active == 1, na.rm = TRUE), .groups = "drop")
  expect_equal(n_active$species_code[n_active$n != 1], numeric(0))
})


test_that("classification: every taxon is in Animalia", {
  need("taxonomic_classification_new")
  x <- g("taxonomic_classification_new")
  y <- x %>% filter(!species_code %in% c(99997, 99999)) # excluding shab
  expect_true(all(y$kingdom_taxon %in% "Animalia"))
})


test_that("classification: active rows have either a species or common name", {
  need("taxonomic_classification_new")
  x <- g("taxonomic_classification_new")
  
  # Safely subset active rows
  act <- x[which(x$active == 1), ]
  
  # Helper to identify missing or empty strings
  is_blank <- function(v) {
    is.na(v) | !nzchar(trimws(v))
  }
  
  # Find rows where BOTH species and common names are blank
  both_missing <- is_blank(act$species_name) & is_blank(act$common_name)
  
  # Assert that no such rows exist
  expect_false(any(both_missing))
})

test_that("every RACE species code is in the classification or was retired via taxonomic_changes", {
  need("race_species", "taxonomic_classification_new", "taxonomic_changes_new", "taxonomic_changes_current")
  known <- c(
    g("taxonomic_classification_new")$species_code,
    g("taxonomic_changes_new")$old_species_code,
    g("taxonomic_changes_current")$old_species_code
  )
  expect_equal(setdiff(g("race_species")$species_code, known), numeric(0))
})

# ---- 01: taxonomic_changes_new ----------------------------------------------
test_that("changes: no duplicate rows; years are sane", {
  need("taxonomic_changes_new")
  x <- g("taxonomic_changes_new")
  keys <- c("old_species_name", "old_species_code", "new_species_name", "new_species_code")
  expect_equal(nrow(x), nrow(dplyr::distinct(x, dplyr::across(dplyr::all_of(keys)))))
  if (nrow(x) > 0 && exists("this_year", envir = globalenv())) {
    expect_true(all(x$year_changed <= g("this_year"), na.rm = TRUE))
  }
})

test_that("changes: a retired species code is never still an active classification code", {
  need("taxonomic_changes_new", "taxonomic_changes_current", "taxonomic_classification_new")
  changes <- dplyr::bind_rows(g("taxonomic_changes_new"), g("taxonomic_changes_current"))
  retired <- changes$old_species_code[!is.na(changes$old_species_code) &
    !is.na(changes$new_species_code) &
    changes$old_species_code != changes$new_species_code]
  cls <- g("taxonomic_classification_new")
  active_codes <- cls$species_code[cls$active == 1]
  expect_equal(intersect(retired, active_codes), numeric(0))
})

test_that("changes: rows already in Oracle keep their original year_changed", {
  need("taxonomic_changes_new", "taxonomic_changes_current")
  keys <- c("old_species_name", "old_species_code", "new_species_name", "new_species_code")
  j <- dplyr::inner_join(g("taxonomic_changes_new"), g("taxonomic_changes_current"),
    by = keys, suffix = c("_new", "_cur")
  )
  expect_true(all(na_eq(j$year_changed_new, j$year_changed_cur)))
})

# ---- 02: groups --------------------------------------------------------------
test_that("stanzas: ids unique, years ordered, no overlaps within a group_code", {
  need("taxon_group_stanzas_new")
  s <- g("taxon_group_stanzas_new")
  expect_false(anyDuplicated(s$group_stanza_id) > 0)
  expect_false(anyNA(s[c("group_stanza_id", "group_code", "year_start", "year_end")]))
  expect_true(all(s$year_start <= s$year_end))
  overlaps <- s %>%
    dplyr::arrange(group_code, year_start) %>%
    dplyr::group_by(group_code) %>%
    dplyr::filter(year_start <= dplyr::lag(year_end))
  expect_equal(nrow(overlaps), 0)
})

test_that("composition: unique rows, every row has a stanza, every stanza has members", {
  need("taxon_group_stanzas_new", "taxon_group_composition_new")
  s <- g("taxon_group_stanzas_new")
  comp <- g("taxon_group_composition_new")
  expect_equal(nrow(comp), nrow(dplyr::distinct(comp)))
  expect_equal(setdiff(comp$group_stanza_id, s$group_stanza_id), numeric(0)) # FK
  expect_equal(setdiff(s$group_stanza_id, comp$group_stanza_id), numeric(0)) # no empty stanzas
})

test_that("composition species are known to the taxonomy", {
  need("taxon_group_composition_new", "taxonomic_classification_new")
  known <- c(
    g("taxonomic_classification_new")$species_code,
    if (exists("taxonomic_changes_new", envir = globalenv())) g("taxonomic_changes_new")$old_species_code,
    if (exists("taxonomic_changes_current", envir = globalenv())) g("taxonomic_changes_current")$old_species_code,
    if (exists("race_species", envir = globalenv())) g("race_species")$species_code
  )
  expect_equal(setdiff(g("taxon_group_composition_new")$species_code, known), numeric(0))
})

test_that("existing stanzas keep their id, group_code and year_start", {
  need("taxon_group_stanzas_new", "taxon_group_stanzas_current")
  j <- dplyr::inner_join(g("taxon_group_stanzas_current"), g("taxon_group_stanzas_new"),
    by = "group_stanza_id", suffix = c("_cur", "_new")
  )
  expect_true(all(j$group_code_cur == j$group_code_new))
  expect_true(all(j$year_start_cur == j$year_start_new))
  expect_true(all(j$year_end_new >= j$year_end_cur)) # stanzas only grow
})

test_that("current taxon groups are covered by a modern stanza that lists all their species", {
  need("taxon_groups", "taxon_group_stanzas_new", "taxon_group_composition_new", "this_year")
  tg <- dplyr::distinct(g("taxon_groups"), group_code, species_code)
  modern <- g("taxon_group_stanzas_new") %>%
    dplyr::filter(year_end == g("this_year")) %>%
    dplyr::select(group_stanza_id, group_code)
  expect_equal(setdiff(unique(tg$group_code), modern$group_code), numeric(0)) # every group has a modern stanza
  expect_equal(setdiff(modern$group_code, unique(tg$group_code)), numeric(0)) # no obsolete modern stanzas
  gaps <- tg %>%
    dplyr::inner_join(modern, by = "group_code") %>%
    dplyr::anti_join(g("taxon_group_composition_new"), by = c("group_stanza_id", "species_code"))
  expect_equal(nrow(gaps), 0)
})

# ---- 04: confidence ---------------------------------------------------------
test_that("confidence: unique keys, valid codes, labels agree with codes", {
  need("taxonomic_confidence_new")
  x <- g("taxonomic_confidence_new")
  expect_equal(nrow(x), nrow(dplyr::distinct(x, survey_definition_id, year, species_code)))
  expect_true(all(x$taxon_confidence_code %in% c(1, 2, 3, NA)))
  expected_label <- dplyr::case_when(
    x$taxon_confidence_code == 1 ~ "High", x$taxon_confidence_code == 2 ~ "Moderate",
    x$taxon_confidence_code == 3 ~ "Low", TRUE ~ "Unassessed"
  )
  expect_equal(x$taxon_confidence, expected_label)
})

test_that("confidence: only taxon-group species; every survey year is present", {
  need("taxonomic_confidence_new", "taxon_group_stanzas_new", "survey_years", "this_year")
  x <- g("taxonomic_confidence_new")
  expect_equal(setdiff(x$species_code, g("taxon_group_stanzas_new")$group_code), numeric(0))
  sy <- g("survey_years") %>%
    dplyr::filter(year <= g("this_year")) %>%
    dplyr::distinct(survey_definition_id, year)
  have <- dplyr::distinct(x, survey_definition_id, year)
  expect_equal(nrow(dplyr::anti_join(sy, have, by = c("survey_definition_id", "year"))), 0)
})

test_that("confidence: scores already in Oracle are unchanged (unless you rebuilt from the spreadsheets)", {
  need("taxonomic_confidence_new", "taxon_confidence_current")
  j <- dplyr::inner_join(g("taxonomic_confidence_new"), g("taxon_confidence_current"),
    by = c("survey_definition_id", "year", "species_code"), suffix = c("_new", "_cur")
  )
  expect_true(all(na_eq(j$taxon_confidence_code_new, j$taxon_confidence_code_cur)))
})

# ---- 03: RACE_DATA ----------------------------------------------------------
test_that("RACE_DATA QA/QC suite passed", {
  need("race_qaqc_passed")
  expect_true(g("race_qaqc_passed"))
})

test_that("RACE_DATA legacy keys are anchored: no ids, names or foreign keys are lost or changed", {
  need(
    "species_taxonomics_new", "species_taxonomics_current",
    "race_species_codes_new", "race_codes_current"
  )
  tn <- g("species_taxonomics_new")
  tc <- g("species_taxonomics_current")
  expect_equal(setdiff(tc$species_taxonomic_id, tn$species_taxonomic_id), numeric(0)) # no deletes
  j <- dplyr::inner_join(tc, tn, by = "species_taxonomic_id", suffix = c("_cur", "_new"))
  expect_true(all(j$name_cur == j$name_new))

  rn <- g("race_species_codes_new")
  rc <- g("race_codes_current")
  expect_equal(setdiff(rc$species_code, rn$species_code), numeric(0))
  jr <- dplyr::inner_join(rc, rn, by = "species_code", suffix = c("_cur", "_new")) %>%
    dplyr::filter(!is.na(species_taxonomic_id_cur))
  expect_true(all(jr$species_taxonomic_id_cur == jr$species_taxonomic_id_new))
})
