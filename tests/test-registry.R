## config.R: tax_registry structure and resolve_registry()

test_that("every registry entry has the fields the pipeline relies on", {
  for (nm in names(tax_registry)) {
    sp <- tax_registry[[nm]]
    info <- paste("registry entry:", nm)
    expect_true(sp$mode %in% c("upsert", "insert_only"), info = info)
    expect_true(is.character(sp$id_keys) && length(sp$id_keys) > 0, info = info)
    expect_true(is.character(sp$audit_keys) && length(sp$audit_keys) > 0, info = info)
    expect_true(is.character(sp$new) && length(sp$new) == 1, info = info)
  }
  expect_false(anyDuplicated(vapply(tax_registry, function(s) s$new, character(1))) > 0)
})

test_that("parents come before children (merge order; deletes run in reverse)", {
  pos <- function(x) which(names(tax_registry) == x)
  expect_lt(pos("taxon_group_stanzas"), pos("taxon_group_composition"))
  expect_lt(pos("species_taxonomics"), pos("race_species_codes"))
})

test_that("resolve_registry drops RACE_DATA tables unless enabled", {
  off <- resolve_registry(tax_registry, list(include_race_data = FALSE, schema = "GAP_TAXONOMY"))
  on <- resolve_registry(tax_registry, list(include_race_data = TRUE, schema = "GAP_TAXONOMY"))
  expect_false(any(c("species_taxonomics", "race_species_codes") %in% names(off)))
  expect_true(all(c("species_taxonomics", "race_species_codes") %in% names(on)))
  expect_equal(length(on) - length(off), 2)
})

test_that("resolve_registry fills upper-case schema defaults", {
  on <- resolve_registry(tax_registry, list(include_race_data = TRUE, schema = "gap_taxonomy"))
  expect_equal(on$taxonomic_changes$schema, "GAP_TAXONOMY")
  expect_equal(on$taxonomic_changes$stage_schema, "GAP_TAXONOMY")
  expect_equal(on$taxonomic_changes$audit_schema, "GAP_TAXONOMY")
  expect_equal(on$species_taxonomics$schema, "RACE_DATA")
})

test_that("RACE_DATA tables are gated by QA/QC and can never delete", {
  for (nm in c("species_taxonomics", "race_species_codes")) {
    expect_equal(tax_registry[[nm]]$gate, "race_qaqc_passed")
    expect_true(isTRUE(tax_registry[[nm]]$no_deletes))
  }
})

test_that("QA/QC gate stops a RACE_DATA update when the flag is FALSE or missing", {
  reg <- resolve_registry(tax_registry, list(include_race_data = TRUE, schema = "GAP_TAXONOMY"))
  e <- new.env(parent = emptyenv()) # no inheritance: a flag in your global env must not leak in
  assign("race_qaqc_passed", FALSE, envir = e)
  # the gate is evaluated before any Oracle call, so a NULL channel is never touched
  expect_error(
    build_stage_payloads(NULL, reg["species_taxonomics"], envir = e),
    "QA/QC gate 'race_qaqc_passed' is missing or FALSE"
  )
  rm("race_qaqc_passed", envir = e)
  expect_error(
    build_stage_payloads(NULL, reg["species_taxonomics"], envir = e),
    "QA/QC gate"
  )
})
