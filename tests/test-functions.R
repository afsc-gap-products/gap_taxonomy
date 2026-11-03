## Pure helpers in functions.R (no network, no database)

test_that("standardize_names cleans survey names for taxonomic lookups", {
  expect_true(is.na(standardize_names(NA_character_)))
  expect_equal(standardize_names("Gadus chalcogrammus"), "Gadus chalcogrammus")
  expect_equal(standardize_names("Sebastes sp. A"), "Sebastes sp.")
  expect_equal(standardize_names("Chionoecetes hybrid"), "Chionoecetes")
  expect_equal(standardize_names("Pollachius cf. virens"), "Pollachius virens")
})

test_that("fix_names keeps qualifiers from the survey name", {
  expect_equal(fix_names("Gadus sp.", NA_character_), "Gadus sp.")
  expect_equal(fix_names("Chionoecetes hybrid", "Chionoecetes opilio"), "Chionoecetes hybrid")
  expect_equal(fix_names("Liparis sp. A", "Liparis"), "Liparis sp. A")
})

test_that("assign_active_status always marks exactly one row, preferring WORMS with an id", {
  one <- function(db, id) assign_active_status(db, id)
  expect_equal(one(c("ITIS", "WORMS"), c(1, 2)), c(0, 1))
  expect_equal(one(c("WORMS", "ITIS"), c(NA, 2)), c(1, 0)) # WORMS without id beats ITIS
  expect_equal(one(c("WORMS", "WORMS"), c(NA, 5)), c(0, 1)) # WORMS with id beats WORMS without
  expect_equal(one(c("ITIS", "ITIS"), c(1, 2)), c(1, 0)) # fallback: first row
  expect_equal(one("ITIS", 1), 1)
  for (db in list(c("ITIS", "WORMS", "WORMS"), c(NA_character_, "ITIS"), "WORMS")) {
    expect_equal(sum(one(db, seq_along(db))), 1)
  }
})

test_that("assign_active_status needs UPPER-case database names (regression: 'worms' was never preferred)", {
  expect_equal(assign_active_status(c("ITIS", "worms"), c(1, 2)), c(1, 0)) # lower-case is NOT recognised
  expect_equal(assign_active_status(c("ITIS", "WORMS"), c(1, 2)), c(0, 1))
})

test_that("fix_syn_codes resolves synonymized codes", {
  single <- tibble::tibble(n_codes = 1, species_code = 10, survey_name = "A b")
  expect_equal(fix_syn_codes(single, "A b"), 10)

  multi <- tibble::tibble(
    n_codes = 2, species_code = c(1, 2),
    survey_name = c("Gadus morhua", "Gadus callarias")
  )
  expect_equal(fix_syn_codes(multi, "Gadus morhua"), 1)
  expect_true(is.na(fix_syn_codes(multi, NA_character_)))

  undescribed <- tibble::tibble(n_codes = 2, species_code = c(1, 2), survey_name = c("Foo sp. A", "Foo sp. B"))
  expect_true(is.na(fix_syn_codes(undescribed, "Foo")))
})

test_that("assign_reason_action labels new codes, synonyms and nomen nudum", {
  new_spp <- tibble::tibble(species_name = "Foo bar")

  added <- assign_reason_action(
    "x", new_spp, "accepted", "Foo bar", "Foo bar", 1, "c", 1, 1, 1
  )
  expect_equal(added$reason, "species code added")
  expect_equal(added$action, "add taxon code")

  syn <- assign_reason_action(
    "x", new_spp, "accepted", "Old name", "New name", 1, "c", 1, 1, 1
  )
  expect_equal(syn$reason, "synonym")
  expect_equal(syn$action, "change taxon name")

  nn <- assign_reason_action(
    "x", new_spp, "accepted", "Weirdus nudus", "Weirdus nudus", c(NA, NA), "c", 1, 1, 1
  )
  expect_equal(nn$reason, "nomen nudum")
  expect_equal(nn$action, "delete taxon code")

  unaccepted <- assign_reason_action(
    "x", new_spp, "unaccepted", "Old name", "New name", 1, "c", 1, 1, 1
  )
  expect_equal(unaccepted$reason, "unaccepted")
})
