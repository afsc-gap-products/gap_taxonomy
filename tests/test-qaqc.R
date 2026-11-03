## run_taxonomy_qaqc(): each check must pass on clean data and fail on its own defect

good_tax <- function() {
  tibble::tibble(
    species_taxonomic_id = c(1, 2, 3),
    taxonomic_rank_id    = c(10, 100, 200),
    name                 = c("Animalia", "Gadus", "morhua"),
    parent_taxonomic_id  = c(NA, 1, 2)
  )
}
good_codes <- function() {
  tibble::tibble(
    species_code = c(100, 101), species_name = c("Gadus morhua", "Unid"),
    species_taxonomic_id = c(3, NA), active = c("Y", "N")
  )
}
qaqc <- function(tax, codes) {
  warns <- character()
  res <- NULL
  withCallingHandlers(
    suppressMessages(utils::capture.output(res <- run_taxonomy_qaqc(tax, codes))),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(passed = res, warnings = warns)
}
expect_fails_check <- function(r, id) {
  expect_false(r$passed)
  expect_true(any(grepl(sprintf("CHECK %d FAILED", id), r$warnings)), info = paste("check", id))
}

test_that("clean data passes every check", {
  r <- qaqc(good_tax(), good_codes())
  expect_true(r$passed)
  expect_length(r$warnings, 0)
})

test_that("check 1: parent rank must be strictly higher than child rank", {
  t <- good_tax()
  t$taxonomic_rank_id[3] <- 100
  expect_fails_check(qaqc(t, good_codes()), 1)
})

test_that("check 2: orphaned parent ids", {
  t <- good_tax()
  t$parent_taxonomic_id[3] <- 99
  expect_fails_check(qaqc(t, good_codes()), 2)
})

test_that("check 3: self-referencing node", {
  t <- good_tax()
  t$parent_taxonomic_id[3] <- 3
  expect_fails_check(qaqc(t, good_codes()), 3)
})

test_that("check 4: duplicate primary keys in either table", {
  expect_fails_check(qaqc(dplyr::bind_rows(good_tax(), good_tax()[3, ]), good_codes()), 4)
  expect_fails_check(qaqc(good_tax(), dplyr::bind_rows(good_codes(), good_codes()[1, ])), 4)
})

test_that("check 5: race_species_codes pointing at a missing taxonomic id", {
  c2 <- good_codes()
  c2$species_taxonomic_id[1] <- 999
  expect_fails_check(qaqc(good_tax(), c2), 5)
})

test_that("check 6: active must be Y or N", {
  c2 <- good_codes()
  c2$active[1] <- "X"
  expect_fails_check(qaqc(good_tax(), c2), 6)
  c3 <- good_codes()
  c3$active[2] <- NA
  expect_fails_check(qaqc(good_tax(), c3), 6)
})

test_that("check 7: lower-rank taxa detached from the tree", {
  t <- good_tax()
  t$parent_taxonomic_id[2] <- NA
  expect_fails_check(qaqc(t, good_codes()), 7)
})
