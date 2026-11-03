## prepare_delta() and clean_for_oracle(): the heart of "what changes in Oracle"

test_that("prepare_delta classifies inserts, updates and deletes", {
  current <- data.frame(id = 1:3, label = c("a", "b", "c"), stringsAsFactors = FALSE)
  new <- data.frame(id = c(2, 3, 4), label = c("b", "C", "d"), stringsAsFactors = FALSE)
  d <- prepare_delta(new, current, "id")
  expect_setequal(paste(d$ID, d$OPERATION), c("3 UPDATE", "4 INSERT", "1 DELETE"))
})

test_that("identical data gives an empty delta", {
  df <- data.frame(id = 1:3, label = c("a", "b", "c"), stringsAsFactors = FALSE)
  expect_equal(nrow(prepare_delta(df, df, "id")), 0)
})

test_that("integer vs double does not create phantom updates", {
  current <- data.frame(id = 1:2, n = c(1L, 2L))
  new <- data.frame(id = c(1, 2), n = c(1, 2))
  expect_equal(nrow(prepare_delta(new, current, "id")), 0)
})

test_that("NA key values match NA (a change becomes an UPDATE, not DELETE + INSERT)", {
  current <- data.frame(code = 1, db = "WORMS", dbid = NA_real_, active = 1)
  new <- data.frame(code = 1, db = "WORMS", dbid = NA_real_, active = 0)
  d <- prepare_delta(new, current, c("code", "db", "dbid"))
  expect_equal(d$OPERATION, "UPDATE")
  expect_equal(d$ACTIVE, 0)
})

test_that("empty string vs NA and stray whitespace are not changes (Oracle stores '' as NULL)", {
  current <- data.frame(id = 1:2, label = c(NA_character_, "a"), stringsAsFactors = FALSE)
  new <- data.frame(id = 1:2, label = c("", " a "), stringsAsFactors = FALSE)
  expect_equal(nrow(prepare_delta(new, current, "id")), 0)
})

test_that("factors are compared as character; column names are upper-cased", {
  current <- data.frame(id = 1, label = "a", stringsAsFactors = FALSE)
  new <- data.frame(id = 1, label = factor("a"))
  d <- prepare_delta(new, current, "id")
  expect_equal(nrow(d), 0)
  expect_true(all(names(clean_for_oracle(new)) == toupper(names(new))))
})

test_that("duplicate keys are rejected (MERGE would be ambiguous)", {
  current <- data.frame(id = 1:2, label = c("a", "b"), stringsAsFactors = FALSE)
  new <- data.frame(id = c(1, 1), label = c("a", "x"), stringsAsFactors = FALSE)
  expect_error(utils::capture.output(prepare_delta(new, current, "id")), "Duplicate id_keys")
  expect_error(utils::capture.output(prepare_delta(current, new, "id")), "Duplicate id_keys")
})

test_that("missing key / missing current columns are rejected", {
  expect_error(
    prepare_delta(data.frame(label = "a"), data.frame(id = 1, label = "a"), "id"),
    "missing from new data"
  )
  expect_error(
    prepare_delta(data.frame(id = 1, extra = 1), data.frame(id = 1), "id"),
    "Current table is missing column"
  )
})

test_that("insert_only returns only new keys: no updates, no deletes", {
  current <- data.frame(id = 1:2, label = c("a", "b"), stringsAsFactors = FALSE)
  new <- data.frame(id = c(2, 3), label = c("CHANGED", "c"), stringsAsFactors = FALSE)
  d <- prepare_delta(new, current, "id", mode = "insert_only")
  expect_equal(d$ID, 3)
  expect_true(all(d$OPERATION == "INSERT"))
})

test_that("clean_for_oracle leaves only ASCII in character columns", {
  out <- clean_for_oracle(data.frame(x = "Caf\u00e9 \u00f1", stringsAsFactors = FALSE))
  expect_false(grepl("[^ -~]", out$X))
})
