## Generated SQL: MERGE / DELETE / INSERT / CREATE TABLE (no database needed)

p <- list(
  table = "ts", schema = "SCH", stage_schema = "STG",
  id_keys = "ID", cols = c("ID", "LABEL", "ACTIVE")
)
flat <- function(x) gsub("\\s+", " ", x)

test_that("assert_schema accepts valid names and rejects injection attempts", {
  expect_equal(assert_schema("gap_taxonomy"), "GAP_TAXONOMY")
  expect_error(assert_schema("x; DROP TABLE y"))
  expect_error(assert_schema("1abc"))
  expect_error(assert_schema(c("a", "b")))
  expect_error(assert_schema(NULL))
})

test_that("sql_match is NULL-safe and joins multiple keys with AND", {
  m <- flat(sql_match(c("A", "B")))
  expect_match(m, "target.A = source.A OR (target.A IS NULL AND source.A IS NULL)", fixed = TRUE)
  expect_match(m, "AND (target.B", fixed = TRUE)
})

test_that("DELETE targets only rows staged with OPERATION = 'DELETE'", {
  sql <- flat(build_delete_sql(p))
  expect_match(sql, "DELETE FROM SCH.TS target", fixed = TRUE)
  expect_match(sql, "FROM STG.STAGE_TS source", fixed = TRUE)
  expect_match(sql, "source.OPERATION = 'DELETE'", fixed = TRUE)
  expect_match(sql, "target.ID IS NULL AND source.ID IS NULL", fixed = TRUE)
})

test_that("MERGE updates only non-key columns and gates both branches by OPERATION", {
  sql <- flat(build_upsert_sql(p))
  expect_match(sql, "MERGE INTO SCH.TS target", fixed = TRUE)
  expect_match(sql, "OPERATION IN ('INSERT', 'UPDATE')", fixed = TRUE)
  set_clause <- sub(".*UPDATE SET (.*) WHERE source\\.OPERATION = 'UPDATE'.*", "\\1", sql)
  expect_match(set_clause, "target.LABEL = source.LABEL", fixed = TRUE)
  expect_match(set_clause, "target.ACTIVE = source.ACTIVE", fixed = TRUE)
  expect_false(grepl("target.ID", set_clause, fixed = TRUE)) # key must never be updated (ORA-38104)
  expect_match(sql, "WHEN NOT MATCHED THEN INSERT (ID, LABEL, ACTIVE)", fixed = TRUE)
  expect_match(sql, "WHERE source.OPERATION = 'INSERT'", fixed = TRUE)
})

test_that("MERGE on an all-key table has no WHEN MATCHED branch", {
  pk <- modifyList(p, list(table = "comp", id_keys = c("PID", "CODE"), cols = c("PID", "CODE")))
  sql <- flat(build_upsert_sql(pk))
  expect_false(grepl("WHEN MATCHED", sql, fixed = TRUE))
  expect_match(sql, "WHEN NOT MATCHED THEN INSERT (PID, CODE)", fixed = TRUE)
})

test_that("insert-only SQL inserts staged INSERT rows and nothing else", {
  sql <- flat(build_insert_only_sql(p))
  expect_match(sql, "INSERT INTO SCH.TS (ID, LABEL, ACTIVE)", fixed = TRUE)
  expect_match(sql, "FROM STG.STAGE_TS WHERE OPERATION = 'INSERT'", fixed = TRUE)
})

test_that("infer_oracle_type maps R types sensibly", {
  expect_equal(infer_oracle_type(1L), "NUMBER(10)")
  expect_equal(infer_oracle_type(1.5), "NUMBER")
  expect_equal(infer_oracle_type(TRUE), "NUMBER(1)")
  expect_equal(infer_oracle_type(Sys.time()), "TIMESTAMP")
  expect_equal(infer_oracle_type(Sys.Date()), "DATE")
  expect_equal(infer_oracle_type("a"), "VARCHAR2(50)")
  expect_equal(infer_oracle_type(strrep("a", 120)), "VARCHAR2(250)") # ~2x headroom
  expect_equal(infer_oracle_type(strrep("a", 3000)), "VARCHAR2(4000)") # capped
  expect_equal(infer_oracle_type(NA_character_), "VARCHAR2(50)")
})

test_that("CREATE TABLE gets a primary key only when keys are complete", {
  df <- clean_for_oracle(data.frame(id = 1:2, label = c("a", "b"), n = c(1.5, 2)))
  sql <- build_create_table_sql("SCH", "t1", df, "id")
  expect_match(sql, "CREATE TABLE SCH.T1", fixed = TRUE)
  expect_match(sql, "ID NUMBER(10) NOT NULL", fixed = TRUE)
  expect_match(sql, "LABEL VARCHAR2(50)", fixed = TRUE)
  expect_match(sql, "PRIMARY KEY (ID)", fixed = TRUE)

  df_na <- clean_for_oracle(data.frame(id = c(1, NA), label = c("a", "b")))
  expect_message(sql2 <- build_create_table_sql("SCH", "t2", df_na, "id"), "WITHOUT a primary key")
  expect_false(grepl("PRIMARY KEY", sql2, fixed = TRUE))
})

test_that("CREATE TABLE can add a foreign key", {
  df <- clean_for_oracle(data.frame(pid = 1, code = 2))
  sql <- build_create_table_sql("SCH", "child", df, c("pid", "code"),
    references = list(cols = "pid", table = "parent", ref_cols = "id")
  )
  expect_match(sql, "FOREIGN KEY (PID) REFERENCES SCH.PARENT (ID)", fixed = TRUE)
  expect_match(sql, "PRIMARY KEY (PID, CODE)", fixed = TRUE)
})
