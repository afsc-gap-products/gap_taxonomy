## Oracle integration tests in a SCRATCH schema (GAP_TAX_TEST_SCHEMA).
## Everything is created with a random ZZT<xxxxx> prefix and dropped afterwards.
## Covers: audit tables + triggers (insert/update/delete, key handling, drift,
## idempotency) and the delta -> stage -> merge -> audit cycle incl. rollback.

audit_rows <- function(ch, sch, tbl, op = NULL) {
  where <- if (is.null(op)) "" else sprintf("WHERE OPERATION_TYPE = '%s'", op)
  ora_q(ch, sprintf("SELECT * FROM %s.AUDIT_%s %s", sch, tbl, where))
}
audit_n <- function(ch, sch, tbl) {
  ora_q(ch, sprintf("SELECT COUNT(*) AS N FROM %s.AUDIT_%s", sch, tbl))$N[1]
}
trigger_status <- function(ch, sch, tbl) {
  ora_q(ch, sprintf(
    "SELECT t.status AS TRG_STATUS, o.status AS OBJ_STATUS
       FROM all_triggers t JOIN all_objects o
         ON o.owner = t.owner AND o.object_name = t.trigger_name AND o.object_type = 'TRIGGER'
      WHERE t.owner = '%s' AND t.trigger_name = 'AUDIT_%s' AND t.table_name = '%s'", sch, tbl, tbl
  ))
}

# ---------------------------------------------------------------------------
test_that("audit trigger records INSERT / UPDATE / DELETE with CURRENT/UPDATE pairs", {
  skip_if_no_oracle()
  ch <- test_channel()
  sch <- oracle_test_schema()
  tag <- local_scratch(ch, sch)
  tbl <- paste0(tag, "_P")
  run_sql(ch, sprintf(
    "CREATE TABLE %s.%s (ID NUMBER NOT NULL, LABEL VARCHAR2(50), ACTIVE NUMBER, CONSTRAINT PK_%s PRIMARY KEY (ID))",
    sch, tbl, tbl
  ))

  sync_audit_table(ch, sch, tbl, "ID", audit_schema = sch)

  expect_setequal(
    get_table_columns(ch, sch, paste0("AUDIT_", tbl)),
    c(
      "OPERATION_TYPE", "OPERATION_TIMESTAMP", "USER_NAME", "ID",
      "LABEL_CURRENT", "LABEL_UPDATE", "ACTIVE_CURRENT", "ACTIVE_UPDATE"
    )
  )
  ts <- trigger_status(ch, sch, tbl)
  expect_equal(nrow(ts), 1)
  expect_equal(ts$TRG_STATUS, "ENABLED")
  expect_equal(ts$OBJ_STATUS, "VALID")
  expect_equal(audit_n(ch, sch, tbl), 0)

  run_sql(ch, sprintf("INSERT INTO %s.%s VALUES (1, 'alpha', 1)", sch, tbl))
  run_sql(ch, sprintf("UPDATE %s.%s SET LABEL = 'beta' WHERE ID = 1", sch, tbl))
  run_sql(ch, sprintf("DELETE FROM %s.%s WHERE ID = 1", sch, tbl))

  ins <- audit_rows(ch, sch, tbl, "INSERT")
  expect_equal(nrow(ins), 1)
  expect_equal(ins$ID, 1)
  expect_equal(ins$LABEL_UPDATE, "alpha")
  expect_equal(ins$ACTIVE_UPDATE, 1)
  expect_true(is.na(ins$LABEL_CURRENT))
  expect_true(is.na(ins$ACTIVE_CURRENT))
  expect_true(nzchar(ins$USER_NAME))
  expect_false(is.na(ins$OPERATION_TIMESTAMP))

  upd <- audit_rows(ch, sch, tbl, "UPDATE")
  expect_equal(nrow(upd), 1)
  expect_equal(upd$LABEL_CURRENT, "alpha")
  expect_equal(upd$LABEL_UPDATE, "beta")
  expect_equal(upd$ACTIVE_CURRENT, upd$ACTIVE_UPDATE)

  del <- audit_rows(ch, sch, tbl, "DELETE")
  expect_equal(nrow(del), 1)
  expect_equal(del$ID, 1)
  expect_equal(del$LABEL_CURRENT, "beta")
  expect_true(is.na(del$LABEL_UPDATE))
  expect_true(is.na(del$ACTIVE_UPDATE))

  # re-running the sync is idempotent: no error, no extra audit rows, trigger still valid
  expect_no_error(sync_audit_table(ch, sch, tbl, "ID", audit_schema = sch))
  expect_equal(audit_n(ch, sch, tbl), 3)
  expect_equal(trigger_status(ch, sch, tbl)$OBJ_STATUS, "VALID")
})

# ---------------------------------------------------------------------------
test_that("all-key table (like TAXON_GROUP_COMPOSITION) is audited with keys only", {
  skip_if_no_oracle()
  ch <- test_channel()
  sch <- oracle_test_schema()
  tag <- local_scratch(ch, sch)
  tbl <- paste0(tag, "_K")
  run_sql(ch, sprintf(
    "CREATE TABLE %s.%s (PID NUMBER NOT NULL, CODE NUMBER NOT NULL, CONSTRAINT PK_%s PRIMARY KEY (PID, CODE))",
    sch, tbl, tbl
  ))
  sync_audit_table(ch, sch, tbl, c("PID", "CODE"), audit_schema = sch)
  expect_setequal(
    get_table_columns(ch, sch, paste0("AUDIT_", tbl)),
    c("OPERATION_TYPE", "OPERATION_TIMESTAMP", "USER_NAME", "PID", "CODE")
  )
  run_sql(ch, sprintf("INSERT INTO %s.%s VALUES (7, 70)", sch, tbl))
  run_sql(ch, sprintf("DELETE FROM %s.%s WHERE PID = 7", sch, tbl))
  a_ins <- audit_rows(ch, sch, tbl, "INSERT")
  expect_equal(c(a_ins$PID, a_ins$CODE), c(7, 70))
  d <- audit_rows(ch, sch, tbl, "DELETE") # DELETE must carry the OLD key
  expect_equal(c(d$PID, d$CODE), c(7, 70))
})

# ---------------------------------------------------------------------------
test_that("adding a column is detected as drift; rebuild = TRUE keeps a backup of the old audit table", {
  skip_if_no_oracle()
  ch <- test_channel()
  sch <- oracle_test_schema()
  tag <- local_scratch(ch, sch)
  tbl <- paste0(tag, "_P")
  run_sql(ch, sprintf("CREATE TABLE %s.%s (ID NUMBER NOT NULL, LABEL VARCHAR2(50))", sch, tbl))
  sync_audit_table(ch, sch, tbl, "ID", audit_schema = sch)
  run_sql(ch, sprintf("INSERT INTO %s.%s VALUES (1, 'a')", sch, tbl))
  expect_equal(audit_n(ch, sch, tbl), 1)

  run_sql(ch, sprintf("ALTER TABLE %s.%s ADD (NOTES VARCHAR2(20))", sch, tbl))
  expect_error(sync_audit_table(ch, sch, tbl, "ID", audit_schema = sch), "columns don't match")

  sync_audit_table(ch, sch, tbl, "ID", audit_schema = sch, rebuild = TRUE)
  expect_true(all(c("NOTES_CURRENT", "NOTES_UPDATE") %in% get_table_columns(ch, sch, paste0("AUDIT_", tbl))))
  expect_equal(audit_n(ch, sch, tbl), 0) # fresh table
  bak <- ora_q(ch, sprintf(
    "SELECT table_name FROM all_tables WHERE owner = '%s' AND table_name LIKE 'AUDIT_%s_BAK%%'", sch, tbl
  ))
  expect_equal(nrow(bak), 1)
  expect_equal(ora_q(ch, sprintf("SELECT COUNT(*) AS N FROM %s.%s", sch, bak$TABLE_NAME))$N, 1) # old history kept

  run_sql(ch, sprintf("UPDATE %s.%s SET NOTES = 'n' WHERE ID = 1", sch, tbl))
  expect_equal(audit_rows(ch, sch, tbl, "UPDATE")$NOTES_UPDATE, "n") # new trigger covers the new column
})

# ---------------------------------------------------------------------------
test_that("stage tables follow the production table's columns", {
  skip_if_no_oracle()
  ch <- test_channel()
  sch <- oracle_test_schema()
  tag <- local_scratch(ch, sch)
  tbl <- paste0(tag, "_P")
  run_sql(ch, sprintf("CREATE TABLE %s.%s (ID NUMBER NOT NULL, LABEL VARCHAR2(50))", sch, tbl))
  ensure_stage_table(ch, sch, sch, tbl)
  expect_setequal(get_table_columns(ch, sch, paste0("STAGE_", tbl)), c("ID", "LABEL", "OPERATION"))
  run_sql(ch, sprintf("ALTER TABLE %s.%s ADD (EXTRA NUMBER)", sch, tbl))
  ensure_stage_table(ch, sch, sch, tbl)
  expect_setequal(get_table_columns(ch, sch, paste0("STAGE_", tbl)), c("ID", "LABEL", "EXTRA", "OPERATION"))
})

# ---------------------------------------------------------------------------
test_that("full cycle: delta -> stage -> merge -> audit across parent, child and NULL-key tables", {
  skip_if_no_oracle()
  ch <- test_channel()
  sch <- oracle_test_schema()
  tag <- local_scratch(ch, sch)
  P <- paste0(tag, "_P") # parent
  C <- paste0(tag, "_C") # child (all-key, FK -> parent)
  N <- paste0(tag, "_N") # key includes a NULL column (like taxonomic_classification.database_id)

  run_sql(ch, sprintf(
    "CREATE TABLE %s.%s (ID NUMBER NOT NULL, LABEL VARCHAR2(50), ACTIVE NUMBER, CONSTRAINT PK_%s PRIMARY KEY (ID))",
    sch, P, P
  ))
  run_sql(ch, sprintf(
    "CREATE TABLE %1$s.%2$s (PID NUMBER NOT NULL, CODE NUMBER NOT NULL,
       CONSTRAINT PK_%2$s PRIMARY KEY (PID, CODE),
       CONSTRAINT FK_%2$s FOREIGN KEY (PID) REFERENCES %1$s.%3$s (ID))",
    sch, C, P
  ))
  run_sql(ch, sprintf("CREATE TABLE %s.%s (CODE NUMBER, DB VARCHAR2(10), DBID NUMBER, ACTIVE NUMBER)", sch, N))

  ora_insert(ch, sch, P, data.frame(ID = 1:10, LABEL = paste0("l", 1:10), ACTIVE = 1))
  ora_insert(ch, sch, C, data.frame(PID = c(10, 1, 1), CODE = c(100, 101, 102)))
  ora_insert(ch, sch, N, data.frame(CODE = c(1, 2), DB = c("WORMS", "ITIS"), DBID = c(NA, 123), ACTIVE = c(1, 1)))

  mk <- function(new, keys) {
    list(
      schema = sch, stage_schema = sch, audit_schema = sch, mode = "upsert",
      id_keys = keys, audit_keys = keys, new = new, current = NULL
    )
  }
  reg <- setNames(
    list(mk("zzt_p_new", "ID"), mk("zzt_c_new", c("PID", "CODE")), mk("zzt_n_new", c("CODE", "DB", "DBID"))),
    c(P, C, N)
  )

  ids <- c(1:9, 11)
  p_new <- data.frame(id = ids, label = paste0("l", ids), active = 1, stringsAsFactors = FALSE)
  p_new$label[p_new$id == 2] <- "BETA"
  p_new$active[p_new$id == 3] <- 0
  env <- new.env()
  env$zzt_p_new <- p_new # -> 2 UPDATE, 1 DELETE (id 10), 1 INSERT (id 11)
  env$zzt_c_new <- data.frame(pid = c(1, 11), code = c(101, 200)) # -> 1 INSERT, 2 DELETE
  env$zzt_n_new <- data.frame(
    code = c(1, 2), db = c("WORMS", "ITIS"), dbid = c(NA, 123), active = c(0, 1),
    stringsAsFactors = FALSE
  ) # -> 1 UPDATE on the NULL-key row

  payloads <- build_stage_payloads(ch, reg, envir = env, max_delete_frac = 1)
  count <- function(p, op) sum(p$data$OPERATION == op)
  expect_equal(c(count(payloads[[P]], "INSERT"), count(payloads[[P]], "UPDATE"), count(payloads[[P]], "DELETE")), c(1, 2, 1))
  expect_equal(c(count(payloads[[C]], "INSERT"), count(payloads[[C]], "UPDATE"), count(payloads[[C]], "DELETE")), c(1, 0, 2))
  expect_equal(c(count(payloads[[N]], "INSERT"), count(payloads[[N]], "UPDATE"), count(payloads[[N]], "DELETE")), c(0, 1, 0))

  # building payloads is read-only
  expect_equal(ora_q(ch, sprintf("SELECT COUNT(*) AS N FROM %s.%s", sch, P))$N, 10)

  # audit infrastructure; rows inserted before the triggers existed are not audited
  sync_all_audits(ch, reg)
  for (t in c(P, C, N)) {
    expect_equal(audit_n(ch, sch, t), 0)
    expect_equal(trigger_status(ch, sch, t)$OBJ_STATUS, "VALID")
  }

  t0 <- get_db_time(ch)
  suppressMessages({
    for (p in payloads) load_stage_table(ch, p)
    expect_true(apply_changes(ch, payloads))
  })
  t1 <- get_db_time(ch)

  # --- production tables now equal the new data
  got_p <- ora_q(ch, sprintf("SELECT ID, LABEL, ACTIVE FROM %s.%s ORDER BY ID", sch, P))
  expect_equal(got_p$ID, ids)
  expect_equal(got_p$LABEL, p_new$label)
  expect_equal(got_p$ACTIVE, p_new$active)
  got_c <- ora_q(ch, sprintf("SELECT PID, CODE FROM %s.%s ORDER BY PID", sch, C))
  expect_equal(list(got_c$PID, got_c$CODE), list(c(1, 11), c(101, 200)))
  got_n <- ora_q(ch, sprintf("SELECT CODE, DBID, ACTIVE FROM %s.%s ORDER BY CODE", sch, N))
  expect_equal(nrow(got_n), 2) # NULL key matched: updated in place, not duplicated
  expect_equal(got_n$ACTIVE, c(0, 1))
  expect_true(is.na(got_n$DBID[1]))

  # --- audit tables recorded exactly what changed
  by_op <- function(t) {
    x <- ora_q(ch, sprintf("SELECT OPERATION_TYPE, COUNT(*) AS N FROM %s.AUDIT_%s GROUP BY OPERATION_TYPE", sch, t))
    setNames(x$N, x$OPERATION_TYPE)
  }
  expect_equal(by_op(P)[c("INSERT", "UPDATE", "DELETE")], c(INSERT = 1, UPDATE = 2, DELETE = 1))
  expect_equal(by_op(C)[c("INSERT", "DELETE")], c(INSERT = 1, DELETE = 2))
  expect_equal(by_op(N), c(UPDATE = 1))
  a_upd <- audit_rows(ch, sch, N, "UPDATE")
  expect_equal(a_upd$ACTIVE_CURRENT, 1)
  expect_equal(a_upd$ACTIVE_UPDATE, 0)
  expect_equal(audit_rows(ch, sch, P, "DELETE")$LABEL_CURRENT, "l10")
  beta <- audit_rows(ch, sch, P, "UPDATE")
  beta <- beta[beta$ID == 2, ]
  expect_equal(c(beta$LABEL_CURRENT, beta$LABEL_UPDATE), c("l2", "BETA"))

  # summarize_all_updates() agrees with the deltas that were applied
  summ <- summarize_all_updates(ch, payloads, t0, t1)
  cmp <- dplyr::full_join(
    expected_audit_counts(payloads),
    dplyr::transmute(summ, TABLE_NAME, OPERATION_TYPE, got = as.integer(NUMBER_RECS)),
    by = c("TABLE_NAME", "OPERATION_TYPE")
  )
  expect_false(anyNA(cmp$NUMBER_RECS))
  expect_false(anyNA(cmp$got))
  expect_equal(cmp$NUMBER_RECS, cmp$got)

  # --- running again with the same data is a no-op
  payloads2 <- build_stage_payloads(ch, reg, envir = env, max_delete_frac = 1)
  expect_true(all(vapply(payloads2, function(p) nrow(p$data) == 0, logical(1))))
  expect_false(suppressMessages(apply_changes(ch, payloads2)))
  expect_equal(sum(vapply(c(P, C, N), function(t) audit_n(ch, sch, t), numeric(1))), 4 + 3 + 1)
})

# ---------------------------------------------------------------------------
test_that("safety guards: large deletes, no_deletes, stale R objects, unknown columns", {
  skip_if_no_oracle()
  ch <- test_channel()
  sch <- oracle_test_schema()
  tag <- local_scratch(ch, sch)
  P <- paste0(tag, "_P")
  run_sql(ch, sprintf("CREATE TABLE %s.%s (ID NUMBER NOT NULL, LABEL VARCHAR2(50), CONSTRAINT PK_%s PRIMARY KEY (ID))", sch, P, P))
  ora_insert(ch, sch, P, data.frame(ID = 1:10, LABEL = paste0("l", 1:10)))

  spec <- list(
    schema = sch, stage_schema = sch, audit_schema = sch, mode = "upsert",
    id_keys = "ID", audit_keys = "ID", new = "zzt_new", current = NULL
  )
  reg <- setNames(list(spec), P)
  env <- new.env()
  env$zzt_new <- data.frame(id = 1, label = "l1") # would delete 9 of 10 rows

  expect_error(build_stage_payloads(ch, reg, envir = env), "would delete 9 of 10")
  expect_no_error(build_stage_payloads(ch, reg, envir = env, allow_large_deletes = TRUE))

  reg_nd <- setNames(list(modifyList(spec, list(no_deletes = TRUE))), P)
  expect_error(
    utils::capture.output(build_stage_payloads(ch, reg_nd, envir = env, allow_large_deletes = TRUE)),
    "no_deletes"
  )

  env$zzt_cur <- data.frame(ID = 1) # R copy of the table is out of date (1 row vs 10)
  reg_stale <- setNames(list(modifyList(spec, list(current = "zzt_cur"))), P)
  expect_error(build_stage_payloads(ch, reg_stale, envir = env, allow_large_deletes = TRUE), "stale")

  env$zzt_new <- data.frame(id = 1:10, label = paste0("l", 1:10), surprise = 1)
  expect_error(build_stage_payloads(ch, reg, envir = env), "not in")
})

# ---------------------------------------------------------------------------
test_that("a failure part-way through rolls back BOTH the data and the audit rows", {
  skip_if_no_oracle()
  ch <- test_channel()
  sch <- oracle_test_schema()
  tag <- local_scratch(ch, sch)
  P <- paste0(tag, "_P")
  run_sql(ch, sprintf("CREATE TABLE %s.%s (ID NUMBER NOT NULL, LABEL VARCHAR2(50), CONSTRAINT PK_%s PRIMARY KEY (ID))", sch, P, P))
  ora_insert(ch, sch, P, data.frame(ID = 1, LABEL = "alpha"))

  reg <- setNames(list(list(
    schema = sch, stage_schema = sch, audit_schema = sch, mode = "upsert",
    id_keys = "ID", audit_keys = "ID", new = "zzt_new", current = NULL
  )), P)
  sync_all_audits(ch, reg)
  env <- new.env()
  env$zzt_new <- data.frame(id = 1, label = "omega")
  payloads <- build_stage_payloads(ch, reg, envir = env)
  suppressMessages(for (p in payloads) load_stage_table(ch, p))

  bogus <- list(
    table = paste0(tag, "_NOPE"), schema = sch, stage_schema = sch, mode = "insert_only",
    id_keys = "ID", cols = "ID", data = data.frame(ID = 1, OPERATION = "INSERT")
  )
  expect_error(
    suppressMessages(apply_changes(ch, c(payloads, list(bogus = bogus)))),
    "rolled back"
  )
  expect_equal(ora_q(ch, sprintf("SELECT LABEL FROM %s.%s", sch, P))$LABEL, "alpha")
  expect_equal(audit_n(ch, sch, P), 0)

  # and the connection is usable (auto-commit restored): the same update now goes through
  suppressMessages(apply_changes(ch, payloads))
  expect_equal(ora_q(ch, sprintf("SELECT LABEL FROM %s.%s", sch, P))$LABEL, "omega")
  expect_equal(audit_n(ch, sch, P), 1)
})
