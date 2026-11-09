## READ-ONLY checks of the real audit objects (needs GAP_TAX_TEST_ORACLE=true).
## Per registry table:
##   * audit table: columns, audit-data sanity
##   * trigger: exists, ENABLED (and VALID when this account is allowed to see that)
##   * id_keys identify rows uniquely in production
##
## Visibility note: ALL_TRIGGERS / ALL_OBJECTS only show another schema's triggers
## to accounts that own them or hold CREATE ANY TRIGGER. If this account can see NO
## triggers in the schema at all, the trigger test is SKIPPED (not failed) and tells
## you how to verify as the schema owner. If it can see other triggers in the schema
## but not this one, that is a genuine failure.

cfg0 <- get0("cfg", envir = globalenv(), ifnotfound = list(include_race_data = FALSE, schema = "GAP_TAXONOMY"))
reg_prod <- resolve_registry(tax_registry, cfg0)

for (tbl in names(reg_prod)) {
  local({
    tbl <- tbl
    sp <- reg_prod[[tbl]]
    table <- toupper(tbl)
    audit <- paste0("AUDIT_", table)
    where <- paste0(sp$schema, ".", table)
    not_found <- sprintf(
      "%s was not found by this account (ORA-00942 / no columns). Check with:\n  SELECT owner, object_name, object_type FROM all_objects WHERE object_name = '%s';\n  SELECT owner, synonym_name, table_owner, table_name FROM all_synonyms WHERE synonym_name = '%s';",
      where, table, table
    )
    
    # ------------------------------------------------------------------
    test_that(paste("audit table is correct for", where), {
      skip_if_no_oracle()
      ch <- test_channel()
      cols <- get_table_columns(ch, sp$schema, table)
      
      if (length(cols) == 0) {
        fail(not_found)
      } else {
        keys <- toupper(sp$audit_keys)
        expect_true(all(keys %in% cols), info = "audit_keys must be columns of the table")
        resp <- setdiff(cols, keys)
        
        # columns = header + keys + CURRENT/UPDATE pair for every other column
        expected <- c(
          "OPERATION_TYPE", "OPERATION_TIMESTAMP", "USER_NAME", cols[cols %in% keys],
          sprintf("%s_CURRENT", resp), sprintf("%s_UPDATE", resp)
        )
        expect_setequal(get_table_columns(ch, sp$audit_schema, audit), expected)
        
        # (PUBLIC SELECT is no longer required: access is managed through registry `grants`)
        
        # audit data sanity (what the trigger guarantees)
        bad_type <- ora_q(ch, sprintf(
          "SELECT COUNT(*) AS N FROM %s.%s WHERE OPERATION_TYPE NOT IN ('INSERT','UPDATE','DELETE')",
          sp$audit_schema, audit
        ))
        expect_equal(bad_type$N, 0)
        if (length(resp) > 0) {
          ins_bad <- ora_q(ch, sprintf(
            "SELECT COUNT(*) AS N FROM %s.%s WHERE OPERATION_TYPE = 'INSERT' AND (%s)",
            sp$audit_schema, audit, paste(sprintf("%s_CURRENT IS NOT NULL", resp), collapse = " OR ")
          ))
          del_bad <- ora_q(ch, sprintf(
            "SELECT COUNT(*) AS N FROM %s.%s WHERE OPERATION_TYPE = 'DELETE' AND (%s)",
            sp$audit_schema, audit, paste(sprintf("%s_UPDATE IS NOT NULL", resp), collapse = " OR ")
          ))
          expect_equal(ins_bad$N, 0, info = "INSERT audit rows must have empty *_CURRENT columns")
          expect_equal(del_bad$N, 0, info = "DELETE audit rows must have empty *_UPDATE columns")
        }
      }
    })
    
    # ------------------------------------------------------------------
    test_that(paste("audit trigger exists and is enabled for", where), {
      skip_if_no_oracle()
      ch <- test_channel()
      trg <- ora_q(ch, sprintf(
        "SELECT status AS TRG_STATUS, table_name AS TBL, table_owner AS TBL_OWNER
           FROM all_triggers WHERE owner = '%s' AND trigger_name = '%s'", sp$schema, audit
      ))
      
      if (nrow(trg) == 0) {
        visible <- ora_q(ch, sprintf("SELECT COUNT(*) AS N FROM all_triggers WHERE owner = '%s'", sp$schema))$N
        if (visible == 0) {
          evidence <- tryCatch(
            {
              a <- ora_q(ch, sprintf("SELECT COUNT(*) AS N, MAX(OPERATION_TIMESTAMP) AS LAST_TS FROM %s.%s", sp$audit_schema, audit))
              sprintf("%s has %d audit rows, latest: %s.", audit, a$N, format(a$LAST_TS))
            },
            error = function(e) "audit table not readable."
          )
          skip(paste0(
            "This account cannot see ANY triggers in ", sp$schema,
            " (needs ownership or CREATE ANY TRIGGER), so trigger ", audit, " cannot be verified here. ",
            evidence, " Verify as the schema owner:\n  SELECT trigger_name, status, table_name FROM all_triggers WHERE owner = '",
            sp$schema, "' AND trigger_name = '", audit, "';"
          ))
        } else {
          fail(sprintf(
            "Trigger %s.%s is MISSING: this account can see %d other trigger(s) in %s but not this one. Run sync_all_audits().",
            sp$schema, audit, visible, sp$schema
          ))
        }
      } else {
        expect_equal(nrow(trg), 1)
        expect_equal(trg$TRG_STATUS, "ENABLED")
        expect_equal(trg$TBL, table)
        expect_equal(trg$TBL_OWNER, sp$schema)
        # VALID/INVALID is only checked when this account is allowed to see the object
        obj <- ora_q(ch, sprintf(
          "SELECT status AS OBJ_STATUS FROM all_objects
            WHERE owner = '%s' AND object_name = '%s' AND object_type = 'TRIGGER'", sp$schema, audit
        ))
        if (nrow(obj) == 1) expect_equal(obj$OBJ_STATUS, "VALID")
      }
    })
    
    # ------------------------------------------------------------------
    test_that(paste("id_keys identify rows uniquely in", where), {
      skip_if_no_oracle()
      ch <- test_channel()
      cols <- get_table_columns(ch, sp$schema, table)
      k <- toupper(sp$id_keys)
      if (length(cols) == 0) {
        fail(not_found)
      } else {
        expect_true(all(k %in% cols))
        dups <- ora_q(ch, sprintf(
          "SELECT COUNT(*) AS N FROM (SELECT 1 FROM %s.%s GROUP BY %s HAVING COUNT(*) > 1)",
          sp$schema, table, paste(k, collapse = ", ")
        ))
        expect_equal(dups$N, 0, info = "duplicate keys would make MERGE ambiguous")
      }
    })
  })
}