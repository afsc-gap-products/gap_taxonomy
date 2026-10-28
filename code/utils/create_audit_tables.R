## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Set up audit tables and triggers across Schemas
## Description:   Creates AUDIT_* tables ONLY IF they do not exist,
##                and creates/replaces triggers with verified 1:1 column mapping.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

channel <- oracle_connect()

audit_targets <- list(
  # --- tax_schema tables ---
  list(schema = tax_schema, table = "TAXONOMIC_CLASSIFICATION", keys = c("ACTIVE", "SPECIES_NAME", "SPECIES_CODE")),
  list(schema = tax_schema, table = "TAXONOMIC_CHANGES", keys = c("OLD_SPECIES_NAME", "OLD_SPECIES_CODE", "NEW_SPECIES_NAME", "NEW_SPECIES_CODE")),
  list(schema = tax_schema, table = "TAXONOMIC_CONFIDENCE", keys = c("SURVEY_DEFINITION_ID", "YEAR", "SPECIES_CODE")),
  list(schema = tax_schema, table = "TAXON_GROUP_STANZAS", keys = c("GROUP_STANZA_ID")),
  list(schema = tax_schema, table = "TAXON_GROUP_COMPOSITION", keys = c("GROUP_STANZA_ID", "SPECIES_CODE"))

  # # --- RACE_DATA tables ---
  # , list(schema = "RACE_DATA", table = "SPECIES_TAXONOMICS", keys = c("SPECIES_TAXONOMIC_ID")),
  # list(schema = "RACE_DATA", table = "RACE_SPECIES_CODES", keys = c("SPECIES_CODE"))
)

for (tgt in audit_targets) {
  schema_nm <- tgt$schema
  tbl_nm <- tgt$table
  audit_schema_nm <- if (!is.null(tgt$audit_schema)) tgt$audit_schema else schema_nm
  audit_tbl_nm <- paste0("AUDIT_", tbl_nm)
  keys <- tgt$keys

  message(sprintf("Processing %s.%s -> %s.%s...", schema_nm, tbl_nm, audit_schema_nm, audit_tbl_nm))

  # ---------------------------------------------------------------------------
  # 1. Fetch column names from Oracle data dictionary in ordinal order
  # ---------------------------------------------------------------------------
  all_cols <- RODBC::sqlQuery(
    channel,
    sprintf("
      SELECT column_name
      FROM all_tab_columns
      WHERE owner = UPPER('%s') AND table_name = UPPER('%s')
      ORDER BY column_id
    ", schema_nm, tbl_nm)
  )$COLUMN_NAME

  key_cols <- all_cols[all_cols %in% keys]
  resp_cols <- all_cols[!all_cols %in% keys]

  curr_cols <- paste0(resp_cols, "_CURRENT")
  upd_cols <- paste0(resp_cols, "_UPDATE")

  # ---------------------------------------------------------------------------
  # 2. Create Audit Table if Missing (Using Oracle CTAS)
  # ---------------------------------------------------------------------------
  chk <- RODBC::sqlQuery(channel, sprintf(
    "SELECT COUNT(*) AS CNT FROM ALL_TABLES WHERE OWNER = UPPER('%s') AND TABLE_NAME = UPPER('%s')",
    audit_schema_nm, audit_tbl_nm
  ))$CNT[1]

  if (isTRUE(chk > 0)) {
    message(sprintf("--> Audit table %s.%s already exists. Skipping DDL.", audit_schema_nm, audit_tbl_nm))
  } else {
    message(sprintf("--> Creating %s.%s...", audit_schema_nm, audit_tbl_nm))

    both_cols_ctas <- if (length(resp_cols) > 0) {
      as.vector(rbind(paste0(resp_cols, " AS ", curr_cols), paste0(resp_cols, " AS ", upd_cols)))
    } else {
      character()
    }

    ctas_sql <- sprintf(
      "
      CREATE TABLE %s.%s AS
      SELECT
        CAST('INSERT' AS VARCHAR2(10)) AS OPERATION_TYPE,
        SYSTIMESTAMP AS OPERATION_TIMESTAMP,
        CAST('USER' AS VARCHAR2(100)) AS USER_NAME,
        %s
      FROM %s.%s WHERE 1 = 0
    ",
      audit_schema_nm, audit_tbl_nm,
      paste(c(key_cols, both_cols_ctas), collapse = ", "),
      schema_nm, tbl_nm
    )

    res_ctas <- RODBC::sqlQuery(channel, ctas_sql)
    if (is.character(res_ctas) && any(grepl("ERROR|ORA-", res_ctas, ignore.case = TRUE))) {
      stop(sprintf("Failed to create %s.%s: %s", audit_schema_nm, audit_tbl_nm, paste(res_ctas, collapse = "\n")))
    }
    # RODBC::sqlQuery(channel, sprintf("GRANT SELECT ON %s.%s TO PUBLIC", audit_schema_nm, audit_tbl_nm))
  }

  # ---------------------------------------------------------------------------
  # 3. Synchronize Trigger
  # ---------------------------------------------------------------------------
  user_expr <- "COALESCE(SYS_CONTEXT('USERENV', 'PROXY_USER'), SYS_CONTEXT('USERENV', 'SESSION_USER'))"
  hdr_cols <- c("OPERATION_TYPE", "OPERATION_TIMESTAMP", "USER_NAME")

  # Interleaved columns and values for UPDATING:
  # (COL1_CURRENT, COL1_UPDATE, COL2_CURRENT, COL2_UPDATE...)
  # (:OLD.COL1, :NEW.COL1, :OLD.COL2, :NEW.COL2...)
  if (length(resp_cols) > 0) {
    both_cols_upd <- as.vector(rbind(curr_cols, upd_cols))
    both_vals_upd <- as.vector(rbind(paste0(":OLD.", resp_cols), paste0(":NEW.", resp_cols)))
  } else {
    both_cols_upd <- character()
    both_vals_upd <- character()
  }

  # --- INSERTING Vector Pair ---
  ins_cols <- c(hdr_cols, key_cols, upd_cols)
  ins_vals <- c("'INSERT'", "SYSTIMESTAMP", user_expr, paste0(":NEW.", key_cols), paste0(":NEW.", resp_cols))

  # --- DELETING Vector Pair ---
  del_cols <- c(hdr_cols, key_cols, curr_cols)
  del_vals <- c("'DELETE'", "SYSTIMESTAMP", user_expr, paste0(":OLD.", key_cols), paste0(":OLD.", resp_cols))

  # --- UPDATING Vector Pair ---
  upd_cols_all <- c(hdr_cols, key_cols, both_cols_upd)
  upd_vals_all <- c("'UPDATE'", "SYSTIMESTAMP", user_expr, paste0(":NEW.", key_cols), both_vals_upd)

  # Pre-flight vector length check: will immediately halt in R if lists diverge
  stopifnot(length(ins_cols) == length(ins_vals))
  stopifnot(length(del_cols) == length(del_vals))
  stopifnot(length(upd_cols_all) == length(upd_vals_all))

  trigger_ddl <- sprintf(
    "
CREATE OR REPLACE TRIGGER %s.AUDIT_%s
AFTER INSERT OR DELETE OR UPDATE ON %s.%s
FOR EACH ROW
BEGIN
  IF INSERTING THEN
    INSERT INTO %s.%s (
      %s
    ) VALUES (
      %s
    );

  ELSIF DELETING THEN
    INSERT INTO %s.%s (
      %s
    ) VALUES (
      %s
    );

  ELSIF UPDATING THEN
    INSERT INTO %s.%s (
      %s
    ) VALUES (
      %s
    );
  END IF;
END;
",
    schema_nm, tbl_nm, schema_nm, tbl_nm,

    # INSERTING
    audit_schema_nm, audit_tbl_nm,
    paste(ins_cols, collapse = ", "),
    paste(ins_vals, collapse = ", "),

    # DELETING
    audit_schema_nm, audit_tbl_nm,
    paste(del_cols, collapse = ", "),
    paste(del_vals, collapse = ", "),

    # UPDATING
    audit_schema_nm, audit_tbl_nm,
    paste(upd_cols_all, collapse = ", "),
    paste(upd_vals_all, collapse = ", ")
  )

  res_trig <- RODBC::sqlQuery(channel, trigger_ddl)

  # Check if compilation failed or if warnings/errors were generated
  err_check <- RODBC::sqlQuery(
    channel,
    sprintf("
      SELECT line, position, text
      FROM all_errors
      WHERE owner = UPPER('%s') AND name = UPPER('AUDIT_%s')
      ORDER BY sequence
    ", schema_nm, tbl_nm)
  )

  if (is.data.frame(err_check) && nrow(err_check) > 0) {
    stop(sprintf(
      "Trigger compilation failed for %s.AUDIT_%s:\n%s",
      schema_nm, tbl_nm,
      paste(sprintf("Line %s, Pos %s: %s", err_check$LINE, err_check$POSITION, err_check$TEXT), collapse = "\n")
    ))
  } else {
    message(sprintf("--> Trigger synchronized and VALID for %s.%s", schema_nm, tbl_nm))
  }
}

message("\nAudit infrastructure check complete.")
