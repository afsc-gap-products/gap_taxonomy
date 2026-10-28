## Purpose: Update taxonomic tables in Oracle. Audit records are written by an
##          Oracle trigger on each base table (nothing is audited from R).
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("code/utils/audit_table_helpers.R")

if (!exists("channel") || !inherits(channel, "RODBC")) {
  channel <- gapindex::get_connected()
}

audit_schema <- "GAP_TAXONOMY"
product_schema <- "GAP_TAXONOMY"

# One-off: clear leftover TMP_* staging tables from earlier failed runs
# drop_stale_temp_tables(channel, product_schema)


## ---------------------------------------------------------------------------
## 1. Table Metadata Definition
## ---------------------------------------------------------------------------

taxon_tables <- list(
  taxonomic_classification = list(pk_cols = c("SPECIES_CODE", "DATABASE")),
  taxonomic_changes = list(pk_cols = c(
    "OLD_SPECIES_CODE", "NEW_SPECIES_CODE",
    "OLD_SPECIES_NAME", "NEW_SPECIES_NAME"
  )),
  taxon_group_composition = list(pk_cols = c("GROUP_STANZA_ID", "SPECIES_CODE")),
  taxon_group_stanzas = list(pk_cols = c("GROUP_STANZA_ID", "GROUP_CODE")),
  taxonomic_confidence = list(pk_cols = c("SPECIES_CODE", "SURVEY_DEFINITION_ID", "YEAR"))
)


## ---------------------------------------------------------------------------
## 2. Main Execution Loop Over Taxon Tables
## ---------------------------------------------------------------------------

for (tbl in names(taxon_tables)) {
  message("\n==================================================")
  message("Processing Table: ", toupper(tbl))
  message("==================================================")

  pk_cols <- toupper(taxon_tables[[tbl]]$pk_cols)

  target_obj_name <- paste0(tbl, "_new")
  if (!exists(target_obj_name)) {
    warning("Target object '", target_obj_name, "' not found in environment. Skipping.")
    next
  }

  update_df <- as.data.frame(dplyr::rename_with(get(target_obj_name), toupper))
  if (!all(pk_cols %in% names(update_df))) {
    stop(
      "Primary key column(s) missing from ", target_obj_name, ": ",
      paste(setdiff(pk_cols, names(update_df)), collapse = ", ")
    )
  }

  n_before <- nrow(update_df)
  update_df <- dplyr::distinct(update_df)
  if (nrow(update_df) < n_before) {
    message("  Removed ", n_before - nrow(update_df), " exact duplicate row(s) from ", target_obj_name)
  }

  base_table_name <- toupper(tbl)
  audit_table_name <- paste0("AUDIT_", base_table_name)

  # ---- Base table: create if missing, otherwise add any missing columns -----
  if (!oracle_table_exists(channel, product_schema, base_table_name)) {
    message(
      "ℹ Base table ", product_schema, ".", base_table_name,
      " does NOT exist. Creating base table..."
    )
    create_base_table(channel, product_schema, base_table_name, update_df)
    oracle_df <- update_df[0, , drop = FALSE]
  } else {
    ensure_table_columns(
      channel, product_schema, base_table_name,
      vapply(update_df, oracle_type, character(1))
    )
    oracle_df <- RODBC::sqlQuery(
      channel,
      paste0("SELECT * FROM ", product_schema, ".", base_table_name),
      stringsAsFactors = FALSE
    )
    if (odbc_failed(oracle_df)) {
      stop(
        "Failed to query ", product_schema, ".", base_table_name, ":\n",
        paste(oracle_df, collapse = "\n")
      )
    }
  }

  # ---- Match R column types to the existing Oracle column types -------------
  ora_types <- oracle_column_types(channel, product_schema, base_table_name)
  update_df <- coerce_to_oracle_types(update_df, ora_types)

  # ---- Oracle-side auditing: audit table + trigger ---------------------------
  # Built from the base table's real columns and re-created every run, so new
  # columns are picked up automatically.
  setup_audit_table(channel, product_schema, base_table_name,
    audit_schema, audit_table_name,
    base_types = ora_types
  )
  grant_audit_insert(channel, audit_schema, audit_table_name, trigger_owner = product_schema)
  create_audit_trigger(channel, product_schema, base_table_name,
    audit_schema, audit_table_name,
    base_types = ora_types
  )

  # ---- Calculate delta -------------------------------------------------------
  changed_rows <- find_changed_rows(
    old_df  = oracle_df,
    new_df  = update_df,
    pk_cols = pk_cols
  )

  n_new <- nrow(changed_rows$new_rows)
  n_changed <- nrow(changed_rows$changed_rows)
  n_deleted <- nrow(changed_rows$deleted_rows)
  n_expected <- n_new + n_changed + n_deleted

  if (n_expected == 0) {
    message("No changes detected for ", base_table_name, ". Skipping Oracle write.\n")
    next
  }

  # ---- Safety warnings -------------------------------------------------------
  if (tbl == "taxonomic_classification" && n_deleted > 0) {
    warning("⚠️ DELETED ROWS DETECTED in TAXONOMIC_CLASSIFICATION! Please review:")
    print(as.data.frame(changed_rows$deleted_rows))
  }

  # ---- Apply table updates (the trigger writes the audit rows) ---------------
  audit_before <- count_table_rows(channel, audit_schema, audit_table_name)

  message("Applying updates to ", product_schema, ".", base_table_name, "...")
  safe_apply_taxonomy_updates(
    channel      = channel,
    tbl          = tbl,
    changed_rows = changed_rows,
    schema       = product_schema,
    pk_cols      = pk_cols
  )

  # ---- Confirm the trigger logged what we changed ----------------------------
  audit_written <- count_table_rows(channel, audit_schema, audit_table_name) - audit_before
  if (audit_written != n_expected) {
    warning(
      "Expected ", n_expected, " audit rows in ", audit_schema, ".", audit_table_name,
      " but ", audit_written, " were added (other users may be writing at the same time)."
    )
  }

  message(
    "Successfully processed ", base_table_name, ":",
    "\n\t Added rows:   ", n_new,
    "\n\t Changed rows: ", n_changed,
    "\n\t Deleted rows: ", n_deleted,
    "\n\t Audit rows written by trigger: ", audit_written, "\n"
  )
}
