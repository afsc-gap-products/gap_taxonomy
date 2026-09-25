## Purpose: Update taxonomic tables in Oracle database and create audit records
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("R/utils/audit_table_helpers.R")

channel <- get.connected(schema='sfriedman', username = "sfriedman", password = "TEMPpw#12345") 

# channel <- gapindex::get_connected(db = "AFSC", check_access = F)
audit_schema <- "GAP_ARCHIVE"
product_schema <- "GAP_PRODUCTS"


## ---------------------------------------------------------------------------
## Table metadata
## ---------------------------------------------------------------------------

taxon_tables <- list(
  taxonomic_classification = list(pk_cols = c("SPECIES_CODE")),
  taxonomic_changes        = list(pk_cols = c("OLD_SPECIES_CODE", "NEW_SPECIES_CODE")),
  taxon_group_composition  = list(pk_cols = c("GROUP_STANZA_ID", "GROUP_CODE", "SPECIES_CODE")),
  taxon_group_stanzas      = list(pk_cols = c("GROUP_STANZA_ID", "GROUP_CODE"))
)


## ---------------------------------------------------------------------------
## Loop over taxon tables
## ---------------------------------------------------------------------------

for (tbl in names(taxon_tables)) {
  
  message("Processing: ", tbl)
  
  pk_cols <- toupper(taxon_tables[[tbl]]$pk_cols)
  
  oracle_df <- RODBC::sqlQuery(channel, glue("SELECT * FROM {product_schema}.{toupper(tbl)}")) 
  if (is.character(oracle_df)) {
    stop("Check Oracle connection and try again: ", oracle_df)
  }  
  
  update_df <- get(paste0(tbl, "_new")) %>%
    rename_with(toupper) 
  
  changed_rows <- find_changed_rows(
    new_df = update_df,
    old_df = oracle_df,
    pk_cols = pk_cols
  )
  

  # ---------------------- Build audit rows -----------------------------
  if(tbl == "taxonomic_changes"){
    audit_rows <- update_df %>%
      filter(YEAR_CHANGED == format(Sys.time(), "%Y")) %>%
      mutate(OPERATION_TYPE = "INSERT",
             OPERATION_TIMESTAMP = Sys.time(),
             USER_NAME = Sys.getenv("USER")) %>%
      select(contains("OPERATION"), USER_NAME, everything())
  } else {
    audit_rows <- dplyr::bind_rows(
      build_audit_rows(changed_rows$new_rows, "INSERT", pk_cols),
      build_audit_rows(changed_rows$deleted_rows, "DELETE", pk_cols),
      build_audit_rows(changed_rows$changed_rows, "UPDATE", pk_cols)
    ) %>%
      select(contains("OPERATION"), USER_NAME, everything())
  }
  
  
  if (nrow(audit_rows) == 0) {
    message("\n No changes detected.\n")
    next
  }
  

  ## WARNING FOR DELETED ROWS IN TAXONOMIC CLASSIFICATION
  if(tbl == "taxonomic_classification" & nrow(changed_rows$deleted_rows) > 0){
    warning("Deleted rows detected in taxonomic_classification table. Please review manually before proceeding.")
    print(as.data.frame(changed_rows$deleted_rows))
  }
  
  
  # ---------------------- Ensure audit table exists ---------------------
  audit_table <- paste0("AUDIT_", toupper(tbl))
  
  # Create table if does not exist
  if (!oracle_table_exists(channel, audit_schema, audit_table)) {
    create_audit_table(channel, audit_schema, audit_table, audit_rows)
  } else {
  
  # Ensure all required columns exist
    ensure_audit_columns(channel, audit_schema, audit_table, audit_rows)
  }
  
  
  # ---------------------- Create/Replace Trigger -----------------------
  non_pk <- setdiff(names(update_df), pk_cols)
  
  create_audit_trigger(
    channel,
    schema = product_schema,
    base_table = toupper(tbl),
    audit_table = audit_table,
    pk_cols = pk_cols,
    non_pk_cols = non_pk
  )
  
  
  # ---------------------- Apply safe updates ---------------------------
  safe_apply_taxonomy_updates(
    channel = channel,
    tbl = tbl,
    changed = changed,
    pk_cols = pk_cols
  )
  
  
  # ---------------------- Write audit rows -----------------------------
  RODBC::sqlSave(
    channel,
    dat = audit_rows,
    tablename = glue("{audit_schema}.{audit_table}"),
    append = TRUE,
    rownames = FALSE
  )
  


  message("Updated rows & audit records for ", toupper(tbl), " table \n",
          "\t Deleted rows: ",nrow(changed_rows$deleted_rows), "\n",
          "\t Added rows: ", nrow(changed_rows$new_rows), "\n",
          "\t Changed rows: ", nrow(changed_rows$changed_rows)
  )
}
