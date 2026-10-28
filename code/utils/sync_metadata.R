## =============================================================================
## Function: sync_metadata
## Purpose: Pull metadata from GAP_PRODUCTS and apply as Oracle comments via RODBC
## =============================================================================

sync_metadata <- function(conn,
                          target_schema = "GAP_TAXONOMY",
                          meta_schema = "GAP_PRODUCTS") {
  message("--- Synchronizing Column Comments from GAP_PRODUCTS.METADATA_COLUMN ---")

  # 1. Fetch metadata definitions using RODBC::sqlQuery
  meta_lookup <- RODBC::sqlQuery(
    channel = conn,
    query   = sprintf("
      SELECT UPPER(TRIM(METADATA_COLNAME)) AS METADATA_COLNAME,
             METADATA_COLNAME_DESC
      FROM %s.METADATA_COLUMN
      WHERE METADATA_COLNAME_DESC IS NOT NULL
    ", meta_schema)
  )

  if (is.character(meta_lookup) && any(grepl("ERROR|ORA-", meta_lookup, ignore.case = TRUE))) {
    stop(sprintf(
      "Failed to retrieve metadata from %s.METADATA_COLUMN:\n%s",
      meta_schema, paste(meta_lookup, collapse = "\n")
    ))
  }

  colnames(meta_lookup) <- toupper(colnames(meta_lookup))

  # 2. Query target schema columns in ALL_TAB_COLUMNS
  tax_cols <- RODBC::sqlQuery(
    channel = conn,
    query   = sprintf("
      SELECT TABLE_NAME, COLUMN_NAME
      FROM ALL_TAB_COLUMNS
      WHERE OWNER = UPPER('%s')
        AND TABLE_NAME IN ('TAXONOMIC_CLASSIFICATION', 'TAXONOMIC_CHANGES',
                           'TAXONOMIC_CONFIDENCE', 'TAXON_GROUP_STANZAS',
                           'TAXON_GROUP_COMPOSITION')
    ", target_schema)
  )

  if (is.character(tax_cols) && any(grepl("ERROR|ORA-", tax_cols, ignore.case = TRUE))) {
    stop(sprintf(
      "Failed to retrieve target table columns from %s:\n%s",
      target_schema, paste(tax_cols, collapse = "\n")
    ))
  }

  colnames(tax_cols) <- toupper(colnames(tax_cols))

  # 3. Join on matching column names
  matches <- dplyr::inner_join(
    tax_cols,
    meta_lookup,
    by = c("COLUMN_NAME" = "METADATA_COLNAME")
  )

  # 4. Apply COMMENT statements
  for (i in seq_len(nrow(matches))) {
    tbl <- matches$TABLE_NAME[i]
    col <- matches$COLUMN_NAME[i]
    desc <- gsub("'", "''", matches$METADATA_COLNAME_DESC[i])

    comment_sql <- sprintf("COMMENT ON COLUMN %s.%s.%s IS '%s'", target_schema, tbl, col, desc)
    res <- RODBC::sqlQuery(conn, comment_sql)

    if (is.character(res) && any(grepl("ERROR|ORA-", res, ignore.case = TRUE))) {
      warning(sprintf("Could not apply comment on %s.%s.%s: %s", target_schema, tbl, col, res))
    }
  }

  message(sprintf("Successfully added/updated column comments on %d columns.", nrow(matches)))
}
