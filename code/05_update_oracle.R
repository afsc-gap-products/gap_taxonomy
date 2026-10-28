## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Update Taxonomic Tables in Target Schema
## Description:   Pushes taxonomic updates into Oracle stage tables, merges
##                changes into production tables, fires audit triggers,
##                and summarizes the run.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

channel <- oracle_connect()
source("code/utils/sync_metadata.R")

time_start <- format(Sys.time(), "%d-%b-%y %I.%M.%S %p")
timestamp <- format(Sys.time(), "%Y-%m-%d")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1. Prepare Staging Data (Mapped to explicit schema and table targets)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stage_payloads <- list(
  # --- tax_schema (GAP_TAXONOMY / personal test schema) ---
  STAGE_TAXONOMIC_CLASSIFICATION = list(
    schema = tax_schema,
    data = prepare_delta(
      taxonomic_classification_new,
      taxonomic_classification_current,
      c("active", "species_name", "species_code")
    )
  ),
  STAGE_TAXONOMIC_CHANGES = list(
    schema = tax_schema,
    data   = taxonomic_changes_new %>% dplyr::mutate(OPERATION = "INSERT")
  ),
  STAGE_TAXONOMIC_CONFIDENCE = list(
    schema = tax_schema,
    data = prepare_delta(
      taxonomic_confidence_new,
      taxon_confidence_current,
      c("survey_definition_id", "year", "species_code")
    )
  ),
  STAGE_TAXON_GROUP_STANZAS = list(
    schema = tax_schema,
    data = prepare_delta(
      taxon_group_stanzas_new,
      taxon_group_stanzas_current,
      c("group_stanza_id")
    )
  ),
  STAGE_TAXON_GROUP_COMPOSITION = list(
    schema = tax_schema,
    data = prepare_delta(
      taxon_group_composition_new,
      taxon_group_composition_current,
      c("group_stanza_id", "species_code")
    )
  )

  # # --- RACE_DATA tables ---
  # , STAGE_SPECIES_TAXONOMICS = list(
  #   schema = "RACE_DATA",
  #   data   = prepare_delta(
  #     species_taxonomics_new,
  #     species_taxonomics_current,
  #     c("species_taxonomic_id")
  #   )
  # ),
  # STAGE_RACE_SPECIES_CODES = list(
  #   schema = "RACE_DATA",
  #   data   = prepare_delta(
  #     race_species_codes_new,
  #     race_species_codes_current,
  #     c("species_code")
  #   )
  # )
)

## Clean column names and ASCII strings before uploading
stage_payloads <- lapply(stage_payloads, function(item) {
  item$data <- item$data %>%
    dplyr::rename_with(toupper) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_trim(iconv(.x, to = "ASCII//TRANSLIT")))) %>%
    as.data.frame()
  return(item)
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2. Upload to Oracle STAGE_* Tables
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (stage_tbl in names(stage_payloads)) {
  target_schema <- stage_payloads[[stage_tbl]]$schema
  df <- stage_payloads[[stage_tbl]]$data
  tbl_path <- paste0(target_schema, ".", stage_tbl)

  # Empty existing stage table
  run_sql(channel, paste0("TRUNCATE TABLE ", tbl_path))

  if (nrow(df) > 0) {
    # fast = FALSE for STAGE_TAXONOMIC_CHANGES prevents ORA-01461 string-binding errors
    is_fast <- stage_tbl != "STAGE_TAXONOMIC_CHANGES"

    RODBC::sqlSave(
      channel   = channel,
      dat       = df,
      tablename = tbl_path,
      append    = TRUE,
      rownames  = FALSE,
      safer     = FALSE,
      fast      = is_fast
    )
    cat(nrow(df), "records uploaded to", tbl_path, "\n")
  } else {
    cat("0 records to upload for", tbl_path, "\n")
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3. Merge Changes from STAGE_* to Production Tables
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tables_to_merge <- list(
  # --- tax_schema tables ---
  list(schema = tax_schema, table = "taxonomic_classification"),
  list(schema = tax_schema, table = "taxonomic_changes"),
  list(schema = tax_schema, table = "taxonomic_confidence"),
  list(schema = tax_schema, table = "taxon_group_stanzas"),
  list(schema = tax_schema, table = "taxon_group_composition")

  # # --- RACE_DATA tables (uncomment when ready to merge) ---
  # , list(schema = "RACE_DATA", table = "species_taxonomics"),
  # list(schema = "RACE_DATA", table = "race_species_codes")
)

for (item in tables_to_merge) {
  cur_schema <- item$schema
  tbl <- item$table
  stage_name <- paste0("STAGE_", toupper(tbl))

  n_recs <- nrow(stage_payloads[[stage_name]]$data)

  if (!is.null(n_recs) && n_recs > 0) {
    cat(sprintf("\nMerging %d changes into %s.%s...\n", n_recs, cur_schema, toupper(tbl)))

    # Path routing for SQL procedure files
    if (cur_schema == "RACE_DATA") {
      sql_file <- paste0("code/sql_procedures/merge_race_data_", tbl, "_changes.sql")
    } else {
      sql_file <- paste0("code/sql_procedures/merge_", tbl, "_changes.sql")
    }

    if (!file.exists(sql_file)) {
      stop(sprintf("SQL merge file does not exist: %s", sql_file))
    }

    # Read script and dynamically inject target schema
    plsql_script <- paste(readLines(sql_file), collapse = "\n")
    plsql_script <- gsub("\\{SCHEMA\\}", cur_schema, plsql_script)

    res <- RODBC::sqlQuery(channel, plsql_script)

    if (is.character(res) && any(grepl("ERROR|ORA-", res, ignore.case = TRUE))) {
      stop(sprintf("Merge failed for %s.%s:\n%s", cur_schema, tbl, paste(res, collapse = "\n")))
    }
    cat(sprintf("Procedure completed successfully for %s.%s\n", cur_schema, toupper(tbl)))
  } else {
    cat(sprintf("No records to change for %s.%s\n", cur_schema, toupper(tbl)))
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4. Synchronize Metadata Comments
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sync_metadata(conn = channel, target_schema = tax_schema, 
              meta_schema = "GAP_PRODUCTS")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5. Query Audit Tables & Render Changelog
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
time_end <- format(Sys.time(), "%d-%b-%y %I.%M.%S %p")

audit_summary <- summarize_all_updates(
  channel    = channel,
  tax_schema = tax_schema,
  time_start = time_start,
  time_end   = time_end
)

cat("\nSummary of All Audit Updates:\n")
print(audit_summary)

# Render HTML changelog report
rmarkdown::render(
  input = "code/report_gap_taxonomy_changes.Rmd",
  output_format = "html_document",
  output_file = paste0("../temp/report_changes_", timestamp, ".html"),
  params = list(
    detailed_notes    = detailed_notes,
    run_user          = Sys.getenv("USER"),
    timestamp         = timestamp,
    audit_summary     = audit_summary,
    taxonomic_changes = stage_payloads$STAGE_TAXONOMIC_CHANGES$data
  )
)

# Archive plain text summary to news/
archive_dir <- "news"
if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)

fs::file_copy(
  path      = "temp/report_changes.txt",
  new_path  = file.path(archive_dir, paste0(timestamp, ".txt")),
  overwrite = TRUE
)

cat(sprintf("Changelog successfully generated and archived in %s/\n", archive_dir))

