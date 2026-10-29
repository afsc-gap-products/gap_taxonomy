## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:     Update Taxonomic Tables in Target Schema
## Description: Computes deltas, loads STAGE_* tables, merges everything in one
##              transaction (audit triggers fire), and summarizes the run.
##              Table list, keys and merge order all come from tax_registry.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
channel <- oracle_connect()
source(here::here("code", "utils", "sync_metadata.R"))

timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

## 1. Compute deltas (read-only) and review them
##    (active_registry already excludes RACE_DATA unless cfg$include_race_data)
payloads <- build_stage_payloads(
  channel, active_registry,
  max_delete_frac     = cfg$max_delete_frac,
  allow_large_deletes = cfg$allow_large_deletes,
  create_missing      = cfg$create_missing_tables
)
print_delta_summary(payloads, out_dir = here::here("temp"))

if (isTRUE(cfg$dry_run)) {
  for (p in payloads) if (!is.null(p$create_sql)) cat("\n-- Would create:\n", p$create_sql, "\n", sep = "")
  message("\nDRY RUN: nothing was written. Deltas saved to temp/delta_*.csv")
} else {
  
  ## 2a. First-time load: create any missing tables (parent -> child)
  create_missing_tables(channel, payloads)
  
  ## 2. Make sure audit tables/triggers exist and match the production tables
  ##    (skipped in dry runs because this creates/replaces database objects)
  sync_all_audits(channel, active_registry, rebuild = cfg$rebuild_audit)
  
  time_start <- get_db_time(channel)
  
  ## 3. Load STAGE_* tables (permanent tables, truncated each run)
  for (p in payloads) load_stage_table(channel, p)
  
  ## 4. Apply all changes in a single transaction (rolls back on any error)
  apply_changes(channel, payloads)
  
  ## 5. Metadata comments (one call per schema that was updated)
  for (sch in unique(vapply(payloads, function(p) p$schema, character(1)))) {
    sync_metadata(conn = channel, target_schema = sch, meta_schema = "GAP_PRODUCTS")
  }
  
  ## 6. Audit summary + changelog
  time_end <- get_db_time(channel)
  audit_summary <- summarize_all_updates(channel, payloads, time_start, time_end)
  cat("\nSummary of All Audit Updates:\n"); print(audit_summary)
  
  notes <- if (exists("detailed_notes")) detailed_notes else changelog_msg
  
  rmarkdown::render(
    input         = here::here("code", "report_taxonomy_changes.Rmd"),
    output_format = "html_document",
    output_file   = paste0("../temp/report_changes_", timestamp, ".html"),
    params = list(
      detailed_notes    = notes,
      run_user          = Sys.getenv("USER"),
      timestamp         = timestamp,
      audit_summary     = audit_summary,
      taxonomic_changes = payloads$taxonomic_changes$data,
      group_stanzas     = payloads$taxon_group_stanzas$data,
      group_composition = payloads$taxon_group_composition$data
    )
  )
  
  archive_dir <- here::here("news")
  dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)
  fs::file_copy(here::here("temp", "report_changes.txt"),
                file.path(archive_dir, paste0(timestamp, ".txt")), overwrite = TRUE)
  message(sprintf("Changelog generated and archived in %s/", archive_dir))
}
