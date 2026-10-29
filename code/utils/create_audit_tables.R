## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Set up audit tables and triggers across Schemas
## Description:   Creates AUDIT_* tables ONLY IF they do not exist,
##                and creates/replaces triggers with verified 1:1 column mapping.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

channel <- oracle_connect()

if (!exists("cfg")) source(here::here("code", "utils", "config.R"))
if (!exists("sync_all_audits")) {
  source(here::here("code", "utils", "functions.R"))
  source(here::here("code", "utils", "oracle_helpers.R"))
}

channel <- oracle_connect()
sync_all_audits(channel, cfg$schema, active_registry, rebuild = cfg$rebuild_audit)
message("\nAudit infrastructure check complete.")
