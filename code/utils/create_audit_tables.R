## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Set up audit tables and triggers across Schemas
## Description:   Creates AUDIT_* tables ONLY IF they do not exist,
##                and creates/replaces triggers with verified 1:1 column mapping.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(here)

if (!exists("load_packages")) source(here::here("code", "utils", "load_packages.R"))
if (!exists("cfg")) source(here::here("code", "utils", "config.R"))
if (!exists("oracle_connect")) source(here::here("code", "utils", "functions.R"))
if (!exists("sync_all_audits")) source(here::here("code", "utils", "oracle_helpers.R"))


channel <- oracle_connect()

active_registry <- resolve_registry(tax_registry, cfg)

sync_all_audits(channel, active_registry, rebuild = cfg$rebuild_audit)
message("\nAudit infrastructure check complete.")
