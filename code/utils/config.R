## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## config.R -- single source of truth for the GAP_TAXONOMY pipeline
##
## tax_registry is listed PARENT -> CHILD. Merges insert/update in this order
## and delete in reverse order, so foreign keys are never violated.
##
##   mode        "upsert"      : INSERT / UPDATE / DELETE to match the new data
##               "insert_only" : only INSERT rows whose id_keys are not in Oracle
##   id_keys     columns that IDENTIFY a row (used to match new vs current).
##               A column that can change over time (e.g. ACTIVE) must NOT be here,
##               otherwise a change shows up as DELETE + INSERT instead of UPDATE.
##   audit_keys  columns written once (unpaired) in the AUDIT_ table. Everything
##               else is written as COL_CURRENT / COL_UPDATE pairs.
##   new/current names of R objects created by scripts 01-04. If `current` is
##               NULL or the object doesn't exist, it is read from Oracle.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Globals that scripts 01-04 already expect
cfg <- list(
  schema                = "SFRIEDMAN", # Oracle schema for taxonomic tables
  rebuild_audit         = TRUE, # TRUE = rename + recreate drifted AUDIT_ tables
  include_race_data     = FALSE, # TRUE = also update the two RACE_DATA tables
  create_missing_tables = FALSE, # TRUE = first-time load: create tables that don't exist yet
  dry_run               = FALSE, # TRUE = show/write deltas, change nothing
  max_delete_frac       = 0.05, # abort if > 5% of a table would be deleted
  allow_large_deletes   = FALSE # override for the guard above
)


tax_registry <- list(
  taxonomic_classification = list(
    mode       = "upsert",
    # One row per species_code x source database (x database_id, which may be NA).
    # ACTIVE and SPECIES_NAME are deliberately NOT keys, so changes to them are
    # UPDATEs and are audited as _CURRENT / _UPDATE pairs.
    id_keys    = c("species_code", "database", "database_id"),
    audit_keys = c("species_code", "database", "database_id"),
    new        = "taxonomic_classification_new",
    current    = "taxonomic_classification_current"
  ),
  taxonomic_changes = list(
    mode       = "insert_only",
    id_keys    = c("old_species_name", "old_species_code",
                   "new_species_name", "new_species_code"),
    audit_keys = c("old_species_name", "old_species_code",
                   "new_species_name", "new_species_code"),
    new        = "taxonomic_changes_new",
    current    = NULL,                                # read from Oracle
    fast_save  = FALSE                                # avoids ORA-01461 on long strings
  ),
  taxonomic_confidence = list(
    mode       = "insert_only",
    id_keys    = c("survey_definition_id", "year", "species_code"),
    audit_keys = c("survey_definition_id", "year", "species_code"),
    new        = "taxonomic_confidence_new",
    current    = "taxon_confidence_current"
  ),
  taxon_group_stanzas = list(                         # parent of composition
    mode       = "upsert",
    id_keys    = "group_stanza_id",
    audit_keys = "group_stanza_id",
    new        = "taxon_group_stanzas_new",
    current    = "taxon_group_stanzas_current"
  ),
  taxon_group_composition = list(                     # child of stanzas
    mode       = "upsert",
    id_keys    = c("group_stanza_id", "species_code"),
    audit_keys = c("group_stanza_id", "species_code"),
    new        = "taxon_group_composition_new",
    current    = "taxon_group_composition_current"
  ),
  
  ## --- RACE_DATA tables (parent -> child). Enable with cfg$include_race_data ---
  species_taxonomics = list(
    group      = "race_data",
    schema     = "RACE_DATA",
    mode       = "upsert",
    id_keys    = "species_taxonomic_id",
    audit_keys = "species_taxonomic_id",
    new        = "species_taxonomics_new",
    current    = "species_taxonomics_current",
    no_deletes = TRUE,                      # script 03 preserves every legacy node
    gate       = "race_qaqc_passed"
  ),
  race_species_codes = list(                # child: FK species_taxonomic_id
    group      = "race_data",
    schema     = "RACE_DATA",
    mode       = "upsert",
    id_keys    = "species_code",
    audit_keys = "species_code",
    new        = "race_species_codes_new",
    current    = "race_codes_current",      # name created in 03
    no_deletes = TRUE,
    gate       = "race_qaqc_passed"
  )
)

#' Drop disabled groups and fill in schema defaults.
resolve_registry <- function(registry, cfg) {
  if (!isTRUE(cfg$include_race_data)) {
    registry <- Filter(function(s) !identical(s$group, "race_data"), registry)
  }
  for (nm in names(registry)) {
    sp <- registry[[nm]]
    sp$schema       <- toupper(if (is.null(sp$schema)) cfg$schema else sp$schema)
    sp$stage_schema <- toupper(if (is.null(sp$stage_schema)) sp$schema else sp$stage_schema)
    sp$audit_schema <- toupper(if (is.null(sp$audit_schema)) sp$schema else sp$audit_schema)
    registry[[nm]] <- sp
  }
  registry
}
active_registry <- resolve_registry(tax_registry, cfg)


# Globals that scripts 01-04 already expect
tax_schema <- cfg$schema
this_year  <- format(Sys.Date(), "%Y")


# Taxonomic ranks to retain
KEEP_RANKS <- c(
  "genus",
  "subfamily", "family", "superfamily",
  "infraorder", "suborder", "order", "superorder",
  "infraclass", "subclass", "class", "superclass",
  "subphylum", "phylum",
  "kingdom"
)


# Regex suffix fragments
RM_BITS <- paste0(c(
  "\\(adult\\)", "\\(juvenile\\)", "eggs", "egg case", "egg",
  "hybrid", "larvae", "larva", "tubes", "group"
), collapse = "|")

# Master regex for removable suffixes
RM_ALL <- paste0(c(RM_BITS, " sp\\. cf\\.", " sp\\.", " cf\\."), collapse = "|")


# pre-defined taxa with known multiple matches in WoRMS/ITIS to avoid user prompts; feeds into get_taxonomy functions
defined_tax <- tibble::tribble(
  ~taxon, ~db_id, ~dbe,
  "Anthozoa", 51938, "itis",
  "Chondrichthyes", 159785, "itis",
  "Clavularia", 125286, "worms",
  "Selachii", 368408, "worms",
  "Selachii", 563987, "itis",
  "Vulcanella", 170325, "worms",
  "Ascidian", 1839, "worms",
  "Ascidian", 158854, "itis",
  "Acoelida", 914208, "itis",
  "Hirudinea", 69290, "itis",
  "Weberella", 659295, "itis",
  "Liparis", 167550, "itis",
  "Calliostoma canaliculatum", 467171, "worms",
  "Ctenophora", 1248, "worms",
  "Ctenophora", 53856, "itis",
  "Cyanea", 51669, "itis",
  "Echinacea", 157821, "itis",
  "Gonostomatidae", 125601, "worms",
  "Nuculana fossa", 254459, "worms",
  "Porella", 156418, "itis",
  "Porella", 110835, "worms",
  "Sebastes", 126175, "worms"
)


# manually created group temporal stanzas table for information prior to taxonomic workflow (2025)
historical_group_stanzas <- tibble::tribble(
  ~group_id, ~group_code, ~year_start, ~year_end,
  1, 10111, 1982, 1991,
  2, 30050, 1982, 2005,
  3, 30150, 1982, 1995,
  4, 405, 1982, 1987,
  5, 405, 1988, 1998,
  6, 400, 1982, 1998
)

# manually created group composition table for information prior to taxonomic workflow (2025)
historical_group_composition <- tibble::tribble(
  ~group_id, ~species_code,
  1, 10110,
  1, 10111,
  1, 10112,
  2, 30050,
  2, 30051,
  2, 30052,
  3, 30150,
  3, 30151,
  3, 30152,
  4, 405,
  4, 435,
  4, 455,
  4, 472,
  4, 480,
  5, 405,
  5, 435,
  5, 455,
  5, 472,
  6, 400,
  6, 471
)
