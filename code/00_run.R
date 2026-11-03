## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       GAP_TAXONOMY standard production workflow
##
## Description:   This script orchestrates the end-to-end data pipelines for
##                the GAP_TAXONOMY database. It extracts authoritative taxonomy,
##                standardizes species synonyms and minimum ID groupings,
##                calculates survey identification confidence scores, updates
##                Oracle production tables, executes audit triggers, syncs
##                metadata comments, and publishes release changelogs.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 0) R Setup & Environment Checks ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(here) 

source(here("code", "utils", "load_packages.R"))
source(here("code", "utils", "config.R"))        
source(here("code", "utils", "functions.R"))
source(here("code", "utils", "oracle_helpers.R"))  

# changelog
dir.create(here("temp"), showWarnings = FALSE, recursive = TRUE)
writeLines(capture.output(sessionInfo()), con = here("temp", "session_info.txt"))


# connect to oracle and ensure connection is working properly
channel <- oracle_connect()


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1) Step 1: Update Taxonomy Classification & Changes ----
##    Downloads external taxonomy (WoRMS, ITIS), matches survey taxa,
##    and standardizes codes, accepted names, and synonyms.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here("code", "01_update_taxonomy_tables.R"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2) Step 2: Update Grouping & Stanza Tables ----
##    Expands minimum ID rules, maps species into taxon groups, and
##    assigns historical vs modern stanza bounds.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here("code", "02_update_grouping_tables.R"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3) Step 3: Update RACE Data Reference Tables ----
##    Synchronizes RACEBASE and RACE_DATA reference dependencies.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here("code", "03_update_race_data_tables.R"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4) Step 4: Update Identification Confidence Table ----
##    Calculates regional and yearly taxon confidence scores (1-3)
##    and imputes scores for new survey years.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here("code", "04_update_confidence_table.R"))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5) Step 5: Run tests ----
##    Runs tests to ensure data integrity
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here("tests", "run_tests.R"))

run_gap_tax_tests()                                             # offline, no database needed
run_gap_tax_tests(oracle = TRUE, scratch_schema = "SFRIEDMAN")  # adds Oracle tests, use scratch schema where you can write tables
run_gap_tax_tests(contract = TRUE)                              # after running steps 01-04 in the same session


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6) Step 6: Push Updates to Oracle & Audit ----
##    Creates temporary stage tables, applies PL/SQL MERGE procedures,
##    fires audit triggers in GAP_TAXONOMY, synchronizes column descriptions
##    from GAP_PRODUCTS.METADATA_COLUMN, and compiles report_changes.html.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
changelog_msg <- paste("
-- Synchronized taxonomy definitions with authoritative external databases.
-- Updated group stanzas and group composition through current survey year.
                       ")

cfg$dry_run <- TRUE # change to FALSE to actually make updates
source(here("code", "05_update_oracle.R"))
