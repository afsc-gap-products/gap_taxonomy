# GAP_TAXONOMY Schema Update

> Code is always in development
>
> This document is for informational purposes only and does not necessarily represent the views or official position of the Department of Commerce, the National Oceanic and Atmospheric Administration, or the National Marine Fisheries Service. Not to be cited without permission from the authors.

------------------------------------------------------------------------

## Maintainers

This code is primarily maintained by:

**Sarah Friedman** (Sarah.Friedman AT noaa.gov; @SarahFriedman-NOAA)

Alaska Fisheries Science Center\
National Marine Fisheries Service\
National Oceanic and Atmospheric Administration\
Seattle, WA 98195

------------------------------------------------------------------------

## Overview

This repository include code to update the taxonomy tables in `RACE_DATA` and `GAP_TAXONOMY` schema based on online databases (namely WORMS and ITIS) to maintain modern, reliable, and consistent taxonomy. It will be run annually in January.

### Table Structure

+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Schema             | Table Name                 | Purpose & Contents                                                                                                                                                                                                   |
+:===================+:===========================+:=====================================================================================================================================================================================================================+
| **`GAP_TAXONOMY`** | `TAXONOMIC_CLASSIFICATION` | Authoritative hierarchical classification for all bottom trawl survey taxa across all major ranks (Kingdom down to Subspecies). Integrates external identifiers (WoRMS AphiaID, ITIS TSN) and accepted nomenclature. |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`GAP_TAXONOMY`** | `TAXONOMIC_CHANGES`        | Audit history and mapping log documenting nomenclatural updates, reclassifications, code changes, splits, and synonymizations across survey years.                                                                   |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`GAP_TAXONOMY`** | `TAXONOMIC_CONFIDENCE`     | Region- and year-specific field identification confidence ratings (levels 1–3) assigned to taxa across standard survey series (e.g., EBS, AI, GOA).                                                                  |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`GAP_TAXONOMY`** | `TAXON_GROUP_STANZAS`      | Temporal definitions and validity windows (`YEAR_START` to `YEAR_END`) for analytical taxon aggregations, separating historical baseline periods from modern groupings.                                              |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`GAP_TAXONOMY`** | `TAXON_GROUP_COMPOSITION`  | Relational bridge linking specific survey `SPECIES_CODE` records to their designated `GROUP_STANZA_ID` aggregate definitions.                                                                                        |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`GAP_TAXONOMY`** | `AUDIT_*`                  | Change-data-capture tables tracking DML transactions (`INSERT`, `UPDATE`, `DELETE`) with timestamps, database/proxy users, and split `_CURRENT` vs. `_UPDATE` values.                                                |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`RACE_DATA`**    | `SPECIES_TAXONOMICS`       | Recursive parent-child tree capturing the full taxonomic hierarchy within the `RACE_DATA` schema, standardizing rank IDs and internal tree references.                                                               |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`RACE_DATA`**    | `RACE_SPECIES_CODES`       | Production look-up linking RACE species survey codes to accepted binomials, common names, survey activity flags, and the taxonomic tree.                                                                             |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| **`RACE_DATA`**    | `AUDIT_*`                  | Transaction logs mirroring `SPECIES_TAXONOMICS` and `RACE_SPECIES_CODES` updates and lineage modifications for schema provenance.                                                                                    |
+--------------------+----------------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

------------------------------------------------------------------------

## Application Structure

The scripts to regenerate our tables are sequentially numbered and are intended to run in a particular order:

### General Flow of the code

- `R/01_update_taxonomy_tables.R`\
  Pulls updated taxonomic information for each species from both ITIS and WoRMS and reconciles taxonomic discrepancies. Generates the `taxonomic_classification` and `taxonomic_changes` tables

- `R/02_update_grouping_tables.R`\
  Leverages updated taxonomic information to regenerate the `taxon_group_stanzas` and `taxon_group_composition` tables. These are primarily used by the `gap_products` repository to generate indices for stock assessment.

- `R/03_update_race_data_tables.R`\
  Generates hierarchical taxonomic tables (`RACE_species_codes`, `species_taxonomics`) in `RACE_DATA` based on updated taxonomy.

- `R/04_update_confidence_table.R`\
  Updates `taxonomic_confidence` table for the most recent year. Duplicates values from the previous year for each species code.

- `R/05_update_oracle.R`\
  Pushes all table updates to Oracle and creates audit records of changes made to each table.

------------------------------------------------------------------------

# NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law.

Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

------------------------------------------------------------------------

# NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105).

The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \|\
[National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|\
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
