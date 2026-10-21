# GAP_TAXONOMY Schema Update

> Code is always in development  
>
> This document is for informational purposes only and does not necessarily represent the views or official position of the Department of Commerce, the National Oceanic and Atmospheric Administration, or the National Marine Fisheries Service. Not to be cited without permission from the authors.

---

## Maintainers

This code is primarily maintained by:

- **Sarah Friedman** (Sarah.Friedman AT noaa.gov; @SarahFriedman-NOAA)  

Alaska Fisheries Science Center  
National Marine Fisheries Service  
National Oceanic and Atmospheric Administration  
Seattle, WA 98195  

---

## Overview

This repository include code to update the taxonomy tables in `RACE_DATA` and `GAP_TAXONOMY` schema based on online databases (namely WORMS and ITIS) to maintain modern, reliable, and consistent taxonomy. It will be run annually in January.

---

## Application Structure

The scripts to regenerate our tables are sequentially numbered and are intended to run in a particular order: 

### General Flow of the code

- `R/01_update_taxonomy_tables.R`  
  Pulls updated taxonomic information for each species from both ITIS and WoRMS and reconciles taxonomic discrepancies. Generates the `taxonomic_classification` and `taxonomic_changes` tables

- `R/02_update_grouping_tables.R`  
  Leverages updated taxonomic information to regenerate the `taxon_group_stanzas` and `taxon_group_composition` tables. These are primarily used by the `gap_products` repository to generate indices for stock assessment. 

- `R/03_update_race_data_tables.R`  
  Generates hierarchical taxonomic tables (`RACE_species_codes`, `species_taxonomics`) in `RACE_DATA` based on updated taxonomy.

- `R/04_update_confidence_table.R`  
  Updates `taxonomic_confidence` table for the most recent year. Duplicates values from the previous year for each species code.
  
- `R/05_update_oracle.R`  
  Pushes all table updates to Oracle and creates audit records of changes made to each table. 
  
---

# NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law.

Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

---

# NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105).

The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

---

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) |  
[National Oceanographic and Atmospheric Administration](https://www.noaa.gov) |  
[NOAA Fisheries](https://www.fisheries.noaa.gov/)