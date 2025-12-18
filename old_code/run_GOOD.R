
## ============================
## 2) Packages & sourcing
## ============================

load_packages <- function(pkgs) {
  pkgs <- unique(pkgs)
  for (p in pkgs) {
    if (!require(p, character.only = TRUE)) {
      if(p == "gapindex") {
        devtools::install_github("afsc-gap-products/gapindex")
      } else {
        install.packages(p)
        }
      require(p, character.only = TRUE)
    }
  }
}

load_packages(c(
  "tidyverse",
  "taxize",
  "janitor",
  "googledrive",
  "gapindex",
  "ritis",
  "worrms",
  "furrr",
  "future.apply",
  "getPass",
  "RODBC",
  "glue",
  "retry"
))



## ============================
## 3) Utilities
## ============================

this_year <- format(Sys.Date(), "%Y")

# Source helper scripts
source("R/functions.R")
source("R/constants.R")


# Source helper docs
# newly_described <- readr::read_csv("data/newly_described_species.csv") # newly described species
# manual_corrections <- readr::read_csv("data/manual_name_corrections.csv") # fixes typos in racebase species names
missing_taxa <- readr::read_csv("data/missing_taxa.csv") %>%
  dplyr::mutate(reason = "",  action = "") # includes taxonomic info for species_codes with only common names (i.e. species_name = NA)



## ============================
## 4) Core functionality
## ============================

# 4.1 Load RACE species codes for the current year (or from cache)
source("R/pull_load_tables.R") # pulls from Oracle and saves to CSVs


# 4.2 Clean names (typos, sp. , newly described, manual corrections)

# adding newly described species and checking spelling/fixing non conventional names for compatability with taxize functions
new_spp <- race_species %>%
  dplyr::filter(!species_code %in% survey_species$species_code &
                  species_name != "test subject" &
                  !species_code %in% taxchanges_current$old_species_code) 

fixed_spp <- bind_rows(survey_species, new_spp) %>%
  dplyr::mutate(survey_name = stringr::str_replace_all(species_name, "_", " "),
                fixed_name = furrr::future_map_chr(survey_name, check_spelling)) %>%
  dplyr::select(-species_name)



# # Apply manual corrections by name or by common_name (generally due to typos in our database); no longer needed if going off of taxonomic_classification table, will still need if going off of racebase since those species names have not been updated
# fixed_spp <- fixed_spp %>%
#   pivot_longer(
#     cols = c(survey_name, common_name),
#     names_to = "type",
#     values_to = "name"
#   ) %>%
#   left_join(manual_corrections, by = "name") %>%
#   mutate(
#     fixed_name = coalesce(fixed_name_manual, fixed_name),  # manual overrides
#     type = coalesce(type_manual, type)
#   ) %>%
#   select(-fixed_name_manual, -type_manual) %>%
#   pivot_wider(
#     names_from = type,
#     values_from = name
#   ) 
#   # filter(species_code != 99999999) # removing test case



# 4.3 Query a taxonomy database for each name
# getting taxonomic information while avoiding duplicate queries
taxonomy_combined <- fixed_spp %>% 
  mutate(name = coalesce(fixed_name, survey_name)) %>%
  dplyr::group_by(name) %>%
  tidyr::nest() %>%
  mutate(taxo = furrr::future_map(name, get_taxonomy_safely)) %>%
  tidyr::unnest(data)


# 4.4 Choose accepted names and clean up database info
taxonomy_cleaned <- taxonomy_combined %>% 
  tidyr::unnest(taxo) %>%  
  dplyr::mutate(
    suggested_name = purrr::map2_chr(
      survey_name, database_name,
      ~ if (grepl(RM_ALL, .x)) fix_names(.x, .y) else NA_character_
    )
  ) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(accepted_name = coalesce(suggested_name, database_name, fixed_name, survey_name),
                database_id   = if_else(
                  grepl(paste0(RM_BITS, "|sp\\. "), survey_name),
                  NA, database_id
                ),
                id_rank = if_else(
                  grepl("sp\\. ", survey_name),
                  "species", id_rank
                )
  ) 



# 4.5 Create final species classification table (Oracle shape); code fixes species_codes for species that have been synonymized and selected the name used in our survey 
taxonomy_final <- taxonomy_cleaned %>% 
  dplyr::group_by(accepted_name) %>%
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>%
  tidyr::nest() %>%
  dplyr::mutate(new_code = furrr::future_map2_dbl(data, accepted_name, fix_syn_codes)) %>%
  tidyr::unnest(cols = c(data)) %>% 
  dplyr::mutate(old_code = species_code,
                species_code = dplyr::coalesce(new_code, old_code)) %>%
  dplyr::arrange(species_code, database, desc(taxonomic_status == "accepted")) %>%
  dplyr::distinct(species_code, database, database_id, .keep_all = TRUE) %>%
  
  # assign reason and action
  dplyr::mutate(
    assign_reason_action(fixed_name, new_spp, taxonomic_status,
                         survey_name, accepted_name,
                         database_id, common_name,
                         species_code, new_code, n_codes)
  ) %>% 
  
  # assign survey species
  dplyr::group_by(species_code) %>% 
  dplyr::mutate(
    survey_species = assign_survey_species(database, database_id)
  ) %>%
  
  # Recheck accepted_name
  dplyr::group_by(accepted_name) %>%
  dplyr::mutate(
    n_codes = dplyr::n_distinct(species_code),
    accepted_name = ifelse(n_codes != 1, survey_name, accepted_name)
  ) %>%
  
  # Final cleanup & adding back in taxa without latin names (missing_taxa)
  dplyr::ungroup() %>%
  dplyr::bind_rows(missing_taxa) %>%
  dplyr::select(
    reason, survey_species, survey_name,
    species_name = accepted_name, common_name,
    dplyr::all_of(c("species_code", "id_rank", "database", "database_id")),
    tidyselect::any_of(KEEP_RANKS),
    old_code, action
  )  
  # dplyr::distinct(survey_name, species_name, species_code, database, .keep_all = TRUE) %>% 
  # dplyr::filter(!(survey_species == 0 & is.na(database) & !is.na(species_name))) 


# QC checks (should be empty)
qc_issues <- taxonomy_final %>%
  dplyr::group_by(species_name) %>%
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(n_survey_sp = sum(survey_species)) %>%
  dplyr::filter(n_codes != 1 & !is.na(species_name) | n_survey_sp > 1 & !is.na(species_name))

if (nrow(qc_issues) > 0) {
  warning("QC issues detected in taxonomy_final. Review before proceeding.")
}


# formatting final taxonomic classification table for Oracle
classification_new <- taxonomy_final %>%
  # dplyr::group_by(species_code) %>%
  # dplyr::mutate(n = sum(survey_species)) %>%
  # dplyr::filter(n != 0) %>%
  dplyr::select(-c(survey_name, reason, action, old_code)) %>%
  #dplyr::distinct(species_name, species_code, database_id, .keep_all = TRUE) %>%
  dplyr::rename_with(~ paste0(., "_taxon"), tidyselect::all_of(KEEP_RANKS)) %>%
  dplyr::arrange(species_code)


# 4.6 Compute taxonomic classification table updates
classification_updates <- dplyr::bind_rows(
  dplyr::anti_join(classification_current, classification_new),
  dplyr::anti_join(classification_new, classification_current)
)



# 4.7 Compute taxonomic changes table given this year's results
taxchanges_updates <- taxonomy_final %>%
    dplyr::filter(survey_species == 1 & reason != "") %>% 
    dplyr::mutate(
      year_changed = this_year,
      old_species_code = ifelse(reason == "species code added", NA_character_, old_code),
      survey_name = ifelse(reason == "species code added", NA_character_, survey_name)
      ) %>%
    dplyr::select(
      old_species_name = survey_name, old_species_code,
      new_species_name = species_name, new_species_code = species_code,
      action, year_changed, reason
    ) %>%
  dplyr::arrange(dplyr::desc(year_changed), old_species_name) 
  

if(!all(classification_updates$species_code %in% taxchanges_updates$new_species_code)) {
  warning("Some changes in taxonomic_classification are not documented in taxonomic_changes. Review before proceeding.")
}


# 4.8 Upload changes to existing Oracle tables
source("R/upload_tables.R") # uploads changes to Oracle tables
