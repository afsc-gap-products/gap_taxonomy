## ============================
## 1) Setup & Utilities
## ============================

# Load necessary packages
# This function checks if packages are installed and installs them if not.
# It then loads all specified packages.
load_packages <- function(pkgs) {
  pkgs <- unique(pkgs)
  for (p in pkgs) {
    if (!require(p, character.only = TRUE)) {
      if (p == "gapindex") {
        devtools::install_github("afsc-gap-products/gapindex")
      } else {
        install.packages(p)
      }
      require(p, character.only = TRUE)
    }
  }
}

# The main list of packages required for the script
required_packages <- c(
  "tidyverse", "taxize", "janitor", "googledrive", "gapindex",
  "ritis", "worrms", "furrr", "future.apply", "getPass",
  "RODBC", "glue", "retry"
)
load_packages(required_packages)


# Set the current year
this_year <- format(Sys.Date(), "%Y")


# Load and preprocess static data
# This dataset contains information for taxa with only a common name
missing_taxa <- readr::read_csv("data/missing_taxa.csv") %>%
  dplyr::mutate(reason = "", action = "")


# Source helper scripts and data files
download_tables <- TRUE # Set to FALSE to skip downloading tables from Oracle

source("R/utils/functions.R")
source("R/utils/constants.R")
source("R/utils/download_tables.R") # Pulls proper tables from Oracle and loads into R




## ============================
## 2) Core Logic: Data Transformation & Taxonomy
## ============================

# Step 1: Prepare species data for taxonomic querying
# This section identifies newly described species and combines them with existing survey species.
# It then cleans the names for compatibility with 'taxize' functions.

new_spp <- race_species %>%
  dplyr::filter(!species_code %in% survey_species$species_code &
    species_name != "test subject" &
    !species_code %in% taxchanges_current$old_species_code)

fixed_spp <- bind_rows(survey_species, new_spp) %>%
  dplyr::mutate(
    survey_name = stringr::str_replace_all(species_name, "_", " "),
    fixed_name = furrr::future_map_chr(survey_name, clean_names)
  ) %>%
  dplyr::select(-species_name)


# Step 2: Get taxonomic information for each unique name
# This approach avoids redundant API calls by grouping by the species name
# before mapping the 'get_taxonomy_safely' function. Note: this will take a long
# time to run.
message("Fetching taxonomy data from external databases. This may take some time...")
taxonomy_combined <- fixed_spp %>%
  dplyr::mutate(name = coalesce(fixed_name, survey_name)) %>%
  dplyr::group_by(name) %>%
  tidyr::nest() %>%
  dplyr::mutate(taxo = furrr::future_map(name, get_taxonomy_safely)) %>%
  tidyr::unnest(data)


# Step 3: Clean and format the combined taxonomy data
# This block standardizes names and IDs, ensuring consistency across the dataset.
taxonomy_cleaned <- taxonomy_combined %>%
  tidyr::unnest(taxo) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(
    suggested_name = purrr::map2_chr(
      survey_name, database_name,
      ~ {
        if (grepl(RM_ALL, .x)) fix_names(.x, .y) else NA_character_
      }
    ),
    accepted_name = coalesce(suggested_name, database_name, fixed_name, survey_name),
    database_id = if_else(grepl(paste0(RM_BITS, "|sp\\. "), survey_name), NA, database_id),
    id_rank = if_else(grepl("sp\\. ", survey_name), "species", id_rank)
  )


## ============================
## 3) Data Processing & Output
## ============================

# Step 4: Create final classification and changes tables
# This complex pipeline handles synonymization, code fixing, and reason/action assignment.
taxonomy_final <- taxonomy_cleaned %>%
  dplyr::group_by(accepted_name) %>%
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>%
  tidyr::nest() %>%
  dplyr::mutate(new_code = furrr::future_map2_dbl(data, accepted_name, fix_syn_codes)) %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::mutate(
    old_code = species_code,
    species_code = dplyr::coalesce(new_code, old_code)
  ) %>%
  dplyr::arrange(species_code, database, desc(taxonomic_status == "accepted")) %>%
  dplyr::distinct(species_code, database, database_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    assign_reason_action(
      fixed_name, new_spp, taxonomic_status, survey_name, accepted_name,
      database_id, common_name, species_code, new_code, n_codes
    )
  ) %>%
  dplyr::group_by(species_code, database) %>%
  dplyr::mutate(n_names = n()) %>%
  dplyr::filter(!(n_names > 1 & survey_species_old == 0)) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(survey_species = assign_survey_species(database, database_id)) %>%
  dplyr::group_by(accepted_name) %>%
  dplyr::mutate(
    n_codes2 = dplyr::n_distinct(species_code),
    accepted_name = ifelse(n_codes2 != 1, survey_name, accepted_name)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(missing_taxa) %>%
  dplyr::select(
    reason, survey_species, survey_name,
    species_name = accepted_name, common_name,
    dplyr::all_of(c("species_code", "id_rank", "database", "database_id")),
    tidyselect::any_of(KEEP_RANKS), old_code, action
  )


# Step 5: Format tables for Oracle upload and QC
# The final tables are prepared, including renaming columns for consistency with Oracle.
classification_new <- taxonomy_final %>%
  dplyr::select(-c(survey_name, reason, action, old_code)) %>%
  dplyr::rename_with(~ paste0(., "_taxon"), tidyselect::all_of(KEEP_RANKS)) %>%
  dplyr::arrange(species_code)

classification_updates <- dplyr::bind_rows(
  # dplyr::anti_join(classification_current, classification_new),
  dplyr::anti_join(classification_new, classification_current)
) %>%
  dplyr::distinct(species_name, species_code, database, .keep_all = TRUE)
View(classification_updates) # manually check changes and ensure they are necessary

taxchanges_updates <- taxonomy_final %>%
  dplyr::filter(survey_species == 1 & reason != "" &
    survey_name != species_name | reason == "species code added" & survey_species == 1) %>%
  dplyr::mutate(
    year_changed = as.numeric(this_year),
    old_species_code = as.numeric(ifelse(reason == "species code added", NA_character_, old_code)),
    survey_name = ifelse(reason == "species code added", NA_character_, survey_name)
  ) %>%
  dplyr::select(
    old_species_name = survey_name, old_species_code,
    new_species_name = species_name, new_species_code = species_code,
    action, year_changed, reason
  ) %>%
  dplyr::arrange(dplyr::desc(year_changed), old_species_name)


# Step 6: Perform QC checks
# This is a crucial step to ensure data integrity before writing to the database.
qc_issues <- taxonomy_final %>%
  dplyr::group_by(species_name) %>%
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(n_survey_sp = sum(survey_species)) %>%
  dplyr::filter((n_codes != 1 & !is.na(species_name)) | (n_survey_sp > 1 & !is.na(species_name)))

# this happens when two undescribed species have been synonymized taxonomically (i.e. separate genera merged), so switch one sp. A to sp. B??
qc_issues2 <- taxonomy_final %>% dplyr::filter(reason != "" & survey_name == species_name &
  survey_species == 1 & reason != "species code added")

if (nrow(qc_issues) > 0 | nrow(qc_issues2) > 0) {
  stop("QC issues detected in taxonomy_final. Please review the 'qc_issues' table before proceeding.")
}

if (!all(classification_updates$species_code %in% taxchanges_updates$new_species_code)) {
  stop("Some changes in taxonomic_classification are not documented in taxonomic_changes. Review before proceeding.")
} # fix this so it doesn't include higher taxonomic level changes

if (!all(classification_new$species_code %in% c(race_species$species_code, taxchanges_updates$old_species_code))) {
  stop("Some species codes will be dropped from the database. Review before proceeding.")
}
