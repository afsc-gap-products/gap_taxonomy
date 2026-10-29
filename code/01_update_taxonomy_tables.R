# Purpose: Create updated taxonomic classification and changes tables for Oracle database
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ============================
## 1) Setup & Utilities
## ============================

# Load and preprocess static data
# This dataset contains information for taxa with only a common name
missing_taxa <- readr::read_csv("data/missing_taxa.csv") %>%
  dplyr::mutate(reason = "", action = "")

source(here("code", "utils", "download_tables.R")) # Pulls proper tables from Oracle and loads into R

## ============================
## 2) Core Logic: Data Transformation & Taxonomy
## ============================

# This section identifies newly described species and combines them with existing survey species.
# It then cleans the names for compatibility with 'taxize' functions.

new_spp <- race_species %>%
  dplyr::filter(!species_code %in% survey_species$species_code &
    species_name != "test subject" &
    !species_code %in% taxonomic_changes_current$old_species_code)

fixed_spp <- bind_rows(survey_species, new_spp) %>%
  dplyr::mutate(
    survey_name = stringr::str_replace_all(species_name, "_", " "),
    fixed_name = furrr::future_map_chr(survey_name, standardize_names)
  ) %>%
  dplyr::select(-species_name)

fixed_spp[fixed_spp$species_code == 70050, "fixed_name"] <- c("Neomeniamorpha", "Neomeniomorpha")


## ============================================================
# 3) Get taxonomic information for each unique name
## ============================================================

# This approach avoids redundant API calls by grouping by the species name
# before mapping the 'get_taxonomy_safely' function. Note: this will take a long
# time to run.

message("Fetching taxonomy data from external databases. This may take some time...")
taxonomy_combined <- fixed_spp %>%
  dplyr::mutate(name = coalesce(fixed_name, survey_name)) %>%
  dplyr::group_by(name) %>%
  tidyr::nest() %>%
  dplyr::mutate(taxo = furrr::future_map(name, get_taxonomy_safely))


# Re-query loop for failed API hits (missing database info)
max_reruns <- 3
rerun_count <- 0

while (rerun_count < max_reruns) {
  # Find names that failed to return valid database records
  failed_names <- taxonomy_combined %>%
    dplyr::filter(purrr::map_lgl(taxo, ~ all(is.na(.x$database)))) %>%
    dplyr::pull(name) %>%
    unique()

  if (length(failed_names) == 0) break

  rerun_count <- rerun_count + 1
  message(glue::glue("Re-querying {length(failed_names)} failed taxa (Attempt {rerun_count}/{max_reruns})..."))

  # Run retry queries sequentially (purrr::map) to avoid hitting rate limits
  retry_results <- tibble::tibble(
    name = failed_names,
    taxo_new = purrr::map(failed_names, function(x) {
      Sys.sleep(1) # 1-second pause between retries
      get_taxonomy_safely(x)
    })
  )

  # Update original taxonomy table with new attempts
  taxonomy_combined <- taxonomy_combined %>%
    dplyr::left_join(retry_results, by = "name") %>%
    dplyr::mutate(
      taxo = purrr::map2(taxo, taxo_new, ~ if (all(is.na(.x$database)) && !is.null(.y)) .y else .x)
    ) %>%
    dplyr::select(-taxo_new)
}

taxonomy_combined <- taxonomy_combined %>% tidyr::unnest(data)


## ============================================================
# 4) Clean and format the combined taxonomy data
## ============================================================

# This block standardizes names and IDs, ensuring consistency across the dataset
taxonomy_cleaned <- taxonomy_combined %>%
  tidyr::unnest(taxo) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(
    suggested_name = purrr::map2_chr(
      survey_name, database_name,
      ~ {
        if (grepl(paste0(RM_ALL, "| var\\."), .x)) fix_names(.x, .y) else NA_character_
      }
    ),
    accepted_name = coalesce(suggested_name, database_name, fixed_name, survey_name),
    database_id = ifelse(grepl(paste0(RM_BITS, "|sp\\. "), survey_name), NA, database_id),
    id_rank = ifelse(grepl("sp\\. ", survey_name), "species", id_rank),
    id_rank = ifelse(grepl("sp\\. egg", survey_name), "genus", id_rank)
  )


## ============================
## 5) Data Processing & Output
## ============================

# Create final classification and changes tables
# This complex pipeline handles synonymization, code fixing, and reason/action assignment.
taxonomy_final <- taxonomy_cleaned %>%
  dplyr::group_by(accepted_name) %>%
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>%
  tidyr::nest() %>%
  dplyr::mutate(new_code = purrr::map2_dbl(data, accepted_name, fix_syn_codes)) %>%
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
  dplyr::filter(!(n_names > 1 & active_old == 0)) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(active = assign_active_status(database, database_id)) %>%
  dplyr::group_by(accepted_name) %>%
  dplyr::mutate(
    n_codes2 = dplyr::n_distinct(species_code),
    accepted_name = ifelse(n_codes2 != 1, survey_name, accepted_name)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(missing_taxa) %>%
  dplyr::select(
    reason, active, survey_name,
    species_name = accepted_name, common_name,
    dplyr::all_of(c("species_code", "id_rank", "database", "database_id")),
    tidyselect::any_of(KEEP_RANKS), old_code, action
  )


## ============================================================
# 6) Format tables for Oracle upload and QC
## ============================================================

# The final tables are prepared, including renaming columns for consistency with Oracle.
taxonomic_classification_new <- taxonomy_final %>%
  # dplyr::filter(reason != "nomen nudum") %>%
  dplyr::select(-c(survey_name, reason, action, old_code)) %>%
  dplyr::rename_with(~ paste0(., "_taxon"), tidyselect::all_of(KEEP_RANKS)) %>%
  dplyr::arrange(species_code)

taxonomic_classification_updates <- dplyr::bind_rows(
  # dplyr::anti_join(classification_current, classification_new),
  dplyr::anti_join(taxonomic_classification_new, taxonomic_classification_current)
) %>%
  dplyr::distinct(species_name, species_code, database, .keep_all = TRUE)
# View(taxonomic_classification_updates) # manually check changes and ensure they are necessary

taxonomic_changes_new <- taxonomy_final %>%
  dplyr::filter(active == 1 & reason != "" &
    survey_name != species_name | reason == "species code added" & active == 1) %>%
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


taxonomic_changes_new <- taxonomic_changes_new %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character), 
      ~ stringr::str_trim(iconv(.x, to = "ASCII//TRANSLIT"))
    )
  )

if (nrow(taxonomic_changes_new) > 0) {
  taxonomic_changes_new <- taxonomic_changes_new %>%
    bind_rows(taxonomic_changes_current) %>%
    dplyr::group_by(old_species_name, old_species_code, new_species_name, new_species_code) %>%
    # Keep only the row with the earliest year_changed per group
    dplyr::slice_min(order_by = year_changed, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(year_changed))
} else {
  print("No taxonomic changes to be made this year.")
  taxonomic_changes_new <- taxonomic_changes_new %>%
    mutate(
      old_species_name = character(),
      old_species_code = numeric(),
      new_species_name = character(),
      new_species_code = numeric(),
      action = character(),
      year_changed = numeric(),
      reason = character()
    )
}


## ============================================================
# 7) Perform QC checks
## ============================================================

# This is a crucial step to ensure data integrity before writing to the database

qc_issues <- taxonomy_final %>%
  dplyr::group_by(species_name) %>%
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(
    n_rows = n(),
    n_survey_sp = sum(active)
  ) %>%
  dplyr::mutate(flag = case_when(
    is.na(active) ~ "No active status",
    active == 1 & reason == "nomen nudum" ~ "Deleted code, manual check required",
    n_codes != 1 & !is.na(species_name) ~ "Multiple codes for same species name",
    n_rows > 2 ~ "More than two rows present with same code",
    n_survey_sp > 1 & !is.na(species_name) ~ "Multiple active rows with same code",
    reason != "" & reason != "species code added" &
      survey_name == species_name & active == 1 ~ "Two undescribed species synonymized",
    kingdom != "Animalia" ~ "Non-animal taxon in database",
    is.na(kingdom) & !grepl("unsorted", common_name) ~ "No taxonomic info found",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(flag)) %>%
  arrange(species_code) %>%
  select(active, survey_name:species_code, flag)


if (nrow(qc_issues) > 0) {
  print(qc_issues)
  stop("Fix quality control issues before proceeding")
}


if (!all(race_species$species_code %in%
  c(taxonomic_classification_new$species_code, taxonomic_changes_new$old_species_code))) {
  code_check <- which(!race_species$species_code %in%
    c(taxonomic_classification_new$species_code, taxonomic_changes_new$old_species_code))
  print(race_species[code_check, ])
  stop("Some species codes will be dropped from the database. Review before proceeding.")
}
