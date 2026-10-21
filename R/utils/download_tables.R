# Oracle connection helpers
tables <- c("race_species_codes", "species_taxonomics", "taxonomic_ranks",
            "taxonomic_classification", "taxonomic_changes", "species_year",
            "taxonomic_confidence", "survey_design")


# Map Oracle tables to local filenames
table_lookup <- tibble::tibble(
  table_key = tables,
  oracle_table = c(
    "RACE_DATA.RACE_SPECIES_CODES",
    "RACE_DATA.SPECIES_TAXONOMICS",
    "RACE_DATA.TAXONOMIC_RANKS",
    "GAP_TAXONOMY.TAXONOMIC_CLASSIFICATION",
    "GAP_TAXONOMY.TAXONOMIC_CHANGES",
    "GAP_TAXONOMY.TAXONOMIC_CONFIDENCE",
   # "GAP_PRODUCTS.SPECIES_YEAR",
    "GAP_PRODUCTS.SURVEY_DESIGN"
  ),
  file_path = file.path("data/oracle", paste0(this_year, "_", tables, ".csv"))
)

# Determine whether each file exists
table_lookup <- table_lookup %>%
  dplyr::mutate(
    exists = if (download_tables) FALSE else file.exists(file_path)
  )

# Connect to Oracle if any table missing
if (any(!table_lookup$exists)) {
  
  message("Connecting to Oracle to fetch missing tables…")
  
  if(!exists("channel")){
    channel <- gapindex::get_connected()
  }
  
  # Loop over tables and download missing ones
  for(i in seq_len(nrow(table_lookup))) {
    
    if (!table_lookup$exists[i]) {
      
      message("Downloading ", table_lookup$table_key[i])
      
      dat <- RODBC::sqlQuery(
        channel,
        paste("SELECT * FROM", table_lookup$oracle_table[i])
      ) %>%
        tibble::as_tibble() %>%
        janitor::clean_names()
      
      readr::write_csv(dat, table_lookup$file_path[i])
    }
  }
}



# ---- Load all CSVs into a named list ----
oracle_tables <- purrr::map(
  table_lookup$file_path,
  ~ readr::read_csv(.x, show_col_types = FALSE) %>%
    janitor::clean_names()
)

names(oracle_tables) <- table_lookup$table_key


# ---- Extract specific tables for workflow ----

race_species <- oracle_tables$race_species_codes %>%
  dplyr::select(
    species_name,
    species_code,
    common_name
  ) %>%
  dplyr::filter(species_code != 99999999)   # removing test species

species_taxonomics_current <- oracle_tables$species_taxonomics 

taxonomic_ranks <- oracle_tables$taxonomic_ranks 

taxonomic_classification_current <- oracle_tables$taxonomic_classification

taxonomic_changes_current <- oracle_tables$taxonomic_changes

species_year_current <- oracle_tables$species_year


survey_species <- taxonomic_classification_current %>%
  dplyr::select(
    survey_species_old = active,
    species_name:species_code
  ) %>%
  dplyr::filter(!species_code %in% missing_taxa$species_code) %>%
  unique()

survey_years <- oracle_tables$survey_design

# manual clunky fix; ok to delete after 2026
survey_species$species_name[survey_species$species_code == 98001] <- "Ascidiacea sp. A"
survey_species$species_name[survey_species$species_code == 98002] <- "Ascidiacea sp. B"
