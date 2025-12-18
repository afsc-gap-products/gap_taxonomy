# Oracle connection helpers
path_oracle_connect <- "Z:/Projects/ConnectToOracle.R" 


# Filenames
nm_species <- file.path("data", paste0(this_year, "_race_species_codes.csv"))
nm_tax     <- file.path("data", paste0(this_year, "_taxonomic_classification.csv"))
nm_changes <- file.path("data", paste0(this_year, "_taxonomic_changes.csv"))

# Check which CSVs exist
if(download_tables){
  species_exists <- tax_exists  <- changes_exists <- FALSE
} else {
  species_exists <- file.exists(nm_species)
  tax_exists     <- file.exists(nm_tax)
  changes_exists <- file.exists(nm_changes)
}

# If either is missing, connect to Oracle once
if (!species_exists || !tax_exists || !changes_exists) {
  message("Connecting to Oracle to fetch missing tables…")
  if (file.exists(path_oracle_connect)) {
    source(path_oracle_connect)
  } else {
    gapindex::get_connected()
  }
  
  if (!species_exists) {
    dat_species <- RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.RACE_SPECIES_CODES") %>%
      tibble::as_tibble() %>%
      janitor::clean_names()
    readr::write_csv(dat_species, nm_species)
  }
  
  if (!tax_exists) {
    dat_tax <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION") %>%
      tibble::as_tibble() %>%
      janitor::clean_names()
    readr::write_csv(dat_tax, nm_tax)
  }
  
  if (!changes_exists) {
    dat_changes <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.TAXONOMIC_CHANGES") %>%
      tibble::as_tibble() %>%
      janitor::clean_names()
    readr::write_csv(dat_changes, nm_changes)
  }
}


# ---- Load from CSVs (always) ----
race_species <- readr::read_csv(nm_species, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::select(
    species_name,
    species_code,
    common_name
  )

taxchanges_current <- readr::read_csv(nm_changes, show_col_types = FALSE) %>%
  janitor::clean_names()

classification_current <- readr::read_csv(nm_tax, show_col_types = FALSE) %>%
  janitor::clean_names()

survey_species <- classification_current %>%
  dplyr::select(survey_species_old = survey_species, species_name:species_code) %>%
  dplyr::filter(!species_code %in% missing_taxa$species_code) %>%
  unique()

# manual clunky fix; ok to delete after 2026
survey_species$species_name[survey_species$species_code == 98001] <- "Ascidiacea sp. A"
survey_species$species_name[survey_species$species_code == 98002] <- "Ascidiacea sp. B"
