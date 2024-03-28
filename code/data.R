# download species_year table for new species added
dir_data <- "https://docs.google.com/spreadsheets/d/1yuw03-ybZBSnzXbmkBXNbvbbpWE48-_b2OqG-_qEBbA/edit#gid=0"

googledrive::drive_download(
  file = googledrive::as_id(dir_data), 
  path = "data/new_species_codes.xlsx", 
  overwrite = TRUE)


# species_year table; table of information on when certain species codes were added, important for estimating indices for commercially important taxa
species_year <- readxl::read_excel(path = "data/new_species_codes.xlsx") %>%
  dplyr::mutate(action = "new taxon added") 



# pre-defined taxa with multiple matches to avoid user prompts
defined_tax <- tibble::tribble(
  ~taxon, ~db_id, ~dbe,
  "Clavularia", 125286, "worms",
  "Selachii", 368408, "worms",
  "Vulcanella", 170325, "worms",
  "Ascidian", 1839, "worms",
  "Ascidian", 158854, "itis",
  "Acoelida", 914208, "itis",
  "Hirudinea", 69290, "itis",
  "Weberella", 659295, "itis",
  "Selachii", 563987, "itis",
  "Liparis", 167550, "itis"
)




# clunky way to add taxonomic info for species_codes with only common names (i.e. species_name = NA)
message("Getting taxonomic information for species codes without scientific names.
      This will take a few minutes.\n")
missing_taxa <- tibble::tribble(
  ~"species_code", ~"level",
  99990, "Animalia",
  99992, "Animalia",
  91002, "Porifera",
  91005, "Porifera",
  91051, "Porifera",
  91055, "Porifera",
  91056, "Porifera",
  91071, "Porifera",
  91072, "Porifera",
  91076, "Porifera",
  91077, "Porifera",
  91079, "Porifera",
  91080, "Porifera",
  91081, "Porifera",
  91083, "Porifera",
  91084, "Porifera",
  91090, "Porifera",
  91093, "Porifera",
  91096, "Porifera",
  95010, "Porifera",
  95011, "Bryozoa",
  1, "Vertebrata",
  2, "Vertebrata",
  3, "Vertebrata",
  29999, "Vertebrata",
  50010, "Polychaeta",
  71405, "Gastropoda",
  80604, "Asteroidea",
  82750, "Crinoidea",
  98003, "Ascidiacea",
  98073, "Ascidiacea",
  98300, "Ascidiacea",
  98330, "Ascidiacea",
  99910, "Ascidiacea",
  99993, "Bivalvia",
  99994, "Gastropoda",
  23952, "Teleostei",
  40015, "Hydroidolina",
  99991, "Cirripedia",
  43006, "Actiniaria",
  43007, "Actiniaria",
  43121, "Actiniaria",
  43201, "Actiniaria",
  60113, "Amphipoda",
  71008, "Nudibranchia",
  78011, "Octopoda",
  82650, "Spatangoidea",
  82672, "Spatangoidea",
  82673, "Spatangoidea",
  82730, "Echinolampadacea",
  66000, "Caridea",
  68000, "Brachyura",
  30050, "Sebastes",
  30150, "Sebastes",
  30580, "Sebastes",
  30590, "Sebastes",
  71700, "Buccinidae",
  74981, "Cardiidae"
) %>%
  dplyr::mutate(tax_worms = purrr::map(level, ~ suppressMessages(get_taxonomy(.x, "worms")))) %>%
  tidyr::unnest(tax_worms) %>%
  tidyr::pivot_wider(names_from = rank, values_from = name) %>%
  dplyr::mutate(
    survey_species = 1,
    database = "WORMS"
  ) %>%
  dplyr::select(survey_species, species_code, database, tidyselect::any_of(keep_ranks))

