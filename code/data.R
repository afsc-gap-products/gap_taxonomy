# download species_year table for new species added
# dir_data <- "https://docs.google.com/spreadsheets/d/1yuw03-ybZBSnzXbmkBXNbvbbpWE48-_b2OqG-_qEBbA/edit#gid=0"

# googledrive::drive_download(
#   file = googledrive::as_id(dir_data), 
#   path = "data/new_species_codes.xlsx", 
#   overwrite = TRUE)
# 
# 
# # species_year table; table of information on when certain species codes were added, important for estimating indices for commercially important taxa
# species_year <- readxl::read_excel(path = "data/new_species_codes.xlsx") %>%
#   dplyr::mutate(action = "new taxon added") 



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
  ~"species_code", ~"level", ~"rrank",
  99990, "Animalia", NA,
  99992, "Animalia", NA,
  91002, "Porifera", "species",
  91005, "Porifera", NA,
  91051, "Porifera", "species",
  91055, "Porifera", "species",
  91056, "Porifera", "species",
  91071, "Porifera", NA,
  91072, "Porifera", NA,
  91076, "Porifera", NA,
  91077, "Porifera", NA,
  91079, "Porifera", NA,
  91080, "Porifera", NA,
  91081, "Porifera", NA,
  91083, "Porifera", NA,
  91084, "Porifera", NA,
  91090, "Porifera", NA,
  91093, "Porifera", "species",
  91096, "Porifera", "species",
  95010, "Bryozoa", "species",
  95011, "Bryozoa", "species",
  1, "Vertebrata", NA,
  2, "Vertebrata", NA,
  3, "Vertebrata", NA,
  29999, "Vertebrata", NA,
  50010, "Polychaeta", NA,
  71405, "Gastropoda", NA,
  80604, "Asteroidea", "species",
  82750, "Crinoidea", "class",
  98003, "Ascidiacea", "genus",
  98073, "Ascidiacea", "species",
  98300, "Ascidiacea", NA,
  98330, "Ascidiacea", "species",
  99910, "Ascidiacea", "species",
  99993, "Bivalvia", NA,
  99994, "Gastropoda", NA,
  23952, "Teleostei", NA,
  40015, "Hydroidolina", "species",
  99991, "Cirripedia", NA,
  43006, "Actiniaria", "species",
  43007, "Actiniaria", "species",
  43121, "Actiniaria", "species",
  43201, "Actiniaria", "species",
  60113, "Amphipoda", "species",
  71008, "Nudibranchia", "order",
  78011, "Octopoda", NA,
  82650, "Spatangoidea", "order",
  82672, "Spatangoidea", "species",
  82673, "Spatangoidea", "species",
  82730, "Echinolampadacea", "order",
  66000, "Caridea", "infraorder",
  68000, "Brachyura", "infraorder",
  30050, "Sebastes", NA,
  30150, "Sebastes", NA,
  30580, "Sebastes", NA,
  30590, "Sebastes", NA,
  71700, "Buccinidae", "family",
  74981, "Cardiidae", "family"
) %>%
  dplyr::mutate(tax_worms = purrr::map(level, ~ suppressMessages(get_taxonomy(.x, "worms")))) %>%
  tidyr::unnest(tax_worms) %>%
  tidyr::pivot_wider(names_from = rank, values_from = name) %>%
  dplyr::mutate(
    survey_species = 1,
    database = "WORMS"
  ) %>%
  dplyr::select(survey_species, species_code, id_rank = rrank, database, tidyselect::any_of(keep_ranks))

