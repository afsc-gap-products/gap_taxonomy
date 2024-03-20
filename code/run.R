## User inputs -------------------------------------------------------

ask <- TRUE # ask for user input if multiple taxa match?
# save_to_gdrive <- TRUE

# yr <- 2022 # if not defined will use current year

# database(s) (worms vs. itis) to check against
db <- "both"
# db2 <- "itis" # optional backup db if not found in first db




## Loading libraries -------------------------------------------------------

pkg <- c(
  "tidyverse",
  "taxize",
  "janitor",
  "googledrive",
  "gapindex"
)

pkg <- unique(pkg)
for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}


source("code/functions.R")



## check database status ------------------------------------------

# make sure db website is working properly before querying; it goes down not infrequently
source("code/check_db.R")




## Get this year's RACE species codes ------------------------------------------

if (!exists("yr")) {
  yr <- format(Sys.Date(), "%Y")
}

# downloading updated species codes
nm <- paste0("data/", yr, "_race_species_codes.csv")
if (file.exists(nm)) {
  tmp <- readr::read_csv(nm) %>%
    janitor::clean_names()
} else {
  cat("Can't find current RACE species codes. Downloading from Oracle.\n")
  if (file.exists("Z:/Projects/ConnectToOracle.R")) {
    source("Z:/Projects/ConnectToOracle.R")
    channel <- channel_products
  } else {
    gapindex::get_connected()
  }
  tmp <- RODBC::sqlQuery(channel, paste0("SELECT * FROM RACE_DATA.RACE_SPECIES_CODES")) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
  readr::write_csv(tmp, nm)
}

race_species <- tmp %>%
  dplyr::select(survey_name = species_name, species_code, common_name)





## Updating/validating names -------------------------------------------------------


# clean up any names/inconsistencies in species list and check for typos; need to fix names first or databases won't recognize them
fixed_spp <- race_species %>%
  dplyr::mutate(
    survey_name = gsub("_", " ", survey_name),
    fixed_name = purrr::map_chr(survey_name, check_spelling),
    reason1 = ifelse(survey_name != fixed_name & !grepl(rm_all, survey_name),
      "misspelling", NA
    )
  )



# manually fixing typos/issues that code doesn't catch; need to find a better solution
fixed_spp <- fixed_spp %>%
  mutate(fixed_name = dplyr::case_when(
    survey_name == "Ciliatoclinocardium ciliatum" ~ "Ciliatocardium cilium",
    survey_name == "Tridonta rollandi" ~ "Astarte rollandi",
    survey_name == "Cancer anthonyi" ~ "Metacarcinus anthonyi",
    survey_name == "Brada villosa" ~ "Bradabyssa villosa",
    survey_name == "Leptasterias truculenta" ~ "Leptasterias coei truculenta",
    survey_name == "Acoelida" ~ "Acoela",
    survey_name == "Buccinum transliratum" ~ "Buccinum angulosum transliratum",
    survey_name == "Buccinum triplostephanum" ~ "Buccinum castaneum",
    survey_name == "Buccinum moarchianum" ~ "Buccinum glaciale",
    survey_name == "Bathybuccinum ovulum" ~ "Buccinum ovulum",
    survey_name == "Geodinella lendenfeldi" ~ "Geodinella robusta",
    survey_name == "Henricia aleutica" ~ "Henricia longispina aleutica",
    survey_name == "Leptasterias katharinae" ~ "Leptasterias polaris katherinae",
    survey_name == "Cyclopecten uschakovi" ~ "Parvamussium uschakovi",
    survey_name == "Capulacmaea commoda" ~ "Piliscus commodus",
    survey_name == "Poraniopsis flexilius" ~ "Poraniopsis inflata",
    survey_name == "Escharopsis sarsi" ~ "Escharoides sarsi",
    survey_name == "Hippoporina insculpta" ~ "Primavelans insculpta",
    survey_name == "Solaster arcticus" ~ "Solaster dawsoni arcticus",
    survey_name == "Stylocordyla eous" ~ "Stylocordyla borealis eous",
    survey_name == "Ancistrolepis bicinctus" ~ "Ancistrolepis eucosmius",
    survey_name == "Chlamys erythocomata" ~ "Chlamys islandica erythrocomata",
    TRUE ~ fixed_name
  ))




## Getting taxonomic info -------------------------------------------------------
# THIS MAY TAKE A WHILE; also checks for accepted name
accepted_spp_worms <- fixed_spp %>%
  dplyr::arrange(survey_name) %>%
  dplyr::mutate(
    tmp_name = ifelse(is.na(fixed_name), survey_name, fixed_name),
    tax_worms = purrr::map(tmp_name, ~ get_taxonomy(.x, "worms", predefined_taxa = defined_tax))
  )

accepted_spp_itis <- fixed_spp %>%
  dplyr::arrange(survey_name) %>%
  dplyr::mutate(
    tmp_name = ifelse(is.na(fixed_name), survey_name, fixed_name),
    tax_itis = purrr::map(tmp_name, ~ get_taxonomy(.x, "itis", predefined_taxa = defined_tax))
  )


# cleaning up data frame, picking final accepted name and reasons for changes
taxonomy_cleaned <- dplyr::bind_rows(
  accepted_spp_worms %>%
    tidyr::unnest(tax_worms) %>%
    tidyr::pivot_wider(names_from = rank, values_from = name),
  accepted_spp_itis %>%
    tidyr::unnest(tax_itis) %>%
    tidyr::pivot_wider(names_from = rank, values_from = name)
) %>%
  dplyr::mutate(suggested_name = purrr::map2_chr( # updates names of sp. with rm_bits
    survey_name, database_name,
    ~ if (grepl(rm_all, .x)) {
      fix_names(.x, .y)
    } else {
      NA
    }
  )) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate( # pick updated name based on available columns
    new_name = dplyr::case_when(
      grepl("hybrid", survey_name) ~ survey_name,
      !is.na(suggested_name) ~ suggested_name,
      is.na(database_name) & is.na(fixed_name) ~ survey_name,
      is.na(database_name) & !is.na(fixed_name) ~ fixed_name,
      !is.na(database_name) ~ database_name
    ),
    reason = dplyr::case_when(
      !is.na(reason2) ~ reason2,
      !is.na(reason1) ~ reason1,
      !grepl(rm_all, survey_name) & sum(is.na(db_id) > 1) &
        !is.na(survey_name) & !grepl("morphotype", common_name) ~ "nomen nudum",
      survey_name != new_name ~ "synonym",
      TRUE ~ ""
    ),
    db_id = ifelse(grepl(paste0(rm_bits, "|sp\\. "), survey_name), NA, db_id),
    id_rank = ifelse(grepl("sp\\. ", survey_name), "species", id_rank)
  ) %>%
  dplyr::select(reason,
    survey_name, accepted_name = new_name,
    common_name, species_code, id_rank,
    database = db, database_id = db_id,
    tidyselect::all_of(keep_ranks)
  )




## Species classification table for Oracle -------------------------------------------------------

# species classification table formatting for oracle; adding variable survey_species, which designate the species name RACE accepts (relies primarily on WORMS with supplemented ITIS codes where necessary); removing species that were synonmized with existing species codes

taxonomy_final <- taxonomy_cleaned %>%
  dplyr::group_by(accepted_name) %>%
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>% # finding species with multiple codes; indicates synonymy
  tidyr::nest() %>%
  dplyr::mutate(new_code = purrr::map2_dbl(data, accepted_name, fix_syn_codes)) %>% # fixing species code issues
  tidyr::unnest(cols = c(data)) %>%
  dplyr::mutate(
    old_code = species_code,
    action = case_when(
      reason == "nomen nudum" ~ "delete species code",
      old_code != new_code & survey_name != accepted_name & n_codes > 1 ~ "change species code/name",
      reason != "" & survey_name != accepted_name ~ "change species name",
      TRUE ~ ""
    ),
    species_code = ifelse(is.na(new_code), old_code, new_code)
  ) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(survey_species = dplyr::case_when( # determining species to retain for each species code
    database == "WORMS" & !is.na(database_id) ~ 1,
    sum(is.na(database_id)) == 2 & database == "WORMS" ~ 1,
    sum(is.na(database_id)) == 2 & row_number() == 1 ~ 1,
    sum(is.na(database_id)) == 1 & !is.na(database_id) ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(survey_species = ifelse(sum(survey_species) > 1 & row_number() != 1, 0, survey_species)) %>%
  dplyr::group_by(accepted_name) %>% # fixing last few problem species
  dplyr::mutate(n_codes = dplyr::n_distinct(species_code)) %>%
  dplyr::mutate(accepted_name = ifelse(n_codes != 1, survey_name, accepted_name)) %>%
  dplyr::rows_update(missing_taxa, by = "species_code") %>% # adding in taxonomy for sp. where survey_name = NA
  dplyr::select(reason, survey_species, survey_name,
    species_name = accepted_name,
    common_name, species_code:database_id, genus:kingdom, old_code, action
  ) %>%
  dplyr::distinct(survey_name, species_name, species_code, database_id, .keep_all = TRUE) %>% # removing unnecessary rows
  dplyr::filter(!(survey_species == 0 & is.na(database) & !is.na(species_name)))




# adding checks to data frame; this should be empty
taxonomy_final %>%
  dplyr::group_by(species_name) %>%
  dplyr::mutate(n_codes = n_distinct(species_code)) %>%
  dplyr::filter(n_codes != 1 & !is.na(species_name))

# species that were synonymized and we will no longer use these codes; will be dropped from final table
taxonomy_final %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(n = sum(survey_species)) %>%
  dplyr::filter(n != 1) %>%
  View()



# final species_classification table
species_classification <- taxonomy_final %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(n = sum(survey_species)) %>%
  dplyr::filter(n != 0) %>% # dropping species that have been synonymized
  dplyr::select(-c(survey_name, reason, n, old_code, action)) %>%
  dplyr::distinct(species_name, species_code, database_id, .keep_all = TRUE) %>%
  dplyr::rename_at(vars(genus:kingdom), list(~ paste0(., "_taxon"))) %>%
  dplyr::arrange(species_code) %>%
  dplyr::rename_with(toupper)


write_csv(species_classification, paste0("output/species_classification_", yr, ".csv"))


## Documenting taxonomic changes -------------------------------------------------------
# dataframe of exact changes that need to be made to species names/codes


# list of taxonomic changes that need to be made
taxonomic_changes <- taxonomy_final %>%
  dplyr::filter(survey_species == 1 | reason != "") %>%
  dplyr::group_by(survey_name) %>%
  dplyr::add_count() %>%
  dplyr::filter(n == 1 | n == 2 & database == "WORMS") %>%
  dplyr::mutate(
    new_species_code = ifelse(old_code == species_code, NA, species_code),
    year_changed = format(Sys.Date(), "%Y")
  ) %>%
  dplyr::distinct(species_name, survey_name, old_code, species_code, action, .keep_all = TRUE) %>%
  dplyr::filter(species_name != survey_name | old_code != species_code) %>%
  dplyr::select(
    old_species_name = survey_name, old_species_code = old_code,
    new_species_name = species_name, new_species_code = species_code, action, year_changed, reason
  ) %>%
  dplyr::arrange(old_species_name) %>%
  dplyr::rename_with(toupper)








## Upload tables to Oracle ----------------------------------------------------------------------------------------
source("C:/Users/sarah.friedman/Work/Rfunctions/ConnectToOracle_GAPproducts.R")


species_classification_metadata <- tibble(
  COLNAME = colnames(species_classification),
  datatype = oracle_datatypes(species_classification)
) %>%
  dplyr::left_join(
    RODBC::sqlQuery(
      channel = channel,
      query = "SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN"
    ),
    by = c("COLNAME" = "METADATA_COLNAME")
  ) %>%
  dplyr::rename_with(~ str_remove(., "METADATA_")) %>%
  dplyr::mutate(DATATYPE = ifelse(is.na(DATATYPE), datatype, DATATYPE)) %>%
  dplyr::select(-datatype) %>%
  janitor::clean_names()
species_classification_metadata$datatype[species_classification_metadata$colname == "DATABASE_ID"] <- "NUMBER(38,0)"
species_classification_metadata$datatype[species_classification_metadata$colname == "SURVEY_SPECIES"] <- "NUMBER(1,0)"

gapindex::upload_oracle(
  x = species_classification,
  table_name = "taxonomic_classification",
  table_metadata = "species classification table",
  metadata_column = species_classification_metadata,
  channel = channel,
  schema = "GAP_PRODUCTS",
  share_with_all_users = TRUE
)




taxonomic_changes_metadata <- tibble(
  COLNAME = colnames(taxonomic_changes),
  datatype = oracle_datatypes(taxonomic_changes)
) %>%
  dplyr::left_join(
    RODBC::sqlQuery(
      channel = channel,
      query = "SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN"
    ),
    by = c("COLNAME" = "METADATA_COLNAME")
  ) %>%
  dplyr::rename_with(~ str_remove(., "METADATA_")) %>%
  dplyr::mutate(DATATYPE = ifelse(is.na(DATATYPE), datatype, DATATYPE)) %>%
  dplyr::select(-datatype) %>%
  janitor::clean_names()


gapindex::upload_oracle(
  x = taxonomic_changes,
  table_name = "taxonomic_changes",
  table_metadata = "Taxonomic_changes table",
  metadata_column = taxonomic_changes_metadata,
  channel = channel,
  schema = "GAP_PRODUCTS",
  share_with_all_users = TRUE
)
