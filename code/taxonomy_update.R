## User inputs -------------------------------------------------------

ask <- TRUE # ask for user input if multiple taxa match?
save_to_gdrive <- TRUE
# year <- 2022 # if not defined will use current year


# database(s) to check against
db <- "worms"
db2 <- "itis" # optional backup db if not found in first db




## Loading libraries -------------------------------------------------------

pkg <- c(
  "tidyverse",
  "taxize",
  "janitor",
  "googledrive"
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

if (!exists("year")) {
  year <- format(Sys.Date(), "%Y")
}

nm <- paste0("data/", year, "_race_species_codes.csv")


if (file.exists(nm)) {
  tmp <- read_csv(nm) %>%
    clean_names()
} else {
  cat("Can't find current RACE species codes. Downloading from Oracle.")
  source("code/connect_to_oracle.R")
  tmp <- RODBC::sqlQuery(channel, paste0("SELECT * FROM RACE_DATA.RACE_SPECIES_CODES")) %>%
    as_tibble() %>%
    clean_names()
  write_csv(tmp, nm)
}

race_species <- tmp %>%
  select(survey_name = species_name, species_code, common_name)





## Updating/validating names -------------------------------------------------------


# clean up any names/inconsistencies in species list and check for typos
fixed_spp <- race_species %>%
  mutate(
    survey_name = gsub("_", " ", survey_name),
    fixed_name = map_chr(survey_name, check_spelling),
    reason1 = ifelse(survey_name != fixed_name & !grepl(rm_all, survey_name),
      "misspelling", NA
    )
  )



# manually fixing typos/issues that code doesn't catch; need to find a better solution
fixed_spp <- fixed_spp %>%
  mutate(fixed_name = case_when(
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
accepted_spp <- fixed_spp %>%
  arrange(survey_name) %>%
  mutate(
    tmp_name = ifelse(is.na(fixed_name), survey_name, fixed_name),
    tax = purrr::map(tmp_name, ~ get_taxonomy(.x, db, db2, predefined_taxa = defined_tax))
  )




# cleaning up data frame
taxonomy <- accepted_spp %>%
  unnest(tax) %>%
  pivot_wider(names_from = rank, values_from = name) %>%
  mutate(suggested_name = map2_chr(
    survey_name, accepted_name, # updates names of sp. with rm_bits
    ~ if (grepl(rm_all, .x) & !is.na(.y)) {
      fix_names(.x, .y)
    } else {
      NA
    }
  )) %>%
  mutate(
    suggested_name = case_when(
      grepl("hybrid", survey_name) ~ survey_name,
      !is.na(suggested_name) ~ suggested_name,
      is.na(accepted_name) & is.na(fixed_name) ~ survey_name,
      is.na(accepted_name) & !is.na(fixed_name) ~ fixed_name,
      !is.na(accepted_name) ~ accepted_name
    ),
    reason = case_when(
      !is.na(reason2) ~ reason2,
      !is.na(reason1) ~ reason1,
      is.na(db) & !grepl(rm_all, survey_name) &
        !is.na(survey_name) & !grepl("morphotype", common_name) ~ "nomen nudum",
      survey_name != suggested_name ~ "synonym",
      TRUE ~ ""
    ),
    db_id = ifelse(grepl(paste0(rm_bits, "|sp\\. "), survey_name), NA, db_id),
    id_rank = ifelse(grepl("sp\\. ", survey_name), "species", id_rank)
  ) %>%
  select(reason,
    survey_name,
    accepted_name = suggested_name,
    common_name, species_code, id_rank,
    database = db, database_id = db_id,
    all_of(keep_ranks)
  ) %>%
  arrange(accepted_name)

#write_csv(taxonomy, paste0("output/", year, "_taxonomy_updates.csv"))


## Checks -------------------------------------------------------

if (save_to_gdrive) {
  filename <- paste0("output/", year, "_taxonomy_tocheck.csv")


  # name changes
  to_check <- taxonomy %>%
    group_by(accepted_name) %>%
    add_count() %>%
    mutate(check = case_when(
      n > 1 & !is.na(survey_name) ~ "combining taxa",
      survey_name != accepted_name ~ "name change",
      !is.na(survey_name) & is.na(database) & !grepl(rm_all, survey_name) ~ "taxon not found",
      database == "ITIS" ~ "double check",
      TRUE ~ ""
    )) %>%
    arrange(accepted_name) %>%
    select(check, everything(), -n)

  write_csv(to_check, filename)



  # authorize google drive
  googledrive::drive_deauth()
  googledrive::drive_auth()
  1


  gpath <- "1j2jSCrjJim8RbinRLG2675mTv9xXJvqu" # path in google drive
  drive_upload(filename,
    path = as_id(gpath),
    type = "spreadsheet",
    overwrite = FALSE
  )
}
