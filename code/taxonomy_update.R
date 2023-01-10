# TO DO
# make this into a package?
# update names properly for things with rm_bits


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
ask <- TRUE # ask for user input if multiple taxa match?


## Set reference databases ------------------------------------------

# database to check against
db <- "worms"
db2 <- "itis" # optional backup db if not found in first db


# make sure WORMS website is working properly before querying; it goes down not infrequently
if (tolower(db) == "worms") {
  source("code/check_worms.R")
}




## Get this year's RACE species codes ------------------------------------------
nm <- paste0("output/", format(Sys.Date(), "%Y"), "_race_species_codes.csv")

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
  select(survey_name = species_name, species_code) %>%
  filter(!is.na(survey_name))






## Updating/validating names -------------------------------------------------------

if (!exists("db2")) db2 <- NA


# clean up any names/inconsistencies in species list and check for typos
fixed_spp <- race_species %>%
  mutate(
    survey_name = str_to_sentence(gsub("_", " ", survey_name)),
    fixed_name = map_chr(survey_name, check_spelling)
  )



# manually fixing typos that code doesn't catch; need to find a better solution
fixed_spp$fixed_name[which(fixed_spp$survey_name == "Ciliatoclinocardium ciliatum")] <- "Ciliatocardium cilium"
fixed_spp$fixed_name[which(fixed_spp$survey_name == "Tridonta rollandi")] <- "Astarte (Tridonta) rollandi"
fixed_spp$fixed_name[which(fixed_spp$survey_name == "Cancer anthonyi")] <- "Metacarcinus anthonyi"
fixed_spp$fixed_name[which(fixed_spp$survey_name == "Brada villosa")] <- "Bradabyssa villosa"
fixed_spp$fixed_name[which(fixed_spp$survey_name == "Leptasterias truculenta")] <- "Leptasterias coei truculenta"
fixed_spp$fixed_name[which(fixed_spp$survey_name == "Acoelida")] <- "Acoela"




## Getting taxonomic info -------------------------------------------------------
# THIS MAY TAKE A WHILE; also checks for accepted name
accepted_spp <- fixed_spp %>%
  mutate(tmp_name = ifelse(is.na(fixed_name), survey_name, fixed_name),
         tax = map(tmp_name, ~ get_taxonomy(.x, db, db2, ask)))



# cleaning up data frame
taxonomy <- accepted_spp %>%
  unnest(tax) %>%
  pivot_wider(names_from = rank, values_from = name) %>%
  mutate( suggested_name = case_when(
      grepl(paste0(rm_bits, "| cf."), survey_name) ~ survey_name,
      is.na(accepted_name) & is.na(fixed_name) ~ survey_name,
      is.na(accepted_name) & !is.na(fixed_name) ~ fixed_name,
      !is.na(accepted_name) ~ accepted_name
    ),
    db_id = ifelse(grepl(str_extract(rm_bits, " .+$"), survey_name), NA, db_id)
  ) %>%
  select(
    survey_name, accepted_name, suggested_name,
    species_code,
    database = db, database_id = db_id,
    all_of(keep_ranks)
  )



## Checks -------------------------------------------------------


filename <- paste0("output/", format(Sys.Date(), "%Y"), "_taxonomy_tocheck.csv")


# name changes
to_check <- taxonomy %>%
  group_by(suggested_name) %>%
  add_count() %>%
  mutate(check = case_when(
    n > 1 ~ "combining taxa",
    survey_name != suggested_name & !grepl("sp.", survey_name) ~ "name change",
    is.na(database) & !grepl("sp.", survey_name) ~ "taxon not found",
    database == "ITIS" ~ "double check",
    is.na(kingdom) | kingdom != "Animalia" ~ "fix taxonomy",
    TRUE ~ ""
  )) %>%
  arrange(suggested_name) %>%
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
