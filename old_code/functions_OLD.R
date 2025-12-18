# ---------------------------
# NAME FIXING
# ---------------------------

fix_names <- function(old_nm, new_nm) {
  # If new name missing, keep old
  if (is.na(new_nm)) return(old_nm)
  
  # Handle "cf." cases separately
  if (stringr::str_detect(old_nm, "cf\\.")) {
    ps <- stringr::str_extract(old_nm, "\\(.+\\)$|group$")
    ps <- ifelse(is.na(ps), "", ps)
    old_nm <- stringr::str_remove(old_nm, " \\(.+\\)$|group$")
    
    md <- stringr::str_extract(old_nm, "sp\\. cf\\.|cf\\.")
    
    if (stringr::str_detect(new_nm, " ")) {
      nm <- paste(stringr::str_extract(new_nm, "^\\w+"), md, stringr::str_extract(new_nm, "\\w+$"), ps)
    } else {
      nm <- paste(stringr::str_extract(new_nm, "^\\w+"), md, stringr::str_extract(old_nm, "\\w+$"), ps)
    }
  } else {
    # General case: append any extra suffix bits from old name
    extra_bits <- stringr::str_extract(old_nm, paste0("(", RM_ALL, ")(.+)?$"))
    nm <- paste(new_nm, extra_bits)
  }
  
  nm <- stringr::str_replace_all(nm, "\\s+", " ")
  trimws(nm)
}


# ---------------------------
# SPELLING CHECK
# ---------------------------

check_spelling <- function(species) {
  if (is.na(species)) return(NA_character_)
  
  sp <- species
  
  # Clean "(= synonym)" formatting
  if (stringr::str_detect(sp, " \\(=")) {
    sp <- stringr::str_remove(sp, "[A-Za-z]+ \\(=")
    sp <- stringr::str_remove(sp, "\\)")
    sp <- stringr::str_to_sentence(sp)
  }
  
  # Remove "cf." and junk suffixes
  if (stringr::str_detect(sp, "cf\\.")) {
    sp <- stringr::str_remove_all(sp, " sp\\. cf\\.|cf\\. |\\(.+\\)$")
  }
  
  # Remove hybrid
  if (stringr::str_detect(sp, "hybrid")) {
    sp <- stringr::str_extract(sp, "\\w+ \\w+")
  }
  
  sp <- stringr::str_remove_all(sp, RM_BITS)
  sp <- trimws(sp)
  
  # Try several taxonomic data sources in priority order
  sources <- c(9, 3, 1, 2, 12)
  new_id <- NULL
  for (sid in sources) {
    res <- try(taxize::gna_verifier(sp, data_sources = sid), silent = TRUE)
    if (!methods::is(res, "try-error") && !grepl("subgen.", res$matchedCanonicalSimple) ) {
      new_id <- res
      break
    }
  }
  
  # Fallback to best match across more sources
  if (is.null(new_id)) {
    res <- try(taxize::gna_verifier(
      sp, all_matches = FALSE,
      data_sources = c(1, 3, 12, 151, 155, 163)
    ), silent = TRUE)
    if (!methods::is(res, "try-error") && !is.na(res$matchedCanonicalSimple)) new_id <- res
  }
  
  # Handle no match
  if (is.null(new_id) || nrow(new_id) == 0) return(NA_character_)
  
  nm <- new_id %>% 
    dplyr::filter(!is.na(matchedCanonicalSimple) & !grepl("Partial", matchType)) %>%
    dplyr::slice(1) %>%
    dplyr::pull("matchedCanonicalSimple")
  
  # Ensure length 1 result
  if (length(nm) == 0) return(NA_character_)
  
  # Require species-level match (two words)
  if (stringr::str_detect(sp, " ") && !stringr::str_detect(nm, " ") && !grepl(RM_ALL, sp)) {
    return(NA_character_)
  }
  nm
}


# ---------------------------
# TAXONOMY CLASSIFICATION
# ---------------------------

get_classification <- function(sp = NA, id = NA, db, ask = TRUE, presets = NA) {
  if (is.na(sp)) {
    return(tibble::tibble(name = NA, rank = NA, id = NA))
  }
  
  if(sp %in% presets$taxon && presets$dbe[presets$taxon == sp] == db){
    preset_id <- presets %>% dplyr::filter(taxon == sp & dbe == tolower(db)) %>% dplyr::pull(db_id)
    if(length(preset_id) == 1) id <- preset_id
  }
  
  query <- ifelse(is.na(id), sp, id)
  res <- taxize::classification(query, db = db, fuzzy = FALSE, accepted = TRUE, ask = ask, messages = FALSE)
  
  # fallback: any classification
  if (is.null(res) || all(is.na(res))) {
    res <- taxize::classification(query, db = db, fuzzy = FALSE, accepted = FALSE, ask = ask, messages = FALSE)
  }
  
  # fallback: genus-level
  if ((is.null(res) || all(is.na(res))) && grepl(" ", sp)) {
    genus <- stringr::str_extract(sp, "^[A-Za-z]+")
    res <- taxize::classification(genus, db = db, accepted = TRUE, messages = FALSE)
  }
  
  if (is.null(res) || all(is.na(res))) {
    tibble::tibble(name = sp, rank = NA, id = NA)
  } else {
    tibble::as_tibble(res[[1]]) %>%
      dplyr::filter(!rank %in% c("Subgenus", "Variety")) %>%
      dplyr::mutate(rank = tolower(rank))
  }
}


get_classification_formatted <- function(sp, database = NA, ...){
  db <- ifelse(is.na(database), "worms", tolower(database))
  classy <- get_classification(sp, db = db, presets = defined_tax,...) %>%
    dplyr::filter(rank != "subspecies") 
  if(grepl(" ", sp) & classy$rank[nrow(classy)] != "species"){
    classy <- classy %>% bind_rows(data.frame(name = sp, rank = "species"))
  }
  out <- classy %>%
    dplyr::mutate(
      rw_id = dplyr::row_number(),
      id_rank = ifelse(rw_id == max(rw_id), tolower(rank), NA)
    ) %>%
    tidyr::fill(id_rank, .direction = "downup") %>%
    dplyr::select(-id, -rw_id)
  return(out)
}



get_taxonomy <- function(species){
  if(is.na(species)) return(tibble(database = NA, database_id = NA, database_name = NA))
  
  message(paste("Getting taxonomic information for:", species))
  
  # pulls most recent taxonomic information from WoRMS and ITIS
  db_info <- bind_rows(
    taxize::gna_verifier(species, data_sources = 3, fuzzy_uninomial = FALSE),
    taxize::gna_verifier(species, data_sources = 9, fuzzy_uninomial = FALSE)
  ) %>% 
    janitor::clean_names() %>%
    dplyr::mutate(database_id = as.numeric(stringr::str_extract(current_record_id, "[0-9]+")),
                  database = tolower(data_source_title_short)) %>%
    dplyr::filter(!is.na(current_canonical_simple))
  
  # filter out partial matches for species-level names, ignoring subspecies
  if(stringr::str_count(species, "\\s") != 2) db_info <- db_info %>% dplyr::filter(!grepl("Partial", match_type))
  
  if(nrow(db_info) == 0) return(tibble(database = NA, database_id = NA, database_name = NA))
  
  # pull classification info from each database
  db_info %>%
    dplyr::select(survey_name = submitted_name, database, 
                  database_id, database_name = current_canonical_simple, taxonomic_status) %>%
    dplyr::mutate(classy = purrr::map2(database_name, database, ~get_classification_formatted(sp = .x, database = .y)),
                  database = toupper(database),
                  taxonomic_status = tolower(taxonomic_status)) %>%
    tidyr::unnest(classy) %>% 
    tidyr::pivot_wider(names_from = rank, values_from = name) %>%
    dplyr::select(-survey_name) 
  
}

# Wrapper to avoid crashing on API errors
get_taxonomy_safely <- function(nm) {
  # short pause to avoid hitting website too fast
  Sys.sleep(0.5)
  
  # wrap in purrr::possibly to catch errors
  purrr::possibly(
    function(x) {
      retry::retry(
        expr      = { get_taxonomy(x) },
        when      = function(e) inherits(e, "error"),
        max_tries = 3,
        interval  = 1
      )
    },
    otherwise = tibble::tibble(classy = NA_character_)
  )(nm)
}



# ---------------------------
# ORACLE DATATYPES
# ---------------------------

oracle_datatypes <- function(x) {
  classes <- sapply(x, class)
  dplyr::case_when(
    classes == "character" ~ "VARCHAR2(255 BYTE)",
    classes == "logical"   ~ "NUMBER(1,0)",
    TRUE ~ "NUMBER"
  )
}


# ---------------------------
# FIX SYNONYMY CODES
# ---------------------------

fix_syn_codes <- function(data, nm) {
  if (data$n_codes[[1]] == 1) {
    return(data$species_code[[1]])
  }
  
  if (is.na(nm) || grepl(" sp\\. ", data$survey_name[[1]])) {
    return(NA)
  }
  
  code <- data %>%
    dplyr::filter(survey_name == nm) %>%
    purrr::pluck("species_code")
  
  if (length(unique(code)) == 1) {
    return(code[[1]])
  }
  
  # Approximate matching fallback
  ii <- which(stringr::str_extract(data$survey_name, " \\w+") ==
                stringr::str_extract(nm, "\\w+$"))
  if (length(ii) != 1) {
    ii <- agrep(stringr::str_extract(nm, "\\w+$"),
                trimws(stringr::str_extract(data$survey_name, " \\w+")))[1]
  }
  
  if (is.na(ii)) NA else data$species_code[[ii]]
}


# Reason + action
assign_reason_action <- function(fixed_name, new_spp, taxonomic_status,
                                 survey_name, accepted_name,
                                 database_id, common_name,
                                 species_code, new_code, n_codes) {
  reason <- dplyr::case_when(
    fixed_name %in% new_spp$species_name ~ "species described",
    taxonomic_status != "accepted" ~ taxonomic_status,
    !grepl(RM_ALL, survey_name) &
      sum(is.na(database_id)) > 1 &
      !is.na(survey_name) &
      !grepl("morphotype", common_name) ~ "nomen nudum",
    survey_name != accepted_name ~ "synonym",
    TRUE ~ ""
  )
  
  action <- dplyr::case_when(
    reason == "nomen nudum" ~ "delete taxon code",
    reason == "species described" ~ "change taxon name",
    species_code != new_code & survey_name != accepted_name & n_codes > 1 ~ "change taxon code/name",
    reason != "" & survey_name != accepted_name ~ "change taxon name",
    TRUE ~ ""
  )
  
  tibble::tibble(reason, action)
}

# assign survey_species 
assign_survey_species <- function(database, database_id) {
  n <- length(database)
  
  # Start with zeros
  survey_species <- rep(0L, n)
  
  # Priority 1: WORMS rows with database_id
  idx <- which(database == "WORMS" & !is.na(database_id))
  if (length(idx) > 0) {
    survey_species[idx[1]] <- 1L
    return(survey_species)
  }
  
  # Priority 2: WORMS rows with NA
  idx <- which(database == "WORMS")
  if (length(idx) > 0) {
    survey_species[idx[1]] <- 1L
    return(survey_species)
  }
  
  # Priority 3: first row of the group
  survey_species[1] <- 1L
  survey_species
}

