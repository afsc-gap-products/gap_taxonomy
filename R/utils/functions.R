# ---------------------------
# STANDARDIZING NAMES FUNCTIONS
# ---------------------------

#' @title Fix Species Names
#' @description Cleans and formats species names, particularly for "cf." (confer) cases,
#'   by appending relevant taxonomic suffixes from old names to new ones.
#' @param old_nm The original species name.
#' @param new_nm The new species name.
#' @return A character string of the cleaned and combined name.
fix_names <- function(old_nm, new_nm) {
  # If new name missing, keep old
  if (is.na(new_nm)) {
    return(old_nm)
  }

  # handle hybrids separately
  if (grepl("hybrid", old_nm)) {
    return(old_nm)
  }

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


#' @title Clean Species Names for Taxize
#' @description Cleans up species names by removing suffixes and fixing formatting for use with taxonomic databases.
#' @param species A character string of the species name.
#' @return A cleaned character string or `NA_character_` if input is `NA`.
clean_names <- function(species) {
  if (is.na(species)) {
    return(NA_character_)
  }

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

  # Clean "sp. X" formatting
  if (stringr::str_detect(sp, "sp\\. ")) {
    sp <- stringr::str_extract(sp, "^[A-Za-z]+ sp\\.")
  }

  # Remove hybrid
  if (stringr::str_detect(sp, "hybrid")) {
    sp <- stringr::str_extract(sp, "\\w+ \\w+")
  }

  sp <- stringr::str_remove_all(sp, RM_BITS)
  trimws(sp)
}


# ---------------------------
# TAXONOMY FUNCTIONS
# ---------------------------

#' @title Get Taxonomic Classification
#' @description Queries a specified taxonomic database for the full classification hierarchy of a name or ID.
#' @param sp A character string of the species name.
#' @param id An optional database ID.
#' @param db The database to query, e.g., "worms", "itis".
#' @param ask Logical, whether to ask for user input on multiple matches.
#' @param presets A data frame of preset taxonomic IDs to use.
#' @return A `tibble` containing the taxonomic hierarchy (name, rank, ID) or a default tibble of NAs if no match is found.
get_classification <- function(sp = NA, id = NA, db, ask = TRUE, presets = NA) {
  if (is.na(sp)) {
    return(tibble::tibble(name = NA, rank = NA, id = NA))
  }

  if (sp %in% presets$taxon && db %in% presets$dbe[presets$taxon == sp]) {
    preset_id <- presets %>%
      dplyr::filter(taxon == sp & dbe == tolower(db)) %>%
      dplyr::pull(db_id)
    if (length(preset_id) == 1) id <- preset_id
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

#' @title Get Formatted Taxonomic Classification
#' @description A wrapper around `get_classification` to format the output into a wide, usable table.
#' @param sp A character string of the species name.
#' @param database An optional database name, e.g., "worms" or "itis".
#' @param ... Additional arguments passed to `get_classification`.
#' @return A wide-format `tibble` with columns for each taxonomic rank and a `survey_species` indicator.
get_classification_formatted <- function(sp, database = NA, ...) {
  db <- ifelse(is.na(database), "worms", tolower(database))
  classy <- get_classification(sp, db = db, presets = defined_tax, ...) %>%
    dplyr::filter(rank != "subspecies")
  if (grepl(" ", sp) & classy$rank[nrow(classy)] != "species") {
    classy <- classy %>%
      dplyr::bind_rows(data.frame(name = sp, rank = "species"))
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


#' @title Get Taxonomic Information from WoRMS and ITIS
#' @description Queries WoRMS and ITIS for taxonomic information for a given species.
#' @param species A character string of the species name.
#' @return A `tibble` containing taxonomic information.
get_taxonomy <- function(species) {
  if (is.na(species)) {
    return(tibble::tibble(database = NA, database_id = NA, database_name = NA))
  }

  message(glue::glue("Getting taxonomic information for: {species}"))

  # pulls most recent taxonomic information from WoRMS and ITIS
  db_info <- dplyr::bind_rows(
    taxize::gna_verifier(species, data_sources = 3, fuzzy_uninomial = FALSE),
    taxize::gna_verifier(species, data_sources = 9, fuzzy_uninomial = FALSE)
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      database_id = as.numeric(stringr::str_extract(current_record_id, "[0-9]+")),
      database = tolower(data_source_title_short)
    ) %>%
    dplyr::filter(!is.na(current_canonical_simple))

  # filter out partial matches for species-level names, ignoring subspecies
  if (stringr::str_count(species, "\\s") != 2) db_info <- db_info %>% dplyr::filter(!grepl("Partial", match_type))

  if (nrow(db_info) == 0 & grepl("\\s", species)) {
    # try genus-level lookup if species-level fails
    genus <- stringr::str_extract(species, "^[A-Za-z]+")
    db_info <- dplyr::bind_rows(
      taxize::gna_verifier(genus, data_sources = 3, fuzzy_uninomial = FALSE),
      taxize::gna_verifier(genus, data_sources = 9, fuzzy_uninomial = FALSE)
    ) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        database_id = NA,
        database = tolower(data_source_title_short)
      ) %>%
      dplyr::filter(!is.na(current_canonical_simple))
  }

  # last ditch effort to find a match if all else fails
  if (nrow(db_info) == 0) {
    tmp <- taxize::classification(species, "worms")[[1]]
    if (!is.null(tmp) && !all(is.na(tmp))) {
      db_info <- tmp %>%
        mutate(
          database_name = tmp$name[nrow(tmp)],
          database = "worms",
          taxonomic_status = "accepted",
          rank = tolower(rank),
          database_id = tmp$id[nrow(tmp)],
          id_rank = tolower(tmp$rank[nrow(tmp)])
        ) %>%
        select(-id) %>%
        pivot_wider(names_from = rank, values_from = name)
      if (nrow(db_info > 0)) {
        return(db_info)
      }
    }
  }

  if (nrow(db_info) == 0) {
    return(tibble::tibble(database = NA, database_id = NA, database_name = NA))
  }

  # pull classification info from each database
  db_info %>%
    dplyr::select(
      survey_name = submitted_name, database,
      database_id, database_name = current_canonical_simple, taxonomic_status
    ) %>%
    dplyr::mutate(
      classy = purrr::map2(database_name, database, ~ get_classification_formatted(sp = .x, database = .y)),
      database = toupper(database),
      taxonomic_status = tolower(taxonomic_status)
    ) %>%
    tidyr::unnest(classy) %>%
    tidyr::pivot_wider(names_from = rank, values_from = name) %>%
    dplyr::select(-survey_name)
}

#' @title Get Taxonomy Safely
#' @description Wrapper to safely call `get_taxonomy` and handle API errors.
#' @param nm The name to look up.
#' @return A `tibble` with taxonomic information or a `NA_character_` if an error occurs.
get_taxonomy_safely <- function(nm) {
  purrr::possibly(
    function(x) {
      retry::retry(
        expr = {
          get_taxonomy(x)
        },
        when = function(e) inherits(e, "error"),
        # max_tries = 3,
        interval = 1
      )
    },
    otherwise = tibble::tibble(database = NA, database_id = NA, database_name = NA)
  )(nm)
}

# ---------------------------
# DATA TRANSFORMATION
# ---------------------------

#' @title Fix Synonymy Codes
#' @description Fixes species codes for synonymized species.
#' @param data A nested `tibble` containing species data.
#' @param nm The accepted name.
#' @return The corrected species code.
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
    ii <- agrep(
      stringr::str_extract(nm, "\\w+$"),
      trimws(stringr::str_extract(data$survey_name, " \\w+"))
    )[1]
  }

  if (is.na(ii)) NA else data$species_code[[ii]]
}


#' @title Assign Reason and Action
#' @description Assigns a reason and action for taxonomic changes.
#' @param fixed_name A fixed species name.
#' @param new_spp A `tibble` of newly described species.
#' @param taxonomic_status The taxonomic status.
#' @param survey_name The original survey name.
#' @param accepted_name The accepted name.
#' @param database_id The database ID.
#' @param common_name The common name.
#' @param species_code The species code.
#' @param new_code A new species code.
#' @param n_codes The number of codes.
#' @return A `tibble` with `reason` and `action` columns.
assign_reason_action <- function(fixed_name, new_spp, taxonomic_status,
                                 survey_name, accepted_name,
                                 database_id, common_name,
                                 species_code, new_code, n_codes) {
  reason <- dplyr::case_when(
    accepted_name %in% new_spp$species_name ~ "species code added",
    taxonomic_status != "accepted" ~ taxonomic_status,
    !grepl(RM_ALL, survey_name) &
      sum(is.na(database_id)) > 1 &
      !is.na(survey_name) &
      !grepl("morphotype", common_name) ~ "nomen nudum",
    survey_name != accepted_name & !grepl("morphotype", common_name) ~ "synonym",
    TRUE ~ ""
  )

  action <- dplyr::case_when(
    reason == "nomen nudum" ~ "delete taxon code",
    reason == "species code added" ~ "add taxon code",
    species_code != new_code & survey_name != accepted_name & n_codes > 1 ~ "change taxon code/name",
    reason != "" & survey_name != accepted_name ~ "change taxon name",
    TRUE ~ ""
  )

  tibble::tibble(reason, action)
}


#' @title Assign Survey Species
#' @description Determines the primary survey species row for each species code group.
#' @param database The database name.
#' @param database_id The database ID.
#' @return A numeric vector (0 or 1) indicating if it's the survey species.
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


# ---------------------------
# ORACLE HELPERS
# ---------------------------

#' @title Get Oracle Datatypes
#' @description Determines the Oracle data type for each column in a data frame.
#' @param x A data frame.
#' @return A character vector of Oracle data types.
oracle_datatypes <- function(x) {
  classes <- sapply(x, class)
  dplyr::case_when(
    classes == "character" ~ "VARCHAR2(255 BYTE)",
    classes == "logical" ~ "NUMBER(1,0)",
    TRUE ~ "NUMBER"
  )
}
