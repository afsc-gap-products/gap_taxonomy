## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions.R -- taxonomy helpers for the GAP_TAXONOMY pipeline
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ---------------------------
# STANDARDIZING NAMES FUNCTIONS
# ---------------------------

#' @title Fix Species Names
#' @description Cleans and formats species names, particularly for "cf." (confer) cases, by appending relevant taxonomic suffixes from old names to new ones.
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

  nm <- stringr::str_remove(new_nm, " var\\.")

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
    if (!is.na(extra_bits)) {
      nm <- paste(new_nm, extra_bits)
    }
  }

  nm <- stringr::str_replace_all(nm, "\\s+", " ")
  trimws(nm)
}


#' @title Clean Species Names for Taxize
#' @description Cleans up species names by removing suffixes and fixing formatting for use with taxonomic databases.
#' @param species A character string of the species name.
#' @return A cleaned character string or `NA_character_` if input is `NA`.
standardize_names <- function(species) {
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
#' @param presets A data frame of preset taxonomic IDs (columns taxon, dbe, db_id), or NULL.
#' @return A `tibble` containing the taxonomic hierarchy (name, rank, ID) or a default tibble of NAs if no match is found.
get_classification <- function(sp = NA, id = NA, db, ask = TRUE, presets = NULL) {
  if (is.na(sp)) {
    return(tibble::tibble(name = NA, rank = NA, id = NA))
  }

  if (!is.null(presets) && sp %in% presets$taxon && db %in% presets$dbe[presets$taxon == sp]) {
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
#' @param presets Preset taxonomic IDs; defaults to the global `defined_tax`.
#' @param ... Additional arguments passed to `get_classification`.
#' @return A `tibble` with columns name, rank and id_rank (one row per taxonomic rank).
get_classification_formatted <- function(sp, database = NA, presets = NULL, ...) {
  if (is.null(presets)) presets <- defined_tax # looked up inside the body so furrr detects the global
  db <- ifelse(is.na(database), "worms", tolower(database))
  classy <- get_classification(sp, db = db, presets = presets, ...) %>%
    dplyr::filter(rank != "subspecies")

  # No usable classification (NA ranks are dropped above): return an empty result
  # instead of failing on `if (logical(0))`
  if (nrow(classy) == 0) {
    return(tibble::tibble(name = character(), rank = character(), id_rank = character()))
  }

  if (grepl(" ", sp) && classy$rank[nrow(classy)] != "species") {
    classy <- classy %>%
      dplyr::bind_rows(data.frame(name = sp, rank = "species"))
  }
  classy %>%
    dplyr::mutate(
      rw_id = dplyr::row_number(),
      id_rank = ifelse(rw_id == max(rw_id), tolower(rank), NA)
    ) %>%
    tidyr::fill(id_rank, .direction = "downup") %>%
    dplyr::select(-id, -rw_id)
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
        dplyr::mutate(
          database_name = tmp$name[nrow(tmp)],
          database = "worms",
          taxonomic_status = "accepted",
          rank = tolower(rank),
          database_id = tmp$id[nrow(tmp)],
          id_rank = tolower(tmp$rank[nrow(tmp)])
        ) %>%
        dplyr::select(-id) %>%
        tidyr::pivot_wider(names_from = rank, values_from = name)
      if (nrow(db_info) > 0) { 
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
    dplyr::rowwise() %>%
    dplyr::mutate(
      database_name = ifelse(species %in% paste("Porifera", c("ball", "erect", "encrusting", "tubular")), species, database_name),
      classy = purrr::map2(database_name, database, ~ get_classification_formatted(sp = .x, database = .y)),
      database = toupper(database),
      taxonomic_status = tolower(taxonomic_status)
    ) %>%
    tidyr::unnest(classy) %>%
    tidyr::pivot_wider(names_from = rank, values_from = name) %>%
    dplyr::filter(kingdom == "Animalia") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      database_name = ifelse(grepl("\\s", species) & !grepl("\\s", database_name) & !grepl(RM_ALL, species), NA, database_name),
      id_rank = ifelse(is.na(database_name), NA, id_rank),
      database_id = ifelse(is.na(database_name), NA, database_id)
    ) %>%
    dplyr::select(-survey_name)
}

#' @title Get Taxonomy Safely
#' @description Wrapper to safely call `get_taxonomy` and handle API errors.
#' @param nm The name to look up.
#' @return A `tibble` with taxonomic information, or an NA tibble if every attempt fails.
get_taxonomy_safely <- function(nm) {
  purrr::possibly(
    function(x) {
      retry::retry(
        expr = {
          get_taxonomy(x)
        },
        when = function(e) inherits(e, "error"),
        max_tries = 5,
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
#' @param fixed_name A fixed species name (currently unused; kept for call compatibility).
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


#' @title Assign Active Status
#' @description Determines the primary survey species row for each species code group.
#' @param database The database name.
#' @param database_id The database ID.
#' @return A numeric vector (0 or 1) indicating if it's the survey species.
assign_active_status <- function(database, database_id) {
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
# ORACLE CONNECTION
# ---------------------------

#' Ensure an Active Oracle Connection Exists
#' @description Checks if an RODBC channel exists and is alive using a test query. If missing or broken, it closes any stale handle and establishes a new connection.
#' @param channel An existing RODBC channel object. If NULL, checks the calling environment.
#' @param db The database name to pass to gapindex::get_connected() (default: "AFSC").
#' @param check_access Logical, whether gapindex checks access levels (default: FALSE).
#' @return An active, verified RODBC connection handle.
oracle_connect <- function(channel = NULL, db = "AFSC", check_access = FALSE) {
  # If channel not supplied directly, check if 'channel' exists in calling scope
  if (is.null(channel)) {
    if (exists("channel", envir = parent.frame())) {
      channel <- get("channel", envir = parent.frame())
    }
  }

  need_connect <- FALSE

  if (is.null(channel)) {
    message("Oracle channel does not exist. Connecting...")
    need_connect <- TRUE
  } else {
    # 1. Test existing connection
    test_result <- try(RODBC::sqlQuery(channel, "SELECT 1 FROM DUAL"), silent = TRUE)

    # 2. Check if query threw an R error or returned an ODBC/Oracle error message
    is_broken <- inherits(test_result, "try-error") ||
      (is.character(test_result) && any(grepl("ERROR|ORA-", test_result, ignore.case = TRUE)))

    if (is_broken) {
      message("Oracle connection is broken or stale. Attempting to reconnect...")
      try(RODBC::odbcClose(channel), silent = TRUE)
      need_connect <- TRUE
    } else {
      message("Oracle connection is active and working.")
    }
  }

  # Establish new connection if needed
  if (need_connect) {
    channel <- gapindex::get_connected(db = db, check_access = check_access)
  }

  channel
}


# ==============================================================================
# QA/QC AUDIT SUITE FOR RACE_DATA TABLES
# ==============================================================================

#' @title Run Taxonomy QA/QC Audit Suite
#' @description Executes data integrity checks on the taxonomic hierarchy and species codes: parent/child rank ordering, orphaned and self-referencing parents, primary key uniqueness, foreign keys from race_species_codes, valid `active` flags, and unintended detached nodes.
#' @param species_taxonomics_new A data frame containing the updated taxonomic hierarchy.
#' @param race_species_codes_new A data frame containing the updated species mapping codes.
#' @param top_level_rank_threshold Ranks with `taxonomic_rank_id` above this must have a parent (e.g. Kingdom = 10 and Phylum = 30 may be roots).
#' @return `TRUE` if all checks passed, otherwise `FALSE`. Assign it (e.g. `race_qaqc_passed <- run_taxonomy_qaqc(...)`): the Oracle update gates on it.
run_taxonomy_qaqc <- function(species_taxonomics_new, race_species_codes_new,
                              top_level_rank_threshold = 30) {
  message("\n==================================================")
  message("         RUNNING TAXONOMY QA/QC AUDIT SUITE       ")
  message("==================================================\n")

  # Reports one check; returns TRUE when n_bad == 0. `detail` is printed on failure.
  check <- function(id, n_bad, pass_msg, fail_msg, detail = NULL) {
    if (n_bad > 0) {
      warning(sprintf("\u274c CHECK %d FAILED: %s", id, sprintf(fail_msg, n_bad)), call. = FALSE)
      if (!is.null(detail)) print(detail)
      FALSE
    } else {
      message(sprintf("\u2705 CHECK %d PASSED: %s", id, pass_msg))
      TRUE
    }
  }

  valid_ids <- species_taxonomics_new$species_taxonomic_id
  results <- logical()

  # 1. Parent rank must be strictly higher than child rank
  rank_check <- species_taxonomics_new %>%
    dplyr::filter(!is.na(parent_taxonomic_id)) %>%
    dplyr::left_join(
      species_taxonomics_new %>%
        dplyr::select(parent_id = species_taxonomic_id, parent_rank_id = taxonomic_rank_id),
      by = c("parent_taxonomic_id" = "parent_id")
    ) %>%
    dplyr::filter(parent_rank_id >= taxonomic_rank_id)
  results[1] <- check(
    1, nrow(rank_check),
    "All parent taxonomic ranks are strictly higher than child ranks.",
    "Found %d records where parent rank is NOT higher than child rank!",
    dplyr::select(
      rank_check, species_taxonomic_id, name, taxonomic_rank_id,
      parent_taxonomic_id, parent_rank_id
    )
  )

  # 2. Orphan parent IDs (foreign key within the hierarchy)
  orphaned_parents <- species_taxonomics_new %>%
    dplyr::filter(!is.na(parent_taxonomic_id) & !parent_taxonomic_id %in% valid_ids)
  results[2] <- check(
    2, nrow(orphaned_parents),
    "Zero orphaned parent_taxonomic_id references found.",
    "Found %d orphaned parent_taxonomic_ids referencing non-existent IDs!",
    dplyr::select(orphaned_parents, species_taxonomic_id, name, parent_taxonomic_id)
  )

  # 3. Self-referencing nodes
  self_loops <- species_taxonomics_new %>%
    dplyr::filter(!is.na(parent_taxonomic_id) & species_taxonomic_id == parent_taxonomic_id)
  results[3] <- check(
    3, nrow(self_loops),
    "Zero self-referencing nodes found.",
    "Found %d self-referencing nodes!",
    dplyr::select(self_loops, species_taxonomic_id, name, parent_taxonomic_id)
  )

  # 4. Primary key uniqueness
  dup_tax_ids <- sum(duplicated(species_taxonomics_new$species_taxonomic_id))
  dup_race_codes <- sum(duplicated(race_species_codes_new$species_code))
  results[4] <- check(
    4, dup_tax_ids + dup_race_codes,
    "Primary keys in both tables are 100% unique.",
    paste0(
      "Duplicate PKs found! (total %d; taxonomics: ", dup_tax_ids,
      ", race codes: ", dup_race_codes, ")"
    )
  )

  # 5. Foreign key: race_species_codes -> species_taxonomics
  unmapped_fk <- race_species_codes_new %>%
    dplyr::filter(!is.na(species_taxonomic_id) & !species_taxonomic_id %in% valid_ids)
  results[5] <- check(
    5, nrow(unmapped_fk),
    "All species_taxonomic_id foreign keys in race_species_codes_new exist.",
    "Found %d species_codes linked to invalid species_taxonomic_ids!",
    dplyr::select(unmapped_fk, species_code, species_name, species_taxonomic_id)
  )

  # 6. Active flags
  invalid_active <- race_species_codes_new %>%
    dplyr::filter(!active %in% c("Y", "N") | is.na(active))
  results[6] <- check(
    6, nrow(invalid_active),
    "All active status flags are valid 'Y' or 'N' values.",
    "Found %d rows with invalid 'active' flags (not Y/N)!"
  )

  # 7. Lower-rank taxa detached from the tree
  unintended_orphans <- species_taxonomics_new %>%
    dplyr::filter(is.na(parent_taxonomic_id) & taxonomic_rank_id > top_level_rank_threshold)
  results[7] <- check(
    7, nrow(unintended_orphans),
    "Zero lower-rank orphan nodes with missing parent IDs.",
    "Found %d lower-rank taxa detached from the tree (parent_taxonomic_id = NA)!",
    dplyr::select(unintended_orphans, species_taxonomic_id, taxonomic_rank_id, name, parent_taxonomic_id)
  )

  passed_all <- all(results)

  message("\n--------------------------------------------------")
  if (passed_all) {
    message("ALL QA/QC CHECKS PASSED SUCCESSFULLY!")
  } else {
    message(" QA/QC AUDIT COMPLETED WITH WARNINGS. REVIEW OUTPUT ABOVE.")
  }
  message("--------------------------------------------------\n")

  passed_all
}
