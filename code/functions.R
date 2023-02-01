# pre-defined taxa with multiple matches to avoid user prompts
defined_tax <- tribble(
  ~taxon, ~db_id,
  "Clavularia", 125286,
  "Selachii", 368408,
  "Vulcanella", 170325,
  "Ascidian", 1839
)



# taxonomic ranks to keep in final output
keep_ranks <- c(
  "genus",
  "subfamily", "family", "superfamily",
  "suborder", "order", "superorder",
  "subclass", "class", "superclass",
  "subphylum", "phylum",
  "kingdom"
)



# vector to temporarily remove extraneous info from species name to find db matches
rm_bits <- paste0(c(
  "\\(adult\\)", "\\(juvenile\\)", "egg", "egg case",
  "hybrid", "larva", "larvae", "tubes", "group"
), collapse = "| ")

rm_all <- paste0(rm_bits, "| sp\\. cf\\.| sp\\.| cf\\.")



# function to get suffix of species with rm_bits so I can update taxonomy appropriately
fix_names <- function(old_nm, new_nm) {
  if (str_detect(old_nm, "cf\\.")) {
    # grab end bits
    ps <- str_extract(old_nm, "\\(.+\\)$|group$")
    if (is.na(ps)) ps <- "" else old_nm <- str_remove(old_nm, " \\(.+\\)$|group$")

    # grab middle bits
    md <- str_extract(old_nm, "sp\\. cf\\.|cf\\.")

    # if new name doesn't have species take species from old name
    if (grepl(" ", new_nm)) {
      nm <- paste(str_extract(new_nm, "^\\w+"), md, str_extract(new_nm, "\\w+$"), ps)
    } else {
      nm <- paste(str_extract(new_nm, "^\\w+"), md, str_extract(old_nm, "\\w+$"), ps)
    }
  } else {
    w <- str_split(rm_all, "\\|")[[1]] # extra bits to paste on
    base <- paste0(w[w != "hybrid"], "(.+)?$", collapse = "|")
    nm <- paste(new_nm, str_extract(old_nm, base))
  }
  trimws( str_replace(nm, "\\s+", " ") )
}





# standardizing naming to update species properly and checking against databse for typos
check_spelling <- function(species) {
  # cleaning up names
  sp <- species

  if (!is.na(sp)) {
    if (str_detect(sp, " \\(=")) {
      s <- ifelse(str_detect(sp, " [a-z]+ \\("), str_remove(sp, "[a-z]+ \\(="),
        str_to_sentence(str_remove(sp, "[A-Za-z]+ \\(="))
      )
      sp <- str_remove(s, "\\)")
    }

    if (str_detect(sp, "cf.")) {
      sp <- str_remove(sp, " sp\\. cf\\.|cf\\. ") %>%
        str_remove("\\(.+\\)$")
    }

    sp <- str_remove_all(sp, rm_bits)
    sp <- trimws(sp)

    # restricting to a few sources, prioritizing WORMS; see https://resolver.globalnames.org/data_sources
    new_id <- gnr_resolve(sp, data_source_ids = 9, canonical = TRUE)

    if (nrow(new_id) == 0) {
      new_id <- gnr_resolve(sp,
        best_match_only = TRUE, canonical = TRUE,
        data_source_ids = c(1, 3, 12, 151, 155, 163)
      )
    }
  }
  if (exists("new_id") && is.null(new_id) & sp != species) new_id <- sp
  if (!exists("new_id") || is.null(new_id) || nrow(new_id) == 0) {
    new_id <- NA
  } else {
    new_id <- new_id %>%
      pluck("matched_name2") %>%
      .[1]

    # if species has two words and only genus returned ignore
    # if (str_detect(sp, "^[A-Za-z]+ sp\\. ") | str_detect(sp, " ") & !str_detect(new, " ")) {
    if (str_detect(sp, " ") & !str_detect(new_id, " ") & !any(grepl(rm_all, sp))) {
      new_id <- NA
    }
  }
  new_id
}



get_classification <- function(sp, id = NA, db = "worms", ask = FALSE) {
  if (!is.na(sp)) {
    if (grepl("sp\\. ", sp)) sp <- str_extract(sp, "^[A-Za-z]+")
    if (is.na(id)) x <- sp else x <- id

    res <- taxize::classification(x,
      db = db, fuzzy = FALSE,
      accepted = TRUE, message = FALSE,
      ask = ask
    )

    if (is.na(res)) { # if no accepted alternatives, find any
      res <- taxize::classification(x, db = db, fuzzy = FALSE, messages = FALSE)
    }

    if (is.na(res) & grepl(" ", sp)) { # if no matching species, find genus
      try2 <- str_extract(sp, "^[A-Za-z]+")
      res <- classification(try2, db = db, fuzzy = FALSE, accepted = TRUE, messages = FALSE)
    }
  }
  if (exists("res") && !is.na(res)) {
    out <- as_tibble(res[[1]]) %>%
      filter(!rank %in% c("Subgenus", "Variety"))
  } else {
    out <- tibble(
      name = sp,
      rank = NA,
      id = NA
    )
  }

  out
}




wrms_syn <- function(species) {
  wm_base <- "https://www.marinespecies.org/rest/"
  first <- "AphiaRecordsByName/"
  s <- gsub(" ", "%20", species)

  syn_tmp <- try(worrms:::wm_GET(paste0(wm_base, first, s, "&marine_only=false")), silent = TRUE)

  # if not found try alternative search method
  if (any(class(syn_tmp) == "try-error")) {
    second <- "AphiaRecordsByMatchNames?scientificnames%5B%5D="
    syn_tmp <- try(worrms:::wm_GET(paste0(wm_base, second, s, "&marine_only=false")), silent = TRUE)
    if (any(class(syn_tmp) == "try-error")) syn <- id <- reason <- NA else syn_tmp <- syn_tmp[[1]]
  }

  # if found and has multiple results remove rows with subpar results
  if (!any(class(syn_tmp) == "try-error")) {
    if (nrow(syn_tmp) > 1) {
      syn_tmp <- syn_tmp %>%
        filter(kingdom == "Animalia") %>%
        filter(status != "deleted" & !grepl("invalid", unacceptreason))
    }
    if (any(syn_tmp$status == "accepted")) {
      syn_tmp <- syn_tmp %>% filter(status %in% c("accepted", "alternate representation"))
    }

    if (nrow(syn_tmp) > 0) {
      tmp_out <- syn_tmp %>%
        filter(!is.na(valid_name)) %>%
        dplyr::select(valid_name, valid_AphiaID, unacceptreason)
      id <- tmp_out$valid_AphiaID[1]

      reason <- tmp_out$unacceptreason[1]
      syn <- tmp_out$valid_name[1]
    } else {
      syn <- id <- reason <- NA
    }
  }

  if (grepl(" ", species) & !grepl(" ", syn) & !grepl("sp\\.", species) |
    !grepl(" ", species) & grepl(" ", syn)) {
    syn <- id <- NA
  }
  list(syn = syn, id = id, reason = reason)
}



itis_syn <- function(species) {
  species <- str_remove(species, " sp\\.")
  id <- as.integer(get_tsn(species, ask = FALSE, messages = FALSE))

  syn <- reason <- NA

  if (!is.na(id)) {
    tmp <- ritis::accepted_names(id)
    if (nrow(tmp) == 0) {
      syn <- species
    } else {
      syn <- tmp %>%
        pluck("acceptedName") %>%
        .[1]
      id <- tmp %>%
        pluck("acceptedTsn") %>%
        .[1]
      reason <- "invalid synonym"
    }
  }

  if (!grepl(" ", species) & grepl(" ", syn)) syn <- NA

  list(syn = syn, id = id, reason = reason)
}



get_taxonomy <- function(species, db, db2 = NA, ask = FALSE, predefined_taxa = NULL) {
  message("\nRetrieving data for taxon '", species, "'\n")

  db <- tolower(db)

  # defining ID for problematic species with multiple db matches
  if (!is.na(species) && !is.null(predefined_taxa) &&
    grepl(rm_all, species) &
    str_extract(species, "\\w+") %in% predefined_taxa$taxon) {
    id <- predefined_taxa %>%
      filter(taxon == str_extract(species, "\\w+")) %>%
      pluck("db_id") %>%
      .[1]
    syn <- str_extract(species, "\\w+")
    reason <- NA
  } else {
    if (!is.na(species) && !grepl("sp\\. .$", species)) {
      if (grepl("sp\\. ..", species)) {
        species <- str_remove_all(species, rm_all)
      }

      if (tolower(db) == "worms") ll <- wrms_syn(species)
      if (tolower(db) == "itis") ll <- itis_syn(species)

      # if no success with database 1, try database 2
      if (is.na(ll$id) & !is.na(db2)) {
        db <- tolower(db2)
        if (tolower(db) == "wrms") ll <- wrms_syn(species)
        if (tolower(db) == "itis") ll <- itis_syn(species)
      }

      # if synonym found in database 2, try using those results to re-search WORMS again
      if (!is.na(db2) && tolower(db2) == tolower(db) & !is.na(ll$syn)) {
        ll2 <- wrms_syn(ll$syn)
        if (!is.na(ll2$syn)) {
          ll <- ll2
          db <- "worms"
        }
      }


      id <- ll$id
      syn <- ll$syn
      reason <- ll$reason
    } else {
      id <- syn <- reason <- NA
    }
  }

  if (is.na(id) | class(syn) == "try-error") {
    syn <- NA
    classy <- get_classification(species, id, db, ask = ask)
  } else {
    # removes subgenus assignment
    if (grepl("\\(", syn) & !grepl("\\(", species)) {
      syn <- str_remove(syn, " \\([A-Za-z]+\\)")
    }
    classy <- get_classification(syn, id, db, ask = ask)
  }

  out <- classy %>%
    mutate(
      rw_id = row_number(),
      accepted_name = syn,
      reason2 = ifelse(reason != "subgenus assignment", reason, NA),
      id_rank = ifelse(rw_id == max(rw_id) & !is.na(syn), tolower(rank), NA),
      db_id = ifelse(rw_id == max(rw_id) & !is.na(accepted_name), as.numeric(id), NA),
      rank = tolower(rank),
      db = ifelse(!is.na(accepted_name), toupper(db), NA)
    ) %>%
    fill(db_id, .direction = "downup") %>%
    fill(id_rank, .direction = "downup") %>%
    select(-id, -rw_id)

  return(out)
}
