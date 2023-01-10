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
  "(adult)", "(juvenile)", " egg", "egg case",
  "hybrid", "larva", "larvae", " tubes"
), collapse = "|")



check_spelling <- function(species) {
  # cleaning up names
  # sp <- ifelse(str_detect(sp, " sp\\. "), sp, str_remove(sp, " sp\\."))
  sp <- species

  if (str_detect(sp, " \\(=")) {
    s <- ifelse(str_detect(sp, " [a-z]+ \\("), str_remove(sp, "[a-z]+ \\(="),
      str_to_sentence(str_remove(sp, "[A-Za-z]+ \\(="))
    )
    sp <- str_remove(s, "\\)")
  }

  sp <- str_remove(sp, rm_bits)
  sp <- trimws(sp)

  # restricting to a few sources, prioritizing WORMS; see https://resolver.globalnames.org/data_sources
  new <- gnr_resolve(sp, data_source_ids = 9, canonical = TRUE)

  if (nrow(new) == 0) {
    new <- gnr_resolve(sp,
      best_match_only = TRUE, canonical = TRUE,
      data_source_ids = c(1, 3, 12, 151, 155, 163)
    )
  }
  if (is.null(new) & sp != species) new <- sp
  if (is.null(new) | nrow(new) == 0) {
    new <- NA
  } else {
    new <- new %>%
      pluck("matched_name2") %>%
      .[1]
    if (str_detect(sp, "^[A-Za-z]+ sp\\. ") | str_detect(sp, " ") & !str_detect(new, " ")) {
      new <- NA
    }
  }
  new
}



get_classification <- function(sp, id = NA, db = NA, ask = TRUE) {
  if (is.na(db)) db <- "worms"
  if (grepl("sp. ", sp)) sp <- str_extract(sp, "^[A-Za-z]+")
  if (is.na(id)) x <- sp else x <- id
  
  res <- taxize::classification(x, db = db, fuzzy = FALSE, accepted = TRUE, ask = ask)
  
  if (is.na(res)) { # if no accepted alternatives, find any
    res <- taxize::classification(x, db = db, fuzzy = FALSE, messages = FALSE)
  }

  if (is.na(res) & grepl(" ", sp)) { # if no matching species, find genus
    try2 <- str_extract(sp, "^[A-Za-z]+")
    res <- classification(try2, db = db, fuzzy = FALSE, accepted = TRUE, messages = FALSE)
  }

  if (!is.na(res)) {
    out <- as_tibble(res[[1]])
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
  second <- "AphiaRecordsByMatchNames?scientificnames%5B%5D="

  syn_tmp <- try(worrms:::wm_GET(paste0(wm_base, first, gsub(" ", "%20", species))), silent = TRUE)

  if (any(class(syn_tmp) == "try-error")) {
    syn_tmp <- try(worrms:::wm_GET(paste0(wm_base, second, gsub(" ", "%20", species))), silent = TRUE)
    if (any(class(syn_tmp) == "try-error")) syn <- id <- NA else syn_tmp <- syn_tmp[[1]]
  }

  if (!any(class(syn_tmp) == "try-error")) {
    syn_tmp <- syn_tmp %>% filter(status != "deleted" & !grepl("invalid", unacceptreason))
    if (any(syn_tmp$status == "accepted")) {
      syn_tmp <- syn_tmp %>% filter(status %in% c("accepted", "alternate representation"))
    }
    if (nrow(syn_tmp) > 0) {
      syn <- syn_tmp %>%
        filter(!is.na(valid_name)) %>%
        pluck("valid_name") %>%
        .[1]
      id <- syn_tmp %>%
        filter(!is.na(valid_name)) %>%
        pluck("valid_AphiaID") %>%
        .[1]
    } else {
      syn <- id <- NA
    }
  }
  if (grepl(" ", species) & !grepl(" ", syn) & !grepl("sp.", species)) syn <- id <- NA
  list(syn = syn, id = id)
}



itis_syn <- function(species) {
  species <- str_remove(species, " sp.")
  id <- as.integer(get_tsn(species))

  syn <- NA

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
    }
  }

  if (!grepl(" ", species) & grepl(" ", syn)) syn <- NA

  list(syn = syn, id = id)
}



get_taxonomy <- function(species, db, db2 = NA, ask = TRUE) {
  db <- tolower(db)
  if (!grepl("sp. ", species)) {
    if (tolower(db) == "worms") ll <- wrms_syn(species)
    if (tolower(db) == "itis") ll <- itis_syn(species)

    if (is.na(ll$id) & !is.na(db2)) {
      db <- tolower(db2)
      if (tolower(db) == "wrms") ll <- wrms_syn(species)
      if (tolower(db) == "itis") ll <- itis_syn(species)
    }

    if (!is.na(db2)) {
      if (tolower(db2) == tolower(db) & !is.na(ll$syn)) {
        ll2 <- wrms_syn(ll$syn)
        if (!is.na(ll2$syn)) {
          ll <- ll2
          db <- "worms"
        }
      }
    }

    id <- ll$id
    syn <- ll$syn
  } else {
    id <- syn <- NA
  }

  if (is.na(id) | class(syn) == "try-error") {
    syn <- NA
    classy <- get_classification(species, id, db, ask)
  } else {
    classy <- get_classification(syn, id, db, ask)
  }


  out <- classy %>%
    mutate(
      rw_id = row_number(),
      accepted_name = syn,
      db_id = ifelse(rw_id == max(rw_id) & !is.na(accepted_name), as.numeric(id), NA),
      rank = tolower(rank),
      db = ifelse(!is.na(accepted_name), toupper(db), NA)
    ) %>%
    fill(db_id, .direction = "downup") %>%
    select(-id, -rw_id)

  return(out)
}
