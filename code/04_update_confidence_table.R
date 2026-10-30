#' -----------------------------------------------------------------------------
#' Updating of the TAXONOMIC_CONFIDENCE table
#' -----------------------------------------------------------------------------
#'
#' This script creates the taxonomic identification confidence table for each
#' survey (survey_definition_id) and survey year, used currently for FOSS.
#'
#' What it does (in order):
#'   1. Starts from the confidence table currently in Oracle
#'      (oracle_tables$taxonomic_confidence). Optionally, it can be rebuilt from the
#'      regional Google Drive spreadsheets instead (commented-out block below).
#'   2. Imputes survey years that have no rows by copying the most recent prior
#'      year of the same survey.
#'   3. Adds species that are NEW to taxonomic_classification. They get a row for
#'      every survey/year with no assessment (taxon_confidence_code = NA, which is
#'      labelled "Unassessed") until someone assesses them in the spreadsheets.
#'   4. Keeps only species_codes that are group_codes in the taxon group stanzas.
#'
#'        Quality Codes:
#'        1 - High confidence and consistency. Taxonomy is stable and reliable
#'            at this level, and field identification characteristics are well
#'            known and reliable.
#'        2 - Moderate confidence. Taxonomy may be questionable at this level,
#'            or field identification characteristics may be variable and
#'            difficult to assess consistently.
#'        3 - Low confidence. Taxonomy is incompletely known, or reliable field
#'            identification characteristics are unknown.
#'        NA - Unassessed. Taxonomy quality has not been assessed.
#'
#' Expects from earlier steps: oracle_tables, survey_years (lowercase columns
#' survey_definition_id, year), this_year, taxonomic_classification_new,
#' taxonomic_classification_current, taxon_group_stanzas_updated.

# Code given to newly added species (NA = "Unassessed"). Change to 1/2/3 to
# assign a default confidence instead.
new_species_default_code <- NA_real_



# ------------------------------------------------------------------------------
# 1. BASE TAXONOMIC CONFIDENCE DATA
# ------------------------------------------------------------------------------

# Current Oracle table. Keep this object UNCHANGED: 05 compares the new table
# against it to work out inserts, updates and deletes.
taxon_confidence_current <- oracle_tables$taxonomic_confidence

stopifnot(all(c("species_code", "year", "taxon_confidence_code", "survey_definition_id") %in%
                names(taxon_confidence_current)))

# Working copy
taxonomic_confidence <- taxon_confidence_current %>%
  dplyr::select(species_code, year, taxon_confidence_code, survey_definition_id)

# To REBUILD the base from the Google Drive spreadsheets instead, uncomment the
# block at the bottom of this file ("OPTIONAL: REBUILD FROM SPREADSHEETS"), run it,
# and then uncomment the next line:
# taxonomic_confidence <- taxonomic_confidence_rebuilt


# ------------------------------------------------------------------------------
# 2. IMPUTE MISSING/NEW SURVEY YEARS FROM PREVIOUS SURVEY YEARS
# ------------------------------------------------------------------------------
missing_survey_years <- dplyr::anti_join(
  x  = subset(survey_years, year <= this_year),
  y  = unique(taxonomic_confidence[, c("survey_definition_id", "year")]),
  by = c("survey_definition_id", "year")
) %>%
  dplyr::arrange(year)   # oldest first so consecutive missing years chain correctly

if (nrow(missing_survey_years) > 0) {
  for (iyear in seq_len(nrow(missing_survey_years))) {
    target_srvy <- missing_survey_years$survey_definition_id[iyear]
    target_year <- missing_survey_years$year[iyear]
    
    # Most recent prior year available for this survey
    prior_years <- subset(
      x = unique(taxonomic_confidence[, c("survey_definition_id", "year")]),
      subset = survey_definition_id == target_srvy & year < target_year
    )
    
    if (nrow(prior_years) > 0) {
      imputed_rows <- taxonomic_confidence %>%
        dplyr::filter(survey_definition_id == target_srvy,
                      year == max(prior_years$year)) %>%
        dplyr::mutate(year = target_year)
      
      taxonomic_confidence <- dplyr::bind_rows(taxonomic_confidence, imputed_rows)
    }
  }
}


# ------------------------------------------------------------------------------
# 3. ADD NEW SPECIES FROM TAXONOMIC_CLASSIFICATION
#    "New" = species_code added to taxonomic_classification this run
#    (in the new table but not in the current one) and not already in the
#    confidence table. They get a row for every survey/year (incl. imputed ones).
# ------------------------------------------------------------------------------
added_species <- setdiff(
  unique(taxonomic_classification_new$species_code),
  unique(taxonomic_classification_current$species_code)
)
new_species <- setdiff(added_species, unique(taxonomic_confidence$species_code))

if (length(new_species) > 0) {
  survey_year_grid <- unique(taxonomic_confidence[, c("survey_definition_id", "year")])
  
  new_species_rows <- tidyr::crossing(species_code = new_species, survey_year_grid) %>%
    dplyr::mutate(taxon_confidence_code = new_species_default_code)
  
  taxonomic_confidence <- dplyr::bind_rows(taxonomic_confidence, new_species_rows)
}
message(sprintf("Confidence table: %d new species from taxonomic_classification (%d survey/year rows each)",
                length(new_species), if (length(new_species) > 0) nrow(survey_year_grid) else 0L))


# ------------------------------------------------------------------------------
# 4. TEXT LABEL + FILTER BY VALID TAXON GROUP SPECIES CODES
# ------------------------------------------------------------------------------
taxonomic_confidence <- taxonomic_confidence %>%
  dplyr::mutate(
    taxon_confidence = dplyr::case_when(
      taxon_confidence_code == 1 ~ "High",
      taxon_confidence_code == 2 ~ "Moderate",
      taxon_confidence_code == 3 ~ "Low",
      TRUE ~ "Unassessed"
    )
  )

spp_codes <- unique(taxon_group_stanzas_updated$group_code)

# New species that are not group_codes are dropped by the filter below; say so
not_in_groups <- setdiff(new_species, spp_codes)
if (length(not_in_groups) > 0) {
  message(sprintf("  %d new species are not in taxon_group_stanzas and were NOT added to the confidence table",
                  length(not_in_groups)))
}

taxonomic_confidence_new <- taxonomic_confidence %>%
  dplyr::filter(species_code %in% spp_codes) %>%
  dplyr::select(
    species_code,
    year,
    taxon_confidence,
    taxon_confidence_code,
    survey_definition_id
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(dplyr::desc(year), species_code)


# ------------------------------------------------------------------------------
# OPTIONAL: REBUILD FROM SPREADSHEETS (commented out)
#
# Recreates the table from the regional Taxon_confidence_<REGION>.xlsx files in
# Google Drive, as the original GAP_PRODUCTS script did. Use it when scores were
# revised or added in the spreadsheets. To use it:
#   1. uncomment this block and run it (needs a Google login and the network),
#   2. uncomment `taxonomic_confidence <- taxonomic_confidence_rebuilt` in section 1,
#   3. rerun this script from section 1. Revised scores then show up in step 05 as
#      UPDATEs, new rows as INSERTs and removed rows as DELETEs. If the rebuild
#      removes more than cfg$max_delete_frac of the table, step 05 stops (see config.R).
#
# Needs: readxl, googledrive, janitor, tidyr, dplyr, stringr.
# There is no NBS spreadsheet: NBS uses the EBS scores, as before.
# ------------------------------------------------------------------------------
# googledrive::drive_auth()
#
# data_sources <- googledrive::drive_ls(
#   path = googledrive::as_id(x = "1s-BKOnfiuF3b0642C_DGhLcuRv-MOPxO")
# )
# dir.create("temp", showWarnings = FALSE)
# for (idata in seq_len(nrow(data_sources))) {
#   googledrive::drive_download(file = data_sources$id[idata],
#                               path = file.path("temp", data_sources$name[idata]),
#                               overwrite = TRUE)
# }
#
# srvy_ids <- c(AI = 52, GOA = 47, EBS = 98, BSS = 78, NBS = 143)
# survey_years_for <- function(id) survey_years$year[survey_years$survey_definition_id == id]
#
# rebuilt <- list()
# for (iregion in c("GOA", "AI", "EBS", "BSS")) {
#
#   ## Import and clean the spreadsheet for this region
#   wide <- readxl::read_excel(
#     path = file.path("temp", paste0("Taxon_confidence_", iregion, ".xlsx")),
#     skip = 1, col_names = TRUE
#   ) %>%
#     dplyr::select(where(~ !all(is.na(.x)))) %>%     # remove empty columns
#     janitor::clean_names() %>%
#     dplyr::rename(species_code = code)
#
#   ## quality_codes is inconsistently provided across tables, so drop it
#   if ("quality_codes" %in% names(wide)) wide <- dplyr::select(wide, -quality_codes)
#
#   ## Year columns look like x1982_0. Find the most recent one (selected BY NAME)
#   year_cols <- grep("^x[0-9]{4}", names(wide), value = TRUE)
#   year_nums <- as.numeric(substr(year_cols, 2, 5))
#   last_year <- max(year_nums)
#   last_col  <- year_cols[which.max(year_nums)]
#
#   ## If the spreadsheet stops before the current year, carry its last year
#   ## forward to the later survey years
#   later_years <- survey_years_for(srvy_ids[[iregion]])
#   later_years <- later_years[later_years > last_year & later_years <= this_year]
#   for (yr in later_years) wide[[paste0("x", yr, "_0")]] <- wide[[last_col]]
#
#   ## Lengthen (creates the year field)
#   long_all <- wide %>%
#     dplyr::select(-dplyr::any_of(c("scientific_name", "common_name"))) %>%
#     tidyr::pivot_longer(cols = tidyr::matches("^x[0-9]{4}"),
#                         names_to = "year", values_to = "taxon_confidence_code") %>%
#     dplyr::mutate(year = as.numeric(stringr::str_extract(year, "[0-9]{4}")),
#                   taxon_confidence_code = suppressWarnings(as.numeric(taxon_confidence_code))) %>%
#     dplyr::distinct()
#
#   ## Keep only years in which a bottom trawl survey occurred
#   rebuilt[[iregion]] <- long_all %>%
#     dplyr::filter(year %in% survey_years_for(srvy_ids[[iregion]])) %>%
#     dplyr::mutate(survey_definition_id = srvy_ids[[iregion]])
#
#   ## No NBS spreadsheet: NBS = EBS scores, in NBS survey years
#   if (iregion == "EBS") {
#     rebuilt[["NBS"]] <- long_all %>%
#       dplyr::filter(year %in% survey_years_for(srvy_ids[["NBS"]])) %>%
#       dplyr::mutate(survey_definition_id = srvy_ids[["NBS"]])
#   }
# }
#
# taxonomic_confidence_rebuilt <- dplyr::bind_rows(rebuilt) %>%
#   dplyr::select(species_code, year, taxon_confidence_code, survey_definition_id)