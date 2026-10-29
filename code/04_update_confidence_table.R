#' -----------------------------------------------------------------------------
#' Updating of the GAP_PRODUCTS.TAXONOMIC_CONFIDENCE table
#' -----------------------------------------------------------------------------

#' This script creates the taxonomic identification confidence tables
#' for each region and survey year used currently for FOSS purposes.
#'
#'        Quality Codes:
#'        1 – High confidence and consistency. Taxonomy is stable and reliable
#'            at this level, and field identification characteristics are well
#'            known and reliable.
#'        2 – Moderate confidence. Taxonomy may be questionable at this level,
#'            or field identification characteristics may be variable and
#'            difficult to assess consistently.
#'        3 – Low confidence. Taxonomy is incompletely known, or reliable field
#'            identification characteristics are unknown.


# ------------------------------------------------------------------------------
# 1. QUERY SURVEY DESIGN AND BASE TAXONOMIC CONFIDENCE DATA
# ------------------------------------------------------------------------------
taxon_confidence_current <- oracle_tables$taxonomic_confidence
current_year <- as.integer(this_year)

# Standardize names to clean lowercase schema
taxonomic_confidence <- taxon_confidence_current %>%
  dplyr::mutate(
    taxon_confidence = dplyr::case_when(
      taxon_confidence_code == 1 ~ "High",
      taxon_confidence_code == 2 ~ "Moderate",
      taxon_confidence_code == 3 ~ "Low",
      TRUE ~ "Unassessed"
    )
  )

# ------------------------------------------------------------------------------
# 2. IMPUTE MISSING/NEW SURVEY YEARS FROM PREVIOUS SURVEY YEARS
# ------------------------------------------------------------------------------
missing_survey_years <- dplyr::anti_join(
  x  = subset(survey_years, year <= current_year),
  y  = unique(taxonomic_confidence[, c("survey_definition_id", "year")]),
  by = c("survey_definition_id", "year")
) %>%
  dplyr::arrange(year) 

if (nrow(missing_survey_years) > 0) {
  for (iyear in seq_len(nrow(missing_survey_years))) {
    target_srvy <- missing_survey_years$survey_definition_id[iyear]
    target_year <- missing_survey_years$year[iyear]
    
    # Find the most recent prior year available for this survey definition
    temp_survey_years <- subset(
      x = unique(taxonomic_confidence[, c("survey_definition_id", "year")]),
      subset = survey_definition_id == target_srvy & year < target_year
    )
    
    if (nrow(temp_survey_years) > 0) {
      imputed_year <- max(temp_survey_years$year)
      
      # Duplicate the prior year's records with the new target year
      imputed_rows <- taxonomic_confidence %>%
        dplyr::filter(
          survey_definition_id == target_srvy,
          year == imputed_year
        ) %>%
        dplyr::mutate(year = target_year)
      
      taxonomic_confidence <- dplyr::bind_rows(taxonomic_confidence, imputed_rows)
    }
  }
}

# ------------------------------------------------------------------------------
# 3. FILTER BY VALID TAXON GROUP SPECIES CODES
# ------------------------------------------------------------------------------

spp_codes <- unique(taxon_group_stanzas_updated$group_code)

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
  arrange(desc(year), species_code)
