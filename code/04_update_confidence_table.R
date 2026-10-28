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
)

if (nrow(missing_survey_years) > 0) {
  for (iyear in 1:nrow(missing_survey_years)) {
    # Query unique survey years for a given survey prior to the missing year
    temp_survey_year <- subset(
      x = unique(taxonomic_confidence[, c("survey_definition_id", "year")]),
      subset = survey_definition_id == missing_survey_years$survey_definition_id[iyear] &
        year < missing_survey_years$year[iyear]
    )

    if (nrow(temp_survey_year) > 0) {
      # Identify the most recent prior survey year
      imputed_year <- temp_survey_year[which.max(temp_survey_year$year), ]

      # Append imputed confidence rows for the missing survey year
      taxonomic_confidence <- dplyr::bind_rows(
        taxonomic_confidence,
        taxonomic_confidence %>%
          dplyr::filter(
            survey_definition_id == imputed_year$survey_definition_id &
              year == imputed_year$year
          ) %>%
          dplyr::mutate(year = missing_survey_years$year[iyear])
      )
    }
  }
}

# ------------------------------------------------------------------------------
# 3. FILTER BY VALID TAXON GROUP SPECIES CODES
# ------------------------------------------------------------------------------

spp_codes <- taxon_group_stanzas_updated %>%
  select(group_code) %>%
  unique() %>%
  .$group_code

taxonomic_confidence_new <- subset(taxonomic_confidence, species_code %in% spp_codes)
