##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create temporal stanzas and taxon groups
##   updated_gp_taxon_groups is the equivalent to taxon_groups 
##   from 02_create_taxon_group_tables.R
##
##   Resultant tables are stanzas adn taxon_groupings
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## By default (most cases) GROUP_CODES are valid for the entire standard 
## time series (1982 to the present year)
stanzas <- 
  data.frame(GROUP_CODE = unique(x = updated_gp_taxon_groups$GROUP_CODE),
             YEAR_START = 1982,
             YEAR_END = format(x = Sys.Date(), "%Y"))
stanzas$GROUP_CODE_ID <- 1:nrow(x = stanzas)

## Update taxon_groupings with unique GROUP_CODE_IDs created from stanzas
taxon_groupings <- 
  merge(x = updated_gp_taxon_groups[, c("SPECIES_CODE", "GROUP_CODE")],
        y = stanzas[, c("GROUP_CODE", "GROUP_CODE_ID")],
        by = "GROUP_CODE")

## Exception case 1: During the early part of the time series, some fish are
## aggregated to genus until GAP established high taxonomic ID confidence
historical_stanzas <- read.csv(file = "data/historical_group_ids.csv")
historical_taxonomic_species <-
  merge(x = historical_stanzas,
        y = read.csv(file = "data/historical_group_id_composition.csv"),
        by = "GROUP_CODE_ID")

for (irow in 1:nrow(x = historical_taxonomic_species)) {
  temp_group_code <- historical_taxonomic_species$SPECIES_CODE[irow]
  new_start_year <- historical_taxonomic_species$YEAR_END[irow]
  stanzas$YEAR_START[stanzas$GROUP_CODE == temp_group_code] <- new_start_year
}

for (igroup in 1:nrow(x = historical_stanzas)) {
  spp_in_group <- historical_taxonomic_species$SPECIES_CODE[
    historical_taxonomic_species$GROUP_CODE_ID == historical_stanzas$GROUP_CODE_ID[igroup]
  ]
  temp_group <- subset(taxon_groupings, 
                       SPECIES_CODE %in% spp_in_group)
  new_group_id <- max(taxon_groupings$GROUP_CODE_ID) + 1
  temp_group$GROUP_CODE_ID <- new_group_id
  temp_group$GROUP_CODE <- historical_stanzas$GROUP_CODE[igroup]
  
  taxon_groupings <-
    rbind(taxon_groupings,
          temp_group)
  
  stanzas <- 
    rbind(stanzas,
          data.frame(GROUP_CODE = historical_stanzas$GROUP_CODE[igroup],
                     GROUP_CODE_ID = new_group_id, 
                     YEAR_START = historical_stanzas$YEAR_START[igroup],
                     YEAR_END = historical_stanzas$YEAR_END[igroup])
    )
  
}

## Exception case 2: for "new" species codes created in the past, we use the 
## modify the starting year from 1982 to whenever they were created. 
spp_start_year <-
  RODBC::sqlQuery(channel = chl, 
                  query = "SELECT * FROM GAP_PRODUCTS.SPECIES_YEAR")

for (irow in 1:nrow(x = spp_start_year)) {
  stanzas$YEAR_START[
    stanzas$GROUP_CODE == spp_start_year$SPECIES_CODE[irow]
  ] <- spp_start_year$YEAR_STARTED[irow]
}

## Some checks
taxon_groupings |> #head()
  subset(subset = GROUP_CODE == 10111)
stanzas |> #head()
  subset(subset = GROUP_CODE == 10111)

merge(x = taxon_groupings,
      y = stanzas,
      by = c("GROUP_CODE_ID", "GROUP_CODE")) |>
  subset(subset = GROUP_CODE == 10111)
