Purpose: Create taxon grouping tables (taxon_group_composition & taxon_group_stanzas) based on minimum ID standards
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## ============================================================
## 1) Filter to survey species only
## ============================================================

survey_species_taxa <- taxonomy_final %>%
  dplyr::select(-c(survey_name, reason, action, old_code)) %>%
  dplyr::arrange(species_code) %>%
  filter(active == 1)


## ============================================================
## 2) Identify species that were renamed or synonymized
##    (map old species codes onto current taxonomy)
## ============================================================

synonyms <- bind_rows(taxonomic_changes_new, taxonomic_changes_current) %>%
  filter(old_species_code != new_species_code) %>%
  select(
    old_species_name,
    species_code = old_species_code,
    new_code = new_species_code
  ) %>%
  left_join(
    survey_species_taxa %>% select(-species_name),
    by = c("new_code" = "species_code")
  ) %>%
  rename(species_name = old_species_name)


## ============================================================
## 3) Long-form taxonomy table
## ============================================================

taxonomy_long <- bind_rows(survey_species_taxa, synonyms) %>%
  pivot_longer(
    cols      = all_of(KEEP_RANKS),
    names_to  = "rank",
    values_to = "taxon"
  )


## ============================================================
## 4) Taxonomic sub-groups that are exceptions to default
##    minimum-ID standards
## ============================================================

exceptions <- tibble(
  taxon    = c("Lycodapus", "Myctophidae", "Liparidae", "Henricia", "Pteraster"),
  id_level = "genus"
)


## ============================================================
## 5) Read and prepare minimum ID table
##    - standardize id_level
##    - add exception taxa
##    - attach taxonomic rank and rank priority
## ============================================================

min_id <- read_csv("data/min_id.csv") %>%
  janitor::clean_names() %>%
  mutate(id_level = tolower(id_level)) %>%
  bind_rows(exceptions) %>%
  left_join(
    taxonomy_long %>%
      select(taxon, taxon_rank = rank) %>%
      unique(),
    by = "taxon"
  ) %>%
  mutate(rank_priority = match(taxon_rank, KEEP_RANKS))


## ============================================================
## 6) Expand minimum ID rules to species level
##    (convert higher-level taxa to all matching species)
## ============================================================

min_id_expanded <- min_id %>%
  filter(is.na(species_code) & !grepl("species", id_level)) %>%
  select(group, taxon, id_level, rank_priority) %>%
  left_join(taxonomy_long, by = "taxon") %>%
  filter(id_rank == id_level) %>%
  select(
    group = taxon,
    taxon = species_name,
    id_level,
    species_code,
    rank_priority
  ) %>%
  mutate(taxon = str_remove(taxon, " sp\\.$")) %>%
  bind_rows(min_id, .) %>%
  filter(!(is.na(species_code) & !grepl("species", id_level)))


## ============================================================
## 7) Sanity check: all minimum-ID taxa must exist in taxonomy
## ============================================================

if (!all(min_id$taxon %in% taxonomy_long$taxon)) {
  min_id[which(!min_id$taxon %in% taxonomy_long$taxon), "taxon"] %>% print()
  stop("Some taxa in min_id_expanded are not found in taxonomy_long")
}


## ============================================================
## 8) Match species to minimum ID rules
##    - find all applicable taxa
##    - resolve conflicts using rank_priority and taxonomic rank
## ============================================================

rank_expanded <- min_id_expanded %>%
  select(group, taxon, id_level, gp_code = species_code, rank_priority) %>%
  full_join(taxonomy_long, by = "taxon") %>%
  mutate(
    taxon_rank = ifelse(
      taxon == str_remove(species_name, " sp\\.$"),
      id_rank,
      NA
    )
  ) %>%
  group_by(taxon) %>%
  fill(taxon_rank, .direction = "downup") %>%
  select(
    group, gp_code,
    taxon, taxon_rank,
    species_name, common_name, species_code, new_code,
    rank_priority
  ) %>%
  group_by(species_code) %>%
  mutate(rank = match(taxon_rank, KEEP_RANKS)) %>%
  arrange(rank_priority, rank) %>%
  slice(1) %>%
  ungroup()


## ============================================================
## 9) Final mapping from species_code to group_code
## ============================================================

taxon_groups <- rank_expanded %>%
  mutate(group_code = coalesce(gp_code, new_code, species_code)) %>%
  distinct(species_code, group_code, .keep_all = TRUE) %>%
<<<<<<<< Updated upstream:R/02_update_grouping_tables.R
  left_join(bind_rows(survey_species_taxa, synonyms)) %>%
  select(taxon, group_code, species_code, species_name, common_name) %>%
  filter(!is.na(species_code)) %>%
  arrange(species_code)


## ============================================================
## 10) Incorporate group stanzas and composition
## ============================================================

## During the early part of the time series, some fish were aggregated to genus
## until GAP established high taxonomic ID confidence.
## Thus, historical groupings are defined for 1980-1981, and modern groupings
## Historical group tables are created in: R/utils/constants.R

historical_groups <- full_join(
  historical_group_stanzas,
  historical_group_composition
) %>%
  group_by(group_id) %>%
  mutate(group_stanza_id = cur_group_id() +
    length(unique(taxon_groups$group_code))) %>%
  select(-group_id)
========
  left_join(bind_rows(survey_species_taxa, synonyms)) %>% 
  select(taxon, group_code, species_code, species_name, common_name) %>%
  filter!is.na(species_code) %>%
  arrange(desc(species_code))



taxon_groups %>%
  group_by(group_code) %>% 
  mutate(year_start = 1985,
         year_end = Sys.Date() %>% format("%Y") %>% as.numeric()
         ) %>%
  arrange(group_code, species_code)


###### TESTING HERE
# species codes without groups
rank_expanded %>% 
  mutate(group_code = coalesce(gp_code, new_code, species_code)) %>%
  distinct(group, species_code, group_code, .keep_all = TRUE) %>%
  select(group, taxon, taxon_rank, group_code, species_code, species_name, common_name) %>%
  left_join(survey_species_taxa) %>% filter(is.na(group)) %>% 
  left_join(min_id_expanded) %>% 
  mutate(id_to = case_when(
    grepl("species", id_level) ~ "species",
    id_level == "genus" ~ "genus",
    TRUE ~ taxon_rank)) %>%
  select(group:common_name, id_to, id_rank:kingdom) %>% filter(id_to != id_rank) %>% view
>>>>>>>> Stashed changes:R/02_create_grouping_tables.R


## Reference table used to update year_start for modern groups
## (based on when historical groupings ended)

year_ref <- historical_groups %>%
  select(group_code, year_end) %>%
  unique() %>%
  group_by(group_code) %>%
  slice_max(year_end)


## Build modern taxon group stanzas and append historical group stanzas

taxon_group_stanzas <- taxon_groups %>%
  # Define modern group time bounds and assign new stanza IDs
  group_by(group_code) %>%
  mutate(
    year_start      = 1982,
    year_end        = format(Sys.Date(), "%Y") %>% as.numeric(),
    group_stanza_id = cur_group_id()
  ) %>%
  # Add species-specific year information
  left_join(species_year_current, by = "species_code") %>%
  # Join historical group end years to adjust modern year_start
  left_join(year_ref, by = "group_code") %>%
  # Update year_start to follow the last historical grouping, if present
  mutate(
    year_start = coalesce(year_end.y + 1, year_started, year_start)
  ) %>%
  # Clean up columns and standardize naming
  select(
    taxon:year_start,
    year_end = year_end.x,
    group_stanza_id
  ) %>%
  # Append historical group stanzas
  bind_rows(historical_groups) %>%
  # Ensure correct temporal ordering
  arrange(group_stanza_id)


## Derive group composition table

taxon_group_composition_new <- taxon_group_stanzas %>%
  select(group_stanza_id, species_code) %>%
  distinct() %>%
  rename_with(toupper)


## Derive group stanza table

taxon_group_stanzas_new <- taxon_group_stanzas %>%
  select(group_stanza_id, group_code, year_start, year_end) %>%
  distinct() %>%
  rename_with(toupper)
