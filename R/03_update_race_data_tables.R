# ==============================================================================
# REGENERATE RACE_SPECIES_CODES AND SPECIES_TAXONOMICS FROM NEW TAXONOMY
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. SETUP & TAXONOMIC RANK COLUMN MAPPING
# ------------------------------------------------------------------------------

race_codes_current <- oracle_tables$race_species_codes

# Map target taxonomic_ranks directly to taxonomic_classification_new columns
rank_map <- taxonomic_ranks %>%
  dplyr::mutate(
    clean_rank_name = stringr::str_replace_all(tolower(name), "[\\s-]", ""),
    column          = paste0(clean_rank_name, "_taxon")
  ) %>%
  dplyr::filter(column %in% colnames(taxonomic_classification_new)) %>%
  dplyr::rename(level = name) %>%
  dplyr::arrange(taxonomic_rank_id)

# Helper function to extract species epithet while preserving operational terms ("sp. A")
extract_species_epithet <- function(full_name) {
  dplyr::case_when(
    is.na(full_name) | !nzchar(trimws(full_name)) ~ NA_character_,
    # Preserve informal/operational descriptors with spaces/dots (e.g., "Colus sp. A" -> "sp. A")
    grepl("^(?:[A-Za-z]+\\s+)(sp\\.|cp\\. cf\\.|var\\.|sp\\s+[A-Za-z0-9()]+.*)$", full_name, ignore.case = TRUE) ~
      stringr::str_replace(full_name, "^[A-Za-z]+\\s+", ""),
    # Standard formal binomials (e.g., "Callianax biplicata" -> "biplicata")
    grepl("^[A-Za-z]+\\s+[A-Za-z0-9.-]+$", full_name) ~
      stringr::str_extract(full_name, "\\S+$"),
    TRUE ~ full_name
  )
}

# Extract active feed & parse species epithet
new_tax <- taxonomic_classification_new %>%
  dplyr::filter(active == 1) %>%
  dplyr::mutate(
    species_taxon = extract_species_epithet(species_name),
    genus_word    = stringr::str_extract(species_name, "^[A-Za-z]+")
  )


# ------------------------------------------------------------------------------
# 2. BUILD TAXONOMIC LINEAGE TREE & IDENTIFY DISTINCT NODES
# ------------------------------------------------------------------------------

# Pivot rank columns into long lineage chain
lineages <- new_tax %>%
  dplyr::select(species_code, dplyr::all_of(rank_map$column)) %>%
  dplyr::mutate(row_id = dplyr::row_number()) %>%
  tidyr::pivot_longer(
    cols      = -c(row_id, species_code),
    names_to  = "column",
    values_to = "name"
  ) %>%
  dplyr::filter(!is.na(name) & nzchar(trimws(name))) %>%
  dplyr::mutate(
    name = gsub(" incertae sedis", "", name),
    name = dplyr::if_else(
      column %in% c("species_taxon", "subspecies_taxon"),
      extract_species_epithet(name),
      name
    )
  ) %>%
  dplyr::left_join(rank_map, by = "column") %>%
  dplyr::arrange(row_id, taxonomic_rank_id)

# Distinct new nodes defined strictly by clean epithets
distinct_nodes <- lineages %>%
  dplyr::distinct(name, .keep_all = TRUE) %>%
  dplyr::select(name, target_rank_id = taxonomic_rank_id)


# ------------------------------------------------------------------------------
# 3. RECONCILE SURROGATE KEYS (STRICT LEGACY PK ANCHORING)
# ------------------------------------------------------------------------------

# Reconcile Legacy Nodes (Keep existing surrogate IDs intact)
legacy_nodes <- species_taxonomics_current %>%
  dplyr::left_join(distinct_nodes, by = "name") %>%
  dplyr::mutate(
    taxonomic_rank_id = dplyr::coalesce(target_rank_id, taxonomic_rank_id)
  ) %>%
  dplyr::select(
    species_taxonomic_id,
    taxonomic_rank_id,
    name,
    parent_taxonomic_id,
    taxonomic_notes,
    common_name,
    report_common_name,
    itis_tsn
  )

# Extract epithets for legacy names to prevent duplicate node creation
legacy_epithets <- extract_species_epithet(legacy_nodes$name)

# Filter truly brand-new nodes missing from species_taxonomics_current
new_nodes_only <- distinct_nodes %>%
  dplyr::filter(!name %in% legacy_nodes$name & !name %in% legacy_epithets) %>%
  dplyr::rename(taxonomic_rank_id = target_rank_id)

# Assign sequential surrogate IDs starting at max(legacy_id) + 1
max_current_id <- max(
  c(species_taxonomics_current$species_taxonomic_id, race_codes_current$species_taxonomic_id),
  na.rm = TRUE
)
if (!is.finite(max_current_id)) max_current_id <- 0

if (nrow(new_nodes_only) > 0) {
  new_nodes_only$species_taxonomic_id <- seq(
    from = max_current_id + 1,
    length.out = nrow(new_nodes_only)
  )
}

# Master ID Lookup Table
full_id_lookup <- dplyr::bind_rows(
  legacy_nodes %>% dplyr::select(name, species_taxonomic_id, taxonomic_rank_id),
  new_nodes_only %>% dplyr::select(name, species_taxonomic_id, taxonomic_rank_id)
) %>%
  dplyr::distinct(name, .keep_all = TRUE)


# ------------------------------------------------------------------------------
# 4. RESOLVE PARENT LINKS WITH DIRECT GENUS FALLBACK & OVERRIDES
# ------------------------------------------------------------------------------

# Extract valid ancestor pairs from lineages
ancestor_pairs <- lineages %>%
  dplyr::select(row_id, child_name = name) %>%
  dplyr::inner_join(
    lineages %>% dplyr::select(row_id, ancestor_name = name),
    by = "row_id",
    relationship = "many-to-many"
  ) %>%
  dplyr::filter(child_name != ancestor_name) %>%
  dplyr::left_join(full_id_lookup %>% dplyr::select(child_name = name, child_rank = taxonomic_rank_id), by = "child_name") %>%
  dplyr::left_join(full_id_lookup %>% dplyr::select(ancestor_name = name, valid_parent_id = species_taxonomic_id, ancestor_rank = taxonomic_rank_id), by = "ancestor_name") %>%
  dplyr::filter(ancestor_rank < child_rank) %>%
  dplyr::group_by(child_name) %>%
  dplyr::slice_max(order_by = ancestor_rank, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(name = child_name, valid_parent_id)

# Direct Genus-Parent Resolvers
binomial_genus_resolver <- full_id_lookup %>%
  dplyr::filter(taxonomic_rank_id >= 200) %>%
  dplyr::mutate(genus_word = stringr::str_extract(name, "^[A-Za-z]+")) %>%
  dplyr::inner_join(
    full_id_lookup %>% dplyr::filter(taxonomic_rank_id == 190) %>% dplyr::select(genus_word = name, genus_parent_id = species_taxonomic_id),
    by = "genus_word"
  ) %>%
  dplyr::select(name, genus_parent_id)

feed_genus_resolver <- new_tax %>%
  dplyr::filter(!is.na(genus_taxon) & !is.na(species_taxon)) %>%
  dplyr::select(species_taxon, genus_taxon) %>%
  dplyr::distinct(species_taxon, .keep_all = TRUE) %>%
  dplyr::left_join(
    full_id_lookup %>% dplyr::select(genus_taxon = name, genus_parent_id2 = species_taxonomic_id),
    by = "genus_taxon"
  ) %>%
  dplyr::select(name = species_taxon, genus_parent_id2)

# Assemble new nodes
if (nrow(new_nodes_only) > 0) {
  new_nodes_only <- new_nodes_only %>%
    dplyr::left_join(ancestor_pairs, by = "name") %>%
    dplyr::left_join(binomial_genus_resolver, by = "name") %>%
    dplyr::left_join(feed_genus_resolver, by = "name") %>%
    dplyr::transmute(
      species_taxonomic_id = as.numeric(species_taxonomic_id),
      taxonomic_rank_id    = as.numeric(taxonomic_rank_id),
      name                 = as.character(name),
      parent_taxonomic_id  = as.numeric(dplyr::coalesce(valid_parent_id, genus_parent_id, genus_parent_id2)),
      taxonomic_notes      = NA_character_,
      common_name          = NA_character_,
      report_common_name   = NA_character_,
      itis_tsn             = NA_real_
    )
}

species_taxonomics_new <- dplyr::bind_rows(legacy_nodes, new_nodes_only) %>%
  dplyr::arrange(species_taxonomic_id)

# Post-Processing Hierarchy Enforcement & Explicit Parent Fixes
rank_lookup_table <- species_taxonomics_new %>%
  dplyr::select(id = species_taxonomic_id, parent_rank_id = taxonomic_rank_id)

target_phylum_id <- species_taxonomics_new %>%
  dplyr::filter(name %in% c("Sipuncula", "Annelida") & taxonomic_rank_id < 80) %>%
  dplyr::pull(species_taxonomic_id) %>%
  head(1)

species_taxonomics_new <- species_taxonomics_new %>%
  dplyr::left_join(rank_lookup_table, by = c("parent_taxonomic_id" = "id")) %>%
  dplyr::left_join(ancestor_pairs, by = "name") %>%
  dplyr::left_join(binomial_genus_resolver, by = "name") %>%
  dplyr::left_join(feed_genus_resolver, by = "name") %>%
  dplyr::mutate(
    rank_is_invalid = !is.na(parent_rank_id) & parent_rank_id >= taxonomic_rank_id,
    parent_taxonomic_id = dplyr::case_when(
      species_taxonomic_id == 3503 ~ 4096, # Direct override for sp. 1 (Bayer)
      species_taxonomic_id == 2721 ~ target_phylum_id, # Re-parent Phascolosomatidea to Phylum
      !rank_is_invalid & !is.na(parent_taxonomic_id) ~ parent_taxonomic_id,
      TRUE ~ dplyr::coalesce(valid_parent_id, genus_parent_id, genus_parent_id2, parent_taxonomic_id)
    )
  ) %>%
  dplyr::select(-parent_rank_id, -rank_is_invalid, -valid_parent_id, -genus_parent_id, -genus_parent_id2)


# ------------------------------------------------------------------------------
# 5. REBUILD RACE_SPECIES_CODES TABLE (STRICT LEGACY KEY PRESERVATION)
# ------------------------------------------------------------------------------

race_fk_mapper <- race_codes_current %>%
  dplyr::filter(is.na(species_taxonomic_id) & !is.na(species_name) & nzchar(trimws(species_name))) %>%
  dplyr::select(species_code, species_name) %>%
  dplyr::left_join(
    new_tax %>% dplyr::select(species_code, feed_species_name = species_name, species_taxon),
    by = "species_code"
  ) %>%
  dplyr::mutate(
    eval_name      = dplyr::coalesce(feed_species_name, species_name),
    parsed_epithet = extract_species_epithet(eval_name)
  ) %>%
  dplyr::left_join(species_taxonomics_new %>% dplyr::select(eval_name = name, id_by_full = species_taxonomic_id), by = "eval_name") %>%
  dplyr::left_join(species_taxonomics_new %>% dplyr::select(parsed_epithet = name, id_by_parsed = species_taxonomic_id), by = "parsed_epithet") %>%
  dplyr::left_join(species_taxonomics_new %>% dplyr::select(species_taxon = name, id_by_taxon = species_taxonomic_id), by = "species_taxon") %>%
  dplyr::transmute(
    species_code,
    resolved_id = dplyr::coalesce(id_by_full, id_by_parsed, id_by_taxon)
  ) %>%
  dplyr::filter(!is.na(resolved_id)) %>%
  dplyr::distinct(species_code, .keep_all = TRUE)

# Rebuild race_codes_new, anchoring existing legacy IDs
race_codes_new <- race_codes_current %>%
  dplyr::left_join(race_fk_mapper, by = "species_code") %>%
  dplyr::left_join(
    new_tax %>% dplyr::select(species_code, active_name = species_name, active_common = common_name, new_active = active),
    by = "species_code"
  ) %>%
  dplyr::mutate(
    species_taxonomic_id = dplyr::coalesce(species_taxonomic_id, resolved_id),
    species_name = dplyr::if_else(species_code %in% new_tax$species_code, active_name, species_name),
    common_name = dplyr::coalesce(common_name, active_common),
    active = dplyr::case_when(
      new_active == 1 ~ "Y",
      new_active == 0 ~ "N",
      TRUE ~ active
    ),
    taxonomically_classifiable = dplyr::if_else(!is.na(species_taxonomic_id), "Y", "N")
  ) %>%
  dplyr::select(-active_name, -active_common, -new_active, -resolved_id)


# ------------------------------------------------------------------------------
# 6. RUN AUDIT CHECKS
# ------------------------------------------------------------------------------

run_taxonomy_qaqc(species_taxonomics_new, race_codes_new)
