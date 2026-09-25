# Taxonomic ranks to retain
KEEP_RANKS <- c(
  "genus",
  "subfamily", "family", "superfamily",
  "infraorder", "suborder", "order", "superorder",
  "infraclass", "subclass", "class", "superclass",
  "subphylum", "phylum",
  "kingdom"
)

# Regex suffix fragments
RM_BITS <- paste0(c(
  "\\(adult\\)", "\\(juvenile\\)", "eggs", "egg case", "egg",
  "hybrid", "larvae", "larva", "tubes", "group"
), collapse = "|")

# Master regex for removable suffixes
RM_ALL <- paste0(c(RM_BITS, " sp\\. cf\\.", " sp\\.", " cf\\."), collapse = "|")



# pre-defined taxa with known multiple matches to avoid user prompts; feeds into get_taxonomy functions
defined_tax <- tibble::tribble(
  ~taxon, ~db_id, ~dbe,
  "Anthozoa", 51938, "itis",
  "Chondrichthyes", 159785, "itis",
  "Clavularia", 125286, "worms",
  "Selachii", 368408, "worms",
  "Selachii", 563987, "itis",
  "Vulcanella", 170325, "worms",
  "Ascidian", 1839, "worms",
  "Ascidian", 158854, "itis",
  "Acoelida", 914208, "itis",
  "Hirudinea", 69290, "itis",
  "Weberella", 659295, "itis",
  "Liparis", 167550, "itis",
  "Calliostoma canaliculatum", 467171, "worms",
  "Ctenophora", 1248, "worms",
  "Ctenophora", 53856, "itis",
  "Cyanea", 51669, "itis",
  "Echinacea", 157821, "itis",
  "Gonostomatidae", 125601, "worms",
  "Nuculana fossa", 254459, "worms",
  "Porella", 156418, "itis",
  "Porella", 110835, "worms",
  "Sebastes", 126175,"worms"
)



# manually created group temporal stanzas table for information prior to taxonomic workflow (2025)
historical_group_stanzas <- tibble::tribble(
  ~group_id, ~group_code, ~year_start, ~year_end,
  1, 10111, 1982, 1991,
  2, 30050, 1982, 2005,
  3, 30150, 1982, 1995,
  4, 405, 1982, 1987,
  5, 405, 1988, 1998,
  6, 400, 1982, 1998
)

# manually created group composition table for information prior to taxonomic workflow (2025)
historical_group_composition <- tibble::tribble(
  ~group_id, ~species_code,
  1, 10110,
  1, 10111,
  1, 10112,
  2, 30050,
  2, 30051,
  2, 30052,
  3, 30150,
  3, 30151,
  3, 30152,
  4, 405,
  4, 435,
  4, 455,
  4, 472,
  4, 480,
  5, 405,
  5, 435,
  5, 455,
  5, 472,
  6, 400,
  6, 471
)
