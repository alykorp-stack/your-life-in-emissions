# Build data/country_groups.csv: region, income group, and an aggregate flag for every code
# we might meet in the data. Use it to keep aggregates (World, EU, income groups, ...) out of
# "country" lists when joining other sources.
#
# Source: World Bank country metadata (raw/worldbank_ch4/metadata_country.csv). Rows with no
# region are aggregates. A few territories the World Bank does not classify, and PRIMAP's
# own aggregates, are added by hand below. Income group is left empty where not classified.
#
# Run from the project root:  Rscript R/country_groups.r

library(tidyverse)

META_FILE <- "data/raw/worldbank_ch4/metadata_country.csv"
OUT_FILE  <- "data/country_groups.csv"

world_bank <- read_csv(META_FILE, show_col_types = FALSE) |>
  transmute(iso_code     = `Country Code`,
            country      = TableName,
            region       = Region,
            income_group = IncomeGroup,
            is_aggregate = is.na(Region))

# Not in the World Bank list, but in our emissions table
territories <- tribble(
  ~iso_code, ~country,         ~region,
  "AIA",     "Anguilla",       "Latin America & Caribbean",
  "COK",     "Cook Islands",   "East Asia & Pacific",
  "NIU",     "Niue",           "East Asia & Pacific",
  "SHN",     "Saint Helena",   "Sub-Saharan Africa",
  "TWN",     "Taiwan",         "East Asia & Pacific"
) |>
  mutate(income_group = NA_character_, is_aggregate = FALSE)

# PRIMAP's regional and political groupings (never countries)
primap_aggregates <- tribble(
  ~iso_code,   ~country,
  "EARTH",     "World",
  "ANNEXI",    "Annex I Parties",
  "NONANNEXI", "Non-Annex I Parties",
  "AOSIS",     "Alliance of Small Island States",
  "BASIC",     "BASIC countries",
  "EU28",      "European Union (28)",
  "UMBRELLA",  "Umbrella Group"
) |>
  mutate(region = NA_character_, income_group = NA_character_, is_aggregate = TRUE)

bind_rows(world_bank, territories, primap_aggregates) |>
  arrange(is_aggregate, iso_code) |>
  write_csv(OUT_FILE)
