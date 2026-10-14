# Build data/emissions_country_year.csv: one row per country and year (1850-2024)
# with CO2 and CH4 emissions, cumulative totals, population, GDP, per-capita and
# per-GDP intensities, shares of the world total, and ranks.
#
# Run from the project root:  Rscript R/emissions_country_year.r
# Sources, units, and method notes: data/README.md

library(tidyverse)
library(countrycode)
library(readxl)

# ---- Settings ----------------------------------------------------------------

RAW <- "data/raw"

PRIMAP_FILE     <- file.path(RAW, "primap/PRIMAP-hist_v2.2_19-Jan-2021.csv")   # 1850-2018
CO2_NEW_FILE    <- file.path(RAW, "owid_co2/co2.csv")                          # OWID/GCB, tonnes, excl. land use
CH4_NEW_FILE    <- file.path(RAW, "worldbank_ch4/ch4_excl_lulucf.csv")         # World Bank/EDGAR, Mt CO2e (AR5), excl. LULUCF
EDGAR_FILE      <- file.path(RAW, "edgar/EDGAR_2025_GHG_booklet_2025.xlsx")    # fills CH4 for countries missing from World Bank
POP_FILE        <- file.path(RAW, "owid_population/population.csv")            # OWID, to 2023
POP_NEW_FILE    <- file.path(RAW, "worldbank_population/population.csv")       # World Bank, extends population past 2023
GDPPC_FILE      <- file.path(RAW, "owid_gdp_per_capita/gdp_per_capita.csv")    # OWID, 2021 international $ (PPP), from 1990
OUT_FILE        <- "data/emissions_country_year.csv"

GWP_CH4 <- 27.9   # IPCC AR6 GWP100. PRIMAP reports CH4 as mass, so we convert it ourselves.

LAST_PRIMAP_YEAR <- 2018
LAST_POP_YEAR    <- 2023   # last year in the OWID population file
LAST_YEAR        <- 2024

# No CO2 after 2018 in any newer source (and no population), so left out of the table.
EXCLUDE_ISO <- c("ATA", "MCO", "SMR", "TKL", "VAT")

# World Bank and EDGAR report these members only inside a combined entity (child = parent).
# The parent's change since 2018 is applied to the child's PRIMAP 2018 value (approximation).
CH4_PROXY <- c(SRB = "SCG", MNE = "SCG", AND = "ESP", LIE = "CHE", SSD = "SDN")

# ---- Helpers -----------------------------------------------------------------

# World Bank wide csv (one column per year) -> long: iso_code, year, new
read_worldbank <- function(path) {
  read_csv(path, skip = 4, col_types = cols(.default = "c")) |>
    select(iso_code = `Country Code`, matches("^\\d{4}$")) |>
    pivot_longer(-iso_code, names_to = "year", values_to = "new",
                 names_transform = as.integer, values_transform = as.numeric)
}

# Growth-rate splice factor: value in each year after `from` divided by its value in `from`.
change_since <- function(new, from = LAST_PRIMAP_YEAR) {
  ref <- filter(new, year == from) |> select(iso_code, ref = new)
  new |>
    filter(year > from, year <= LAST_YEAR) |>
    left_join(ref, by = "iso_code") |>
    transmute(iso_code, year, ratio = if_else(ref > 0, new / ref, NA_real_))
}

# ---- 1. PRIMAP emissions, 1850-2018 -----------------------------------------
# HISTCR = country-reported priority; IPCM0EL = national total excluding land use.
# Only real countries are kept (drops EARTH, ANNEXI, EU28, AOSIS, etc.).

emissions <- read_csv(PRIMAP_FILE, show_col_types = FALSE) |>
  filter(scenario == "HISTCR", category == "IPCM0EL",
         entity %in% c("CO2", "CH4"), unit == "Gg") |>
  pivot_longer(matches("^\\d{4}$"), names_to = "year", values_to = "gg",
               names_transform = as.integer) |>
  rename(iso_code = country) |>
  mutate(country = countrycode(iso_code, "iso3c", "country.name", warn = FALSE)) |>
  filter(!is.na(country), !iso_code %in% EXCLUDE_ISO)

# Gg -> Mt, and CH4 -> Mt CO2e
emissions <- emissions |>
  mutate(mt = if_else(entity == "CH4", gg * GWP_CH4, gg) / 1000,
         entity = recode(entity, CO2 = "co2_mt", CH4 = "ch4_mtco2e")) |>
  select(iso_code, country, year, entity, mt) |>
  pivot_wider(names_from = entity, values_from = mt) |>
  arrange(iso_code, year)

# ---- 2. Extend to 2024 --------------------------------------------------------
# Growth-rate splice: PRIMAP's 2018 value x the newer source's change since 2018.
# This keeps PRIMAP's level (no jump at 2018). Countries missing from the newer
# source stay NA after 2018.

co2_new <- read_csv(CO2_NEW_FILE, show_col_types = FALSE) |>
  set_names(c("entity", "code", "year", "tonnes")) |>
  filter(!is.na(code)) |>                                # drop regional groupings without a code
  transmute(iso_code = code, year, new = tonnes / 1e6)   # t -> Mt

ch4_new <- read_worldbank(CH4_NEW_FILE) |>
  filter(!iso_code %in% names(CH4_PROXY))                # children get their parent's series below

# EDGAR CH4 (AR5, Mt CO2e, sum of the 8 non-LULUCF sectors) for countries the
# World Bank file lacks: TWN, AIA, COK, SHN, SCG (Serbia and Montenegro).
edgar_ch4 <- read_excel(EDGAR_FILE, sheet = "GHG_by_sector_and_country") |>
  rename(substance = 1, sector = 2, iso_code = 3) |>
  filter(substance == "GWP_100_AR5_CH4") |>
  select(iso_code, matches("^\\d{4}$")) |>
  pivot_longer(-iso_code, names_to = "year", values_to = "new", names_transform = as.integer) |>
  summarise(new = sum(new, na.rm = TRUE), .by = c(iso_code, year))

have_wb <- ch4_new |> filter(year == LAST_YEAR, !is.na(new)) |> pull(iso_code)
ch4_new <- bind_rows(ch4_new, filter(edgar_ch4, !iso_code %in% have_wb))

ch4_proxy <- tibble(iso_code = names(CH4_PROXY), parent = unname(CH4_PROXY)) |>
  inner_join(ch4_new, by = c(parent = "iso_code"), relationship = "many-to-many") |>  # one parent can feed several children
  select(iso_code, year, new)
ch4_new <- bind_rows(ch4_new, ch4_proxy)

extension <- emissions |>
  filter(year == LAST_PRIMAP_YEAR) |>
  select(iso_code, country, co2_base = co2_mt, ch4_base = ch4_mtco2e) |>
  cross_join(tibble(year = (LAST_PRIMAP_YEAR + 1):LAST_YEAR)) |>
  left_join(change_since(co2_new), by = c("iso_code", "year")) |>
  rename(co2_ratio = ratio) |>
  left_join(change_since(ch4_new), by = c("iso_code", "year")) |>
  transmute(iso_code, country, year,
            co2_mt     = co2_base * co2_ratio,
            ch4_mtco2e = ch4_base * ratio)

emissions <- bind_rows(emissions, extension) |>
  arrange(iso_code, year)

# ---- 3. Cumulative emissions since 1850 --------------------------------------

emissions <- emissions |>
  group_by(iso_code) |>
  mutate(co2_cum = cumsum(replace_na(co2_mt, 0)),
         ch4_cum = cumsum(replace_na(ch4_mtco2e, 0))) |>
  ungroup()

# ---- 4. Population and GDP ----------------------------------------------------
# The GDP file is per capita, so total GDP = GDP per capita x population.

population <- read_csv(POP_FILE, show_col_types = FALSE) |>
  select(iso_code = Code, year = Year, population = Population)

# After 2023: OWID's 2023 population x the World Bank's change since then (same splice as emissions).
pop_ext <- population |>
  filter(year == LAST_POP_YEAR) |>
  select(iso_code, base = population) |>
  inner_join(change_since(read_worldbank(POP_NEW_FILE), from = LAST_POP_YEAR), by = "iso_code") |>
  transmute(iso_code, year, population = base * ratio)

population <- bind_rows(population, pop_ext)

gdp_pc <- read_csv(GDPPC_FILE, show_col_types = FALSE) |>
  select(iso_code = Code, year = Year, gdp_per_capita = `GDP per capita`)

emissions <- emissions |>
  left_join(population, by = c("iso_code", "year")) |>
  left_join(gdp_pc, by = c("iso_code", "year")) |>
  mutate(gdp = gdp_per_capita * population) |>
  select(-gdp_per_capita)

# ---- 5. Intensities, shares, ranks (per gas, per year) -----------------------

emissions <- emissions |>
  mutate(co2_per_capita = co2_mt     * 1e6 / population,   # t per person
         ch4_per_capita = ch4_mtco2e * 1e6 / population,
         co2_per_gdp    = co2_mt     * 1e9 / gdp,          # kg per international $
         ch4_per_gdp    = ch4_mtco2e * 1e9 / gdp) |>
  group_by(year) |>
  mutate(co2_share_annual = co2_mt     / sum(co2_mt, na.rm = TRUE),
         ch4_share_annual = ch4_mtco2e / sum(ch4_mtco2e, na.rm = TRUE),
         co2_share_cum    = co2_cum    / sum(co2_cum),
         ch4_share_cum    = ch4_cum    / sum(ch4_cum),
         co2_rank_annual     = min_rank(desc(co2_mt)),
         ch4_rank_annual     = min_rank(desc(ch4_mtco2e)),
         co2_rank_cum        = min_rank(desc(co2_cum)),
         ch4_rank_cum        = min_rank(desc(ch4_cum)),
         co2_rank_per_capita = min_rank(desc(co2_per_capita)),
         ch4_rank_per_capita = min_rank(desc(ch4_per_capita))) |>
  ungroup()

# ---- 6. Write ------------------------------------------------------------------

emissions |>
  select(iso_code, country, year, co2_mt, ch4_mtco2e, co2_cum, ch4_cum,
         population, gdp, co2_per_capita, ch4_per_capita, co2_per_gdp, ch4_per_gdp,
         co2_share_annual, ch4_share_annual, co2_share_cum, ch4_share_cum,
         co2_rank_annual, ch4_rank_annual, co2_rank_cum, ch4_rank_cum,
         co2_rank_per_capita, ch4_rank_per_capita) |>
  arrange(iso_code, year) |>
  write_csv(OUT_FILE)
