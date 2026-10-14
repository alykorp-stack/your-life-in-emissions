# Data

`emissions_country_year.csv` is the master table behind the site. It is built by
`R/emissions_country_year.r` from the files in `raw/` (run from the project root:
`Rscript R/emissions_country_year.r`).

## Folder layout

```
data/
  emissions_country_year.csv      <- master table: one row per country and year, 1850-2024
  emissions_global_year.csv       <- derived tables for the site (see below)
  top_emitter_by_year.csv
  country_groups.csv
  generations.csv
  events.csv
  raw/
    primap/                       PRIMAP-hist v2.2 (1850-2018), the historical backbone
    owid_co2/                     Annual CO2 by country, 2018-2024 (OWID / Global Carbon Budget)
    worldbank_ch4/                Annual CH4 by country, 1960-2024 (World Bank / EDGAR)
    edgar/                        EDGAR 2025 workbook (fills CH4 gaps) and the JRC report PDF
    owid_population/              Population, to 2023 (OWID)
    worldbank_population/         Population, to 2024 (World Bank), extends OWID past 2023
    owid_gdp_per_capita/          GDP per capita, 1990 onward (OWID)
```

## Derived tables

Each one has its own script in `R/`. Run `R/emissions_country_year.r` first for the ones that read the master table.

| File | Contents | Built from |
|---|---|---|
| `emissions_global_year.csv` | `year, co2_mt, ch4_mtco2e, co2_cum, ch4_cum`: world totals (sum of the countries in the master table) | master table |
| `top_emitter_by_year.csv` | `year, gas, iso_code, country, share`: the largest annual emitter per gas and its share of the world total | master table |
| `country_groups.csv` | `iso_code, country, region, income_group, is_aggregate`: use to keep aggregates (World, EU, income groups) out of country lists | World Bank metadata, plus a few territories and PRIMAP groupings added by hand |
| `generations.csv` | `generation, start_birth_year, end_birth_year`: Silent to Gen Alpha (Pew definitions; Gen Alpha ends at 2024, the last data year) | typed in |
| `events.csv` | `year, label, description, category`: annotations for the charts | typed in |

## Columns in `emissions_country_year.csv`

| Column | Meaning | Unit |
|---|---|---|
| `iso_code`, `country`, `year` | Key (ISO3 code, name, year) | |
| `co2_mt`, `ch4_mtco2e` | Annual emissions, excluding land use. CH4 converted at GWP100 = 27.9 (IPCC AR6) | Mt CO2e |
| `co2_cum`, `ch4_cum` | Cumulative since 1850 | Mt CO2e |
| `population` | Total population | people |
| `gdp` | GDP per capita x population, 2021 international $ (PPP) | $ |
| `co2_per_capita`, `ch4_per_capita` | Emissions per person | t per person |
| `co2_per_gdp`, `ch4_per_gdp` | Emissions per unit of GDP | kg per $ |
| `co2_share_annual`, `ch4_share_annual` | Country share of the world total that year | 0 to 1 |
| `co2_share_cum`, `ch4_share_cum` | Country share of the world cumulative total | 0 to 1 |
| `*_rank_annual`, `*_rank_cum`, `*_rank_per_capita` | Rank within the year (1 = highest), per gas | |

World totals are the sum of the countries in the table.

## How 2019-2024 is built

PRIMAP ends in 2018. For later years, each country's 2018 PRIMAP value is scaled by the
newer source's change since 2018 (a growth-rate splice). This keeps PRIMAP's level, so there
is no jump at 2018, and it also means the newer sources' absolute levels are not used.
Population after 2023 works the same way (OWID 2023 x World Bank change).

## Known gaps and approximations

- **Excluded entirely:** Antarctica, Monaco, San Marino, Tokelau, Vatican (no CO2 after 2018).
- **CH4 from EDGAR:** Taiwan, Anguilla, Cook Islands, St Helena (not in the World Bank file).
- **CH4 proxy:** Serbia, Montenegro, Andorra, Liechtenstein and South Sudan only exist inside
  combined entities in the newer sources; each uses its parent's growth (`CH4_PROXY` in the script).
- **Niue:** no CH4 after 2018.
- **Population:** missing before 1950 for Ireland, Aruba, Anguilla, Cook Islands, Macao, Niue,
  St Helena, Turks and Caicos, British Virgin Islands. Missing in 2024 for Anguilla, Cook
  Islands, Niue, St Helena, Taiwan.
- **GDP:** none before 1990, and none at all for some countries (e.g. Cuba, Venezuela, North
  Korea, Taiwan, Yemen, Eritrea, Somalia). `gdp`, `*_per_gdp` are empty there.
- **Borders:** PRIMAP assigns history to present-day borders (e.g. the USSR is split among
  its successor states).
- **Small states:** per-capita ranks include very small populations (e.g. Palau), which can
  rank high.
- **Sources differ by country.** Levels from the newer sources can differ from PRIMAP by 10%
  or more for some countries (especially methane), which is why they are only used for growth.

## Sources

- PRIMAP-hist v2.2 (Gütschow et al.), `HISTCR` scenario, category `IPCM0EL` (national total excluding land use)
- Our World in Data: annual CO2 (Global Carbon Budget), population (HYDE, Gapminder, UN WPP), GDP per capita (World Bank, Eurostat, OECD, IMF)
- World Bank WDI: `EN.GHG.CH4.MT.CE.AR5` (CH4 excluding LULUCF, from EDGAR) and `SP.POP.TOTL` (population)
- EDGAR_2025_GHG, JRC/IEA "GHG emissions of all world countries, 2025 Report"
