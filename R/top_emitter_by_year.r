# Build data/top_emitter_by_year.csv: the country with the largest annual emissions
# each year, per gas, and its share of the world total. Powers the leader strip.
#
# Run from the project root:  Rscript R/top_emitter_by_year.r
# Requires data/emissions_country_year.csv (run R/emissions_country_year.r first).

library(tidyverse)

COUNTRY_FILE <- "data/emissions_country_year.csv"
OUT_FILE     <- "data/top_emitter_by_year.csv"

emissions <- read_csv(COUNTRY_FILE, show_col_types = FALSE)

bind_rows(
  emissions |> filter(co2_rank_annual == 1) |>
    transmute(year, gas = "co2", iso_code, country, share = co2_share_annual),
  emissions |> filter(ch4_rank_annual == 1) |>
    transmute(year, gas = "ch4", iso_code, country, share = ch4_share_annual)
) |>
  arrange(gas, year) |>
  write_csv(OUT_FILE)
