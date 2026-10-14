# Build data/emissions_global_year.csv: world totals per year, annual and cumulative, per gas.
# Drives the landing page and the generational chart.
#
# World = the sum of the countries in emissions_country_year.csv (so shares in that table add to 1).
# Run from the project root:  Rscript R/emissions_global_year.r
# Requires data/emissions_country_year.csv (run R/emissions_country_year.r first).

library(tidyverse)

COUNTRY_FILE <- "data/emissions_country_year.csv"
OUT_FILE     <- "data/emissions_global_year.csv"

read_csv(COUNTRY_FILE, show_col_types = FALSE) |>
  summarise(co2_mt     = sum(co2_mt, na.rm = TRUE),
            ch4_mtco2e = sum(ch4_mtco2e, na.rm = TRUE),
            .by = year) |>
  arrange(year) |>
  mutate(co2_cum = cumsum(co2_mt),
         ch4_cum = cumsum(ch4_mtco2e)) |>
  write_csv(OUT_FILE)
