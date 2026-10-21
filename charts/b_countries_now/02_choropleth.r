# World map of emissions by country.
#
# Choropleth for one year with a metric choice (annual, cumulative, per capita). Needs a world geometry file joined on iso_code, the one non-CSV input. Better built when the site goes interactive.
# Data: data/emissions_country_year.csv, a world geometry file (not yet in the project)
# Run from the project root:  Rscript charts/b_countries_now/02_choropleth.r
# Saves: charts/b_countries_now/02_choropleth.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/b_countries_now/02_choropleth.png", width = 8, height = 5, bg = "white")
