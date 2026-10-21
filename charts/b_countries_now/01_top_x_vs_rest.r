# Top X emitters vs the rest of the world.
#
# Vertical bar chart of share of the world total, with annual and cumulative versions. Rest = world minus the top X. X is fixed at 10 for the static version.
# Data: data/emissions_country_year.csv, data/country_groups.csv (drop aggregates)
# Run from the project root:  Rscript charts/b_countries_now/01_top_x_vs_rest.r
# Saves: charts/b_countries_now/01_top_x_vs_rest.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/b_countries_now/01_top_x_vs_rest.png", width = 8, height = 5, bg = "white")
