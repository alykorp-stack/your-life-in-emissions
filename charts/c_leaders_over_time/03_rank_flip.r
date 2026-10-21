# Do rankings flip when we adjust for population?
#
# Slope chart comparing each featured country's annual total rank with its per capita rank in one year.
# Data: data/emissions_country_year.csv (co2_rank_annual, co2_rank_per_capita)
# Run from the project root:  Rscript charts/c_leaders_over_time/03_rank_flip.r
# Saves: charts/c_leaders_over_time/03_rank_flip.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/c_leaders_over_time/03_rank_flip.png", width = 8, height = 5, bg = "white")
