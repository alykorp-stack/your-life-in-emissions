# Cumulative CO2 over time for China, USA, India, UK, Russia.
#
# Line chart with direct labels at the line ends instead of a legend. A filtered slice of the master table.
# Data: data/emissions_country_year.csv
# Run from the project root:  Rscript charts/c_leaders_over_time/01_cumulative_featured.r
# Saves: charts/c_leaders_over_time/01_cumulative_featured.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/c_leaders_over_time/01_cumulative_featured.png", width = 8, height = 5, bg = "white")
