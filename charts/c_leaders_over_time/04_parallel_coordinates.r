# All featured countries compared across total, cumulative, per capita and per GDP.
#
# Parallel coordinates for the latest year. Last exploratory chart; needs a small derived data/parallel_coords_latest.csv (not yet built). Normalize each axis to 0-1 or keep independent scales.
# Data: data/emissions_country_year.csv
# Run from the project root:  Rscript charts/c_leaders_over_time/04_parallel_coordinates.r
# Saves: charts/c_leaders_over_time/04_parallel_coordinates.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/c_leaders_over_time/04_parallel_coordinates.png", width = 8, height = 5, bg = "white")
