# Cumulative global emissions since an example birth year, with events annotated.
#
# Line or area chart. Running sum is computed in code from the birth year (not the dataset's cumulative-since-1850 column). One panel per gas (CO2, CH4). Events from events.csv are marked on the x-axis.
# Data: data/emissions_global_year.csv, data/events.csv
# Run from the project root:  Rscript charts/a_generations/01_cumulative_since_birth.r
# Saves: charts/a_generations/01_cumulative_since_birth.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/a_generations/01_cumulative_since_birth.png", width = 8, height = 5, bg = "white")
