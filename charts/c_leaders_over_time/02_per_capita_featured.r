# Per capita CO2 for Qatar, Australia, Germany, Brazil, Indonesia, Canada, Nigeria.
#
# Line chart over time, or a dot plot of the latest year. Per capita columns from the master table.
# Data: data/emissions_country_year.csv
# Run from the project root:  Rscript charts/c_leaders_over_time/02_per_capita_featured.r
# Saves: charts/c_leaders_over_time/02_per_capita_featured.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/c_leaders_over_time/02_per_capita_featured.png", width = 8, height = 5, bg = "white")
