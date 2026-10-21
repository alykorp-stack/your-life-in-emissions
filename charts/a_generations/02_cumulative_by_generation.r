# Cumulative emissions over each generation's lifetime (Silent to Gen Alpha).
#
# Vertical bar chart, one bar per generation in generation order. Answers the page's headline question: who has lived through the most emissions?
# Data: data/emissions_global_year.csv, data/generations.csv
# Run from the project root:  Rscript charts/a_generations/02_cumulative_by_generation.r
# Saves: charts/a_generations/02_cumulative_by_generation.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/a_generations/02_cumulative_by_generation.png", width = 8, height = 5, bg = "white")
