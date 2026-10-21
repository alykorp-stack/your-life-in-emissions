# Which country was the top emitter in each period.
#
# Timeline of colored segments (e.g. UK, then USA, then China) built by grouping consecutive years with the same top emitter. Shares the x-axis with chart 01 so the two stack.
# Data: data/top_emitter_by_year.csv
# Run from the project root:  Rscript charts/a_generations/03_leader_strip.r
# Saves: charts/a_generations/03_leader_strip.png

library(tidyverse)
library(cowplot)

# TODO: build the chart, then ggsave("charts/a_generations/03_leader_strip.png", width = 8, height = 5, bg = "white")
