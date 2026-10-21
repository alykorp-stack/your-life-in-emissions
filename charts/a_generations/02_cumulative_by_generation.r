# Cumulative emissions over each generation's lifetime (Silent to Gen Alpha).
#
# Vertical bar chart, one bar per generation in generation order. Answers the page's headline question: who has lived through the most emissions?
# Data: data/emissions_global_year.csv, data/generations.csv
# Run from the project root:  Rscript charts/a_generations/02_cumulative_by_generation.r
# Saves: charts/a_generations/02_cumulative_by_generation.png (static) and charts/a_generations/02_cumulative_by_generation.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         Hover tooltip with exact totals; legend click toggles each gas.

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/a_generations/02_cumulative_by_generation.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/a_generations/02_cumulative_by_generation.html", selfcontained = TRUE)
