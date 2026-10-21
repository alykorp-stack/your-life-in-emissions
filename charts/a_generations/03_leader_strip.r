# Which country was the top emitter in each period.
#
# Timeline of colored segments (e.g. UK, then USA, then China) built by grouping consecutive years with the same top emitter. Shares the x-axis with chart 01 so the two stack.
# Data: data/top_emitter_by_year.csv
# Run from the project root:  Rscript charts/a_generations/03_leader_strip.r
# Saves: charts/a_generations/03_leader_strip.png (static) and charts/a_generations/03_leader_strip.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         Hover tooltip with country, years, and share; link the x-axis to chart 01 (subplot, shareX = TRUE).

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/a_generations/03_leader_strip.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/a_generations/03_leader_strip.html", selfcontained = TRUE)
