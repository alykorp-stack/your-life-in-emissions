# Cumulative global emissions since an example birth year, with events annotated.
#
# Line or area chart. Running sum is computed in code from the birth year (not the dataset's cumulative-since-1850 column). One panel per gas (CO2, CH4). Events from events.csv are marked on the x-axis.
# Data: data/emissions_global_year.csv, data/events.csv
# Run from the project root:  Rscript charts/a_generations/01_cumulative_since_birth.r
# Saves: charts/a_generations/01_cumulative_since_birth.png (static) and charts/a_generations/01_cumulative_since_birth.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         Legend click toggles a line, double-click isolates it. The birth-year input that picks the generation needs Quarto OJS (plotly cannot take it).

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/a_generations/01_cumulative_since_birth.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/a_generations/01_cumulative_since_birth.html", selfcontained = TRUE)
