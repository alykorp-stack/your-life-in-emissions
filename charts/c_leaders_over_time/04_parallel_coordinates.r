# All featured countries compared across total, cumulative, per capita and per GDP.
#
# Parallel coordinates for the latest year. Last exploratory chart; needs a small derived data/parallel_coords_latest.csv (not yet built). Normalize each axis to 0-1 or keep independent scales.
# Data: data/emissions_country_year.csv
# Run from the project root:  Rscript charts/c_leaders_over_time/04_parallel_coordinates.r
# Saves: charts/c_leaders_over_time/04_parallel_coordinates.png (static) and charts/c_leaders_over_time/04_parallel_coordinates.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         Use the plotly parcoords trace; axis brushing is built in.

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/c_leaders_over_time/04_parallel_coordinates.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/c_leaders_over_time/04_parallel_coordinates.html", selfcontained = TRUE)
