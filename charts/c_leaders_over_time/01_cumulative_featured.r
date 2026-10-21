# Cumulative CO2 over time for China, USA, India, UK, Russia.
#
# Line chart with direct labels at the line ends instead of a legend (static). A filtered slice of the master table.
# Data: data/emissions_country_year.csv
# Run from the project root:  Rscript charts/c_leaders_over_time/01_cumulative_featured.r
# Saves: charts/c_leaders_over_time/01_cumulative_featured.png (static) and charts/c_leaders_over_time/01_cumulative_featured.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         Legend click toggles a line, double-click isolates it. Keep the legend in plotly as the click control.

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/c_leaders_over_time/01_cumulative_featured.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/c_leaders_over_time/01_cumulative_featured.html", selfcontained = TRUE)
