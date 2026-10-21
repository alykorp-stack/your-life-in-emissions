# Top X emitters vs the rest of the world.
#
# Vertical bar chart of share of the world total, with annual and cumulative versions. Rest = world minus the top X. X is fixed at 10 for the static version.
# Data: data/emissions_country_year.csv, data/country_groups.csv (drop aggregates)
# Run from the project root:  Rscript charts/b_countries_now/01_top_x_vs_rest.r
# Saves: charts/b_countries_now/01_top_x_vs_rest.png (static) and charts/b_countries_now/01_top_x_vs_rest.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         The selector for X and the pick-your-countries input need Quarto OJS; plotly only handles the hover and the annual/cumulative dropdown.

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/b_countries_now/01_top_x_vs_rest.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/b_countries_now/01_top_x_vs_rest.html", selfcontained = TRUE)
