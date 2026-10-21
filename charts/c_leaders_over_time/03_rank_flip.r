# Do rankings flip when we adjust for population?
#
# Slope chart comparing each featured country's annual total rank with its per capita rank in one year.
# Data: data/emissions_country_year.csv (co2_rank_annual, co2_rank_per_capita)
# Run from the project root:  Rscript charts/c_leaders_over_time/03_rank_flip.r
# Saves: charts/c_leaders_over_time/03_rank_flip.png (static) and charts/c_leaders_over_time/03_rank_flip.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         Hover tooltip with both ranks; legend click toggles a country.

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/c_leaders_over_time/03_rank_flip.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/c_leaders_over_time/03_rank_flip.html", selfcontained = TRUE)
