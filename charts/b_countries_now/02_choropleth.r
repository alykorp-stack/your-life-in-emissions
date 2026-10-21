# World map of emissions by country.
#
# Choropleth for one year with a metric choice (annual, cumulative, per capita). Needs a world geometry file joined on iso_code, the one non-CSV input.
# Data: data/emissions_country_year.csv, a world geometry file (not yet in the project)
# Run from the project root:  Rscript charts/b_countries_now/02_choropleth.r
# Saves: charts/b_countries_now/02_choropleth.png (static) and charts/b_countries_now/02_choropleth.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         plot_geo() choropleth on iso_code with a year slider and metric dropdown. The static version is a single year with sf + geom_sf.

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# ---- Step 1: static ggplot ----
# TODO: build the chart, then ggsave("charts/b_countries_now/02_choropleth.png", width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# TODO: rebuild with plot_ly() (or ggplotly() for a simple bar chart), then
#       saveWidget(p, "charts/b_countries_now/02_choropleth.html", selfcontained = TRUE)
