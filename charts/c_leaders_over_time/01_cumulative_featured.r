# Cumulative CO2 over time for China, USA, India, UK, Russia.
#
# Line chart with direct labels at the line ends instead of a legend. A filtered slice of the master table:
# each line is the running total of CO2 emitted since 1850 (the data's own cumulative column), in Gt.
# Data: data/emissions_country_year.csv
# Run from the project root:  Rscript charts/c_leaders_over_time/01_cumulative_featured.r
# Saves: charts/c_leaders_over_time/01_cumulative_featured.png (static) and charts/c_leaders_over_time/01_cumulative_featured.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML.
#         Hover shows each country's total for the year; a year slider under the chart narrows the range.

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

font    <- "sans"
caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget. CO2 only, excludes land use. Cumulative since 1850."
hat     <- as.vector(ltc("hat"))
featured <- c("China", "United States", "India", "United Kingdom", "Russia")
countryColors <- setNames(hat[c(3, 6, 1, 7, 9)], featured)   # red, blue, yellow, teal, green

d <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE) |>
  filter(country %in% featured) |>
  transmute(country = factor(country, levels = featured), year, gt = co2_cum / 1000)
last_year <- max(d$year)

# ---- Step 1: static ggplot ----
ends <- filter(d, year == last_year)

p <- ggplot(d, aes(year, gt, color = country)) +
  geom_line(linewidth = 1) +
  geom_text_repel(data = ends, aes(label = country), hjust = 0, direction = "y", nudge_x = 2,
                  size = 4, family = font, segment.color = NA) +
  scale_color_manual(values = countryColors) +
  scale_x_continuous(breaks = seq(1850, 2000, 50), expand = expansion(mult = c(0.01, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
  labs(x = NULL, y = "Cumulative CO2 emissions (Gt)",
       title = paste0("Cumulative CO2 emissions, 1850 to ", last_year),
       caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/c_leaders_over_time/01_cumulative_featured.png", p, width = 8, height = 5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
wide <- d |> mutate(gt = round(gt, 2)) |> pivot_wider(names_from = country, values_from = gt)

e <- wide |>
  e_charts(year)
for (cn in featured) {
  e <- e_line_(e, cn, symbol = "none", lineStyle = list(width = 3),
               endLabel = list(show = TRUE, formatter = "{a}", fontSize = 14),
               labelLayout = list(moveOverlap = "shiftY"))
}
e <- e |>
  e_color(unname(countryColors)) |>
  e_x_axis(type = "value", min = min(d$year), max = last_year, splitLine = list(show = FALSE),
           axisLabel = list(formatter = JS("function(v) { return String(v); }")), interval = 25) |>
  e_y_axis(name = "Cumulative CO2 emissions (Gt)", nameLocation = "middle", nameGap = 55, min = 0,
           axisLabel = list(formatter = "{value}")) |>
  e_datazoom(type = "slider", x_index = 0, toolbox = FALSE, bottom = 30) |>
  e_tooltip(trigger = "axis") |>
  e_legend(show = FALSE) |>
  e_toolbox(show = FALSE) |>
  e_title(paste0("Cumulative CO2 emissions, 1850 to ", last_year), left = 0) |>
  e_grid(top = 80, right = 150, bottom = 100, left = 80)
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6,
                         style = list(text = caption, fill = "#888", fontSize = 11))

save_echart(e, "charts/c_leaders_over_time/01_cumulative_featured.html")
