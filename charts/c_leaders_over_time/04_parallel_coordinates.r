# All featured countries compared across total, cumulative, per capita and per GDP.
#
# Parallel coordinates for the latest year, one line per country. Featured countries = those of charts c1 and c2:
# blue = the five leaders of c1 (China, United States, India, United Kingdom, Russia), red = the per capita
# comparison of c2 (Qatar, Australia, Germany, Brazil, Indonesia, Canada, Nigeria).
# Static version: each axis is scaled 0 to 1 between its lowest and highest featured country (real values are printed
# under each axis). Interactive version: each axis keeps its own real scale, and dragging on an axis brushes countries.
# Read straight from the master table, so no separate derived file is needed.
# Data: data/emissions_country_year.csv (co2_mt, co2_cum, co2_per_capita, co2_per_gdp)
# Run from the project root:  Rscript charts/c_leaders_over_time/04_parallel_coordinates.r
# Saves: charts/c_leaders_over_time/04_parallel_coordinates.png (static) and charts/c_leaders_over_time/04_parallel_coordinates.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML.
#         Clicking a country in the legend hides or shows it.

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

font    <- "sans"
hat     <- as.vector(ltc("hat"))
groups  <- c("Top emitters" = hat[6], "Per capita comparison" = hat[3])
leaders <- c("China", "United States", "India", "United Kingdom", "Russia")
others  <- c("Qatar", "Australia", "Germany", "Brazil", "Indonesia", "Canada", "Nigeria")

axes <- tribble(
  ~col,             ~axis,                          ~unit,
  "co2_mt",         "Total, annual",                "Mt CO2",
  "co2_cum",        "Cumulative since 1850",        "Mt CO2",
  "co2_per_capita", "Per person",                   "t CO2",
  "co2_per_gdp",    "Per unit of GDP",              "kg CO2 per $")

all  <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
YEAR <- max(all$year)
caption <- paste0("Data: PRIMAP-hist v2.2, Global Carbon Budget, OWID, World Bank. CO2 only, ", YEAR, ". GDP in 2021 international $ (PPP).")

d <- all |>
  filter(year == YEAR, country %in% c(leaders, others)) |>
  select(country, all_of(axes$col)) |>
  mutate(group = factor(if_else(country %in% leaders, "Top emitters", "Per capita comparison"), levels = names(groups)))
stopifnot(!anyNA(d))

# ---- Step 1: static ggplot ----
long <- d |>
  pivot_longer(all_of(axes$col), names_to = "col", values_to = "value") |>
  left_join(axes, by = "col") |>
  mutate(axis = factor(axis, levels = axes$axis),
         scaled = (value - min(value)) / (max(value) - min(value)), .by = col)

ranges <- long |>
  summarise(lo = min(value), hi = max(value), unit = first(unit), .by = axis) |>
  mutate(lab_hi = paste(scales::comma(hi, accuracy = if_else(hi < 10, 0.01, 1)), unit),
         lab_lo = paste(scales::comma(lo, accuracy = if_else(hi < 10, 0.01, 1)), unit))

p <- ggplot(long, aes(axis, scaled, group = country, color = group)) +
  geom_vline(xintercept = seq_along(axes$axis), color = "grey70") +
  geom_line(linewidth = 1, key_glyph = "path") +
  geom_point(size = 2) +
  geom_text_repel(data = filter(long, axis == axes$axis[1]), aes(label = country), hjust = 1, direction = "y",
                  nudge_x = -0.22, ylim = c(-0.02, 1.02), size = 3.6, family = font, segment.color = "grey70", min.segment.length = 0, show.legend = FALSE) +
  geom_text(data = ranges, aes(x = axis, y = 1.1, label = lab_hi), inherit.aes = FALSE, size = 3.2, color = "grey40", family = font) +
  geom_text(data = ranges, aes(x = axis, y = -0.1, label = lab_lo), inherit.aes = FALSE, size = 3.2, color = "grey40", family = font) +
  scale_color_manual(values = groups) +
  scale_x_discrete(position = "top", expand = expansion(add = c(1.3, 0.4))) +
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  labs(x = NULL, y = NULL, color = NULL,
       title = paste0("Featured countries across four measures, ", YEAR),
       caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 11),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/c_leaders_over_time/04_parallel_coordinates.png", p, width = 9, height = 6, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# Real values, one independent scale per axis. One series per country, so clicking a country in the legend hides or shows its line.
series <- map(seq_len(nrow(d)), \(i) {
  list(type = "parallel", name = d$country[i], lineStyle = list(width = 3, color = groups[[as.character(d$group[i])]], opacity = 0.85),
       emphasis = list(lineStyle = list(width = 5, opacity = 1)),
       data = list(list(name = d$country[i], value = map_dbl(axes$col, \(cl) signif(d[[cl]][i], 5)))))
})

e <- tibble(x = 0, y = 0) |> e_charts(x) |> e_line(y)   # placeholder chart, options replaced below
e$x$opts$xAxis <- NULL
e$x$opts$yAxis <- NULL
e$x$opts$series <- series
e$x$opts$parallelAxis <- map(seq_len(nrow(axes)), \(i)
  list(dim = i - 1, name = paste0(axes$axis[i], "
(", axes$unit[i], ")"), nameTextStyle = list(fontSize = 12),
       min = 0, max = max(pretty(c(0, d[[axes$col[i]]]))),   # explicit range, so every country's value fits on the axis
       nameLocation = "start", nameGap = 18, areaSelectStyle = list(width = 18)))
e$x$opts$parallel <- list(left = 80, right = 80, top = 150, bottom = 60, parallelAxisDefault = list(
  type = "value", nameLocation = "start", realtime = FALSE, axisLabel = list(fontSize = 11)))
e$x$opts$animation <- FALSE
e$x$opts$animation <- FALSE
e$x$opts$tooltip <- list(trigger = "item", formatter = JS("function(p) {
  var n = ['Total, annual (Mt CO2)', 'Cumulative since 1850 (Mt CO2)', 'Per person (t CO2)', 'Per unit of GDP (kg CO2 per $)'];
  var s = '<b>' + p.name + '</b>';
  p.value.forEach(function(v, i) { s += '<br>' + n[i] + ': ' + v.toLocaleString(); });
  return s;
}"))
e$x$opts$legend <- list(data = as.list(d$country), top = 34, left = 0, selectedMode = TRUE)
e$x$opts$title <- list(list(text = paste0("Featured countries across four measures, ", YEAR), left = 0))
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6, style = list(text = paste0(caption, " Click a country in the legend to hide or show it; drag on an axis to select countries."), fill = "#888", fontSize = 11))

save_echart(e, "charts/c_leaders_over_time/04_parallel_coordinates.html")
