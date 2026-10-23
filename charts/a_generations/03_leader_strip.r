# Which country was the top emitter in each period.
#
# Timeline of colored segments (e.g. UK, then USA, then China) built by grouping consecutive years with the same top emitter. One strip per gas (CO2, CH4).
# Data: data/top_emitter_by_year.csv
# Run from the project root:  Rscript charts/a_generations/03_leader_strip.r
# Saves: charts/a_generations/03_leader_strip.png (static) and charts/a_generations/03_leader_strip.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML.
#         Hover tooltip with country, years, and average share of the world total.

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets

font     <- "sans"
OUT_PNG  <- "charts/a_generations/03_leader_strip.png"
OUT_HTML <- "charts/a_generations/03_leader_strip.html"

caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9."

# One row per run of consecutive years with the same top emitter
segments <- read_csv("data/top_emitter_by_year.csv", show_col_types = FALSE) |>
  arrange(gas, year) |>
  mutate(run = consecutive_id(country), .by = gas) |>
  summarise(from = min(year), to = max(year), share = mean(share), .by = c(gas, run, country)) |>
  mutate(gas = factor(str_to_upper(gas), levels = c("CH4", "CO2")),   # CO2 on top
         years = to - from + 1)

# One color per country, in order of first appearance (UK, then USA, then China)
countries <- unique(arrange(segments, from, desc(gas))$country)
hat <- as.vector(ltc("hat"))
countryColors <- setNames(hat[c(9, 6, 3)[seq_along(countries)]], countries)   # green, blue, red: three clearly different hues

# ---- Step 1: static ggplot ----
p_static <- ggplot(segments) +
  geom_rect(aes(xmin = from, xmax = to + 1, ymin = as.numeric(gas) - 0.35, ymax = as.numeric(gas) + 0.35, fill = country)) +
  geom_text(aes(x = (from + to + 1) / 2, y = as.numeric(gas), label = if_else(country == "United Kingdom", "UK", country)),   # short so it fits the narrow segment
            color = "white", size = 3.5, family = font, fontface = "bold") +
  scale_fill_manual(values = countryColors) +
  scale_y_continuous(breaks = 1:2, labels = levels(segments$gas), expand = expansion(add = 0.5)) +
  scale_x_continuous(breaks = seq(1850, 2000, 50), expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = NULL, y = NULL,
       title = "Top emitter in each period",
       caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(OUT_PNG, p_static, width = 8, height = 3, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# Horizontal bars stacked in time order: each segment is as long as the years it led, on a year axis from 1850.
# NA (not 0) where a country never led that gas, so no empty label is drawn.
by_country <- segments |>
  mutate(gas = as.character(gas)) |>
  select(gas, country, years) |>
  pivot_wider(names_from = country, values_from = years) |>
  select(gas, all_of(countries)) |>             # columns in time order, so the stack reads left to right
  mutate(gas = factor(gas, levels = c("CH4", "CO2"))) |>
  arrange(gas)

# Tooltip text per gas and country, built here so the JavaScript only looks it up
tip <- segments |>
  mutate(text = sprintf("<b>%s</b><br>%s, %d to %d<br>Average share of world %s: %.0f%%",
                        country, gas, from, to, gas, share * 100)) |>
  select(gas, country, text) |>
  nest_by(gas) |>
  deframe() |>
  map(\(d) as.list(deframe(d)))

p <- e_charts(by_country, gas)
for (ctry in countries) {
  p <- e_bar_(p, ctry, stack = "leader", barWidth = "55%",
              label = list(show = TRUE, formatter = "{a}", color = "#fff", fontWeight = "bold"))
}
p <- p |>
  e_flip_coords() |>
  e_x_axis(min = 0, max = 175, interval = 50, splitLine = list(show = FALSE),
           axisLabel = list(formatter = JS("function(v) { return 1850 + v; }"))) |>
  e_color(unname(countryColors)) |>
  e_tooltip(trigger = "item", formatter = JS(paste0(
    "function(p) { var t = ", jsonlite::toJSON(tip, auto_unbox = TRUE), ";",
    " return t[p.name][p.seriesName]; }"))) |>
  e_legend(show = FALSE) |>
  e_toolbox(show = FALSE) |>
  e_title("Top emitter in each period", left = 0) |>
  e_grid(top = 60, bottom = 40, left = 60, right = 20)

save_echart(p, OUT_HTML)
