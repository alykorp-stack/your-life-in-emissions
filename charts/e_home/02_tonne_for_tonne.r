# Warming from one tonne of CH4 compared with one tonne of CO2, over 20 and 100 years (global warming potential).
#
# Explains the multiplier behind chart 01: methane is released in small amounts, but each tonne traps
# far more heat, especially in the first decades, because it breaks down in about 12 years.
# Data: data/gases.csv (IPCC AR6 WG1 Table 7.SM.7)
# Run from the project root:  Rscript charts/e_home/02_tonne_for_tonne.r
# Saves: charts/e_home/02_tonne_for_tonne.png (static) and charts/e_home/02_tonne_for_tonne.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style): horizontal bars, one panel per time horizon.
# Step 2: interactive echarts4r: one bar chart with an "Over 100 years / Over 20 years" switch.
#         The x axis stays fixed, so the CH4 bar grows when switching to 20 years.

library(tidyverse)
library(cowplot)
library(ltc)
library(echarts4r)
source("charts/e_home/toggle_helper.r")            # add_toggle()
source("charts/a_generations/echarts_helpers.r")   # save_echart()

if (!file.exists("data/gases.csv")) {
  stop("Run this script from the project root: Rscript charts/e_home/02_tonne_for_tonne.r")
}

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])   # same colors as chart 01
OUT_PNG   <- "charts/e_home/02_tonne_for_tonne.png"
OUT_HTML  <- "charts/e_home/02_tonne_for_tonne.html"

caption <- "Data: IPCC AR6 WG1 (2021), Table 7.SM.7. Global warming potential: heat trapped over a set period, relative to the same mass of CO2."

gases <- read_csv("data/gases.csv", show_col_types = FALSE)
ch4   <- filter(gases, gas == "CH4")
title <- "One tonne of methane traps far more heat than one tonne of CO2"

# ---- Step 1: static ggplot ----
bars <- gases |>
  select(gas, `Over 20 years` = gwp20, `Over 100 years` = gwp100) |>
  pivot_longer(-gas, names_to = "horizon", values_to = "gwp") |>
  mutate(horizon = factor(horizon, levels = c("Over 20 years", "Over 100 years")),
         gas     = factor(paste("1 tonne of", gas), levels = paste("1 tonne of", c("CO2", "CH4"))),
         label   = paste0(gwp, "x"))   # paste0 keeps 1 as "1", not "1.0"

p_static <- ggplot(bars, aes(gwp, gas, fill = str_remove(gas, "1 tonne of "))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), hjust = -0.15, size = 4, family = font) +
  facet_wrap(~horizon, ncol = 1) +
  scale_fill_manual(values = gasColors) +
  scale_x_continuous(limits = c(0, 90), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Warming compared with 1 tonne of CO2", y = NULL, title = title,
       subtitle = sprintf("Methane breaks down in about %s years, so its effect is strongest in the first decades",
                          round(ch4$lifetime_years)),
       caption = caption) +
  theme_minimal_vgrid(font_size = 11, font_family = font) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(OUT_PNG, p_static, width = 8, height = 4.5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# Opens on 100 years (the horizon used everywhere else on the site); switching to 20 years grows the CH4 bar.
bar_data <- function(col) {
  map2(gases$gas, gases[[col]], \(g, v) list(value = v, itemStyle = list(color = gasColors[[g]])))
}
payload <- list(
  y100 = bar_data("gwp100"),
  y20  = bar_data("gwp20"),
  sub  = list(
    y100 = sprintf("Over 100 years: %s times as much. This is the factor used for CO2-equivalent on this site.", ch4$gwp100),
    y20  = sprintf("Over 20 years: %s times as much. Methane breaks down in about %s years, so it hits hardest early.",
                   ch4$gwp20, round(ch4$lifetime_years))
  )
)

# Hover explains how long each gas stays in the air.
tooltip_js <- JS(sprintf("
  function(p) {
    var note = p.name.indexOf('CH4') > -1
      ? 'Breaks down in about %s years.'
      : 'No single lifetime: part of each tonne stays in the air for centuries to millennia.';
    return '<b>' + p.name + '</b><br>Warming: ' + p.value + 'x a tonne of CO2<br>' + note;
  }", ch4$lifetime_years))

p <- tibble(gas = paste("1 tonne of", gases$gas), gwp = gases$gwp100) |>
  e_charts(gas) |>
  e_bar(gwp, barWidth = "55%",
        label = list(show = TRUE, position = "right", fontSize = 16, fontWeight = "bold",
                     formatter = JS("function(p) { return p.value + 'x'; }"))) |>
  e_flip_coords() |>
  e_x_axis(min = 0, max = 100, interval = 20, name = "Warming compared with 1 tonne of CO2",
           nameLocation = "middle", nameGap = 30) |>
  e_y_axis(axisLabel = list(fontSize = 14), axisTick = list(show = FALSE)) |>
  e_tooltip(trigger = "item", formatter = tooltip_js) |>
  e_legend(show = FALSE) |>
  e_title(title, subtext = payload$sub$y100, left = 0,
          textStyle = list(fontSize = 17), subtextStyle = list(fontSize = 13, color = "#555")) |>
  e_grid(top = 130, right = 60, bottom = 70, left = 130)

p$x$opts$graphic <- list(type = "text", left = 0, bottom = 6,
                         style = list(text = caption, fill = "#888", fontSize = 11))

p <- p |>
  add_toggle(c(y100 = "Over 100 years", y20 = "Over 20 years"), default = "y100", data = payload,
             update_js = "function(chart, mode, d) {
               chart.setOption({series: [{data: d[mode]}], title: [{subtext: d.sub[mode]}]});
             }")

save_echart(p, OUT_HTML)
