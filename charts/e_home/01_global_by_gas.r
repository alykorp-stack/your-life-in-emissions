# Global emissions of CO2 and CH4, 1850-2024, by weight and by warming.
#
# Methane is a tiny share of emissions by weight, but each tonne warms much more than a tonne of CO2.
# The chart shows both views: "by weight" (tonnes of each gas) and "by warming" (CO2-equivalent, CH4 x GWP100).
# Actual yearly values, nothing smoothed or summed.
# Data: data/emissions_global_year.csv, data/gases.csv
# Run from the project root:  Rscript charts/e_home/01_global_by_gas.r
# Saves: charts/e_home/01_global_by_gas.png (static) and charts/e_home/01_global_by_gas.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style): the two views side by side on the same y scale.
# Step 2: interactive echarts4r: one chart with a "By weight / By warming" switch. The y axis stays fixed,
#         so the CH4 line visibly rises when switching to warming. Legend click hides a gas.

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets
source("charts/e_home/toggle_helper.r")            # add_toggle()

if (!file.exists("data/emissions_global_year.csv")) {
  stop("Run this script from the project root: Rscript charts/e_home/01_global_by_gas.r")
}

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])   # blue, teal (no red/green pair)
OUT_PNG   <- "charts/e_home/01_global_by_gas.png"
OUT_HTML  <- "charts/e_home/01_global_by_gas.html"

caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9 (IPCC AR6)."

# CH4 is stored as CO2e at GWP100, so dividing by GWP100 gives tonnes of methane itself.
gwp100 <- read_csv("data/gases.csv", show_col_types = FALSE) |> filter(gas == "CH4") |> pull(gwp100)

# Gt per year (data is in Mt)
global <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year,
            co2         = co2_mt / 1000,
            ch4_warming = ch4_mtco2e / 1000,
            ch4_weight  = ch4_warming / gwp100)

latest <- slice_max(global, year)
share_weight  <- with(latest, ch4_weight  / (co2 + ch4_weight))
share_warming <- with(latest, ch4_warming / (co2 + ch4_warming))
message(sprintf("CH4 share in %d: %.1f%% by weight, %.1f%% by warming", latest$year,
                100 * share_weight, 100 * share_warming))

title <- sprintf("Methane is about %d%% of emissions by weight, but %d%% by warming (%d)",
                 round(100 * share_weight), round(100 * share_warming), latest$year)
ytop  <- ceiling(max(global$co2) / 10) * 10   # same y range in both views

# ---- Step 1: static ggplot ----
views <- c(weight = "By weight (Gt of gas per year)", warming = "By warming (Gt CO2e per year)")
long <- bind_rows(
  global |> transmute(year, view = views[["weight"]],  CO2 = co2, CH4 = ch4_weight),
  global |> transmute(year, view = views[["warming"]], CO2 = co2, CH4 = ch4_warming)
) |>
  mutate(view = factor(view, levels = views)) |>
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "gt")
ends <- long |> slice_max(year, by = c(view, gas))

p_static <- ggplot(long, aes(year, gt, color = gas)) +
  geom_line(linewidth = 1) +
  geom_text_repel(data = ends, aes(label = gas), hjust = 0, direction = "y",
                  nudge_x = 4, size = 3.8, family = font, segment.color = NA) +
  facet_wrap(~view) +
  scale_color_manual(values = gasColors) +
  scale_x_continuous(breaks = seq(1850, 2000, 50), expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(limits = c(0, ytop), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Gt per year", title = title, caption = caption) +
  theme_minimal_hgrid(font_size = 11, font_family = font) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", hjust = 0),
        panel.spacing = unit(1.5, "lines"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(OUT_PNG, p_static, width = 9, height = 5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# Opens on "By weight" so the reader makes the switch and sees CH4 rise.
pairs <- function(v) unname(map2(global$year, round(v, 3), \(x, y) c(x, y)))
payload <- list(
  weight  = pairs(global$ch4_weight),
  warming = pairs(global$ch4_warming),
  yname   = list(weight = "Gt of gas per year", warming = "Gt CO2e per year"),
  sub     = list(weight  = "Showing: tonnes of each gas released per year",
                 warming = "Showing: warming effect, in tonnes of CO2-equivalent (CH4 x 27.9)")
)

# Axis tooltip: both values and CH4's share of the total in the current view.
tooltip_js <- JS("
  function(ps) {
    var unit = window.chartMode === 'warming' ? ' Gt CO2e' : ' Gt', v = {};
    var s = '<b>' + ps[0].value[0] + '</b>';
    ps.forEach(function(p) {
      v[p.seriesName] = p.value[1];
      s += '<br>' + p.marker + p.seriesName + ': ' +
           p.value[1].toLocaleString(undefined, {maximumFractionDigits: 2}) + unit;
    });
    if (v.CO2 !== undefined && v.CH4 !== undefined) {
      s += '<br>CH4 share: <b>' + (100 * v.CH4 / (v.CO2 + v.CH4)).toFixed(1) + '%</b>';
    }
    return s;
  }")

p <- global |>
  transmute(year, CO2 = round(co2, 3), CH4 = round(ch4_weight, 3)) |>
  e_charts(year) |>
  e_line(CO2, symbol = "none", lineStyle = list(width = 3),
         endLabel = list(show = TRUE, formatter = "{a}", fontSize = 14)) |>
  e_line(CH4, symbol = "none", lineStyle = list(width = 3),
         endLabel = list(show = TRUE, formatter = "{a}", fontSize = 14)) |>
  e_color(unname(gasColors)) |>
  # String(v) keeps years as 1850, not 1,850 (a "{value}" template still adds the thousands separator)
  e_x_axis(type = "value", min = min(global$year), max = max(global$year), interval = 25,
           splitLine = list(show = FALSE), axisLabel = list(formatter = JS("function(v) { return String(v); }"))) |>
  e_y_axis(name = payload$yname$weight, nameLocation = "middle", nameGap = 45, min = 0, max = ytop) |>
  e_datazoom(type = "inside", x_index = 0, toolbox = FALSE, zoomOnMouseWheel = FALSE) |>
  e_tooltip(trigger = "axis", formatter = tooltip_js) |>
  e_legend(top = 108, right = 20, textStyle = list(fontSize = 14)) |>
  e_toolbox(show = FALSE) |>
  e_title(title, subtext = payload$sub$weight, left = 0,
          textStyle = list(fontSize = 17), subtextStyle = list(fontSize = 13, color = "#555")) |>
  e_grid(top = 130, right = 60, bottom = 60, left = 70)

p$x$opts$graphic <- list(type = "text", left = 0, bottom = 6,
                         style = list(text = caption, fill = "#888", fontSize = 11))

# "By weight / By warming" switch, top left under the title. It swaps the CH4 data, y-axis name and subtitle.
p <- p |>
  add_toggle(c(weight = "By weight", warming = "By warming"), default = "weight", data = payload,
             update_js = "function(chart, mode, d) {
               chart.setOption({series: [{}, {data: d[mode]}], yAxis: [{name: d.yname[mode]}], title: [{subtext: d.sub[mode]}]});
             }") |>
  add_buttons("global_emissions_by_gas") |>
  # The title is long, so the zoom/download buttons move down to the switch row (add_buttons puts them at top: 12px).
  onRender("function(el) { el.querySelector('.ctl-top').style.top = '62px'; }")

save_echart(p, OUT_HTML)
