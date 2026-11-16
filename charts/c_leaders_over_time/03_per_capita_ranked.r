# Emissions per person: the top 20 countries in a year, against the world average.
#
# Horizontal bars (country names are long), sorted, with a dashed line for the world average so every bar has a reference.
# Only countries with at least 1 million people: otherwise very small states top every list.
# Data: data/emissions_country_year.csv, data/country_groups.csv
# Run from the project root:  Rscript charts/c_leaders_over_time/03_per_capita_ranked.r
# Saves: charts/c_leaders_over_time/03_per_capita_ranked.png (static) and charts/c_leaders_over_time/03_per_capita_ranked.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style): CO2 in 2024.
# Step 2: interactive echarts4r: CO2 / CH4 switch and a year slider (1950-2024).
#         The x axis fits each year, because a few years have extreme values (Kuwait 1991, the oil-well fires: 362 t per person)
#         that would flatten every other year; the world-average line keeps years comparable.

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()
source("charts/e_home/toggle_helper.r")            # add_toggle()

if (!file.exists("data/emissions_country_year.csv")) {
  stop("Run this script from the project root: Rscript charts/c_leaders_over_time/03_per_capita_ranked.r")
}

font     <- "sans"
seafarer <- as.vector(ltc("seafarer"))   # the site palette
barColor <- seafarer[1]                  # dark blue
refColor <- seafarer[5]                  # orange, for the world average
TOP_N    <- 20
MIN_POP  <- 1e6
FIRST    <- 1950                         # population is incomplete before 1950
OUT_PNG  <- "charts/c_leaders_over_time/03_per_capita_ranked.png"
OUT_HTML <- "charts/c_leaders_over_time/03_per_capita_ranked.html"
caption  <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR, OWID, World Bank. Excludes land use. CH4 at GWP100 = 27.9. Countries with at least 1 million people."

emissions <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
groups    <- read_csv("data/country_groups.csv", show_col_types = FALSE)
countries <- emissions |>
  filter(iso_code %in% groups$iso_code[!groups$is_aggregate], year >= FIRST)

# World average per person: all countries' emissions divided by all their people (countries with a population figure).
world <- countries |>
  filter(!is.na(population)) |>
  summarise(CO2 = sum(co2_mt, na.rm = TRUE) * 1e6 / sum(population),
            CH4 = sum(ch4_mtco2e, na.rm = TRUE) * 1e6 / sum(population), .by = year) |>
  arrange(year)

top_in <- function(yr, col) {
  countries |>
    filter(year == yr, population >= MIN_POP, !is.na(.data[[col]])) |>
    slice_max(.data[[col]], n = TOP_N, with_ties = FALSE) |>
    transmute(country, value = .data[[col]], population)
}

# ---- Step 1: static ggplot (CO2, latest year) ----
last_year <- max(countries$year)
d   <- top_in(last_year, "co2_per_capita") |> mutate(country = fct_reorder(country, value))
avg <- world$CO2[world$year == last_year]
title <- sprintf("%s emitted the most CO2 per person in %d, %.0f times the world average",
                 d$country[which.max(d$value)], last_year, max(d$value) / avg)

p <- ggplot(d, aes(value, country)) +
  geom_col(fill = barColor, width = 0.75) +
  geom_vline(xintercept = avg, linetype = "dashed", color = refColor, linewidth = 0.8) +
  annotate("text", x = avg, y = 2, label = sprintf(" World average: %.1f t", avg),
           hjust = 0, color = refColor, size = 3.5, family = font) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Tonnes of CO2 per person per year", y = NULL, title = title,
       subtitle = paste0("Top ", TOP_N, " countries, ", last_year), caption = str_wrap(caption, 120)) +
  theme_minimal_vgrid(font_family = font) +
  theme(plot.title.position = "plot", plot.caption.position = "plot",
        plot.title = element_text(size = 13),
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(OUT_PNG, p, width = 8, height = 6, dpi = 300)

# ---- Step 2: interactive echarts4r ----
years <- sort(unique(countries$year))
ranked <- function(col) {
  map(years, \(y) top_in(y, col) |> transmute(c = country, v = signif(value, 3), p = population) |> as.list())
}
payload <- list(
  years   = years,
  last    = last_year,
  top     = list(CO2 = ranked("co2_per_capita"), CH4 = ranked("ch4_per_capita")),
  world   = list(CO2 = signif(world$CO2, 3), CH4 = signif(world$CH4, 3)),
  unit    = list(CO2 = "t CO2", CH4 = "t CO2e"),
  colors  = list(bar = barColor, ref = refColor),
  topN    = TOP_N
)

e <- tibble(x = "a", y = 0) |> e_charts(x) |> e_bar(y)   # placeholder, render() below draws the chart
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6,
                         style = list(text = caption, fill = "#888", fontSize = 11))

# Gas switch (shared helper). It only asks render() to redraw; render() reads the gas and the year itself.
e <- e |>
  add_toggle(c(CO2 = "CO2", CH4 = "CH4 (methane)"), default = "CO2",
             update_js = "function(chart, mode, d) { if (window.renderRanked) window.renderRanked(); }") |>
  onRender("
    function(el, x, d) {
      var chart = echarts.getInstanceByDom(el);

      // ---- Year slider under the chart ----
      var css = document.createElement('style');
      css.textContent = `
        .rk-bottom { position: absolute; left: 10px; right: 20px; bottom: 26px; z-index: 5; display: flex; align-items: center; gap: 12px;
                     font-family: Inter, system-ui, sans-serif; color: #1e3a5f; }
        .rk-year { min-width: 64px; text-align: center; font-size: 18px; font-weight: 700; color: #fff; background: #2b6cb0;
                   border-radius: 12px; padding: 6px 10px; }
        .rk-ends { font-size: 13px; font-weight: 600; color: #5b7390; }
        .rk-bottom input[type=range] { flex: 1; min-width: 80px; accent-color: #2b6cb0; cursor: pointer; }
      `;
      document.head.appendChild(css);
      var bottom = document.createElement('div'); bottom.className = 'rk-bottom';
      var lab = document.createElement('div'); lab.className = 'rk-year';
      var rng = document.createElement('input'); rng.type = 'range'; rng.min = 0; rng.max = d.years.length - 1; rng.step = 1;
      rng.value = d.years.indexOf(d.last); rng.setAttribute('aria-label', 'Year');
      var lo = document.createElement('span'); lo.className = 'rk-ends'; lo.textContent = d.years[0];
      var hi = document.createElement('span'); hi.className = 'rk-ends'; hi.textContent = d.last;
      [lab, lo, rng, hi].forEach(function(n) { bottom.appendChild(n); });
      el.appendChild(bottom);

      function render() {
        var gas = window.chartMode || 'CO2', yi = +rng.value, year = d.years[yi];
        var t = d.top[gas][yi], avg = d.world[gas][yi], unit = d.unit[gas];
        // largest at the top: the category axis is inverted. Bars get plain numbers (matched to names by position);
        // tooltip details are looked up by row number.
        var rows = t.c.map(function(c, i) { return {name: c, value: t.v[i], pop: t.p[i]}; });
        lab.textContent = year;
        var lead = rows[0];
        chart.setOption({
          title: {text: lead.name + ' emitted the most ' + gas + ' per person in ' + year + ', ' +
                        Math.round(lead.value / avg) + ' times the world average',
                  subtext: 'Top ' + rows.length + ' countries. ' + unit + ' per person per year. Dashed line: world average (' + avg.toFixed(1) + ' ' + unit + ').',
                  left: 0, top: 0, textStyle: {fontSize: 16}, subtextStyle: {fontSize: 12}},
          grid: {top: 140, right: 40, bottom: 110, left: 150},
          tooltip: {trigger: 'item', formatter: function(p) {
            var r = rows[p.dataIndex];
            return '<b>' + r.name + '</b>, ' + year + '<br>' + r.value.toLocaleString() + ' ' + unit + ' per person<br>' +
                   (r.value / avg).toFixed(1) + ' times the world average<br>Population: ' + (r.pop / 1e6).toFixed(1) + ' million';
          }},
          xAxis: {type: 'value', min: 0, name: unit + ' per person per year', nameLocation: 'middle', nameGap: 28},
          yAxis: {type: 'category', inverse: true, data: rows.map(function(r) { return r.name; }),
                  axisTick: {show: false}, axisLabel: {fontSize: 12}},
          legend: {show: false},
          series: [{type: 'bar', data: rows.map(function(r) { return r.value; }), barWidth: '70%', itemStyle: {color: d.colors.bar},
                    markLine: {silent: true, symbol: 'none', data: [{xAxis: avg}],
                               lineStyle: {color: d.colors.ref, type: 'dashed', width: 2},
                               // 'start' is the top of the inverted axis, clear of the x-axis numbers
                               label: {formatter: 'World average', position: 'start', color: d.colors.ref, fontWeight: 'bold'}}}]
        });
      }
      window.renderRanked = render;
      rng.addEventListener('input', render);
      render();
    }
  ", data = payload)

save_echart(e, OUT_HTML)
