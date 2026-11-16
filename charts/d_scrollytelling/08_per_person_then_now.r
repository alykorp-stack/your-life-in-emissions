# Emissions per person in your birth year vs 2024, for the largest emitters (scrollytelling, dumbbell).
#
# One row per country: a grey dot for the birth year, a blue dot for 2024, joined by a line. Rows are the 15 largest
# CO2 emitters of 2024 plus the world, sorted by the change (largest rise at the top). Real yearly values.
# Countries without a population figure in the birth year (some, before 1950) are left out, and the subtitle says so.
# Data: data/emissions_country_year.csv, data/country_groups.csv
# Run from the project root:  Rscript charts/d_scrollytelling/08_per_person_then_now.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/08_per_person_then_now.png (static) and charts/d_scrollytelling/08_per_person_then_now.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version, saved as standalone HTML. The scrolly page passes ?embed=1&birth=YYYY;
#         opened on its own, the chart shows a birth year box.

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args)) as.integer(args[1]) else 1988L

font      <- "sans"
hat       <- as.vector(ltc("hat"))
nowColor  <- hat[6]       # blue, as CO2 elsewhere in the scrolly charts
thenColor <- "grey60"     # the earlier year is context
N_TOP     <- 15
caption   <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, OWID, World Bank. Excludes land use. Population before 1950 is incomplete."

emissions <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
groups    <- read_csv("data/country_groups.csv", show_col_types = FALSE)
countries <- emissions |> filter(iso_code %in% groups$iso_code[!groups$is_aggregate])
LAST_YEAR <- max(countries$year)
FIRST     <- min(countries$year)
stopifnot(BIRTH >= FIRST, BIRTH <= LAST_YEAR)

# The 15 largest CO2 emitters of the last year, and the world average per person (all countries with a population figure).
top_codes <- countries |> filter(year == LAST_YEAR) |> slice_max(co2_mt, n = N_TOP) |> pull(iso_code)
world <- countries |>
  filter(!is.na(population)) |>
  summarise(pc = sum(co2_mt, na.rm = TRUE) * 1e6 / sum(population), .by = year) |>
  mutate(country = "World")
per_person <- countries |>
  filter(iso_code %in% top_codes) |>
  transmute(country, year, pc = co2_per_capita) |>
  bind_rows(world)

# ---- Step 1: static ggplot ----
d <- per_person |>
  filter(year %in% c(BIRTH, LAST_YEAR)) |>
  pivot_wider(names_from = year, values_from = pc, names_prefix = "y") |>
  rename(then = paste0("y", BIRTH), now = paste0("y", LAST_YEAR)) |>
  filter(!is.na(then), !is.na(now)) |>
  mutate(country = fct_reorder(country, now - then))
rose <- sum(d$now > d$then & d$country != "World"); fell <- sum(d$now < d$then & d$country != "World")
title <- sprintf("Born in %d: CO2 per person rose in %d of these countries and fell in %d", BIRTH, rose, fell)

p <- ggplot(d, aes(y = country)) +
  geom_segment(aes(x = then, xend = now, yend = country), color = "grey80", linewidth = 1.5) +
  geom_point(aes(x = then), color = thenColor, size = 3) +
  geom_point(aes(x = now), color = nowColor, size = 3) +
  expand_limits(x = 0) +
  labs(x = "Tonnes of CO2 per person per year", y = NULL, title = title,
       subtitle = sprintf("Grey: %d. Blue: %d. The %d largest emitters of %d, plus the world.", BIRTH, LAST_YEAR, N_TOP, LAST_YEAR),
       caption = caption) +
  theme_minimal_vgrid(font_family = font) +
  theme(plot.title.position = "plot", plot.caption.position = "plot",
        plot.title = element_text(size = 13), plot.subtitle = element_text(size = 10.5, color = "grey30"),
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("charts/d_scrollytelling/08_per_person_then_now.png", p, width = 8, height = 5.5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
years <- FIRST:LAST_YEAR
series_of <- function(cn) {
  per_person |> filter(country == cn) |> right_join(tibble(year = years), by = "year") |> arrange(year) |>
    pull(pc) |> signif(3)
}
names_all <- c(countries |> filter(iso_code %in% top_codes, year == LAST_YEAR) |> pull(country), "World")
payload <- list(
  years = years, names = names_all, pc = map(names_all, series_of),
  birth = BIRTH, first = FIRST, last = LAST_YEAR, topN = N_TOP,
  colors = list(now = nowColor, then = "#999999"), caption = caption
)

e <- tibble(x = 0, y = 0) |> e_charts(x) |> e_line(y)   # placeholder, the real chart is built by the script below
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6, style = list(text = caption, fill = "#888", fontSize = 11))

e <- e |> htmlwidgets::onRender("
  function(el, x, d) {
    var chart = echarts.getInstanceByDom(el);
    var q = new URLSearchParams(location.search); if (q.get('birth')) d.birth = +q.get('birth');
    var embed = q.get('embed') === '1';
    el.style.position = 'relative';
    var state = {birth: d.birth};
    if (!embed) {
      var css = document.createElement('style');
      css.textContent = '.c8 { font-family: Inter, system-ui, sans-serif; position: absolute; top: 8px; left: 0; z-index: 5; color: #1e3a5f; } ' +
        '.c8 label { display: block; font-size: 12px; font-weight: 600; margin: 0 0 3px 2px; } ' +
        '.c8 input { font: 600 14px Inter, system-ui, sans-serif; width: 88px; height: 34px; padding: 0 10px; color: #1e3a5f; background: #f2f7fd; border: 2px solid #2b6cb0; border-radius: 10px; }';
      document.head.appendChild(css);
      var box = document.createElement('div'); box.className = 'c8';
      var lab = document.createElement('label'); lab.textContent = 'Birth year';
      var inp = document.createElement('input'); inp.type = 'number'; inp.min = d.first; inp.max = d.last; inp.value = state.birth;
      box.appendChild(lab); box.appendChild(inp); el.appendChild(box);
      inp.addEventListener('change', function() {
        var v = Math.round(+inp.value); if (!(v >= d.first && v <= d.last)) { inp.value = state.birth; return; }
        state.birth = v; render();
      });
    }
    function render() {
      var i0 = d.years.indexOf(state.birth), i1 = d.years.length - 1;
      // rows with a value in both years, sorted by the change (largest rise first; the axis is inverted)
      var rows = [];
      d.names.forEach(function(n, k) {
        var a = d.pc[k][i0], b = d.pc[k][i1];
        if (typeof a === 'number' && typeof b === 'number') rows.push({name: n, then: a, now: b});
      });
      rows.sort(function(r, s) { return (s.now - s.then) - (r.now - r.then); });
      var c = rows.filter(function(r) { return r.name !== 'World'; });
      var rose = c.filter(function(r) { return r.now > r.then; }).length, fell = c.filter(function(r) { return r.now < r.then; }).length;
      var missing = d.topN + 1 - rows.length;
      var title = state.birth === d.last ? 'Born in ' + state.birth + ': your birth year is today'
        : 'Born in ' + state.birth + ': CO2 per person rose in ' + rose + ' of these countries and fell in ' + fell;
      var sub = 'The ' + d.topN + ' largest emitters of ' + d.last + ', plus the world.' +
                (missing > 0 ? ' ' + missing + ' left out: no population figure for ' + state.birth + '.' : '');
      var top = embed ? 8 : 72;
      chart.setOption({
        title: {text: title, subtext: sub, left: 0, top: top, textStyle: {fontSize: 16}, subtextStyle: {fontSize: 12}},
        grid: {top: top + 100, right: 40, bottom: 70, left: 130},
        // Legend for the two dots, under the title. Not clickable: hiding one dot would leave the lines hanging.
        legend: {show: true, top: top + 52, left: 0, selectedMode: false, itemWidth: 16, itemHeight: 16, itemGap: 24,
                 textStyle: {fontSize: 14, fontWeight: 'bold', color: '#333'},
                 data: [{name: 'Your birth year (' + state.birth + ')', icon: 'circle'}, {name: String(d.last), icon: 'circle'}]},
        tooltip: {trigger: 'item', formatter: function(p) {
          var r = rows[p.value[1]];
          return '<b>' + r.name + '</b><br>' + state.birth + ': ' + r.then.toFixed(1) + ' t per person<br>' + d.last + ': ' + r.now.toFixed(1) +
                 ' t per person<br>Change: ' + (r.now >= r.then ? '+' : '') + (r.now - r.then).toFixed(1) + ' t';
        }},
        graphic: {type: 'text', left: 0, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11, fontFamily: 'Inter, system-ui, sans-serif'}},
        xAxis: {type: 'value', min: 0, name: 'Tonnes of CO2 per person per year', nameLocation: 'middle', nameGap: 28},
        yAxis: {type: 'category', inverse: true, data: rows.map(function(r) { return r.name; }), axisTick: {show: false},
                axisLabel: {fontSize: 12, formatter: function(v) { return v === 'World' ? '{b|World}' : v; }, rich: {b: {fontWeight: 'bold', fontSize: 12}}}},
        series: [
          // the connecting line, drawn from the row number
          {type: 'custom', silent: true, data: rows.map(function(r, i) { return [r.then, r.now, i]; }),
           renderItem: function(params, api) {
             var a = api.coord([api.value(0), api.value(2)]), b = api.coord([api.value(1), api.value(2)]);
             return {type: 'line', shape: {x1: a[0], y1: a[1], x2: b[0], y2: b[1]}, style: {stroke: '#cccccc', lineWidth: 4}};
           }, encode: {x: [0, 1], y: 2}, z: 1},
          {name: 'Your birth year (' + state.birth + ')', type: 'scatter', symbolSize: 13, itemStyle: {color: d.colors.then},
           data: rows.map(function(r, i) { return [r.then, i]; }), z: 2},
          {name: String(d.last), type: 'scatter', symbolSize: 13, itemStyle: {color: d.colors.now},
           data: rows.map(function(r, i) { return [r.now, i]; }), z: 3}
        ]
      }, true);
    }
    render();
  }
", data = payload)

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/08_per_person_then_now.html")
