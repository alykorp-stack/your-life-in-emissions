# Emissions in your birth year vs today (scrollytelling, "then vs now").
#
# Two bars per gas: global annual emissions in the birth year and in the last data year (2024), in Gt CO2e, with the multiplier
# ("2024 emissions are 1.7 times those of 1988"). Real yearly values, nothing modelled.
# Data: data/emissions_global_year.csv
# Run from the project root:  Rscript charts/d_scrollytelling/06_year_vs_today.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/06_year_vs_today.png (static) and charts/d_scrollytelling/06_year_vs_today.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML (birth year box under the title).

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args)) as.integer(args[1]) else 1988L

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])
caption   <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9."

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt / 1000, CH4 = ch4_mtco2e / 1000)
LAST_YEAR <- max(world$year)
stopifnot(BIRTH >= min(world$year), BIRTH <= LAST_YEAR)

pair <- \(birth) world |> filter(year %in% c(birth, LAST_YEAR)) |>
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "gt") |>
  mutate(when = factor(if_else(year == birth, paste0("Your birth year, ", birth), paste0("Today, ", LAST_YEAR)),
                       levels = c(paste0("Your birth year, ", birth), paste0("Today, ", LAST_YEAR))),
         gas = fct_relevel(gas, "CO2"))

# ---- Step 1: static ggplot ----
d <- pair(BIRTH)
mult <- d |> summarise(x = gt[year == LAST_YEAR] / gt[year == BIRTH], .by = gas)
x_co2 <- mult$x[mult$gas == "CO2"]; x_ch4 <- mult$x[mult$gas == "CH4"]
headline <- if (BIRTH == LAST_YEAR) paste0("Born in ", BIRTH, ": your birth year is today") else
  paste0("Born in ", BIRTH, ": today's CO2 emissions are ", formatC(x_co2, format = "f", digits = 1), " times those of your birth year")
subtitle <- if (BIRTH == LAST_YEAR) "" else paste0("CH4 is ", formatC(x_ch4, format = "f", digits = 1), " times as high")

p <- ggplot(d, aes(gas, gt, fill = gas, alpha = when)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = formatC(gt, format = "f", digits = 1), group = when), position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.8, family = font, show.legend = FALSE, color = "grey20", alpha = 1) +
  scale_fill_manual(values = gasColors, guide = "none") +
  scale_alpha_manual(values = setNames(c(0.45, 1), levels(d$when)), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Annual emissions (Gt CO2e per year)", title = headline, subtitle = subtitle, caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "top", legend.justification = "left", plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10.5, color = "grey30"), plot.title.position = "plot", plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA))
ggsave("charts/d_scrollytelling/06_year_vs_today.png", p, width = 8, height = 5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
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
      css.textContent = '.c6 { font-family: system-ui, sans-serif; position: absolute; top: 8px; left: 0; z-index: 5; color: #1e3a5f; } ' +
        '.c6 label { display: block; font-size: 12px; font-weight: 600; margin: 0 0 3px 2px; } ' +
        '.c6 input { font: 600 14px system-ui, sans-serif; width: 88px; height: 34px; padding: 0 10px; color: #1e3a5f; background: #f2f7fd; border: 2px solid #2b6cb0; border-radius: 10px; }';
      document.head.appendChild(css);
      var box = document.createElement('div'); box.className = 'c6';
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
      var gases = ['CO2', 'CH4'];
      var then = gases.map(function(g) { return +d[g][i0].toFixed(2); }), now = gases.map(function(g) { return +d[g][i1].toFixed(2); });
      var m = gases.map(function(g, k) { return now[k] / then[k]; });
      var title = state.birth === d.last ? 'Born in ' + state.birth + ': your birth year is today'
        : 'Born in ' + state.birth + ': today\\'s CO2 emissions are ' + m[0].toFixed(1) + ' times those of your birth year';
      var sub = state.birth === d.last ? '' : 'CH4 is ' + m[1].toFixed(1) + ' times as high. Light bars: ' + state.birth + '. Dark bars: ' + d.last + '.';
      var mk = function(vals, name, op) {
        return {name: name, type: 'bar', barGap: '10%', data: vals.map(function(v, k) { return {value: v, itemStyle: {color: d.colors[gases[k]], opacity: op}}; }),
                label: {show: true, position: 'top', formatter: function(p) { return p.value.toFixed(1); }}};
      };
      chart.setOption({
        title: {text: title, subtext: sub, left: 0, top: embed ? 8 : 56, textStyle: {fontSize: 16}, subtextStyle: {fontSize: 12}},
        grid: {top: embed ? 100 : 150, right: 30, bottom: 70, left: 70},
        legend: {show: false},
        tooltip: {trigger: 'item', formatter: function(p) { return p.seriesName + '<br>' + gases[p.dataIndex] + ': ' + p.value.toFixed(1) + ' Gt CO2e'; }},
        graphic: {type: 'text', left: 0, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11}},
        xAxis: {type: 'category', data: gases, axisLabel: {fontSize: 14, fontWeight: 'bold'}},
        yAxis: {type: 'value', min: 0, name: 'Gt CO2e per year', nameLocation: 'middle', nameGap: 48},
        series: [mk(then, 'Your birth year, ' + state.birth, 0.45), mk(now, 'Today, ' + d.last, 1)]
      }, true);
    }
    render();
  }
", data = list(years = world$year, CO2 = world$CO2, CH4 = world$CH4, colors = as.list(gasColors), caption = caption,
               birth = BIRTH, first = min(world$year), last = LAST_YEAR))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/06_year_vs_today.html")
