# Annual global emissions over a person's lifetime, with the three world events that moved CO2 furthest from its trend
# (scrollytelling step 2).
#
# Time runs left to right. Lines are the real yearly values of CO2 and CH4 (Gt CO2e per year) from the birth year to 2024; nothing is
# smoothed. Of the events in data/events_impact.csv that happened after the birth year, only the three with the largest gap between
# CO2 and its trend are shown (events, not policy milestones). Each is a shaded window with a dashed line for the CO2 trend from the
# 10 years before the event, projected forward, and a label written horizontally above the chart: a thin line drops from the label
# to a dot on the CO2 curve, so it is clear which event is which. Labels that would sit close together go on different rows.
# "Against trend" is not "caused by": it only says how emissions moved relative to their earlier path while the event was going on.
# Method: R/event_impacts.r.
# Data: data/emissions_global_year.csv, data/events_impact.csv, data/events_baseline_year.csv
# Run from the project root:  Rscript charts/d_scrollytelling/02_events_impact.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/02_events_impact.png (static) and charts/d_scrollytelling/02_events_impact.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML. A birth year box (hidden when the chart is
#         embedded in the page) redraws everything. Hover a year for the values and the event happening then.

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args)) as.integer(args[1]) else 1988L
TOP_N <- 3

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])   # same colors as the other charts
caption   <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9. Dashed line = CO2 trend from the 10 years before the event."

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt / 1000, CH4 = ch4_mtco2e / 1000)
LAST_YEAR <- max(world$year)
stopifnot(BIRTH >= min(world$year), BIRTH <= LAST_YEAR)
YMAX <- ceiling(max(world$CO2) / 10) * 10

impact   <- read_csv("data/events_impact.csv", show_col_types = FALSE)
baseline <- read_csv("data/events_baseline_year.csv", show_col_types = FALSE)

# One row per event with both gases' numbers side by side (policy markers have NA numbers)
events_all <- impact |>
  select(event, start, end, kind, description, gas, pct_vs_trend) |>
  pivot_wider(names_from = gas, values_from = pct_vs_trend, names_prefix = "pct_") |>
  select(-any_of("pct_NA"))

measured <- events_all |> filter(kind == "event", start > BIRTH)
top <- measured |> slice_max(abs(pct_CO2), n = TOP_N, with_ties = FALSE) |> arrange(start)

fmt_pct <- \(x) ifelse(is.na(x), "", paste0(ifelse(x > 0, "+", "−"), abs(x), "%"))
headline <- if (nrow(top) == 0) paste0("Born in ", BIRTH, ": none of the events in our list happened in your lifetime") else
  paste0("Born in ", BIRTH, ": ", if (nrow(top) == TOP_N) "the three events" else "the events", " that moved CO2 furthest from its trend")
subtitle <- if (nrow(top) == 0) "" else
  paste0("Out of ", nrow(measured), " events in your lifetime. The percentage is how far CO2 ran from its trend while the event lasted.")

# ---- Step 1: static ggplot ----
lines <- world |> filter(year >= BIRTH) |> pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "gt") |>
  mutate(gas = fct_relevel(gas, "CO2"))
base_win <- baseline |> filter(gas == "CO2", event %in% top$event) |> mutate(gt = baseline_mt / 1000)

# Labels go in rows above the plot: each label takes the first row where it clears the previous one (width estimated from its length)
char_w <- 0.0105 * max(LAST_YEAR - BIRTH, 1)   # about one character, in years on the x axis
lab <- top |>
  mutate(x = (start + end) / 2, label = paste0(event, ": ", fmt_pct(pct_CO2)), half = nchar(label) * char_w / 2 + 0.8,
         dot = world$CO2[match(floor(x), world$year)])
lane <- integer(nrow(lab)); row_end <- numeric(0)
for (i in seq_len(nrow(lab))) {
  l <- which(row_end < lab$x[i] - lab$half[i])[1]
  if (is.na(l)) { l <- length(row_end) + 1; row_end[l] <- -Inf }
  lane[i] <- l; row_end[l] <- lab$x[i] + lab$half[i]
}
lab$lane <- lane
n_lanes <- max(c(lane, 1))
lab$y <- YMAX * (1.1 + 0.16 * (lab$lane - 1))

p <- ggplot(lines, aes(year, gt, color = gas)) +
  geom_rect(data = top, aes(xmin = start - 0.5, xmax = end + 0.5, ymin = 0, ymax = YMAX), inherit.aes = FALSE,
            fill = "grey85", alpha = 0.6) +
  geom_line(linewidth = 1.2) +
  geom_line(data = base_win, aes(x = year, y = gt, group = event), color = "black", linetype = "dashed", linewidth = 0.8,
            inherit.aes = FALSE) +
  geom_segment(data = lab, aes(x = x, xend = x, y = y - YMAX * 0.045, yend = dot), inherit.aes = FALSE, color = "grey35", linewidth = 0.5) +
  geom_point(data = lab, aes(x = x, y = dot), inherit.aes = FALSE, color = "grey20", size = 2.2) +
  geom_label(data = lab, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 3.5, color = "grey15", family = font,
             fill = "white", linewidth = 0, label.padding = unit(0.15, "lines")) +
  geom_text(data = filter(lines, year == LAST_YEAR), aes(label = gas), hjust = 0, nudge_x = 0.6, size = 3.8, family = font,
            show.legend = FALSE) +
  scale_color_manual(values = gasColors) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.08)),
                     breaks = \(lims) { b <- round(scales::breaks_pretty()(lims)); unique(b[b <= LAST_YEAR]) }) +
  scale_y_continuous(breaks = seq(0, YMAX, by = 10), expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, YMAX * (1.1 + 0.16 * n_lanes)), clip = "off") +
  labs(x = NULL, y = "Annual emissions (Gt CO2e per year)", title = headline, subtitle = subtitle, caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none", plot.title = element_text(size = 13), plot.subtitle = element_text(size = 10, color = "grey30"),
        plot.title.position = "plot", plot.caption.position = "plot", plot.caption = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/d_scrollytelling/02_events_impact.png", p, width = 9, height = 5.5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# The chart is drawn in the browser from the data below. The label rows and the lines from each label to its dot on the CO2 curve
# are computed from the plot's pixel scale.
events_js <- events_all |> filter(kind == "event") |>
  pmap(\(event, start, end, kind, description, pct_CO2, pct_CH4) list(
    name = event, start = start, end = end, desc = description, co2 = pct_CO2, ch4 = pct_CH4))
base_js <- baseline |> filter(gas == "CO2") |>
  summarise(years = list(year), gt = list(round(baseline_mt / 1000, 3)), .by = event)
base_js <- setNames(pmap(base_js, \(event, years, gt) list(years = as.list(years), gt = as.list(gt))), base_js$event)

e <- tibble(x = 0, y = 0) |> e_charts(x) |> e_line(y)   # placeholder, the real chart is built by the script below

e <- e |> htmlwidgets::onRender("
  function(el, x, d) {
    var chart = echarts.getInstanceByDom(el);
    var q = new URLSearchParams(location.search); if (q.get('birth')) d.birth = +q.get('birth');
    var embed = q.get('embed') === '1';
    el.style.position = 'relative';
    var state = {birth: d.birth}, T = embed ? 0 : 52, ROW = 24, LEFT = 70, RIGHT = 70, picked = [], lanes = [];
    function fmt(v) { return v == null ? '' : (v > 0 ? '+' : '\\u2212') + Math.abs(v) + '%'; }

    if (!embed) {
      var css = document.createElement('style');
      css.textContent = '.c2 { font-family: system-ui, sans-serif; position: absolute; top: 8px; left: 0; z-index: 5; color: #1e3a5f; } ' +
        '.c2 label { display: block; font-size: 12px; font-weight: 600; margin: 0 0 3px 2px; } ' +
        '.c2 input { font: 600 14px system-ui, sans-serif; width: 88px; height: 34px; padding: 0 10px; color: #1e3a5f; background: #f2f7fd; border: 2px solid #2b6cb0; border-radius: 10px; }';
      document.head.appendChild(css);
      var box = document.createElement('div'); box.className = 'c2';
      var lab = document.createElement('label'); lab.textContent = 'Birth year';
      var inp = document.createElement('input'); inp.type = 'number'; inp.min = d.first; inp.max = d.last; inp.value = state.birth;
      box.appendChild(lab); box.appendChild(inp); el.appendChild(box);
      inp.addEventListener('change', function() {
        var v = Math.round(+inp.value); if (!(v >= d.first && v <= d.last)) { inp.value = state.birth; return; }
        state.birth = v; render();
      });
    }

    // label text above the plot, a thin line down to a dot on the CO2 curve; labels that would touch go on another row
    function placeLabels() {
      var g = [{type: 'text', left: 0, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11}}];
      picked.forEach(function(it, k) {
        var xPix = chart.convertToPixel({xAxisIndex: 0}, it.mid);
        var dotPix = chart.convertToPixel({xAxisIndex: 0, yAxisIndex: 0}, [it.mid, it.dot]);
        var yText = T + 74 + ROW * (lanes.length - 1 - it.lane);   // lane 0 is the lowest row, nearest the plot
        g.push({type: 'text', silent: true, x: xPix, y: yText, style: {text: it.text, fill: '#222', font: '13px system-ui, sans-serif', textAlign: 'center', textVerticalAlign: 'top'}});
        g.push({type: 'line', silent: true, shape: {x1: xPix, y1: yText + 20, x2: xPix, y2: dotPix[1]}, style: {stroke: '#555', lineWidth: 1.2}});
        g.push({type: 'circle', silent: true, shape: {cx: xPix, cy: dotPix[1], r: 4}, style: {fill: '#222'}});
      });
      chart.setOption({graphic: g}, {replaceMerge: ['graphic']});
    }

    function render() {
      var birth = state.birth;
      var idx = []; d.years.forEach(function(y, i) { if (y >= birth) idx.push(i); });
      var co2 = idx.map(function(i) { return [d.years[i], d.CO2[i]]; });
      var ch4 = idx.map(function(i) { return [d.years[i], d.CH4[i]]; });
      var measured = d.events.filter(function(ev) { return ev.start > birth; });
      var top = measured.slice().sort(function(a, b) { return Math.abs(b.co2) - Math.abs(a.co2); }).slice(0, 3)
        .sort(function(a, b) { return a.start - b.start; });
      var title = top.length === 0 ? 'Born in ' + birth + ': none of the events in our list happened in your lifetime'
        : 'Born in ' + birth + ': ' + (top.length === 3 ? 'the three events' : 'the events') + ' that moved CO2 furthest from its trend';
      var sub = top.length === 0 ? '' : 'Out of ' + measured.length + ' events in your lifetime. The percentage is how far CO2 ran from its trend while the event lasted.';

      // rows: each label goes in the first row where it clears the previous one (width estimated from its length)
      var plotW = Math.max(300, el.clientWidth - LEFT - RIGHT);
      picked = top.map(function(ev) {
        var mid = (ev.start + ev.end) / 2, i = d.years.indexOf(Math.floor(mid));
        return {ev: ev, mid: mid, dot: d.CO2[i], text: ev.name + ': ' + fmt(ev.co2)};
      });
      var rowEnd = [];
      picked.forEach(function(it) {
        var px = LEFT + (it.mid - birth) / (d.last - birth || 1) * plotW, half = it.text.length * 3.8 + 10, l = 0;
        while (rowEnd[l] !== undefined && rowEnd[l] >= px - half) l++;
        it.lane = l; rowEnd[l] = px + half;
      });
      lanes = rowEnd.length ? rowEnd : [0];

      var trend = top.map(function(ev) {
        var b = d.base[ev.name];
        return {name: 'CO2 trend before the event', type: 'line', symbol: 'none', z: 4, tooltip: {show: false},
                data: b.years.map(function(y, i) { return [y, b.gt[i]]; }), lineStyle: {width: 2, type: 'dashed', color: '#000'}};
      });
      var areas = top.map(function(ev) { return [{name: ev.name, xAxis: ev.start - 0.5}, {xAxis: ev.end + 0.5}]; });

      chart.setOption({
        title: {text: title, subtext: sub, left: 0, top: T + 4, textStyle: {fontSize: 16}, subtextStyle: {fontSize: 12}},
        color: [d.colors.CO2, d.colors.CH4, '#000'],
        grid: {top: T + 80 + ROW * lanes.length, right: RIGHT, bottom: 96, left: LEFT},
        legend: {bottom: 30, left: 'center', selectedMode: false, data: ['CO2', 'CH4', 'CO2 trend before the event']},
        tooltip: {trigger: 'axis', axisPointer: {type: 'line'}, formatter: function(ps) {
          var yr = ps[0].value[0], i = d.years.indexOf(yr); if (i < 0) return '';
          var s = '<b>' + yr + '</b><br>CO2: ' + d.CO2[i].toFixed(1) + ' Gt<br>CH4: ' + d.CH4[i].toFixed(1) + ' Gt CO2e';
          top.forEach(function(ev) {
            if (yr >= ev.start && yr <= ev.end) {
              s += '<br><b>' + ev.name + '</b> (' + (ev.start === ev.end ? ev.start : ev.start + ' to ' + ev.end) + '): CO2 ' + fmt(ev.co2) + ', CH4 ' + fmt(ev.ch4) + ' vs trend';
            }
          });
          return s;
        }},
        xAxis: {type: 'value', min: birth, max: d.last, splitLine: {show: false},
                axisLabel: {formatter: function(v) { return String(v); }, hideOverlap: true}},
        yAxis: {type: 'value', min: 0, max: d.ymax, name: 'Annual emissions (Gt CO2e per year)', nameLocation: 'middle', nameGap: 42},
        series: [
          {name: 'CO2', type: 'line', data: co2, symbol: 'none', lineStyle: {width: 3}, z: 3,
           markArea: {silent: true, itemStyle: {color: 'rgba(0,0,0,0.08)'}, label: {show: false}, data: areas}},
          {name: 'CH4', type: 'line', data: ch4, symbol: 'none', lineStyle: {width: 3}, z: 3}
        ].concat(trend)
      }, true);
      placeLabels();
    }
    var t = null;
    window.addEventListener('resize', function() { clearTimeout(t); t = setTimeout(render, 250); });
    render();
  }
", data = list(years = world$year, CO2 = round(world$CO2, 3), CH4 = round(world$CH4, 3), colors = as.list(gasColors),
               caption = gsub("\n", " ", caption, fixed = TRUE), events = events_js, base = base_js, birth = BIRTH,
               first = min(world$year), last = LAST_YEAR, ymax = YMAX))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/02_events_impact.html")
