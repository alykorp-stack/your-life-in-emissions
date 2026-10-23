# Which countries led global emissions during a person's lifetime (scrollytelling step 4).
#
# Strip chart with three rows, the 1st, 2nd and 3rd largest emitter of each year from the birth year to 2024. Consecutive
# years with the same country in a row are joined into one colored segment, so the strip reads left to right as the lifetime.
# The 1st row is the "leader"; the 2nd and 3rd rows show who was close behind (and why a leader changed).
# History is assigned to today's borders (e.g. the USSR is split among its successor states, so Russia ranks high before 1991).
# Data: data/emissions_country_year.csv (co2_rank_annual, ch4_rank_annual, co2_share_annual, ch4_share_annual)
# Run from the project root:  Rscript charts/d_scrollytelling/04_leaders_lifetime.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/04_leaders_lifetime.png (static, CO2) and charts/d_scrollytelling/04_leaders_lifetime.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML. Controls change the birth year and the gas.
#         Hover a segment for the country, its years and its average share of the world total. Prototype of step 4.

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args)) as.integer(args[1]) else 1988L

font    <- "sans"
hat     <- as.vector(ltc("hat"))
palette <- c(hat[c(6, 3, 9, 1, 4, 7, 2, 5, 8)], "#7a7a7a")
caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9. History is assigned to today's borders."
short   <- c("United States" = "USA", "United Kingdom" = "UK", "Saudi Arabia" = "Saudi Arabia")

master <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
FIRST_YEAR <- min(master$year)
LAST_YEAR  <- max(master$year)
stopifnot(BIRTH >= FIRST_YEAR, BIRTH <= LAST_YEAR)

# The three largest emitters of each year and gas, with their share of the world total
top3 <- map(c(co2 = "co2", ch4 = "ch4"), \(g) {
  master |>
    transmute(year, country, rank = .data[[paste0(g, "_rank_annual")]], share = .data[[paste0(g, "_share_annual")]]) |>
    filter(rank <= 3) |>
    arrange(year, rank, desc(share), country) |>       # a tie (CO2 in 1861: Germany and France) goes to the larger share
    slice_head(n = 3, by = year) |>
    mutate(rank = row_number(), .by = year)
})
stopifnot(all(map_lgl(top3, \(t) all(count(t, year)$n == 3))))

# One fixed color per country across both gases, in order of first appearance
countries <- bind_rows(top3) |> arrange(year) |> pull(country) |> unique()
stopifnot(length(countries) <= length(palette))
countryColors <- setNames(palette[seq_along(countries)], countries)

# Runs of consecutive years with the same country in the same row
runs <- \(t, birth) t |> filter(year >= birth) |> arrange(rank, year) |>
  mutate(run = consecutive_id(country), .by = rank) |>
  summarise(from = min(year), to = max(year), share = mean(share), .by = c(rank, run, country)) |>
  mutate(years = to - from + 1)

# ---- Step 1: static ggplot (CO2) ----
seg <- runs(top3$co2, BIRTH) |>
  mutate(row = factor(rank, levels = 3:1, labels = c("3rd", "2nd", "1st")),
         label = coalesce(short[country], country))
leaders <- seg |> filter(rank == 1)
describe <- paste0(leaders$country, " (", leaders$years, " ", if_else(leaders$years == 1, "year", "years"), ")", collapse = ", then ")
headline <- paste0("Born in ", BIRTH, ": the top CO2 emitter was ", describe)

p <- ggplot(seg) +
  geom_rect(aes(xmin = from, xmax = to + 1, ymin = as.numeric(row) - 0.38, ymax = as.numeric(row) + 0.38, fill = country),
            color = "white", linewidth = 0.4) +
  geom_text(data = filter(seg, years >= 4), aes(x = (from + to + 1) / 2, y = as.numeric(row), label = label),
            color = "white", size = 3.4, family = font, fontface = "bold") +
  scale_fill_manual(values = countryColors) +
  scale_y_continuous(breaks = 1:3, labels = levels(seg$row), expand = expansion(add = 0.5)) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = \(lims) { b <- round(scales::breaks_pretty()(lims)); unique(b[b <= LAST_YEAR]) }) +
  labs(x = NULL, y = NULL, title = headline,
       subtitle = "The three largest CO2 emitters in each year of your life (1st = largest)", caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none", panel.grid = element_blank(), plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10.5, color = "grey30"), axis.text.y = element_text(face = "bold"),
        plot.title.position = "plot", plot.caption.position = "plot", plot.caption = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/d_scrollytelling/04_leaders_lifetime.png", p, width = 9, height = 3.6, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# Per gas and row, one country and share per year; the browser groups them into segments for any birth year.
lead_js <- map(top3, \(t) {
  map(1:3, \(r) {
    x <- t |> filter(rank == r) |> arrange(year)
    list(country = x$country, share = round(x$share, 4))
  })
})

e <- tibble(x = 0, y = 0) |> e_charts(x) |> e_line(y)   # placeholder, the real chart is built by the script below
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6, style = list(text = caption, fill = "#888", fontSize = 11))

e <- e |> htmlwidgets::onRender("
  function(el, x, d) {
    var chart = echarts.getInstanceByDom(el);
    var q = new URLSearchParams(location.search); if (q.get('birth')) d.birth = +q.get('birth');
    var embed = q.get('embed') === '1';
    el.style.position = 'relative';
    var css = document.createElement('style');
    css.textContent = `
      .c4 { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; position: absolute; top: 8px; left: 0; z-index: 5; color: #1e3a5f; }
      .c4 .row { display: flex; gap: 12px; align-items: flex-end; }
      .c4 label { display: block; font-size: 12px; font-weight: 600; margin: 0 0 3px 2px; }
      .c4 input, .c4 select { font: 600 14px system-ui, sans-serif; height: 34px; padding: 0 10px; color: #1e3a5f; background: #f2f7fd;
                              border: 2px solid #2b6cb0; border-radius: 10px; }
      .c4 input[type=number] { width: 88px; }
    `;
    document.head.appendChild(css);
    var state = {birth: d.birth, gas: 'co2'};
    function mk(tag, attrs, text) {
      var n = document.createElement(tag);
      Object.keys(attrs || {}).forEach(function(k) { n.setAttribute(k, attrs[k]); });
      if (text) n.textContent = text; return n;
    }
    function field(label, ctl) { var w = mk('div'); w.appendChild(mk('label', {}, label)); w.appendChild(ctl); return w; }
    var box = mk('div', {class: 'c4'}), row = mk('div', {class: 'row'});
    var inBirth = mk('input', {type: 'number', min: d.first, max: d.last, value: state.birth});
    var selGas = mk('select');
    [['co2', 'CO2'], ['ch4', 'CH4 (as CO2e)']].forEach(function(g) { selGas.appendChild(mk('option', {value: g[0]}, g[1])); });
    row.appendChild(field('Birth year', inBirth)); row.appendChild(field('Gas', selGas));
    box.appendChild(row); el.appendChild(box);
    if (embed) { var bi = box.querySelector('input[type=number]'); if (bi) bi.parentNode.style.display = 'none'; }

    var rowNames = ['1st', '2nd', '3rd'];
    function shortName(n) { return d.short[n] || n; }
    function runsOf(r) {
      var L = d.lead[state.gas][r], out = [], cur = null;
      for (var i = 0; i < d.years.length; i++) {
        var y = d.years[i]; if (y < state.birth) continue;
        if (!cur || cur.country !== L.country[i]) { cur = {country: L.country[i], from: y, to: y, sum: 0, n: 0}; out.push(cur); }
        cur.to = y; cur.sum += L.share[i]; cur.n++;
      }
      return out;
    }
    function render() {
      var all = [0, 1, 2].map(runsOf);
      var gasName = state.gas === 'co2' ? 'CO2' : 'CH4';
      var lead = all[0].map(function(r) { return r.country + ' (' + (r.to - r.from + 1) + (r.to === r.from ? ' year)' : ' years)'); }).join(', then ');
      var series = [], tips = {};
      all.forEach(function(runs, ri) {
        runs.forEach(function(r, k) {
          var name = 'r' + ri + '-' + k, len = r.to - r.from + 1;
          tips[name] = '<b>' + r.country + '</b><br>' + rowNames[ri] + ' largest ' + gasName + ' emitter, ' + r.from + (r.to > r.from ? ' to ' + r.to : '') +
                       '<br>Average share of world ' + gasName + ': ' + Math.round(100 * r.sum / r.n) + '%';
          var data = [null, null, null]; data[ri] = len;
          series.push({name: name, type: 'bar', stack: 'strip', barWidth: '62%', data: data, itemStyle: {color: d.colors[r.country], borderColor: '#fff', borderWidth: 1},
                       label: {show: len >= 4, formatter: shortName(r.country), color: '#fff', fontWeight: 'bold', fontSize: 12}});
        });
      });
      chart.setOption({
        title: {text: 'Born in ' + state.birth + ': the top ' + gasName + ' emitter was ' + lead, left: 0, top: 70, textStyle: {fontSize: 16},
                subtext: 'The three largest ' + gasName + ' emitters in each year of your life (1st = largest)', subtextStyle: {fontSize: 12}},
        grid: {top: 160, right: 30, bottom: 60, left: 50},
        tooltip: {trigger: 'item', formatter: function(p) { return tips[p.seriesName]; }},
        legend: {show: false},
        xAxis: {type: 'value', min: 0, max: d.last - state.birth + 1, splitLine: {show: false},
                axisLabel: {formatter: function(v) { return String(state.birth + v); }, hideOverlap: true, showMaxLabel: false}},
        yAxis: {type: 'category', data: rowNames, inverse: true, axisLine: {show: false}, axisTick: {show: false}, axisLabel: {fontWeight: 'bold', fontSize: 13}},
        graphic: {type: 'text', left: 0, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11}},
        series: series
      }, true);
    }
    inBirth.addEventListener('change', function() {
      var v = Math.round(+inBirth.value); if (!(v >= d.first && v <= d.last)) { inBirth.value = state.birth; return; }
      state.birth = v; render();
    });
    selGas.addEventListener('change', function() { state.gas = selGas.value; render(); });
    render();
  }
", data = list(years = FIRST_YEAR:LAST_YEAR, lead = lead_js, colors = as.list(countryColors), short = as.list(short),
               caption = caption, birth = BIRTH, first = FIRST_YEAR, last = LAST_YEAR))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/04_leaders_lifetime.html")
