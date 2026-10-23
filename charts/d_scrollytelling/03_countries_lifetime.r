# Which countries emitted the most during a person's lifetime (scrollytelling step 3).
#
# Lines are each country's running total of emissions since the birth year, in Gt (CO2, or CH4 as CO2e). The default is the five
# countries with the largest total since birth; the interactive version lets the visitor tick any country in a sidebar to show or hide it, find one by name, remove
# one, switch gas, switch between total emissions and emissions per person, switch to emissions per year, and change the birth
# year. Per person = tonnes per resident (the master table's per capita columns). The per person default ranking only includes
# countries with at least 1 million people (MIN_POP), because very small states top any per person list; any country can still be added.
# Countries only (aggregates such as regions and income groups are dropped); the world total is the sum of the countries.
# Data: data/emissions_country_year.csv, data/country_groups.csv
# Run from the project root:  Rscript charts/d_scrollytelling/03_countries_lifetime.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/03_countries_lifetime.png and 03_countries_lifetime_per_person.png (static) and charts/d_scrollytelling/03_countries_lifetime.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as two PNGs: top 5 countries for CO2, total and per person.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML. The controls above the chart change the
#         birth year, gas and view; the sidebar lists every country (ranked by the chosen measure) and a click shows or hides its line. Prototype of the countries
#         step on the scrollytelling page.

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args)) as.integer(args[1]) else 1988L
TOP_N <- 5
MIN_POP <- 1e6   # per person ranking: minimum population in the last year

font    <- "sans"
hat     <- as.vector(ltc("hat"))
palette <- c(hat[c(6, 3, 9, 1, 4, 7, 2, 5, 8)], "#7a7a7a")   # blue, red, green, yellow, ... one color per line
caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9. Countries missing a value count as zero."

groups <- read_csv("data/country_groups.csv", show_col_types = FALSE)
d <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE) |>
  filter(iso_code %in% groups$iso_code[!groups$is_aggregate]) |>
  transmute(country, year, CO2 = co2_mt, CH4 = ch4_mtco2e, CO2pc = co2_per_capita, CH4pc = ch4_per_capita, pop = population)
FIRST_YEAR <- min(d$year)
LAST_YEAR  <- max(d$year)
stopifnot(BIRTH >= FIRST_YEAR, BIRTH <= LAST_YEAR, !anyDuplicated(distinct(d, country, year)[c("country", "year")]))
eligible <- d |> filter(year == LAST_YEAR, pop >= MIN_POP) |> pull(country)

# ---- Step 1: static ggplot (CO2, top 5 since birth) ----
since <- d |> filter(year >= BIRTH) |>
  mutate(CO2 = coalesce(CO2, 0)) |>
  arrange(country, year) |>
  mutate(gt = CO2 / 1000)
totals <- since |> summarise(total = sum(CO2) / 1000, .by = country) |> arrange(desc(total))
top <- totals |> slice_head(n = TOP_N)
world_total <- sum(totals$total)
share_top <- sum(top$total) / world_total

topColors <- setNames(palette[seq_len(TOP_N)], top$country)
lines <- since |> filter(country %in% top$country) |> mutate(country = factor(country, levels = top$country))
ends <- lines |> filter(year == LAST_YEAR)

headline <- paste0("Born in ", BIRTH, ": ", paste(top$country[1:2], collapse = " and "), " lead CO2 emissions during your lifetime")
subtitle <- paste0("The top ", TOP_N, " countries emitted ", round(100 * share_top), "% of all CO2 since ", BIRTH,
                   " (", round(world_total), " Gt); ", top$country[1], " alone ", round(100 * top$total[1] / world_total), "%")

p <- ggplot(lines, aes(year, gt, color = country)) +
  geom_line(linewidth = 1) +
  geom_text_repel(data = ends, aes(label = country), hjust = 0, direction = "y", nudge_x = 0.8, size = 3.8, family = font,
                  segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = topColors) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.2)),
                     breaks = \(lims) { b <- round(scales::breaks_pretty()(lims)); unique(b[b <= LAST_YEAR]) }) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
  labs(x = NULL, y = "CO2 emitted per year (Gt)", title = headline, subtitle = subtitle, caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none", plot.title = element_text(size = 13), plot.subtitle = element_text(size = 10.5, color = "grey30"),
        plot.title.position = "plot", plot.caption.position = "plot", plot.caption = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/d_scrollytelling/03_countries_lifetime.png", p, width = 9, height = 5.5, dpi = 300)

# Per person version: tonnes of CO2 per resident each year, countries with at least MIN_POP people ranked by their average since birth
pc <- d |> filter(year >= BIRTH, country %in% eligible) |> mutate(CO2pc = coalesce(CO2pc, 0))
top_pc <- pc |> summarise(avg = mean(CO2pc), .by = country) |> arrange(desc(avg)) |> slice_head(n = TOP_N)
pcColors <- setNames(palette[seq_len(TOP_N)], top_pc$country)
lines_pc <- pc |> filter(country %in% top_pc$country) |> mutate(country = factor(country, levels = top_pc$country))
p_pc <- ggplot(lines_pc, aes(year, CO2pc, color = country)) +
  geom_line(linewidth = 1) +
  geom_text_repel(data = filter(lines_pc, year == LAST_YEAR), aes(label = country), hjust = 0, direction = "y", nudge_x = 0.8,
                  size = 3.8, family = font, segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = pcColors) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.2)),
                     breaks = \(lims) { b <- round(scales::breaks_pretty()(lims)); unique(b[b <= LAST_YEAR]) }) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "CO2 per person (tonnes per year)",
       title = paste0("Born in ", BIRTH, ": per person, ", top_pc$country[1], " leads CO2 emissions during your lifetime"),
       subtitle = paste0("Ranked by average tonnes per person per year since ", BIRTH, " among countries with at least ", MIN_POP / 1e6,
                         " million people.\nBy total emissions the top three are ", paste(top$country[1:2], collapse = ", "), " and ", top$country[3], "."),
       caption = paste0(sub("Excludes land use. CH4 at GWP100 = 27.9. Countries missing a value count as zero.", "Excludes land use.", caption, fixed = TRUE),
                        if ("Kuwait" %in% top_pc$country && BIRTH <= 1991) " Kuwait's 1991 spike is the Gulf War oil-well fires." else "")) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none", plot.title = element_text(size = 13), plot.subtitle = element_text(size = 10, color = "grey30"),
        plot.title.position = "plot", plot.caption.position = "plot", plot.caption = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("charts/d_scrollytelling/03_countries_lifetime_per_person.png", p_pc, width = 9, height = 5.5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# All countries' yearly values go into the page (one array per gas, years in order); the browser does the sums, so any birth
# year, gas and country set works without re-running R.
years <- FIRST_YEAR:LAST_YEAR
country_js <- d |>
  complete(country, year = years) |>
  arrange(country, year) |>
  summarise(CO2 = list(round(CO2, 1)), CH4 = list(round(CH4, 1)), CO2pc = list(round(CO2pc, 2)), CH4pc = list(round(CH4pc, 2)),
            pop = last(pop), .by = country) |>
  pmap(\(country, CO2, CH4, CO2pc, CH4pc, pop) list(name = country, CO2 = CO2, CH4 = CH4, CO2pc = CO2pc, CH4pc = CH4pc, ok = !is.na(pop) && pop >= MIN_POP))

e <- tibble(x = 0, y = 0) |> e_charts(x) |> e_line(y)   # placeholder, the real chart is built by the script below
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6, style = list(text = caption, fill = "#888", fontSize = 11))

e <- e |> htmlwidgets::onRender("
  function(el, x, d) {
    var chart = echarts.getInstanceByDom(el);
    var q = new URLSearchParams(location.search); if (q.get('birth')) d.birth = +q.get('birth');
    var embed = q.get('embed') === '1';
    el.style.position = 'relative';
    var SIDE = 230;   // sidebar width in px
    var css = document.createElement('style');
    css.textContent = `
      .c3 { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; color: #1e3a5f; }
      .c3-top { position: absolute; top: 8px; left: 0; right: 0; z-index: 5; display: flex; flex-wrap: wrap; gap: 12px; align-items: flex-end; }
      .c3 label.f { display: block; font-size: 12px; font-weight: 600; margin: 0 0 3px 2px; }
      .c3 input[type=number], .c3 select, .c3 input[type=search] { font: 600 14px system-ui, sans-serif; height: 34px; padding: 0 10px; color: #1e3a5f;
                              background: #f2f7fd; border: 2px solid #2b6cb0; border-radius: 10px; }
      .c3 input[type=number] { width: 88px; }
      .c3 button { font: 700 13px system-ui, sans-serif; height: 34px; padding: 0 14px; color: #fff; background: #2b6cb0; border: 0; border-radius: 10px; cursor: pointer; }
      .c3 button:hover { background: #1e4e8c; }
      .c3-side { position: absolute; z-index: 5; left: 0; top: 66px; bottom: 28px; width: ${SIDE}px; display: flex; flex-direction: column;
                 border: 1px solid #cfd8e3; border-radius: 12px; background: #f8fafc; overflow: hidden; }
      .c3-side .head { padding: 8px; border-bottom: 1px solid #e1e8f0; }
      .c3-side input[type=search] { width: 100%; box-sizing: border-box; height: 30px; font-size: 13px; }
      .c3-side .tools { display: flex; gap: 6px; margin-top: 6px; }
      .c3-side .tools button { height: 26px; padding: 0 10px; font-size: 12px; }
      .c3-side .count { font-size: 12px; margin-top: 6px; color: #4a6283; }
      .c3-side .note { font-size: 11px; margin-top: 2px; color: #7b8da6; }
      .c3-side .list { overflow-y: auto; flex: 1; padding: 2px 0; }
      .c3-side .item { display: flex; align-items: center; gap: 8px; padding: 3px 10px; font-size: 13px; cursor: pointer; user-select: none; }
      .c3-side .item:hover { background: #e9f0f8; }
      .c3-side .item.on { font-weight: 700; background: #eaf2fb; }
      .c3-side .rk { width: 24px; text-align: right; color: #7b8da6; font-size: 11px; flex: none; }
      .c3-side .nm { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .c3-side .vl { color: #7b8da6; font-size: 11px; flex: none; }
      .c3-side .dot { width: 10px; height: 10px; border-radius: 50%; flex: none; border: 1px solid #9fb2c8; background: #fff; }
    `;
    document.head.appendChild(css);

    var yr0 = d.first, yr1 = d.last;
    var state = {birth: d.birth, gas: 'CO2', measure: 'total', mode: 'year', countries: [], custom: false, filter: ''};
    var byName = {}; d.countries.forEach(function(c) { byName[c.name] = c; });
    var colorOf = {};

    function mk(tag, attrs, text) {
      var n = document.createElement(tag);
      Object.keys(attrs || {}).forEach(function(k) { n.setAttribute(k, attrs[k]); });
      if (text) n.textContent = text;
      return n;
    }
    function field(label, ctl) { var w = mk('div'); w.appendChild(mk('label', {class: 'f'}, label)); w.appendChild(ctl); return w; }

    // top controls
    var root = mk('div', {class: 'c3'});
    var top = mk('div', {class: 'c3-top'});
    var inBirth = mk('input', {type: 'number', min: yr0, max: yr1, value: state.birth});
    var selGas = mk('select'); ['CO2', 'CH4'].forEach(function(g) { selGas.appendChild(mk('option', {value: g}, g)); });
    var selMeasure = mk('select');
    [['total', 'Total emissions'], ['pc', 'Emissions per person']].forEach(function(m) { selMeasure.appendChild(mk('option', {value: m[0]}, m[1])); });
    var selMode = mk('select');
    [['year', 'Per year'], ['total', 'Running total since birth']].forEach(function(m) { selMode.appendChild(mk('option', {value: m[0]}, m[1])); });
    var birthField = field('Birth year', inBirth);
    if (embed) birthField.style.display = 'none';
    top.appendChild(birthField); top.appendChild(field('Gas', selGas)); top.appendChild(field('Measure', selMeasure)); top.appendChild(field('View', selMode));
    root.appendChild(top);

    // sidebar: every country, ranked by the chosen measure, tick to show or hide its line
    var side = mk('div', {class: 'c3-side'});
    var head = mk('div', {class: 'head'});
    var inFind = mk('input', {type: 'search', placeholder: 'Find a country'});
    var tools = mk('div', {class: 'tools'});
    var btnTop = mk('button', {title: 'Show the 5 largest again'}, 'Top 5');
    var btnClear = mk('button', {title: 'Hide all lines'}, 'Clear');
    tools.appendChild(btnTop); tools.appendChild(btnClear);
    var count = mk('div', {class: 'count'});
    var note = mk('div', {class: 'note'});
    head.appendChild(inFind); head.appendChild(tools); head.appendChild(count); head.appendChild(note);
    var list = mk('div', {class: 'list'});
    side.appendChild(head); side.appendChild(list); root.appendChild(side);
    el.appendChild(root);

    // value of one country for each year from the birth year: Gt (total) or tonnes per person
    function key() { return state.measure === 'pc' ? state.gas + 'pc' : state.gas; }
    function val(c, i) { return (c[key()][i] || 0) / (state.measure === 'pc' ? 1 : 1000); }
    function series(name) {
      var c = byName[name], out = [], run = 0;
      for (var i = 0; i < d.years.length; i++) {
        var y = d.years[i]; if (y < state.birth) continue;
        var v = val(c, i);
        run += v;
        out.push([y, +(state.mode === 'total' ? run : v).toFixed(3)]);
      }
      return out;
    }
    // every country with its lifetime sum of the measure, largest first
    function ranking() {
      var tot = d.countries.map(function(c) {
        var s = 0; for (var i = 0; i < d.years.length; i++) if (d.years[i] >= state.birth) s += val(c, i);
        return {name: c.name, s: s, ok: c.ok};
      });
      tot.sort(function(a, b) { return b.s - a.s; });
      return tot;
    }
    function topN(n, rk) {
      return rk.filter(function(t) { return state.measure === 'total' || t.ok; }).slice(0, n).map(function(t) { return t.name; });
    }
    function colorFor(n) {
      if (colorOf[n]) return colorOf[n];
      var used = {}; Object.keys(colorOf).forEach(function(k) { if (state.countries.indexOf(k) >= 0) used[colorOf[k]] = true; });
      var free = d.palette.filter(function(c) { return !used[c]; });
      colorOf[n] = free.length ? free[0] : 'hsl(' + ((Object.keys(colorOf).length * 47) % 360) + ', 55%, 42%)';
      return colorOf[n];
    }
    function fmtVal(s) {
      if (state.measure === 'pc') return Math.round(s).toLocaleString() + ' t';
      return (s >= 10 ? Math.round(s) : s.toFixed(2)) + ' Gt';
    }
    function toggle(n) {
      state.custom = true;
      var i = state.countries.indexOf(n);
      if (i >= 0) { state.countries.splice(i, 1); delete colorOf[n]; } else { state.countries.push(n); }
      render();
    }
    function render() {
      var rk = ranking();
      if (!state.custom) { state.countries = topN(5, rk); colorOf = {}; }
      state.countries.forEach(colorFor);

      // sidebar rows
      var keepScroll = list.scrollTop;
      list.innerHTML = '';
      var f = state.filter.toLowerCase();
      rk.forEach(function(t, idx) {
        if (f && t.name.toLowerCase().indexOf(f) < 0) return;
        var on = state.countries.indexOf(t.name) >= 0;
        var row = mk('div', {class: 'item' + (on ? ' on' : ''), title: (on ? 'Click to hide ' : 'Click to show ') + t.name});
        var dot = mk('span', {class: 'dot'}); if (on) { dot.style.background = colorOf[t.name]; dot.style.borderColor = colorOf[t.name]; }
        row.appendChild(dot);
        row.appendChild(mk('span', {class: 'rk'}, String(idx + 1)));
        row.appendChild(mk('span', {class: 'nm'}, t.name));
        row.appendChild(mk('span', {class: 'vl'}, fmtVal(t.s)));
        row.addEventListener('click', function() { toggle(t.name); });
        list.appendChild(row);
      });
      list.scrollTop = keepScroll;
      count.textContent = state.countries.length + ' of ' + d.countries.length + ' countries shown';
      note.textContent = state.measure === 'pc' ? 'Ranked by tonnes per person summed over your lifetime' : 'Ranked by total emitted since ' + state.birth;

      var per = state.measure === 'pc';
      var unit = state.mode === 'total' ? 'running total since ' + state.birth : 'per year';
      var gasName = state.gas === 'CO2' ? 'CO2' : 'CH4 (as CO2e)';
      var worldTotal = 0, sel = 0;
      d.countries.forEach(function(c) { for (var i = 0; i < d.years.length; i++) if (d.years[i] >= state.birth) worldTotal += c[state.gas][i] || 0; });
      state.countries.forEach(function(n) { var c = byName[n]; for (var i = 0; i < d.years.length; i++) if (d.years[i] >= state.birth) sel += c[state.gas][i] || 0; });
      var yrs = d.years.filter(function(y) { return y >= state.birth; }).length;
      var lead = state.countries[0] ? byName[state.countries[0]] : null, leadAvg = 0;
      if (lead) for (var j = 0; j < d.years.length; j++) if (d.years[j] >= state.birth) leadAvg += (lead[state.gas + 'pc'][j] || 0) / yrs;
      var sub = !state.countries.length ? 'Tick a country in the list to start'
        : per ? state.countries[0] + ' averaged ' + leadAvg.toFixed(1) + ' t per person per year since ' + state.birth +
                (state.custom ? '' : '; default ranking covers countries with at least 1 million people')
              : 'The ' + state.countries.length + ' countries shown emitted ' + Math.round(100 * sel / worldTotal) +
                '% of all ' + state.gas + ' since ' + state.birth + ' (' + Math.round(worldTotal / 1000) + ' Gt)';
      chart.setOption({
        title: {text: 'Emissions of ' + gasName + (per ? ' per person' : '') + ' by country, ' + unit, subtext: sub, left: SIDE + 20, top: 66,
                textStyle: {fontSize: 16}, subtextStyle: {fontSize: 12}},
        color: state.countries.map(function(n) { return colorOf[n]; }),
        grid: {top: 140, right: 150, bottom: 60, left: SIDE + 80},
        tooltip: {trigger: 'axis', valueFormatter: function(v) { return v.toLocaleString() + (per ? ' t per person' : ' Gt'); }},
        legend: {show: false},
        graphic: {type: 'text', left: SIDE + 20, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11}},
        xAxis: {type: 'value', min: state.birth, max: yr1, splitLine: {show: false}, axisLabel: {formatter: function(v) { return String(v); }, hideOverlap: true}},
        yAxis: {type: 'value', min: 0, name: (per ? 'Tonnes per person ' : 'Gt ') + (state.gas === 'CO2' ? 'CO2' : 'CO2e') + (state.mode === 'year' ? ' per year' : ' (running total)'), nameLocation: 'middle', nameGap: 52},
        series: state.countries.map(function(n) {
          return {name: n, type: 'line', data: series(n), symbol: 'none', lineStyle: {width: 3},
                  endLabel: {show: true, formatter: '{a}', fontSize: 13}, labelLayout: {moveOverlap: 'shiftY'}};
        })
      }, true);
    }
    btnTop.addEventListener('click', function() { state.custom = false; render(); });
    btnClear.addEventListener('click', function() { state.custom = true; state.countries = []; colorOf = {}; render(); });
    inFind.addEventListener('input', function() { state.filter = inFind.value.trim(); render(); });
    inBirth.addEventListener('change', function() {
      var v = Math.round(+inBirth.value); if (!(v >= yr0 && v <= yr1)) { inBirth.value = state.birth; return; }
      state.birth = v; render();
    });
    selGas.addEventListener('change', function() { state.gas = selGas.value; render(); });
    selMeasure.addEventListener('change', function() { state.measure = selMeasure.value; render(); });
    selMode.addEventListener('change', function() { state.mode = selMode.value; render(); });
    render();
  }
", data = list(years = years, countries = country_js, birth = BIRTH, first = FIRST_YEAR, last = LAST_YEAR, palette = as.list(palette), caption = caption))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/03_countries_lifetime.html")
