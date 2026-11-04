# Which countries led global emissions during a person's lifetime (scrollytelling step 4).
#
# Time runs left to right. One line per country: its real yearly emissions (Gt per year) from the birth year to 2024. The countries
# shown are every country that was the largest or second largest emitter in at least one year of the lifetime, so the lines are the
# ones that competed for the lead. Where one line crosses another, the lead changed hands; the vertical distance between lines is
# the gap. Each country has its own color, and its name sits at the right end of its line.
# History is assigned to today's borders (e.g. the USSR is split among its successor states, so Russia ranks high before 1991).
# Data: data/emissions_country_year.csv (co2_mt, ch4_mtco2e, co2_rank_annual, ch4_rank_annual, co2_share_annual, ch4_share_annual)
# Run from the project root:  Rscript charts/d_scrollytelling/04_leaders_lifetime.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/04_leaders_lifetime.png (static, CO2) and charts/d_scrollytelling/04_leaders_lifetime.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML. Controls change the birth year and the gas.
#         Hover a year for each country's value and who was 1st and 2nd. Prototype of step 4.

library(tidyverse)
library(cowplot)
library(ggrepel)
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

# The largest and second largest emitter of each year and gas
top2 <- map(c(co2 = "co2", ch4 = "ch4"), \(g) {
  master |>
    transmute(year, country, rank = .data[[paste0(g, "_rank_annual")]], share = .data[[paste0(g, "_share_annual")]]) |>
    filter(rank <= 3) |>
    arrange(year, rank, desc(share), country) |>       # a tie (CO2 in 1861: Germany and France) goes to the larger share
    slice_head(n = 3, by = year) |>
    mutate(rank = row_number(), .by = year) |>
    filter(rank <= 2)
})
stopifnot(all(map_lgl(top2, \(t) all(count(t, year)$n == 2))))

# One fixed color per country across both gases, in order of first appearance
countries <- bind_rows(top2) |> arrange(year) |> pull(country) |> unique()
stopifnot(length(countries) <= length(palette))
countryColors <- setNames(palette[seq_along(countries)], countries)

# Runs of consecutive years with the same country in 1st place
leader_runs <- \(t, birth) t |> filter(year >= birth, rank == 1) |> arrange(year) |>
  mutate(run = consecutive_id(country)) |>
  summarise(from = min(year), to = max(year), .by = c(run, country)) |>
  mutate(years = to - from + 1)

# ---- Step 1: static ggplot (CO2) ----
leaders <- leader_runs(top2$co2, BIRTH)
describe <- paste0(leaders$country, " (", leaders$years, " ", if_else(leaders$years == 1, "year", "years"), ")", collapse = ", then ")
headline <- paste0("Born in ", BIRTH, ": the top CO2 emitter was ", describe)

shown <- top2$co2 |> filter(year >= BIRTH) |> pull(country) |> unique()
lines <- master |> filter(country %in% shown, year >= BIRTH) |> transmute(year, country, gt = co2_mt / 1000)
ends  <- lines |> filter(year == LAST_YEAR) |> mutate(label = coalesce(short[country], country))

p <- ggplot(lines, aes(year, gt, color = country)) +
  geom_line(linewidth = 1.2) +
  geom_text_repel(data = ends, aes(label = label), hjust = 0, direction = "y", nudge_x = 0.8, xlim = c(LAST_YEAR + 0.5, NA),
                  segment.color = NA, size = 3.8, family = font, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = countryColors) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.14)),
                     breaks = \(lims) { b <- round(scales::breaks_pretty()(lims)); unique(b[b <= LAST_YEAR]) }) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Annual CO2 emissions (Gt per year)", title = headline,
       subtitle = "Every country that was the largest or second largest CO2 emitter in a year of your life", caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none", plot.title = element_text(size = 13), plot.subtitle = element_text(size = 10.5, color = "grey30"),
        plot.title.position = "plot", plot.caption.position = "plot", plot.caption = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/d_scrollytelling/04_leaders_lifetime.png", p, width = 9, height = 5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# Per gas: who was 1st and 2nd in each year, and the yearly Gt of every country that was 1st or 2nd at any point.
# The browser picks the countries that matter for any birth year.
lead_js <- map(top2, \(t) list(first = t$country[t$rank == 1][order(t$year[t$rank == 1])],
                               second = t$country[t$rank == 2][order(t$year[t$rank == 2])]))
val_js <- imap(c(co2 = "co2_mt", ch4 = "ch4_mtco2e"), \(col, g) {
  cs <- unique(top2[[g]]$country)
  setNames(map(cs, \(cn) {
    x <- master |> filter(country == cn) |> arrange(year)
    stopifnot(nrow(x) == length(FIRST_YEAR:LAST_YEAR), !anyNA(x[[col]]))
    as.list(round(x[[col]] / 1000, 3))
  }), cs)
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

    function shortName(n) { return d.short[n] || n; }
    function render() {
      var g = state.gas, L = d.lead[g], gasName = g === 'co2' ? 'CO2' : 'CH4';
      // countries that were 1st or 2nd in some year of the lifetime, and who led in each stretch
      var names = [], seen = {}, runs = [], cur = null;
      d.years.forEach(function(y, i) {
        if (y < state.birth) return;
        [L.first[i], L.second[i]].forEach(function(n) { if (!seen[n]) { seen[n] = 1; names.push(n); } });
        if (!cur || cur.country !== L.first[i]) { cur = {country: L.first[i], n: 0}; runs.push(cur); }
        cur.n++;
      });
      var lead = runs.map(function(r) { return r.country + ' (' + r.n + (r.n === 1 ? ' year)' : ' years)'); }).join(', then ');
      var series = names.map(function(n) {
        var data = [];
        d.years.forEach(function(y, i) { if (y >= state.birth) data.push([y, d.val[g][n][i]]); });
        return {name: n, type: 'line', data: data, symbol: 'none', lineStyle: {width: 3, color: d.colors[n]}, itemStyle: {color: d.colors[n]},
                endLabel: {show: true, formatter: function(p) { return shortName(p.seriesName); }, color: d.colors[n], fontSize: 13, fontWeight: 'bold'},
                labelLayout: {moveOverlap: 'shiftY'}};
      });
      chart.setOption({
        title: {text: 'Born in ' + state.birth + ': the top ' + gasName + ' emitter was ' + lead, left: 0, top: 70, textStyle: {fontSize: 16, width: Math.max(300, el.clientWidth - 40), overflow: 'break'},
                subtext: 'Every country that was the largest or second largest ' + gasName + ' emitter in a year of your life', subtextStyle: {fontSize: 12}},
        grid: {top: 160, right: 130, bottom: 60, left: 70},
        tooltip: {trigger: 'axis', axisPointer: {type: 'line'}, formatter: function(ps) {
          var yr = ps[0].value[0], i = d.years.indexOf(yr), rows = ps.slice().sort(function(a, b) { return b.value[1] - a.value[1]; });
          return '<b>' + yr + '</b>' + rows.map(function(p) {
            var tag = L.first[i] === p.seriesName ? ' (largest)' : L.second[i] === p.seriesName ? ' (2nd largest)' : '';
            return '<br>' + p.marker + shortName(p.seriesName) + ': ' + p.value[1].toFixed(2) + ' Gt' + tag;
          }).join('');
        }},
        legend: {show: false},
        xAxis: {type: 'value', min: state.birth, max: d.last, splitLine: {show: false},
                axisLabel: {formatter: function(v) { return String(v); }, hideOverlap: true}},
        yAxis: {type: 'value', min: 0, name: 'Annual ' + gasName + (g === 'co2' ? '' : ' (as CO2e)') + ' emissions (Gt per year)', nameLocation: 'middle', nameGap: 42},
        graphic: {type: 'text', left: 0, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11}},
        series: series
      }, true);
    }
    inBirth.addEventListener('change', function() {
      var v = Math.round(+inBirth.value); if (!(v >= d.first && v <= d.last)) { inBirth.value = state.birth; return; }
      state.birth = v; render();
    });
    selGas.addEventListener('change', function() { state.gas = selGas.value; render(); });
    window.addEventListener('resize', function() { render(); });
    render();
  }
", data = list(years = FIRST_YEAR:LAST_YEAR, lead = lead_js, val = val_js, colors = as.list(countryColors), short = as.list(short),
               caption = caption, birth = BIRTH, first = FIRST_YEAR, last = LAST_YEAR))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/04_leaders_lifetime.html")
