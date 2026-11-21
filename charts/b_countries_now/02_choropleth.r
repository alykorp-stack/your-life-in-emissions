# World map of emissions by country.
#
# Choropleth for one year with a metric choice (annual, cumulative, per capita). Needs a world geometry file joined on iso_code, the one non-CSV input.
# Data: data/emissions_country_year.csv, data/raw/natural_earth/ne_50m_admin_0_countries.geojson (Natural Earth, saved in the project)
# Run from the project root:  Rscript charts/b_countries_now/02_choropleth.r
# Saves: charts/b_countries_now/02_choropleth.png (static) and charts/b_countries_now/02_choropleth.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         plot_geo() choropleth on iso_code with a year slider and metric dropdown. The static version is a single year with sf + geom_sf.
#         The HTML is one file, but plotly downloads the base-map shapes (world_50m.json) from cdn.plot.ly, so it needs an internet connection to draw the map.

# sf reads the world boundaries. If a package is missing: install.packages(c("sf", "plotly", "htmlwidgets"))
library(tidyverse)
library(cowplot)
library(sf)
library(plotly)
library(htmlwidgets)
library(ltc)

# Map colors: a two-part scale from the ltc "alger" and "dora" palettes.
#   Below the break (most countries): greens, light to dark. A pale tint of alger's sage, alger's sage, dora's
#     teal-grey, alger's dark teal, and a deeper shade of that dark teal. The tint and shade stretch the ramp
#     (lightness 89 to 18 instead of 78 to 35) so differences between countries are easier to see.
#   Above the break (the biggest emitters): reds, light to dark. Dora's orange, alger's terracotta, dora's crimson,
#     dora's plum.
# The break is set per metric in `metrics` below (3,000 Mt for annual emissions); the color jumps from dark green
# to orange there. Used by the static map, the plotly map and the "Find similar emitters" bar, so all three match.
dora   <- as.character(ltc("dora"))
alger  <- as.character(ltc("alger"))
mix    <- function(col, with, amount) colorRampPalette(c(col, with))(101)[round(amount * 100) + 1]
greens <- c(mix(alger[3], "white", 0.5), alger[3], dora[1], alger[2], mix(alger[2], "black", 0.45))
warms  <- c(dora[4], alger[5], dora[3], dora[2])
# Yellow "Find similar emitters" highlight: stands out against both the greens and the oranges.
highlight_color <- "#efb306"

# Color stops for a scale from 0 to zmax with the break at brk. Positions are on the sqrt scale the map uses,
# so they match where values sit on the color bar. The reds start a hair after the greens end, which makes a sharp
# step at the break (ggplot drops stops at exactly the same position, so they cannot be equal).
color_stops <- function(brk, zmax) {
  p <- sqrt(brk / zmax)
  list(pos = c(seq(0, p, length.out = length(greens)), seq(p + 1e-6, 1, length.out = length(warms))),
       col = c(greens, warms))
}

# Paths are relative to the project root, so fail early with a clear message if run from somewhere else.
if (!file.exists("data/emissions_country_year.csv")) {
  stop("Run this script from the project root: Rscript charts/b_countries_now/02_choropleth.r")
}

# ---- Data (shared by both steps) ----
emissions <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
groups    <- read_csv("data/country_groups.csv", show_col_types = FALSE)

# Keep individual countries only (drops World, regions, income groups), so nothing is double-counted.
country_codes <- groups |> filter(!is_aggregate) |> pull(iso_code)
countries <- emissions |> filter(iso_code %in% country_codes)

# World boundaries: Natural Earth 50m countries, downloaded once from
# https://github.com/nvkelso/natural-earth-vector (public domain) and kept in data/raw/natural_earth/.
# 50m (not 110m) so small states like Singapore, Malta and Bahrain have a shape.
# ISO_A3 is -99 for France and Norway, so we use ISO_A3_EH, which fills them in.
# Natural Earth codes South Sudan as SDS; our data uses the ISO code SSD.
# Two uninhabited Australian territories (Indian Ocean Ter. "IOA", Ashmore and Cartier Is. "ATC") also carry "AUS".
# We drop those two shapes so Australia's emissions are not repeated by the join (merging shapes with
# st_union() instead leaves stripes across the map). The check below stops the script if any other code repeats.
# Shapes with no ISO code (-99: Northern Cyprus, Somaliland, Kosovo, Siachen) have no emissions rows and are dropped.
world <- st_read("data/raw/natural_earth/ne_50m_admin_0_countries.geojson", quiet = TRUE) |>
  mutate(iso_code = if_else(ISO_A3_EH == "SDS", "SSD", ISO_A3_EH)) |>
  filter(!iso_code %in% c("-99", "ATA"),             # Antarctica has no emissions data and takes up map space
         !ADM0_A3 %in% c("IOA", "ATC")) |>
  select(iso_code, geometry)
stopifnot(!anyDuplicated(world$iso_code))

default_year <- 2024

# ---- Step 1: static ggplot ----
map_data <- world |>
  left_join(filter(countries, year == default_year), by = "iso_code")

# Check the ISO codes: emitters in the data that did not find a shape on the map.
unmatched <- countries |>
  filter(year == default_year, !iso_code %in% world$iso_code) |>
  arrange(desc(co2_mt))
message("Countries with data but no shape on the map: ", nrow(unmatched))
if (nrow(unmatched) > 0) message("  largest: ", unmatched$country[1], ", ", round(unmatched$co2_mt[1]), " Mt")
# The join must not add or repeat rows.
stopifnot(nrow(map_data) == nrow(world))
message("Map shapes with no emissions data (drawn grey): ", sum(is.na(map_data$co2_mt)))

# Same color range and break as the interactive map's annual view (see payload$zmax below).
static_zmax  <- signif(max(countries$co2_mt, na.rm = TRUE), 3)
static_stops <- color_stops(3000, static_zmax)

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = co2_mt), color = "white", linewidth = 0.1) +
  # sqrt scale so mid-sized emitters stay visible next to China and the US; green below 3,000 Mt, orange above
  scale_fill_gradientn(
    colors = static_stops$col, values = static_stops$pos, limits = c(0, static_zmax), oob = scales::squish,
    trans = "sqrt", na.value = "#d9d9d9",
    breaks = c(100, 1000, 3000, 10000), labels = scales::comma,
    name = "Mt CO\u2082",
    guide = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "top")
  ) +
  coord_sf(crs = "+proj=eqearth", expand = FALSE) +   # Equal Earth: true country areas
  labs(
    title    = paste0("Annual CO\u2082 emissions by country, ", default_year),
    subtitle = "Million tonnes of CO\u2082 per year. Grey = no data.",
    caption  = "Source: PRIMAP-hist v2.2 (to 2018), OWID / Global Carbon Budget (2019\u20132024). Boundaries: Natural Earth."
  ) +
  theme_map(font_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.caption = element_text(size = 7, color = "grey40", hjust = 0),
    legend.position = "bottom",
    legend.justification = "left",
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave("charts/b_countries_now/02_choropleth.png", p_map, width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# plot_geo() draws the map; a little JavaScript (onRender) adds the year slider and metric dropdown.
# Both controls call one update function that reads BOTH current values, so changing one never resets the other.
# (plotly's built-in sliders and dropdowns cannot see each other, which is why we do it this way.)

# Metrics: column name, label, unit shown on hover.
# cap_p: the colour scale tops out at this quantile of the values. Per-capita has a few extreme outliers
# (up to 515 t per person), so it is capped at the 99.5th percentile; those countries get the darkest colour.
# brk: where the color switches from the green ramp to orange (the biggest emitters are above it).
metrics <- tribble(
  ~key,            ~label,                               ~unit,                   ~cap_p, ~brk,
  "co2_mt",        "Annual emissions",                   "Mt CO\u2082",           1,      3000,
  "co2_cum",       "Cumulative emissions (1850\u2013)", "Mt CO\u2082",           1,      100000,
  "co2_per_capita","Per-capita emissions",               "t CO\u2082 per person", 0.995,  8
)
# Legend tick values per metric (the colour scale is a sqrt scale, like the static map). Each includes the break.
tick_values <- list(
  co2_mt         = c(0, 100, 1000, 3000, 10000),
  co2_cum        = c(0, 10000, 50000, 100000, 250000),
  co2_per_capita = c(0, 1, 5, 8, 20)
)
stopifnot(map2_lgl(tick_values, metrics$brk, \(v, b) b %in% v))

years     <- sort(unique(countries$year))
iso_codes <- sort(unique(countries$iso_code))
names_by_iso <- countries |> distinct(iso_code, country) |> arrange(iso_code) |> pull(country)

# One years x countries matrix per metric. Missing values stay NA (shown grey), never 0.
make_matrix <- function(col) {
  countries |>
    select(iso_code, year, value = all_of(col)) |>
    complete(year = years, iso_code = iso_codes) |>
    arrange(year, iso_code) |>
    pull(value) |>
    matrix(nrow = length(years), ncol = length(iso_codes), byrow = TRUE) |>
    signif(4) |>
    (\(m) lapply(seq_len(nrow(m)), \(i) m[i, ]))()
}
# Colour range per metric is fixed across all years, so the slider is comparable over time.
zmax  <- map2_dbl(metrics$key, metrics$cap_p, \(k, p) signif(quantile(countries[[k]], p, na.rm = TRUE), 3))
stops <- map2(metrics$brk, zmax, color_stops)
plotly_scale <- function(s) map2(s$pos, s$col, \(p, col) list(p, col))

payload <- list(
  years     = years,
  iso       = iso_codes,
  names     = names_by_iso,
  metrics   = metrics$key,
  labels    = metrics$label,
  units     = metrics$unit,
  defaultYear = default_year,
  zmax      = zmax,
  capped    = metrics$cap_p < 1,
  tickVals  = unname(tick_values),
  tickText  = map(tick_values, \(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)) |> unname(),
  values    = setNames(map(metrics$key, make_matrix), metrics$key),
  stops     = stops,   # per metric: color stops for the map and the color bar on the left
  highlight = highlight_color
)

i0 <- which(years == default_year)
p_int <- plot_geo() |>
  add_trace(
    type = "choropleth", locations = iso_codes, z = sqrt(unlist(payload$values$co2_mt[[i0]])),
    text = names_by_iso, hoverinfo = "text",
    zmin = 0, zmax = sqrt(payload$zmax[1]), autocolorscale = FALSE,
    colorscale = plotly_scale(stops[[1]]),
    marker = list(line = list(color = "white", width = 0.3)),
    showscale = FALSE   # the legend is our own color bar on the left (see "Similar emitters" in onRender)
  ) |>
  # Yellow layer on top: the "Similar emitters" bar fills it with the countries near the value under the pointer.
  # hoverinfo "skip" lets the mouse reach the country tooltips underneath.
  add_trace(
    type = "choropleth", locations = list(), z = list(), hoverinfo = "skip", showscale = FALSE,
    zmin = 0, zmax = 1, colorscale = list(c(0, highlight_color), c(1, highlight_color)),
    marker = list(line = list(color = "#8a6400", width = 0.6))
  ) |>
  layout(
    # Title is centred on the whole chart (xref "container"), not just the map area. The JavaScript below rewrites its text.
    # The title sits 16 px from the top of the chart (yref "container"); the download button is level with it on the
    # right (.ctl-top in onRender). The top margin leaves room for the title only, so the map starts just below it.
    # The bottom margin is for the year slider, the left margin for the control column.
    title = list(text = "<b>CO\u2082 emissions by country</b>", x = 0.5, y = 1, xref = "container", yref = "container",
                 xanchor = "center", yanchor = "top", pad = list(t = 16), font = list(size = 22, color = "#1f2a37")),
    margin = list(t = 56, b = 95, l = 212, r = 10),
    geo = list(
      lataxis = list(range = c(-58, 85)),   # crop Antarctica, as in the static map
      resolution = 50,   # 50m shapes so small states are drawn (the default 110m has none of them)
      projection = list(type = "equal earth"), showframe = FALSE, showcoastlines = FALSE,
      showland = TRUE, landcolor = "#d9d9d9",      # countries without data stay neutral grey
      showcountries = FALSE, bgcolor = "white"
    ),
    paper_bgcolor = "white",
    font = list(family = "Inter, system-ui, sans-serif"),   # the site font (loaded in onRender below)
    annotations = list(list(
      text = "Source: PRIMAP-hist v2.2 (to 2018), OWID / Global Carbon Budget (2019\u20132024). Grey = no data.",
      x = 0, y = -0.01, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left", yanchor = "top",
      font = list(size = 10, color = "#666666")
    ))
  ) |>
  # No Plotly toolbar: zoom and download are our own bigger buttons (see onRender).
  config(displayModeBar = FALSE) |>
  onRender("
    function(el, x, d) {
      // ---- Inter, the site font: load it, then let plotly re-measure its text ----
      var font = document.createElement('link');
      font.rel = 'stylesheet';
      font.href = 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap';
      font.onload = function() { document.fonts.load('400 12px Inter').then(function() { Plotly.Plots.resize(el); }); };
      document.head.appendChild(font);

      // ---- Styles for the custom controls ----
      var css = document.createElement('style');
      css.textContent = `
        .ctl { font-family: Inter, system-ui, sans-serif; color: #1e3a5f; }
        /* Download button: small, on the right, level with the title */
        .ctl-top { position: absolute; top: 18px; right: 16px; z-index: 5; display: flex; flex-direction: column; align-items: flex-end; }
        .ctl.ctl-top button { height: 26px; min-width: 0; padding: 0 10px; font-size: 12px; box-shadow: none; }
        .ctl-label { display: block; font-size: 13px; font-weight: 600; margin: 0 0 5px 4px; }
        .sel-wrap { position: relative; }
        .sel-wrap::after { content: '\\u25BE'; position: absolute; right: 14px; top: 50%; transform: translateY(-50%);
                           color: #2b6cb0; font-size: 18px; pointer-events: none; }
        /* White controls with a thin blue border */
        .ctl select { appearance: none; -webkit-appearance: none; font: 600 15px Inter, system-ui, sans-serif; color: #1e3a5f;
                      padding: 8px 36px 8px 12px; width: 100%; height: 42px; background: #fff;
                      border: 1px solid #2b6cb0; border-radius: 4px; cursor: pointer; transition: background .15s; }
        .ctl select:hover { background: #f2f7fd; }
        .ctl select:focus-visible, .ctl button:focus-visible, .ctl input:focus-visible { outline: 3px solid #93c5fd; outline-offset: 2px; }
        .ctl button { font: 700 15px Inter, system-ui, sans-serif; height: 46px; min-width: 46px; padding: 0 16px; color: #2b6cb0;
                      background: #fff; border: 1px solid #2b6cb0; border-radius: 4px; cursor: pointer;
                      box-shadow: 0 1px 3px rgba(43, 108, 176, 0.15); transition: background .15s, transform .05s; }
        .ctl button:hover { background: #f2f7fd; }
        .ctl button:active { transform: translateY(1px); }
        /* Zoom buttons: 32 px, about 30% smaller than the other buttons (46 px) */
        .ctl .zoom { font-size: 18px; line-height: 1; padding: 0; width: 32px; height: 32px; min-width: 32px; border-radius: 4px; }
        .zoom-row { display: flex; align-items: center; gap: 8px; margin-top: 8px; justify-content: flex-end; }
        .zoom-row .ctl-label { margin: 0 4px 0 0; }
        /* Year slider starts at the map edge (plotly margin l 212), so the left control column can run full height */
        .ctl-bottom { position: absolute; left: 212px; right: 24px; bottom: 24px; z-index: 5; display: flex;
                      align-items: center; gap: 14px; }
        .ctl .play { min-width: 112px; }   /* same width for Play and Pause, so the slider does not jump */
        .yr { min-width: 80px; text-align: center; font-size: 20px; font-weight: 700; color: #fff; background: #2b6cb0;
              border-radius: 4px; padding: 7px 12px; box-shadow: 0 2px 6px rgba(43, 108, 176, 0.35); }
        .ends { font-size: 13px; font-weight: 600; color: #5b7390; }
        .ctl input[type=range] { -webkit-appearance: none; appearance: none; flex: 1; height: 10px; border-radius: 6px;
                                 background: #cbd5e1; cursor: pointer; margin: 0; }
        .ctl input[type=range]::-webkit-slider-thumb { -webkit-appearance: none; width: 28px; height: 28px; border-radius: 50%;
                                 background: #fff; border: 5px solid #2b6cb0; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3); }
        .ctl input[type=range]::-moz-range-thumb { width: 18px; height: 18px; border-radius: 50%; background: #fff;
                                 border: 5px solid #2b6cb0; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3); }
        /* Left column: metric dropdown, the Find similar emitters bar, zoom buttons */
        .sim { position: absolute; left: 16px; top: 16px; width: 185px; z-index: 5; }
        .sim-title { font-size: 14px; font-weight: 700; margin: 18px 0 4px; }
        .sim-help { font-size: 12px; line-height: 1.35; color: #44546a; margin: 0 0 10px; }
        .sim-unit { font-size: 12px; font-weight: 600; margin-bottom: 6px; }
        .sim-wrap { display: flex; height: 170px; }
        .sim-bar { position: relative; width: 18px; height: 100%; border-radius: 4px; cursor: ns-resize; touch-action: none; }
        .sim-bar:focus-visible { outline: 3px solid #93c5fd; outline-offset: 2px; }
        .sim-handle { display: none; position: absolute; left: -7px; width: 32px; height: 0; border-top: 3px solid var(--hl);
                      transform: translateY(1.5px); pointer-events: none; }
        .sim-handle::before { content: ''; position: absolute; left: -9px; top: -9px; border: 7px solid transparent;
                              border-left: 9px solid var(--hl); }
        .sim-ticks { position: relative; flex: 1; margin-left: 8px; font-size: 11px; color: #44546a; }
        .sim-tick { position: absolute; left: 0; transform: translateY(50%); white-space: nowrap; }
        .sim-out { font-size: 12px; line-height: 1.35; margin-top: 10px; min-height: 3em; }
      `;
      document.head.appendChild(css);
      el.style.position = 'relative';
      el.style.setProperty('--hl', d.highlight);   // highlight color, used by the marker on the color bar

      function make(tag, cls, text) {
        var n = document.createElement(tag);
        if (cls) n.className = cls;
        if (text) n.textContent = text;
        return n;
      }

      // ---- Controls: the dropdown and zoom buttons go in the left column (built below); download stays top right ----
      var top = make('div', 'ctl ctl-top');
      var selBox = make('div');
      var selLabel = make('label', 'ctl-label', 'Select what you want to view:');
      var wrap = make('div', 'sel-wrap');
      var sel = make('select');
      selLabel.htmlFor = sel.id = 'metric-select';
      d.labels.forEach(function(l, i) { var o = document.createElement('option'); o.value = i; o.text = l; sel.appendChild(o); });
      wrap.appendChild(sel); selBox.appendChild(selLabel); selBox.appendChild(wrap);
      var zoomOut = make('button', 'zoom', '\\u2212'); zoomOut.title = zoomOut.ariaLabel = 'Zoom out';
      var zoomIn  = make('button', 'zoom', '+');        zoomIn.title  = zoomIn.ariaLabel  = 'Zoom in';
      var dl = make('button', '', '\\u2193  Download PNG'); dl.title = 'Download the map as a PNG image';
      top.appendChild(dl);
      el.appendChild(top);

      // ---- Bottom: play button and year slider under the map ----
      var bottom = make('div', 'ctl ctl-bottom');
      var play = make('button', 'play', '\\u25B6 Play'); play.setAttribute('aria-label', 'Play through the years');
      bottom.appendChild(play);
      var lab = make('div', 'yr');
      var rng = make('input');
      rng.type = 'range'; rng.min = 0; rng.max = d.years.length - 1; rng.step = 1;
      rng.value = d.years.indexOf(d.defaultYear); rng.setAttribute('aria-label', 'Year');
      bottom.appendChild(lab);
      bottom.appendChild(make('span', 'ends', d.years[0]));
      bottom.appendChild(rng);
      bottom.appendChild(make('span', 'ends', d.years[d.years.length - 1]));
      el.appendChild(bottom);

      function fmt(v, m) {
        if (m === 2) return v.toLocaleString(undefined, {maximumFractionDigits: 2});
        return v >= 100 ? Math.round(v).toLocaleString() : v.toLocaleString(undefined, {maximumFractionDigits: 1});
      }
      // One update function reads BOTH controls every time.
      function update() {
        var m = +sel.value, yi = +rng.value, year = d.years[yi];
        var row = d.values[d.metrics[m]][yi], z = [], txt = [];
        for (var i = 0; i < d.iso.length; i++) {
          var v = row[i], ok = (typeof v === 'number');
          z.push(ok ? Math.sqrt(v) : null);   // sqrt scale; null = no data, shown grey (not zero)
          txt.push('<b>' + d.names[i] + '</b><br>' + year + '<br>' +
                   (ok ? fmt(v, m) + ' ' + d.units[m] : 'No data'));
        }
        lab.textContent = year;
        var pct = yi / (d.years.length - 1) * 100;   // filled part of the slider track
        rng.style.background = 'linear-gradient(to right, #2b6cb0 ' + pct + '%, #cbd5e1 ' + pct + '%)';
        // Per-capita is capped (see cap_p in R): values above zmax get the darkest colour, so say so in the legend title.
        var cbTitle = d.units[m] + (d.capped[m] ? '<br>(scale capped at ' + d.zmax[m] + ')' : '');
        // Colors for this metric: green ramp below its break, orange above (stops built in R by color_stops()).
        var cs = d.stops[m].pos.map(function(p, i) { return [p, d.stops[m].col[i]]; });
        Plotly.restyle(el, {z: [z], text: [txt], zmax: [Math.sqrt(d.zmax[m])], colorscale: [cs]}, [0]);
        Plotly.relayout(el, {'title.text': '<b>' + d.labels[m] + ' by country, ' + year + '</b>'});
        drawBar(m, cbTitle.replace('<br>', ' '));
        showSimilar();   // keep the highlight in step when the year or metric changes
      }

      // ---- Left: 'Similar emitters' color bar. Hover or drag along it; countries near that value turn yellow ----
      var sim = make('div', 'ctl sim');
      sim.appendChild(selBox);
      sim.appendChild(make('div', 'sim-title', 'Find similar emitters'));
      sim.appendChild(make('p', 'sim-help',
        'This bar is the map legend. Hover over it, drag along it, or use the arrow keys: ' +
        'countries with about the same emissions as that point light up in yellow.'));
      var unitLab = make('div', 'sim-unit');
      var barWrap = make('div', 'sim-wrap'), bar = make('div', 'sim-bar'), ticks = make('div', 'sim-ticks');
      var handle = make('div', 'sim-handle');
      bar.appendChild(handle);
      bar.tabIndex = 0;
      bar.setAttribute('role', 'slider');
      bar.setAttribute('aria-label', 'Find countries with similar emissions');
      barWrap.appendChild(bar); barWrap.appendChild(ticks);
      var out = make('div', 'sim-out');
      var zoomRow = make('div', 'zoom-row');
      [make('span', 'ctl-label', 'Zoom'), zoomOut, zoomIn].forEach(function(n) { zoomRow.appendChild(n); });
      [unitLab, barWrap, out].forEach(function(n) { sim.appendChild(n); });
      top.appendChild(zoomRow);   // zoom sits under the download button, top right
      el.appendChild(sim);

      // Legend ticks for the current metric, placed on the same sqrt scale as the map colors.
      function drawBar(m, unitText) {
        unitLab.textContent = unitText;
        bar.style.background = 'linear-gradient(to top, ' +   // same stops as the map, so the sharp break matches
          d.stops[m].col.map(function(c, i) { return c + ' ' + (d.stops[m].pos[i] * 100) + '%'; }).join(', ') + ')';
        ticks.innerHTML = '';
        var top = Math.sqrt(d.zmax[m]), last = -100;
        d.tickVals[m].forEach(function(v, i) {
          var p = Math.sqrt(v) / top * 100;
          if (p > 100 || p - last < 10) return;   // off the bar, or too close to the label below it
          last = p;
          var t = make('div', 'sim-tick', d.tickText[m][i]);
          t.style.bottom = p + '%';
          ticks.appendChild(t);
        });
      }

      // pos: 0 (bottom of the bar) to 1 (top), or null when nothing is picked.
      // A country matches when its color is within BAND of the picked point (the same idea as the home-page map).
      var pos = null, BAND = 0.04;
      function showSimilar() {
        if (pos === null) {
          handle.style.display = 'none';
          out.textContent = '';
          Plotly.restyle(el, {locations: [[]], z: [[]]}, [1]);
          return;
        }
        var m = +sel.value, yi = +rng.value, row = d.values[d.metrics[m]][yi], top = Math.sqrt(d.zmax[m]);
        var picked = Math.pow(pos * top, 2), hits = [];
        for (var i = 0; i < d.iso.length; i++) {
          var v = row[i];
          if (typeof v !== 'number') continue;
          if (Math.abs(Math.min(Math.sqrt(Math.max(v, 0)) / top, 1) - pos) <= BAND) hits.push(i);
        }
        hits.sort(function(a, b) { return row[b] - row[a]; });
        handle.style.display = 'block';
        handle.style.bottom = (pos * 100) + '%';
        bar.setAttribute('aria-valuenow', Math.round(picked));
        var names = hits.slice(0, 6).map(function(i) { return d.names[i]; }).join(', ');
        out.innerHTML = '';
        out.appendChild(make('b', '', 'About ' + fmt(picked, m) + ' ' + d.units[m] + ': '));
        out.appendChild(document.createTextNode(hits.length === 0 ? 'no countries' :
          names + (hits.length > 6 ? ' and ' + (hits.length - 6) + ' more' : '')));
        Plotly.restyle(el, {locations: [hits.map(function(i) { return d.iso[i]; })],
                            z: [hits.map(function() { return 1; })]}, [1]);
      }
      function pick(e) {
        var r = bar.getBoundingClientRect();
        pos = Math.min(1, Math.max(0, (r.bottom - e.clientY) / r.height));
        showSimilar();
      }
      var dragging = false;
      bar.addEventListener('pointerdown', function(e) { dragging = true; bar.setPointerCapture(e.pointerId); pick(e); });
      bar.addEventListener('pointermove', pick);
      bar.addEventListener('pointerup', function() { dragging = false; });
      bar.addEventListener('pointerleave', function() { if (!dragging) { pos = null; showSimilar(); } });
      bar.addEventListener('keydown', function(e) {
        var step = {ArrowUp: 0.02, ArrowRight: 0.02, ArrowDown: -0.02, ArrowLeft: -0.02}[e.key];
        if (step === undefined) return;
        e.preventDefault();
        pos = Math.min(1, Math.max(0, (pos === null ? 0.5 : pos) + step));
        showSimilar();
      });
      bar.addEventListener('blur', function() { pos = null; showSimilar(); });
      sel.addEventListener('change', update);
      rng.addEventListener('input', update);

      // ---- Play: steps through the years (same as the home-page map); starts over if already at the end ----
      var timer = null;
      function stopPlay() { clearInterval(timer); timer = null; play.textContent = '\\u25B6 Play'; }
      play.addEventListener('click', function() {
        if (timer) { stopPlay(); return; }
        if (+rng.value >= d.years.length - 1) rng.value = 0;
        play.textContent = '\\u275A\\u275A Pause';
        timer = setInterval(function() {
          if (+rng.value >= d.years.length - 1) { stopPlay(); return; }
          rng.value = +rng.value + 1;
          update();
        }, 150);
      });
      rng.addEventListener('pointerdown', stopPlay);   // grabbing the slider takes over from Play

      // ---- Zoom buttons: change the map scale; update() never touches it, so zoom survives slider and dropdown ----
      var zoom = 1;
      function setZoom(z) {
        zoom = Math.min(8, Math.max(1, z));
        var u = {'geo.projection.scale': zoom};
        if (zoom === 1) { u['geo.center.lon'] = 0; u['geo.center.lat'] = 15; }   // back to the full-world view
        Plotly.relayout(el, u);
      }
      zoomIn.addEventListener('click', function() { setZoom(zoom * 1.5); });
      zoomOut.addEventListener('click', function() { setZoom(zoom / 1.5); });

      // ---- Download button: saves the map (with its title and legend) as a PNG ----
      dl.addEventListener('click', function() {
        var m = +sel.value, year = d.years[+rng.value];
        Plotly.downloadImage(el, {format: 'png', width: 1600, height: 900, scale: 2,
                                  filename: 'map_' + d.metrics[m] + '_' + year});
      });

      update();
    }
  ", data = payload)

# selfcontained = TRUE needs pandoc. If it is not installed, look for the copy that ships with Quarto
# (found through the `quarto` command on the PATH, so no fixed folder is assumed).
if (!rmarkdown::pandoc_available()) {
  quarto <- Sys.which("quarto")
  if (nzchar(quarto)) {
    quarto_dir <- dirname(normalizePath(quarto))
    found <- list.files(dirname(quarto_dir), pattern = "^pandoc(\\.exe)?$", recursive = TRUE, full.names = TRUE)
    # Quarto keeps pandoc in bin/tools/<arch>/ on some systems and bin/tools/ on others; prefer one that runs here.
    for (f in found) {
      if (system2(f, "--version", stdout = FALSE, stderr = FALSE) == 0) {
        Sys.setenv(RSTUDIO_PANDOC = dirname(f))
        break
      }
    }
  }
  if (!rmarkdown::pandoc_available()) {
    stop("saveWidget(selfcontained = TRUE) needs pandoc. Install pandoc (https://pandoc.org) or Quarto and rerun.")
  }
}

saveWidget(p_int, "charts/b_countries_now/02_choropleth.html", selfcontained = TRUE)
# saveWidget leaves its temporary support folder behind; the HTML file does not use it.
unlink("charts/b_countries_now/02_choropleth_files", recursive = TRUE)
