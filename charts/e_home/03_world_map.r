# World map of CO2 or CH4 emissions by country, 1850-2024 (echarts4r version for the home page).
#
# Choropleth with a gas choice (CO2 / CH4), a measure choice (annual / per person) and a year slider with a play button.
# CH4 is in CO2-equivalent (GWP100 = 27.9), so both gases are on the warming scale.
# The plotly map on the Explore page (charts/b_countries_now/02_choropleth.r) stays as it is; geometry and ISO fixes match it.
# Data: data/emissions_country_year.csv, data/country_groups.csv, data/raw/natural_earth/ne_50m_admin_0_countries.geojson
# Run from the project root:  Rscript charts/e_home/03_world_map.r
# Saves: charts/e_home/03_world_map.png (static) and charts/e_home/03_world_map.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style): CH4 in 2024 (the CO2 version already exists as 02_choropleth.png).
# Step 2: interactive echarts4r map. The world shapes are simplified to keep the HTML small, and the map is drawn
#         from those shapes inside the file, so it works offline (unlike the plotly version).
#         One update() reads all three controls every time, so changing one never resets the others.

library(tidyverse)
library(cowplot)
library(sf)
library(echarts4r)
source("charts/a_generations/echarts_helpers.r")   # save_echart()

if (!file.exists("data/emissions_country_year.csv")) {
  stop("Run this script from the project root: Rscript charts/e_home/03_world_map.r")
}

OUT_PNG  <- "charts/e_home/03_world_map.png"
OUT_HTML <- "charts/e_home/03_world_map.html"
default_year <- 2024
caption <- "Data: PRIMAP-hist v2.2 (to 2018), OWID / Global Carbon Budget and World Bank / EDGAR (2019-2024). Excludes land use. CH4 at GWP100 = 27.9. Boundaries: Natural Earth. Grey = no data."

# ---- Data (shared by both steps) ----
emissions <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
groups    <- read_csv("data/country_groups.csv", show_col_types = FALSE)

# Individual countries only (drops World, regions, income groups), so nothing is double-counted.
country_codes <- groups |> filter(!is_aggregate) |> pull(iso_code)
countries <- emissions |> filter(iso_code %in% country_codes)

# Same geometry and ISO fixes as charts/b_countries_now/02_choropleth.r (see the notes there):
# ISO_A3_EH fills France and Norway, SDS -> SSD, drop Antarctica, unnamed shapes and the two small "AUS" territories.
sf_use_s2(FALSE)   # simplify in degrees, keeping shared borders intact
world <- st_read("data/raw/natural_earth/ne_50m_admin_0_countries.geojson", quiet = TRUE) |>
  mutate(iso_code = if_else(ISO_A3_EH == "SDS", "SSD", ISO_A3_EH)) |>
  filter(!iso_code %in% c("-99", "ATA"),
         !ADM0_A3 %in% c("IOA", "ATC")) |>
  select(iso_code, name = NAME, geometry)
stopifnot(!anyDuplicated(world$iso_code))

unmatched <- countries |> filter(year == default_year, !iso_code %in% world$iso_code)
message("Countries with data but no shape on the map: ", nrow(unmatched))

# ---- Step 1: static ggplot ----
map_data <- world |> left_join(filter(countries, year == default_year), by = "iso_code")
stopifnot(nrow(map_data) == nrow(world))   # the join must not add or repeat rows

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = ch4_mtco2e), color = "white", linewidth = 0.1) +
  # sqrt scale so mid-sized emitters stay visible next to the largest
  scale_fill_gradient(
    low = "#e8f1f4", high = "#0b3c4f", trans = "sqrt", na.value = "#d9d9d9",
    breaks = c(10, 100, 500, 1000), labels = scales::comma,
    name = "Mt CO2e",
    guide = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "top")
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  labs(
    title    = paste0("Annual methane (CH4) emissions by country, ", default_year),
    subtitle = "Million tonnes of CO2-equivalent per year. Grey = no data.",
    caption  = str_wrap(caption, 140)
  ) +
  theme_map(font_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.caption = element_text(size = 7, color = "grey40", hjust = 0),
    legend.position = "bottom",
    legend.justification = "left",
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(OUT_PNG, p_map, width = 8, height = 5, bg = "white")

# ---- Step 2: interactive echarts4r ----
# Simplified shapes (about 0.05 degrees, ~5 km): small states keep a shape, the file stays small.
# st_simplify warns that lon/lat tolerances are in degrees; that is what we want here, so the warning is muted.
world_simple <- world |>
  st_simplify(dTolerance = 0.05, preserveTopology = TRUE) |>
  suppressWarnings() |>
  filter(!st_is_empty(geometry))
geo_file <- tempfile(fileext = ".geojson")
# 2 decimals (~1 km) is plenty at world scale and roughly halves the file
st_write(world_simple |> select(iso_code), geo_file, quiet = TRUE, layer_options = "COORDINATE_PRECISION=2")
geo_json <- jsonlite::read_json(geo_file)

# Four views: gas x measure. cap_p: the color scale tops out at this quantile; per person has a few
# extreme outliers, so it is capped at the 99.5th percentile (those countries get the darkest color).
metrics <- tribble(
  ~key,             ~gas,  ~measure, ~label,                ~unit,                  ~cap_p,
  "co2_mt",         "CO2", "annual", "Annual CO2 emissions",      "Mt CO2 per year",      1,
  "ch4_mtco2e",     "CH4", "annual", "Annual CH4 emissions",      "Mt CO2e per year",     1,
  "co2_per_capita", "CO2", "capita", "CO2 emissions per person",  "t CO2 per person",     0.995,
  "ch4_per_capita", "CH4", "capita", "CH4 emissions per person",  "t CO2e per person",    0.995
)

years     <- sort(unique(countries$year))
iso_codes <- sort(intersect(unique(countries$iso_code), world_simple$iso_code))

# One list per metric: for each year, the values in iso_codes order. Missing stays NA (grey), never 0.
make_matrix <- function(col) {
  countries |>
    filter(iso_code %in% iso_codes) |>
    select(iso_code, year, value = all_of(col)) |>
    complete(year = years, iso_code = iso_codes) |>
    arrange(year, iso_code) |>
    pull(value) |>
    matrix(nrow = length(years), ncol = length(iso_codes), byrow = TRUE) |>
    signif(3) |>
    (\(m) lapply(seq_len(nrow(m)), \(i) m[i, ]))()
}

payload <- list(
  years       = years,
  iso         = iso_codes,
  names       = as.list(setNames(world_simple$name, world_simple$iso_code)),   # also covers shapes with no data
  keys        = metrics$key,
  gas         = metrics$gas,
  measure     = metrics$measure,
  labels      = metrics$label,
  units       = metrics$unit,
  capped      = metrics$cap_p < 1,
  # Color range per metric is fixed across all years, so moving the slider compares like with like.
  zmax        = map2_dbl(metrics$key, metrics$cap_p, \(k, p) signif(quantile(countries[[k]], p, na.rm = TRUE), 2)),
  defaultYear = default_year,
  values      = setNames(map(metrics$key, make_matrix), metrics$key)
)

# Start from an empty map series; update() in the browser fills in data, title and color range.
p <- tibble(iso_code = iso_codes[1], value = 0) |>
  e_charts(iso_code) |>
  e_map_register("world", geo_json) |>
  e_map(value, map = "world") |>
  e_tooltip(trigger = "item", formatter = JS("
    function(p) {
      var d = p.data, name = (window.mapNames && window.mapNames[p.name]) || p.name;
      if (!d || d.raw === undefined) return '<b>' + name + '</b><br>No data';
      return '<b>' + name + '</b><br>' + d.year + '<br>' + d.text;
    }")) |>
  e_title("", left = 0, textStyle = list(fontSize = 17)) |>
  e_legend(show = FALSE)

p$x$opts$series[[1]] <- modifyList(p$x$opts$series[[1]], list(
  nameProperty = "iso_code",
  showLegendSymbol = FALSE,            # otherwise echarts puts a dot on every country
  roam = "move",                       # drag to pan; zoom with the buttons, so the page still scrolls
  top = 120, bottom = 110, left = 10, right = 10,
  itemStyle = list(areaColor = "#d9d9d9", borderColor = "#ffffff", borderWidth = 0.3),
  emphasis = list(label = list(show = FALSE), itemStyle = list(areaColor = "#efb306")),
  select = list(disabled = TRUE),
  label = list(show = FALSE)
))
p$x$opts$visualMap <- list(list(
  type = "continuous", min = 0, max = 1, calculable = FALSE, orient = "horizontal",
  left = 10, bottom = 34, itemWidth = 12, itemHeight = 150,
  inRange = list(color = c("#e8f1f4", "#8fc1d1", "#1f6f8b", "#0b3c4f")),
  textStyle = list(color = "#444", fontSize = 12)
))
# The caption wraps to the chart width (set in the browser, see fitCaption below).
p$x$opts$graphic <- list(list(id = "caption", type = "text", left = 0, bottom = 2,
                              style = list(text = caption, fill = "#888", fontSize = 10, overflow = "break")))

p <- p |>
  onRender("
    function(el, x, d) {
      var chart = echarts.getInstanceByDom(el);
      window.mapNames = d.names;

      // ---- Styles for the controls (same look as the plotly map) ----
      var css = document.createElement('style');
      css.textContent = `
        .ctl { font-family: Inter, system-ui, sans-serif; color: #1e3a5f; }
        .ctl-top { position: absolute; top: 40px; right: 10px; z-index: 5; display: flex; flex-wrap: wrap;
                   justify-content: flex-end; align-items: flex-end; gap: 10px; max-width: calc(100% - 20px); }
        .ctl-label { display: block; font-size: 13px; font-weight: 600; margin: 0 0 5px 4px; }
        .sel-wrap { position: relative; }
        .sel-wrap::after { content: '\\u25BE'; position: absolute; right: 14px; top: 50%; transform: translateY(-50%);
                           color: #2b6cb0; font-size: 18px; pointer-events: none; }
        .ctl select { appearance: none; -webkit-appearance: none; font: 600 15px Inter, system-ui, sans-serif; color: #1e3a5f;
                      padding: 8px 38px 8px 14px; min-width: 150px; height: 42px; background: #f2f7fd;
                      border: 2px solid #2b6cb0; border-radius: 12px; cursor: pointer; }
        .ctl select:hover { background: #e6f0fb; }
        .ctl select:focus-visible, .ctl button:focus-visible, .ctl input:focus-visible { outline: 3px solid #93c5fd; outline-offset: 2px; }
        .ctl button { font: 700 15px Inter, system-ui, sans-serif; height: 42px; min-width: 42px; padding: 0 14px; color: #fff;
                      background: #2b6cb0; border: 0; border-radius: 12px; cursor: pointer;
                      box-shadow: 0 2px 6px rgba(43, 108, 176, 0.35); transition: background .15s; }
        .ctl button:hover { background: #1e4e8c; }
        .ctl .zoom { font-size: 22px; line-height: 1; padding: 0; width: 42px; }
        .ctl-bottom { position: absolute; left: 260px; right: 20px; bottom: 34px; z-index: 5; display: flex;
                      align-items: center; gap: 12px; }
        .yr { min-width: 64px; text-align: center; font-size: 18px; font-weight: 700; color: #fff; background: #2b6cb0;
              border-radius: 12px; padding: 6px 10px; }
        .ends { font-size: 13px; font-weight: 600; color: #5b7390; }
        .ctl input[type=range] { -webkit-appearance: none; appearance: none; flex: 1; min-width: 80px; height: 10px;
                                 border-radius: 6px; background: #cbd5e1; cursor: pointer; margin: 0; }
        .ctl input[type=range]::-webkit-slider-thumb { -webkit-appearance: none; width: 24px; height: 24px; border-radius: 50%;
                                 background: #fff; border: 5px solid #2b6cb0; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3); }
        .ctl input[type=range]::-moz-range-thumb { width: 16px; height: 16px; border-radius: 50%; background: #fff;
                                 border: 5px solid #2b6cb0; }
        @media (max-width: 640px) { .ctl-bottom { left: 10px; bottom: 80px; } }
      `;
      document.head.appendChild(css);
      el.style.position = 'relative';

      function make(tag, cls, text) {
        var n = document.createElement(tag);
        if (cls) n.className = cls;
        if (text) n.textContent = text;
        return n;
      }
      function select(id, label, options) {
        var box = make('div'), lab = make('label', 'ctl-label', label), wrap = make('div', 'sel-wrap'), s = make('select');
        lab.htmlFor = s.id = id;
        options.forEach(function(o) { var op = document.createElement('option'); op.value = o[0]; op.text = o[1]; s.appendChild(op); });
        wrap.appendChild(s); box.appendChild(lab); box.appendChild(wrap);
        return [box, s];
      }

      // ---- Top right: gas and measure selects, zoom, download ----
      var top = make('div', 'ctl ctl-top');
      var g = select('map-gas', 'Gas', [['CO2', 'CO2'], ['CH4', 'CH4 (methane)']]);
      var m = select('map-measure', 'Measure', [['annual', 'Total per year'], ['capita', 'Per person']]);
      var zoomOut = make('button', 'zoom', '\\u2212'); zoomOut.title = 'Zoom out'; zoomOut.setAttribute('aria-label', 'Zoom out');
      var zoomIn  = make('button', 'zoom', '+');        zoomIn.title  = 'Zoom in';  zoomIn.setAttribute('aria-label', 'Zoom in');
      var dl = make('button', '', '\\u2193 PNG'); dl.title = 'Download the map as a PNG image';
      [g[0], m[0], zoomOut, zoomIn, dl].forEach(function(n) { top.appendChild(n); });
      el.appendChild(top);

      // ---- Bottom: play button and year slider ----
      var bottom = make('div', 'ctl ctl-bottom');
      var play = make('button', '', '\\u25B6 Play'); play.setAttribute('aria-label', 'Play through the years');
      var lab = make('div', 'yr');
      var rng = make('input');
      rng.type = 'range'; rng.min = 0; rng.max = d.years.length - 1; rng.step = 1;
      rng.value = d.years.indexOf(d.defaultYear); rng.setAttribute('aria-label', 'Year');
      [play, lab, make('span', 'ends', d.years[0]), rng, make('span', 'ends', d.years[d.years.length - 1])]
        .forEach(function(n) { bottom.appendChild(n); });
      el.appendChild(bottom);

      function fmt(v) {
        return v >= 100 ? Math.round(v).toLocaleString() : v.toLocaleString(undefined, {maximumFractionDigits: v < 1 ? 2 : 1});
      }

      // One update function reads all three controls every time.
      function update() {
        var k = -1;
        for (var i = 0; i < d.keys.length; i++) if (d.gas[i] === g[1].value && d.measure[i] === m[1].value) k = i;
        var yi = +rng.value, year = d.years[yi], row = d.values[d.keys[k]][yi], data = [];
        for (var j = 0; j < d.iso.length; j++) {
          var v = row[j];
          if (typeof v !== 'number') continue;   // no data: drawn grey, tooltip says so
          // sqrt scale so mid-sized emitters stay visible next to the largest
          data.push({name: d.iso[j], value: Math.sqrt(Math.max(v, 0)), raw: v, year: year, text: fmt(v) + ' ' + d.units[k]});
        }
        lab.textContent = year;
        var pct = yi / (d.years.length - 1) * 100;
        rng.style.background = 'linear-gradient(to right, #2b6cb0 ' + pct + '%, #cbd5e1 ' + pct + '%)';
        var unitText = d.units[k] + (d.capped[k] ? ' (scale capped at ' + fmt(d.zmax[k]) + ')' : '');
        chart.setOption({
          title: [{text: d.labels[k] + ' by country, ' + year, subtext: unitText}],
          series: [{data: data}],
          // labels at both ends of the color bar, in real units (the bar itself is on the sqrt scale)
          visualMap: [{max: Math.sqrt(d.zmax[k]), text: [fmt(d.zmax[k]) + (d.capped[k] ? '+' : ''), '0']}]
        });
      }
      g[1].addEventListener('change', update);
      m[1].addEventListener('change', update);
      rng.addEventListener('input', update);

      // ---- Play: steps through the years; starts over if already at the end ----
      var timer = null;
      function stop() { clearInterval(timer); timer = null; play.textContent = '\\u25B6 Play'; }
      play.addEventListener('click', function() {
        if (timer) { stop(); return; }
        if (+rng.value >= d.years.length - 1) rng.value = 0;
        play.textContent = '\\u275A\\u275A Pause';
        timer = setInterval(function() {
          if (+rng.value >= d.years.length - 1) { stop(); return; }
          rng.value = +rng.value + 1;
          update();
        }, 120);
      });
      rng.addEventListener('pointerdown', stop);

      // ---- Zoom buttons: change the map zoom; update() never touches it ----
      var zoom = 1;
      function setZoom(z) {
        zoom = Math.min(8, Math.max(1, z));
        var opt = {zoom: zoom};
        if (zoom === 1) opt.center = null;   // back to the full-world view
        chart.setOption({series: [opt]});
      }
      zoomIn.addEventListener('click', function() { setZoom(zoom * 1.5); });
      zoomOut.addEventListener('click', function() { setZoom(zoom / 1.5); });

      dl.addEventListener('click', function() {
        var a = document.createElement('a');
        a.href = chart.getDataURL({type: 'png', pixelRatio: 2, backgroundColor: '#fff'});
        a.download = 'map_' + g[1].value + '_' + m[1].value + '_' + d.years[+rng.value] + '.png';
        a.click();
      });

      function fitCaption() { chart.setOption({graphic: [{id: 'caption', style: {width: el.clientWidth - 10}}]}); }
      window.addEventListener('resize', fitCaption);
      fitCaption();
      update();
    }
  ", data = payload)

save_echart(p, OUT_HTML)
message("HTML size: ", round(file.size(OUT_HTML) / 1e6, 1), " MB")
