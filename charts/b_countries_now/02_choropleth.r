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

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = co2_mt), color = "white", linewidth = 0.1) +
  # sqrt scale so mid-sized emitters stay visible next to China and the US
  scale_fill_gradient(
    low = "#e8f1f4", high = "#0b3c4f", trans = "sqrt", na.value = "#d9d9d9",
    breaks = c(100, 1000, 5000, 10000), labels = scales::comma,
    name = "Mt CO\u2082",
    guide = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "top")
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
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
metrics <- tribble(
  ~key,            ~label,                               ~unit,                   ~cap_p,
  "co2_mt",        "Annual emissions",                   "Mt CO\u2082",           1,
  "co2_cum",       "Cumulative emissions (1850\u2013)", "Mt CO\u2082",           1,
  "co2_per_capita","Per-capita emissions",               "t CO\u2082 per person", 0.995
)
# Legend tick values per metric (the colour scale is a sqrt scale, like the static map).
tick_values <- list(
  co2_mt         = c(0, 100, 1000, 5000, 10000),
  co2_cum        = c(0, 10000, 50000, 100000, 250000),
  co2_per_capita = c(0, 1, 5, 10, 20)
)

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
payload <- list(
  years     = years,
  iso       = iso_codes,
  names     = names_by_iso,
  metrics   = metrics$key,
  labels    = metrics$label,
  units     = metrics$unit,
  defaultYear = default_year,
  # Colour range per metric is fixed across all years, so the slider is comparable over time.
  zmax      = map2_dbl(metrics$key, metrics$cap_p, \(k, p) signif(quantile(countries[[k]], p, na.rm = TRUE), 3)),
  capped    = metrics$cap_p < 1,
  tickVals  = unname(tick_values),
  tickText  = map(tick_values, \(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)) |> unname(),
  values    = setNames(map(metrics$key, make_matrix), metrics$key)
)

i0 <- which(years == default_year)
p_int <- plot_geo() |>
  add_trace(
    type = "choropleth", locations = iso_codes, z = sqrt(unlist(payload$values$co2_mt[[i0]])),
    text = names_by_iso, hoverinfo = "text",
    zmin = 0, zmax = sqrt(payload$zmax[1]), autocolorscale = FALSE,
    colorscale = list(c(0, "#e8f1f4"), c(0.25, "#8fc1d1"), c(0.6, "#1f6f8b"), c(1, "#0b3c4f")),
    marker = list(line = list(color = "white", width = 0.3)),
    colorbar = list(title = "Mt CO\u2082", len = 0.6, thickness = 12, tickmode = "array",
                    tickvals = sqrt(tick_values$co2_mt), ticktext = payload$tickText[[1]])
  ) |>
  layout(
    # Title is centred over the map (xref "paper" = the map area). The JavaScript below rewrites its text.
    # The title sits just above the map (yref "paper"). The big top margin leaves room for the controls in the
    # top-right corner; the bottom margin leaves room for the year slider under the map.
    title = list(text = "<b>CO\u2082 emissions by country</b>", x = 0.5, y = 1, xref = "paper", yref = "paper",
                 xanchor = "center", yanchor = "bottom", font = list(size = 22, color = "#1f2a37")),
    margin = list(t = 150, b = 110, l = 10, r = 10),
    geo = list(
      lataxis = list(range = c(-58, 85)),   # crop Antarctica, as in the static map
      resolution = 50,   # 50m shapes so small states are drawn (the default 110m has none of them)
      projection = list(type = "natural earth"), showframe = FALSE, showcoastlines = FALSE,
      showland = TRUE, landcolor = "#d9d9d9",      # countries without data stay neutral grey
      showcountries = FALSE, bgcolor = "white"
    ),
    paper_bgcolor = "white",
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
      // ---- Styles for the custom controls ----
      var css = document.createElement('style');
      css.textContent = `
        .ctl { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; color: #1e3a5f; }
        .ctl-top { position: absolute; top: 12px; right: 20px; z-index: 5; display: flex; flex-wrap: wrap;
                   justify-content: flex-end; align-items: flex-end; gap: 10px; max-width: calc(100% - 40px); }
        .ctl-label { display: block; font-size: 13px; font-weight: 600; margin: 0 0 5px 4px; }
        .sel-wrap { position: relative; }
        .sel-wrap::after { content: '\u25BE'; position: absolute; right: 14px; top: 50%; transform: translateY(-50%);
                           color: #2b6cb0; font-size: 18px; pointer-events: none; }
        .ctl select { appearance: none; -webkit-appearance: none; font: 600 16px system-ui, sans-serif; color: #1e3a5f;
                      padding: 10px 40px 10px 16px; min-width: 250px; height: 46px; background: #f2f7fd;
                      border: 2px solid #2b6cb0; border-radius: 14px; box-shadow: 0 2px 6px rgba(43, 108, 176, 0.18);
                      cursor: pointer; transition: box-shadow .15s, background .15s; }
        .ctl select:hover { background: #e6f0fb; box-shadow: 0 3px 10px rgba(43, 108, 176, 0.3); }
        .ctl select:focus-visible, .ctl button:focus-visible, .ctl input:focus-visible { outline: 3px solid #93c5fd; outline-offset: 2px; }
        .ctl button { font: 700 15px system-ui, sans-serif; height: 46px; min-width: 46px; padding: 0 16px; color: #fff;
                      background: #2b6cb0; border: 0; border-radius: 12px; cursor: pointer;
                      box-shadow: 0 2px 6px rgba(43, 108, 176, 0.35); transition: background .15s, transform .05s; }
        .ctl button:hover { background: #1e4e8c; }
        .ctl button:active { transform: translateY(1px); }
        .ctl .zoom { font-size: 24px; line-height: 1; padding: 0; width: 46px; }
        .ctl-bottom { position: absolute; left: 40px; right: 40px; bottom: 38px; z-index: 5; display: flex;
                      align-items: center; gap: 14px; }
        .yr { min-width: 112px; text-align: center; font-size: 20px; font-weight: 700; color: #fff; background: #2b6cb0;
              border-radius: 12px; padding: 7px 12px; box-shadow: 0 2px 6px rgba(43, 108, 176, 0.35); }
        .ends { font-size: 13px; font-weight: 600; color: #5b7390; }
        .ctl input[type=range] { -webkit-appearance: none; appearance: none; flex: 1; height: 10px; border-radius: 6px;
                                 background: #cbd5e1; cursor: pointer; margin: 0; }
        .ctl input[type=range]::-webkit-slider-thumb { -webkit-appearance: none; width: 28px; height: 28px; border-radius: 50%;
                                 background: #fff; border: 5px solid #2b6cb0; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3); }
        .ctl input[type=range]::-moz-range-thumb { width: 18px; height: 18px; border-radius: 50%; background: #fff;
                                 border: 5px solid #2b6cb0; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3); }
      `;
      document.head.appendChild(css);
      el.style.position = 'relative';

      function make(tag, cls, text) {
        var n = document.createElement(tag);
        if (cls) n.className = cls;
        if (text) n.textContent = text;
        return n;
      }

      // ---- Top right: metric dropdown, zoom buttons, download button ----
      var top = make('div', 'ctl ctl-top');
      var selBox = make('div');
      var selLabel = make('label', 'ctl-label', 'Select what you want to view:');
      var wrap = make('div', 'sel-wrap');
      var sel = make('select');
      selLabel.htmlFor = sel.id = 'metric-select';
      d.labels.forEach(function(l, i) { var o = document.createElement('option'); o.value = i; o.text = l; sel.appendChild(o); });
      wrap.appendChild(sel); selBox.appendChild(selLabel); selBox.appendChild(wrap);
      var zoomOut = make('button', 'zoom', '\u2212'); zoomOut.title = zoomOut.ariaLabel = 'Zoom out';
      var zoomIn  = make('button', 'zoom', '+');        zoomIn.title  = zoomIn.ariaLabel  = 'Zoom in';
      var dl = make('button', '', '\u2193  Download PNG'); dl.title = 'Download the map as a PNG image';
      [selBox, zoomOut, zoomIn, dl].forEach(function(n) { top.appendChild(n); });
      el.appendChild(top);

      // ---- Bottom: year slider under the map ----
      var bottom = make('div', 'ctl ctl-bottom');
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
        lab.textContent = 'Year: ' + year;
        var pct = yi / (d.years.length - 1) * 100;   // filled part of the slider track
        rng.style.background = 'linear-gradient(to right, #2b6cb0 ' + pct + '%, #cbd5e1 ' + pct + '%)';
        // Per-capita is capped (see cap_p in R): values above zmax get the darkest colour, so say so in the legend title.
        var cbTitle = d.units[m] + (d.capped[m] ? '<br>(scale capped at ' + d.zmax[m] + ')' : '');
        Plotly.restyle(el, {z: [z], text: [txt], zmax: [Math.sqrt(d.zmax[m])], 'colorbar.title.text': cbTitle,
                            'colorbar.tickvals': [d.tickVals[m].map(Math.sqrt)], 'colorbar.ticktext': [d.tickText[m]]}, [0]);
        Plotly.relayout(el, {'title.text': '<b>' + d.labels[m] + ' by country, ' + year + '</b>'});
      }
      sel.addEventListener('change', update);
      rng.addEventListener('input', update);

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
