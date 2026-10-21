# Top X emitters vs the rest of the world.
#
# Vertical bar chart of share of the world total, with annual and cumulative versions. Rest = world minus the top X. X is fixed at 10 for the static version.
# Data: data/emissions_country_year.csv, data/country_groups.csv (drop aggregates)
# Run from the project root:  Rscript charts/b_countries_now/01_top_x_vs_rest.r
# Saves: charts/b_countries_now/01_top_x_vs_rest.png (static) and charts/b_countries_now/01_top_x_vs_rest.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         The selector for X and the pick-your-countries input need Quarto OJS; plotly only handles the hover and the annual/cumulative dropdown.

library(tidyverse)
library(cowplot)
library(plotly)
library(htmlwidgets)

# Paths are relative to the project root, so fail early with a clear message if run from somewhere else.
if (!file.exists("data/emissions_country_year.csv")) {
  stop("Run this script from the project root: Rscript charts/b_countries_now/01_top_x_vs_rest.r")
}

# ---- Step 1: static ggplot ----

# Read the data. Columns used: iso_code, country, year, co2_mt (annual), co2_cum (cumulative since 1850).
emissions <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
groups    <- read_csv("data/country_groups.csv", show_col_types = FALSE)

# Keep individual countries only: drop World, continents/regions and income groups.
country_codes <- groups |> filter(!is_aggregate) |> pull(iso_code)
countries <- emissions |> filter(iso_code %in% country_codes)

# Most recent year in the data.
latest_year <- max(countries$year)

# The world total is the sum of the countries (as the data README describes).
# Summing only countries means nothing is double-counted.
top_n <- 10

# One function builds the data for either chart:
#   value_col = the column to rank and share (annual: co2_mt, cumulative: co2_cum)
# The top 10 is chosen separately for each chart, then "Rest of World" = world - top 10.
make_shares <- function(value_col) {
  latest <- countries |>
    filter(year == latest_year) |>
    transmute(country, value = .data[[value_col]]) |>
    filter(!is.na(value))

  world_total <- sum(latest$value)

  top <- latest |>
    slice_max(value, n = top_n) |>
    mutate(share = value / world_total * 100)

  rest <- tibble(
    country = "Rest of World",
    value   = world_total - sum(top$value),
    share   = value / world_total * 100
  )

  bind_rows(top, rest) |>
    mutate(country = factor(country, levels = country))  # keep order: biggest first, Rest last
}

# Cumulative emissions are already a running total, so the latest year's value is the 1850-to-now total.
annual     <- make_shares("co2_mt")
cumulative <- make_shares("co2_cum")

# Check: each chart should add up to 100%.
stopifnot(abs(sum(annual$share) - 100) < 0.01, abs(sum(cumulative$share) - 100) < 0.01)

# One colour for countries, grey for Rest of World.
bar_colors <- c("#1f6f8b", "#9a9a9a")

# One function draws either chart so both look identical.
make_chart <- function(df, title, subtitle) {
  df |>
    mutate(group = if_else(country == "Rest of World", "rest", "top")) |>
    ggplot(aes(x = country, y = share, fill = group)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f%%", share)), vjust = -0.4, size = 3) +
    scale_fill_manual(values = c(top = bar_colors[1], rest = bar_colors[2])) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Share of world CO\u2082 (%)") +
    theme_minimal_hgrid(font_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title.position = "plot"
    )
}

p_annual <- make_chart(annual,
  paste0("Annual CO\u2082 emissions, ", latest_year),
  "Top 10 emitters vs the rest of the world")
p_cumulative <- make_chart(cumulative,
  paste0("Cumulative CO\u2082 emissions, 1850\u2013", latest_year),
  "Top 10 emitters vs the rest of the world")

# Put the two charts side by side.
final <- plot_grid(p_annual, p_cumulative, ncol = 2, align = "h")

ggsave("charts/b_countries_now/01_top_x_vs_rest.png", final, width = 8, height = 5, bg = "white")

# ---- Step 2: interactive plotly ----
# Same two charts (annual and cumulative, top 10 vs Rest of World), one at a time, chosen from a dropdown.
# Hover shows the country, its share and its emissions in Mt CO2. The data above is reused as is.
# A little JavaScript (onRender) adds the styled dropdown and the Download PNG button, the same look as 02_choropleth.r.

# One bar trace per chart. Bar colours match the static chart: teal for countries, grey for Rest of World.
add_bars <- function(p, df, visible) {
  p |> add_trace(
    type = "bar", x = as.character(df$country), y = df$share, customdata = df$value,
    text = sprintf("%.1f%%", df$share), textposition = "outside", cliponaxis = FALSE,
    marker = list(color = if_else(df$country == "Rest of World", bar_colors[2], bar_colors[1])),
    hovertemplate = "<b>%{x}</b><br>%{y:.1f}% of world CO\u2082<br>%{customdata:,.0f} Mt CO\u2082<extra></extra>",
    visible = visible, showlegend = FALSE
  )
}

payload <- list(
  labels = c("Annual emissions", "Cumulative emissions (1850\u2013)"),
  titles = c(paste0("Annual CO\u2082 emissions, ", latest_year),
             paste0("Cumulative CO\u2082 emissions, 1850\u2013", latest_year)),
  cats   = list(as.character(annual$country), as.character(cumulative$country)),
  ymax   = c(max(annual$share), max(cumulative$share)) * 1.15   # headroom for the % labels
)

p_int <- plot_ly() |>
  add_bars(annual, TRUE) |>
  add_bars(cumulative, FALSE) |>
  layout(
    title = list(text = paste0("<b>", payload$titles[1], "</b>"), x = 0.5, y = 1, xref = "paper", yref = "paper",
                 xanchor = "center", yanchor = "bottom", font = list(size = 22, color = "#1f2a37")),
    margin = list(t = 150, b = 130, l = 70, r = 30),
    # The source note is the x-axis title, so plotly places it below the country labels.
    xaxis = list(title = list(text = "Top 10 emitters vs the rest of the world. Source: PRIMAP-hist v2.2 (to 2018), OWID / Global Carbon Budget (2019\u20132024).",
                              font = list(size = 11, color = "#666666"), standoff = 10),
                 categoryorder = "array", categoryarray = payload$cats[[1]], tickangle = -35,
                 tickfont = list(size = 13)),
    yaxis = list(title = "Share of world CO\u2082 (%)", range = c(0, payload$ymax[1]), gridcolor = "#e5e7eb",
                 zeroline = FALSE, ticksuffix = "%"),
    plot_bgcolor = "white", paper_bgcolor = "white", hoverlabel = list(font = list(size = 14))
  ) |>
  config(displayModeBar = FALSE) |>
  onRender("
    function(el, x, d) {
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
        .ctl select:focus-visible, .ctl button:focus-visible { outline: 3px solid #93c5fd; outline-offset: 2px; }
        .ctl button { font: 700 15px system-ui, sans-serif; height: 46px; padding: 0 16px; color: #fff;
                      background: #2b6cb0; border: 0; border-radius: 12px; cursor: pointer;
                      box-shadow: 0 2px 6px rgba(43, 108, 176, 0.35); transition: background .15s, transform .05s; }
        .ctl button:hover { background: #1e4e8c; }
        .ctl button:active { transform: translateY(1px); }
      `;
      document.head.appendChild(css);
      el.style.position = 'relative';

      function make(tag, cls, text) {
        var n = document.createElement(tag);
        if (cls) n.className = cls;
        if (text) n.textContent = text;
        return n;
      }

      // Top right: chart dropdown and download button.
      var top = make('div', 'ctl ctl-top');
      var box = make('div');
      var label = make('label', 'ctl-label', 'Select what you want to view:');
      var wrap = make('div', 'sel-wrap');
      var sel = make('select');
      label.htmlFor = sel.id = 'chart-select';
      d.labels.forEach(function(l, i) { var o = document.createElement('option'); o.value = i; o.text = l; sel.appendChild(o); });
      wrap.appendChild(sel); box.appendChild(label); box.appendChild(wrap);
      var dl = make('button', '', '\u2193  Download PNG'); dl.title = 'Download the chart as a PNG image';
      top.appendChild(box); top.appendChild(dl);
      el.appendChild(top);

      // Show one trace, and match the title, bar order and y range to it.
      sel.addEventListener('change', function() {
        var i = +sel.value;
        Plotly.restyle(el, {visible: [i === 0, i === 1]});
        Plotly.relayout(el, {'title.text': '<b>' + d.titles[i] + '</b>', 'xaxis.categoryarray': d.cats[i],
                             'yaxis.range': [0, d.ymax[i]]});
      });

      dl.addEventListener('click', function() {
        var i = +sel.value;
        Plotly.downloadImage(el, {format: 'png', width: 1600, height: 900, scale: 2,
                                  filename: 'top10_vs_rest_' + (i === 0 ? 'annual' : 'cumulative')});
      });
    }
  ", data = payload)

# selfcontained = TRUE needs pandoc. If it is not installed, look for the copy that ships with Quarto
# (found through the `quarto` command on the PATH, so no fixed folder is assumed).
if (!rmarkdown::pandoc_available()) {
  quarto <- Sys.which("quarto")
  if (nzchar(quarto)) {
    quarto_dir <- dirname(normalizePath(quarto))
    found <- list.files(dirname(quarto_dir), pattern = "^pandoc(\\.exe)?$", recursive = TRUE, full.names = TRUE)
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

saveWidget(p_int, "charts/b_countries_now/01_top_x_vs_rest.html", selfcontained = TRUE)
# saveWidget leaves its temporary support folder behind; the HTML file does not use it.
unlink("charts/b_countries_now/01_top_x_vs_rest_files", recursive = TRUE)
