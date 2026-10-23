# Shared helpers for the interactive (echarts4r) versions of the generations charts.
#
# Sourced by 01, 02, 03 and 04:  source("charts/a_generations/echarts_helpers.r")
#   save_echart()    saves a widget as standalone HTML
#   add_buttons()    adds zoom in, zoom out and Download PNG buttons (same look as the choropleth's)
#   lifetime_chart() line chart of CO2 and CH4 with dashed event lines (charts 01 and 04)

library(echarts4r)
library(htmlwidgets)

# Hover tooltips on event lines show the event's description, looked up by its label.
.lifetime_tooltip <- function(events) {
  lookup <- jsonlite::toJSON(as.list(setNames(events$description, events$label)), auto_unbox = TRUE)
  JS(paste0("function(p) { var d = ", lookup, "; return '<b>' + p.name + '</b><br>' + (d[p.name] || ''); }"))
}

# selfcontained = TRUE needs pandoc; use the copy bundled with Quarto if none is on the PATH
save_echart <- function(widget, path) {
  if (!rmarkdown::pandoc_available()) {
    Sys.setenv(RSTUDIO_PANDOC = file.path(dirname(Sys.which("quarto")), "tools"))
  }
  saveWidget(widget, file.path(getwd(), path), selfcontained = TRUE)
  unlink(sub("[.]html$", "_files", path), recursive = TRUE)   # saveWidget leaves this behind on Windows
}

# Zoom works on the first x axis (the dataZoom component added by lifetime_chart); download saves the chart as a PNG.
add_buttons <- function(widget, filename) {
  onRender(widget, paste0("
    function(el) {
      var chart = echarts.getInstanceByDom(el);
      var css = document.createElement('style');
      css.textContent = `
        .ctl { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; }
        .ctl-top { position: absolute; top: 12px; right: 20px; z-index: 5; display: flex; flex-wrap: wrap;
                   justify-content: flex-end; align-items: flex-end; gap: 10px; max-width: calc(100% - 40px); }
        .ctl button { font: 700 15px system-ui, sans-serif; height: 46px; min-width: 46px; padding: 0 16px; color: #fff;
                      background: #2b6cb0; border: 0; border-radius: 12px; cursor: pointer;
                      box-shadow: 0 2px 6px rgba(43, 108, 176, 0.35); transition: background .15s, transform .05s; }
        .ctl button:hover { background: #1e4e8c; }
        .ctl button:active { transform: translateY(1px); }
        .ctl button:focus-visible { outline: 3px solid #93c5fd; outline-offset: 2px; }
        .ctl .zoom { font-size: 24px; line-height: 1; padding: 0; width: 46px; }
      `;
      document.head.appendChild(css);
      el.style.position = 'relative';

      function make(cls, text, title) {
        var b = document.createElement('button');
        if (cls) b.className = cls;
        b.textContent = text; b.title = title; b.setAttribute('aria-label', title);
        return b;
      }
      var top = document.createElement('div');
      top.className = 'ctl ctl-top';
      var zoomOut = make('zoom', '\\u2212', 'Zoom out');
      var zoomIn  = make('zoom', '+', 'Zoom in');
      var dl      = make('', '\\u2193  Download PNG', 'Download the chart as a PNG image');
      [zoomOut, zoomIn, dl].forEach(function(n) { top.appendChild(n); });
      el.appendChild(top);

      // Zoom narrows the year axis around its centre; zooming back out stops at the full range.
      function setZoom(f) {
        var z = chart.getOption().dataZoom[0], mid = (z.start + z.end) / 2;
        var half = Math.min(100, (z.end - z.start) * f) / 2;
        var lo = Math.max(0, mid - half), hi = Math.min(100, mid + half);
        chart.dispatchAction({type: 'dataZoom', start: lo, end: hi});
      }
      zoomIn.addEventListener('click', function() { setZoom(1 / 1.5); });
      zoomOut.addEventListener('click', function() { setZoom(1.5); });

      dl.addEventListener('click', function() {
        var a = document.createElement('a');
        a.href = chart.getDataURL({type: 'png', pixelRatio: 2, backgroundColor: '#fff'});
        a.download = '", filename, ".png';
        a.click();
      });
    }
  "))
}

# d: year, CO2, CH4 (Gt CO2e per year). events: year, label, description, plus gt_year when dots = TRUE.
# Lines are labeled at their ends (no legend); hovering an event line shows its description.
# ytop sets a fixed y maximum so charts can be compared; NULL lets the axis fit the data.
lifetime_chart <- function(d, events, title, gas_colors, caption, ytop = NULL, dots = FALSE) {
  d <- mutate(d, across(c(CO2, CH4), \(x) round(x, 3)))
  ev <- pmap(events, \(year, label, description, ...) list(xAxis = year, name = label))

  e <- d |>
    e_charts(year) |>
    e_line(CO2, symbol = "none", lineStyle = list(width = 3), endLabel = list(show = TRUE, formatter = "{a}", fontSize = 14)) |>
    e_line(CH4, symbol = "none", lineStyle = list(width = 3), endLabel = list(show = TRUE, formatter = "{a}", fontSize = 14)) |>
    e_color(unname(gas_colors)) |>
    e_x_axis(type = "value", min = min(d$year), max = max(d$year), splitLine = list(show = FALSE),
             axisLabel = list(formatter = "{value}")) |>
    e_y_axis(name = "Annual emissions (Gt CO2e per year)", nameLocation = "middle", nameGap = 45,
             min = 0, max = ytop) |>
    e_datazoom(type = "inside", x_index = 0, toolbox = FALSE, zoomOnMouseWheel = FALSE) |>
    e_tooltip(trigger = "axis") |>
    e_legend(show = FALSE) |>
    e_toolbox(show = FALSE) |>
    e_title(title, left = 0) |>
    e_grid(top = 80, right = 60, bottom = 60, left = 70)

  # e_mark_line() and e_mark_point() nest the data one level too deep, so the marks are set on the CO2 series directly.
  e$x$opts$series[[1]]$markLine <- list(
    data = ev, symbol = "none", lineStyle = list(type = "dashed", color = "#999"),
    label = list(formatter = "{b}", position = "insideEndTop", rotate = 90, color = "#666"),
    tooltip = list(formatter = .lifetime_tooltip(events)))
  if (dots) {
    e$x$opts$series[[1]]$markPoint <- list(
      data = pmap(events, \(year, gt_year, ...) list(coord = c(year, round(gt_year, 3)))),
      symbolSize = 9, label = list(show = FALSE), itemStyle = list(color = gas_colors[["CO2"]]))
  }
  e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6,
                           style = list(text = caption, fill = "#888", fontSize = 11))
  e
}
