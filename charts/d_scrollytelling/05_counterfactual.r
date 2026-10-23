# What if emissions had stopped growing when you were born? (scrollytelling, the counterfactual step)
#
# Takes global emissions in the birth year and holds them flat (or lets them change by a constant percentage each year, chosen
# by the visitor), then compares that made-up path with the real one. The shaded gap between the two lines, summed over the
# lifetime, is "how much less (or more) the world would have emitted". The gap is also expressed in years of today's emissions.
# This is an illustration of the size of the change, not a forecast or a claim about what would have been possible:
# real emissions are tied to population and economic growth, which the counterfactual ignores.
# Counterfactual in year t:  E_birth * (1 + rate)^(t - birth).  Gap = sum over the lifetime of (real - counterfactual).
# Data: data/emissions_global_year.csv
# Run from the project root:  Rscript charts/d_scrollytelling/05_counterfactual.r [birth_year] [rate_percent_per_year]   (defaults 1988, 0)
# Saves: charts/d_scrollytelling/05_counterfactual.png (static, CO2) and charts/d_scrollytelling/05_counterfactual.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML. Controls: birth year, gas, and a slider for
#         the yearly rate of change (default 0%). The headline, the gap and the shaded area update as the slider moves.

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args) >= 1) as.integer(args[1]) else 1988L
RATE  <- if (length(args) >= 2) as.numeric(args[2]) else 0   # percent per year

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])   # same colors as the other charts
caption   <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9. An illustration, not a forecast."

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt / 1000, CH4 = ch4_mtco2e / 1000)   # Gt CO2e per year
LAST_YEAR <- max(world$year)
stopifnot(BIRTH >= min(world$year), BIRTH <= LAST_YEAR)

# Phrase for the counterfactual, shared by the title and the legend
rate_phrase <- \(r, birth_level) {
  if (r == 0) paste0("stayed at your birth-year level (", formatC(birth_level, format = "f", digits = 1), " Gt a year)")
  else paste0(if (r < 0) "fallen " else "grown ", abs(r), "% a year from your birth year")
}

# ---- Step 1: static ggplot (CO2) ----
d <- world |> filter(year >= BIRTH) |>
  transmute(year, real = CO2) |>
  mutate(cf = real[1] * (1 + RATE / 100)^(year - BIRTH))
gap <- sum(d$real - d$cf)
now <- d$real[d$year == LAST_YEAR]
years_equiv <- gap / now

headline <- if (BIRTH == LAST_YEAR) paste0("Born in ", BIRTH, ": no lifetime to compare yet") else
  paste0("Born in ", BIRTH, ": if CO2 emissions had ", rate_phrase(RATE, d$real[1]), ", the world would have emitted ",
         formatC(abs(gap), format = "f", digits = 0), " Gt ", if (gap >= 0) "less" else "more")
headline <- paste(strwrap(headline, 85), collapse = "\n")
subtitle <- paste0(if (gap >= 0) "The gap, " else "The extra amount, ", formatC(abs(gap), format = "f", digits = 0),
                   " Gt, is what the world emits in about ", formatC(abs(years_equiv), format = "f", digits = 1),
                   " years at today's rate of ", formatC(now, format = "f", digits = 1), " Gt a year")

ends <- tibble(year = LAST_YEAR, y = c(d$real[nrow(d)], d$cf[nrow(d)]), label = c("Actual", "Counterfactual"),
               type = c("real", "cf"))
p <- ggplot(d, aes(year)) +
  geom_ribbon(aes(ymin = pmin(real, cf), ymax = pmax(real, cf)), fill = gasColors[["CO2"]], alpha = 0.18) +
  geom_line(aes(y = real), color = gasColors[["CO2"]], linewidth = 1.2) +
  geom_line(aes(y = cf), color = "grey30", linewidth = 1, linetype = "dashed") +
  geom_text(data = ends, aes(x = year, y = y, label = label), hjust = 0, nudge_x = 0.8, size = 3.8, family = font,
            color = c(gasColors[["CO2"]], "grey30")) +
  annotate("text", x = mean(range(d$year)), y = mean(c(d$real[round(nrow(d) / 2)], d$cf[round(nrow(d) / 2)])),
           label = paste0(formatC(abs(gap), format = "f", digits = 0), " Gt"), size = 4.2, fontface = "bold", family = font,
           color = gasColors[["CO2"]]) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18)),
                     breaks = \(lims) { b <- round(scales::breaks_pretty()(lims)); unique(b[b <= LAST_YEAR]) }) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)), limits = c(0, NA)) +
  labs(x = NULL, y = "Annual CO2 emissions (Gt per year)", title = headline, subtitle = subtitle, caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(plot.title = element_text(size = 12.5), plot.subtitle = element_text(size = 10.5, color = "grey30"),
        plot.title.position = "plot", plot.caption.position = "plot", plot.caption = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/d_scrollytelling/05_counterfactual.png", p, width = 9, height = 5.5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# The browser recomputes the counterfactual and the gap whenever a control changes.
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
      .c5 { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; position: absolute; top: 8px; left: 0; z-index: 5; color: #1e3a5f; }
      .c5 .row { display: flex; flex-wrap: wrap; gap: 14px; align-items: flex-end; }
      .c5 label { display: block; font-size: 12px; font-weight: 600; margin: 0 0 3px 2px; }
      .c5 input[type=number], .c5 select { font: 600 14px system-ui, sans-serif; height: 34px; padding: 0 10px; color: #1e3a5f; background: #f2f7fd;
                                           border: 2px solid #2b6cb0; border-radius: 10px; }
      .c5 input[type=number] { width: 88px; }
      .c5 input[type=range] { width: 240px; accent-color: #2b6cb0; vertical-align: middle; }
      .c5 .val { font: 700 14px system-ui, sans-serif; display: inline-block; min-width: 56px; margin-left: 8px; }
    `;
    document.head.appendChild(css);

    var state = {birth: d.birth, gas: 'CO2', rate: d.rate};
    function mk(tag, attrs, text) {
      var n = document.createElement(tag);
      Object.keys(attrs || {}).forEach(function(k) { n.setAttribute(k, attrs[k]); });
      if (text) n.textContent = text; return n;
    }
    function field(label, ctl) { var w = mk('div'); w.appendChild(mk('label', {}, label)); w.appendChild(ctl); return w; }
    var box = mk('div', {class: 'c5'}), row = mk('div', {class: 'row'});
    var inBirth = mk('input', {type: 'number', min: d.first, max: d.last, value: state.birth});
    var selGas = mk('select');
    [['CO2', 'CO2'], ['CH4', 'CH4 (as CO2e)']].forEach(function(g) { selGas.appendChild(mk('option', {value: g[0]}, g[1])); });
    var inRate = mk('input', {type: 'range', min: -5, max: 3, step: 0.5, value: state.rate});
    var rateVal = mk('span', {class: 'val'});
    var rateWrap = mk('div'); rateWrap.appendChild(inRate); rateWrap.appendChild(rateVal);
    row.appendChild(field('Birth year', inBirth)); row.appendChild(field('Gas', selGas));
    row.appendChild(field('Change in emissions per year, from your birth year', rateWrap));
    box.appendChild(row); el.appendChild(box);
    if (embed) { var bi = box.querySelector('input[type=number]'); if (bi) bi.parentNode.style.display = 'none'; }

    function f0(v) { return Math.round(v).toLocaleString(); }
    function phrase(r, level) {
      return r === 0 ? 'stayed at your birth-year level (' + level.toFixed(1) + ' Gt a year)'
                     : (r < 0 ? 'fallen ' : 'grown ') + Math.abs(r) + '% a year from your birth year';
    }
    function render() {
      var gas = state.gas, colr = d.colors[gas];
      var yrs = [], real = [], cf = [], i0 = d.years.indexOf(state.birth);
      for (var i = i0; i < d.years.length; i++) {
        yrs.push(d.years[i]); real.push(d[gas][i]);
        cf.push(d[gas][i0] * Math.pow(1 + state.rate / 100, d.years[i] - state.birth));
      }
      var gap = 0, tot = 0;
      for (var k = 0; k < real.length; k++) { gap += real[k] - cf[k]; tot += real[k]; }
      var now = real[real.length - 1], equiv = Math.abs(gap) / now;
      var gname = gas === 'CO2' ? 'CO2' : 'CH4 (as CO2e)';
      var title = state.birth === d.last ? 'Born in ' + state.birth + ': no lifetime to compare yet'
        : 'Born in ' + state.birth + ': if ' + gname + ' emissions had ' + phrase(state.rate, real[0]) + ', the world would have emitted ' +
          f0(Math.abs(gap)) + ' Gt ' + (gap >= 0 ? 'less' : 'more');
      var sub = state.birth === d.last ? '' : (gap >= 0 ? 'The gap, ' : 'The extra amount, ') + f0(Math.abs(gap)) + ' Gt, is what the world emits in about ' +
                equiv.toFixed(1) + ' years at today\\'s rate of ' + now.toFixed(1) + ' Gt a year';
      var base = [], diff = [], realP = [], cfP = [];
      for (var j = 0; j < yrs.length; j++) {
        base.push(+Math.min(real[j], cf[j]).toFixed(3));
        diff.push(+Math.abs(real[j] - cf[j]).toFixed(3));
        realP.push(+real[j].toFixed(3)); cfP.push(+cf[j].toFixed(3));
      }
      chart.setOption({
        title: {text: title, subtext: sub, left: 0, top: embed ? 80 : 62, textStyle: {fontSize: 15, width: Math.max(300, el.clientWidth - 40), overflow: 'break'},
                subtextStyle: {fontSize: 12}},
        color: [colr, '#444'],
        grid: {top: embed ? 175 : 190, right: 120, bottom: 60, left: 70},
        legend: {show: false},
        tooltip: {trigger: 'axis', formatter: function(ps) {
          var y = +ps[0].axisValue, i = yrs.indexOf(y); if (i < 0) return '';
          return '<b>' + y + '</b><br>Actual: ' + real[i].toFixed(1) + ' Gt<br>Counterfactual: ' + cf[i].toFixed(1) + ' Gt<br>Gap: ' +
                 (real[i] - cf[i] >= 0 ? '+' : '\\u2212') + Math.abs(real[i] - cf[i]).toFixed(1) + ' Gt';
        }},
        graphic: {type: 'text', left: 0, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11}},
        xAxis: {type: 'category', data: yrs, boundaryGap: false, axisLabel: {hideOverlap: true, interval: function(i, v) { return +v % 10 === 0 || i === 0 || +v === d.last; }}, axisTick: {show: false}},
        yAxis: {type: 'value', min: 0, name: 'Gt ' + (gas === 'CO2' ? 'CO2' : 'CO2e') + ' per year', nameLocation: 'middle', nameGap: 48},
        series: [
          {name: 'base', type: 'line', data: base, stack: 'gap', symbol: 'none', lineStyle: {width: 0}, areaStyle: {opacity: 0}, tooltip: {show: false}, silent: true},
          {name: 'gap', type: 'line', data: diff, stack: 'gap', symbol: 'none', lineStyle: {width: 0}, areaStyle: {color: colr, opacity: 0.18}, tooltip: {show: false}, silent: true},
          {name: 'Actual', type: 'line', data: realP, symbol: 'none', lineStyle: {width: 3, color: colr}, itemStyle: {color: colr}, z: 4,
           endLabel: {show: true, formatter: 'Actual', fontSize: 13, color: colr}},
          {name: 'Counterfactual', type: 'line', data: cfP, symbol: 'none', lineStyle: {width: 2.5, type: 'dashed', color: '#444'}, itemStyle: {color: '#444'}, z: 4,
           endLabel: {show: true, formatter: 'Counterfactual', fontSize: 13, color: '#444'}, labelLayout: {moveOverlap: 'shiftY'}}
        ]
      }, true);
      rateVal.textContent = (state.rate > 0 ? '+' : '') + state.rate + '%';
      if (embed) parent.postMessage({rate: state.rate}, '*');
    }
    inBirth.addEventListener('change', function() {
      var v = Math.round(+inBirth.value); if (!(v >= d.first && v <= d.last)) { inBirth.value = state.birth; return; }
      state.birth = v; render();
    });
    selGas.addEventListener('change', function() { state.gas = selGas.value; render(); });
    inRate.addEventListener('input', function() { state.rate = +inRate.value; render(); });
    render();
  }
", data = list(years = world$year, CO2 = round(world$CO2, 3), CH4 = round(world$CH4, 3), colors = as.list(gasColors),
               caption = caption, birth = BIRTH, rate = RATE, first = min(world$year), last = LAST_YEAR))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/05_counterfactual.html")
