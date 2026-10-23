# The generational ladder (scrollytelling, "then vs now").
#
# Share of all emissions since 1850 released from the year each of three people was born to 2024: you, a parent (born 30 years
# before you) and a grandparent (born 60 years before you). It shows how concentrated emissions are in recent decades: each step
# back in the family adds only a little. Where a relative would be born before 1850 (the start of the data) the share is 100%
# and the bar is marked "since 1850". Same formula as charts a/02 and d/01.
# Data: data/emissions_global_year.csv
# Run from the project root:  Rscript charts/d_scrollytelling/07_generational_ladder.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/07_generational_ladder.png (static) and charts/d_scrollytelling/07_generational_ladder.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML (birth year box under the title).

library(tidyverse)
library(cowplot)
library(ltc)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args)) as.integer(args[1]) else 1988L
GAP   <- 30   # years between generations

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])
caption   <- "Share released from each birth year to 2024. Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. CH4 at GWP100 = 27.9."

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt, CH4 = ch4_mtco2e)
FIRST_YEAR <- min(world$year)
LAST_YEAR  <- max(world$year)
stopifnot(BIRTH >= FIRST_YEAR, BIRTH <= LAST_YEAR)

ladder <- \(birth) {
  tibble(who = factor(c("You", "A parent", "A grandparent"), levels = c("You", "A parent", "A grandparent")),
         born = birth - GAP * 0:2) |>
    mutate(clamped = born < FIRST_YEAR, from = pmax(born, FIRST_YEAR)) |>
    cross_join(tibble(gas = c("CO2", "CH4"))) |>
    mutate(share = map2_dbl(from, gas, \(f, g) sum(world[[g]][world$year >= f]) / sum(world[[g]])),
           gas = fct_relevel(gas, "CO2"))
}

# ---- Step 1: static ggplot ----
d <- ladder(BIRTH)
co2 <- d |> filter(gas == "CO2")
headline <- paste0("Born in ", BIRTH, ": ", round(100 * co2$share[1]), "% of all CO2 came in your lifetime. Going back ", GAP,
                   " years adds ", round(100 * (co2$share[2] - co2$share[1])), " points, another ", GAP, " adds ",
                   round(100 * (co2$share[3] - co2$share[2])))
headline <- paste(strwrap(headline, 80), collapse = "\n")
d <- d |> mutate(label = paste0(round(100 * share), "%"), who_lab = paste0(who, "\n(born ", if_else(clamped, paste0("before ", FIRST_YEAR), as.character(born)), ")"))
d$who_lab <- factor(d$who_lab, levels = unique(d$who_lab))

p <- ggplot(d, aes(who_lab, share, fill = gas)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = label), position = position_dodge(width = 0.8), vjust = -0.5, size = 3.6, family = font) +
  scale_fill_manual(values = gasColors) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.12)), limits = c(0, 1.05)) +
  labs(x = NULL, y = "Share of all emissions", fill = NULL, title = headline, caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "top", legend.justification = "left", plot.title = element_text(size = 12.5),
        plot.title.position = "plot", plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA))
ggsave("charts/d_scrollytelling/07_generational_ladder.png", p, width = 8, height = 5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
e <- tibble(x = 0, y = 0) |> e_charts(x) |> e_line(y)   # placeholder, the real chart is built by the script below
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6, style = list(text = caption, fill = "#888", fontSize = 11))

e <- e |> htmlwidgets::onRender("
  function(el, x, d) {
    var chart = echarts.getInstanceByDom(el);
    var q = new URLSearchParams(location.search); if (q.get('birth')) d.birth = +q.get('birth');
    var embed = q.get('embed') === '1';
    el.style.position = 'relative';
    var state = {birth: d.birth};
    if (!embed) {
      var css = document.createElement('style');
      css.textContent = '.c7 { font-family: system-ui, sans-serif; position: absolute; top: 8px; left: 0; z-index: 5; color: #1e3a5f; } ' +
        '.c7 label { display: block; font-size: 12px; font-weight: 600; margin: 0 0 3px 2px; } ' +
        '.c7 input { font: 600 14px system-ui, sans-serif; width: 88px; height: 34px; padding: 0 10px; color: #1e3a5f; background: #f2f7fd; border: 2px solid #2b6cb0; border-radius: 10px; }';
      document.head.appendChild(css);
      var box = document.createElement('div'); box.className = 'c7';
      var lab = document.createElement('label'); lab.textContent = 'Birth year';
      var inp = document.createElement('input'); inp.type = 'number'; inp.min = d.first; inp.max = d.last; inp.value = state.birth;
      box.appendChild(lab); box.appendChild(inp); el.appendChild(box);
      inp.addEventListener('change', function() {
        var v = Math.round(+inp.value); if (!(v >= d.first && v <= d.last)) { inp.value = state.birth; return; }
        state.birth = v; render();
      });
    }
    function share(g, from) {
      var s = 0, t = 0; d.years.forEach(function(y, i) { t += d[g][i]; if (y >= from) s += d[g][i]; }); return s / t;
    }
    function render() {
      var who = ['You', 'A parent', 'A grandparent'], gases = ['CO2', 'CH4'];
      var born = [0, 1, 2].map(function(k) { return state.birth - d.gap * k; });
      var cats = who.map(function(w, k) { return w + '\\n(born ' + (born[k] < d.first ? 'before ' + d.first : born[k]) + ')'; });
      var vals = gases.map(function(g) { return born.map(function(b) { return +share(g, Math.max(b, d.first)).toFixed(4); }); });
      var d1 = Math.round(100 * (vals[0][1] - vals[0][0])), d2 = Math.round(100 * (vals[0][2] - vals[0][1]));
      var title = 'Born in ' + state.birth + ': ' + Math.round(100 * vals[0][0]) + '% of all CO2 came in your lifetime. Going back ' + d.gap +
                  ' years adds ' + d1 + ' points, another ' + d.gap + ' adds ' + d2;
      chart.setOption({
        title: {text: title, left: 0, top: embed ? 8 : 56, textStyle: {fontSize: 16, width: Math.max(300, el.clientWidth - 40), overflow: 'break'}},
        color: [d.colors.CO2, d.colors.CH4],
        grid: {top: embed ? 120 : 170, right: 30, bottom: 80, left: 70},
        legend: {top: embed ? 84 : 134, left: 0, selectedMode: false},
        tooltip: {trigger: 'item', formatter: function(p) { return p.name.replace('\\n', ' ') + '<br>' + p.seriesName + ': ' + (100 * p.value).toFixed(1) + '% of all emissions since 1850'; }},
        graphic: {type: 'text', left: 0, bottom: 6, style: {text: d.caption, fill: '#888', fontSize: 11}},
        xAxis: {type: 'category', data: cats, axisLabel: {fontSize: 12, interval: 0}},
        yAxis: {type: 'value', min: 0, max: 1, axisLabel: {formatter: function(v) { return Math.round(100 * v) + '%'; }}},
        series: gases.map(function(g, k) {
          return {name: g, type: 'bar', data: vals[k], barGap: '10%', label: {show: true, position: 'top', formatter: function(p) { return Math.round(100 * p.value) + '%'; }}};
        })
      }, true);
    }
    render();
  }
", data = list(years = world$year, CO2 = world$CO2, CH4 = world$CH4, colors = as.list(gasColors), caption = caption,
               birth = BIRTH, gap = GAP, first = FIRST_YEAR, last = LAST_YEAR))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/07_generational_ladder.html")
