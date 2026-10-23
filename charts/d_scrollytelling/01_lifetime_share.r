# Share of all emissions since 1850 that were released during a person's lifetime (scrollytelling step 1).
#
# Lines show how much of the 1850-to-2024 total had been emitted by each year (0 to 100%), for CO2 and CH4. The shaded
# band is the person's lifetime, from their birth year to 2024; the climb of each line across the band is the share
# released while they were alive. Same formula as chart a/02: emissions from the birth year to 2024 divided by the
# 1850 to 2024 total. Also marks the halfway year (when each gas passed 50% of the 1850 to 2024 total, with your age
# then) and your age every 10 years across the lifetime band.
# Data: data/emissions_global_year.csv
# Run from the project root:  Rscript charts/d_scrollytelling/01_lifetime_share.r [birth_year]   (default 1988)
# Saves: charts/d_scrollytelling/01_lifetime_share.png (static) and charts/d_scrollytelling/01_lifetime_share.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML. Click anywhere on the chart to
#         move the birth year; the shaded band, the age ticks and the percentages update. This is the prototype of the birth-year
#         input on the scrollytelling page.

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

args  <- commandArgs(trailingOnly = TRUE)
BIRTH <- if (length(args)) as.integer(args[1]) else 1988L

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])   # same colors as the a_generations charts
caption   <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9."

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt, CH4 = ch4_mtco2e)
LAST_YEAR <- max(world$year)
stopifnot(BIRTH >= min(world$year), BIRTH <= LAST_YEAR)

# Share of the 1850 to 2024 total emitted by each year (running total / final total)
shares <- world |>
  mutate(across(c(CO2, CH4), \(x) cumsum(x) / sum(x))) |>
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "share") |>
  mutate(gas = fct_relevel(gas, "CO2"))

# Share released from the birth year to the last year, per gas
lifetime <- world |>
  summarise(across(c(CO2, CH4), \(x) sum(x[year >= BIRTH]) / sum(x))) |>
  pivot_longer(everything(), names_to = "gas", values_to = "lifetime") |>
  mutate(gas = fct_relevel(gas, "CO2"))

# Halfway year per gas: first year the running share reaches 50%
halfway <- shares |> filter(share >= 0.5) |> slice_min(year, by = gas) |> select(gas, year)
half_text <- \(g, birth) {
  h <- halfway$year[halfway$gas == g]
  paste0(g, " passed 50% in ", h, " (", if (birth > h) "before you were born" else paste0("age ", h - birth), ")")
}
AGES <- seq(10, 100, by = 10)
ticks <- tibble(age = AGES, year = BIRTH + AGES) |> filter(year <= LAST_YEAR)

pct <- \(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
headline <- paste0("Born in ", BIRTH, ": ", pct(lifetime$lifetime[lifetime$gas == "CO2"]), " of all CO2 and ",
                   pct(lifetime$lifetime[lifetime$gas == "CH4"]), " of all CH4 emissions came in your lifetime")

subtitle <- paste0("Halfway point: ", half_text("CO2", BIRTH), "; ", half_text("CH4", BIRTH))

# ---- Step 1: static ggplot ----
ends <- shares |> filter(year == LAST_YEAR) |> left_join(lifetime, by = "gas") |>
  mutate(label = paste0(gas, ": ", pct(lifetime), " in your lifetime"))

p <- ggplot(shares, aes(year, share, color = gas)) +
  annotate("rect", xmin = BIRTH, xmax = LAST_YEAR, ymin = 0, ymax = 1, fill = "grey90") +
  annotate("text", x = BIRTH + 1, y = 0.97, label = "Your lifetime", hjust = 0, size = 3.6, color = "grey40", family = font) +
  geom_segment(data = ticks, aes(x = year, xend = year, y = 0, yend = 0.05), inherit.aes = FALSE, color = "white", linewidth = 0.6) +
  geom_text(data = ticks, aes(x = year, y = 0.065, label = age), inherit.aes = FALSE,
            size = 2.9, color = "grey40", family = font) +
  annotate("text", x = BIRTH + 0.5, y = 0.065, label = "age", hjust = 0, size = 2.9, color = "grey40", family = font) +
  geom_line(linewidth = 1) +
  geom_point(data = left_join(halfway, shares, by = c("gas", "year")), aes(y = share), size = 2.5) +
  annotate("text", x = halfway$year[halfway$gas == "CO2"] + 2, y = 0.5, label = paste0("CO2 halfway: ", halfway$year[halfway$gas == "CO2"]),
           hjust = 0, vjust = 1.6, size = 3.4, color = gasColors[["CO2"]], family = font) +
  annotate("text", x = halfway$year[halfway$gas == "CH4"] - 2, y = 0.5, label = paste0("CH4 halfway: ", halfway$year[halfway$gas == "CH4"]),
           hjust = 1, vjust = -0.6, size = 3.4, color = gasColors[["CH4"]], family = font) +
  geom_text_repel(data = ends, aes(label = label), hjust = 0, direction = "y", nudge_x = 2,
                  size = 3.8, family = font, segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = gasColors) +
  scale_x_continuous(breaks = seq(1850, 2000, 50), expand = expansion(mult = c(0.01, 0.38))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL, y = "Share of all emissions since 1850",
       title = headline, subtitle = subtitle, caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "none",
        plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10.5, color = "grey30"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/d_scrollytelling/01_lifetime_share.png", p, width = 9, height = 5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# The chart holds the running shares; the lifetime percentages are recomputed in the browser from the yearly emissions
# whenever the birth year changes (click on the chart).
wide <- shares |> mutate(share = round(share, 5)) |> pivot_wider(names_from = gas, values_from = share)

e <- wide |>
  e_charts(year) |>
  e_line(CO2, symbol = "none", lineStyle = list(width = 3)) |>
  e_line(CH4, symbol = "none", lineStyle = list(width = 3)) |>
  e_color(unname(gasColors)) |>
  e_x_axis(type = "value", min = min(world$year), max = LAST_YEAR, splitLine = list(show = FALSE),
           axisLabel = list(formatter = JS("function(v) { return String(v); }")), interval = 25) |>
  e_y_axis(min = 0, max = 1, name = "Share of all emissions since 1850", nameLocation = "middle", nameGap = 50,
           axisLabel = list(formatter = JS("function(v) { return Math.round(v * 100) + '%'; }"))) |>
  e_tooltip(trigger = "axis", formatter = JS("function(ps) {
    var s = '<b>' + ps[0].value[0] + '</b>';
    ps.forEach(function(p) { s += '<br>' + p.seriesName + ': ' + (p.value[1] * 100).toFixed(1) + '% emitted by this year'; });
    return s;
  }")) |>
  e_toolbox(show = FALSE) |>
  e_grid(top = 105, right = 30, bottom = 70, left = 80)
e$x$opts$title <- list(list(text = headline, subtext = subtitle, left = 0, textStyle = list(fontSize = 16), subtextStyle = list(fontSize = 12)))
# Halfway points on the lines (fixed: they do not depend on the birth year)
for (i in 1:2) {
  g <- c("CO2", "CH4")[i]
  e$x$opts$series[[i]]$markPoint <- list(
    symbolSize = 10, itemStyle = list(color = gasColors[[g]]),
    label = list(show = TRUE, formatter = paste0(g, " halfway: ", halfway$year[halfway$gas == g]), color = gasColors[[g]],
                 position = if (g == "CO2") "right" else "left", offset = list(0, if (g == "CO2") 14 else -14), fontSize = 12),
    data = list(list(coord = list(halfway$year[halfway$gas == g],
                                  round(shares$share[shares$gas == g & shares$year == halfway$year[halfway$gas == g]], 5)))))
}
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6,
                         style = list(text = paste0(caption, " Click the chart to change the birth year."), fill = "#888", fontSize = 11))
# Both lines end at 100%, so end labels would overlap: use a legend instead
e$x$opts$legend <- list(top = 62, left = 0, selectedMode = FALSE)

e <- e |> htmlwidgets::onRender("
  function(el, x, d) {
    var chart = echarts.getInstanceByDom(el);
    var q = new URLSearchParams(location.search); if (q.get('birth')) d.birth = +q.get('birth');
    var embed = q.get('embed') === '1';
    var cum = {CO2: 0, CH4: 0};
    function pct(v) { return (100 * v).toFixed(1) + '%'; }
    function halfText(g, birth) {
      var h = d.half[g];
      return g + ' passed 50% in ' + h + ' (' + (birth > h ? 'before you were born' : 'age ' + (h - birth)) + ')';
    }
    function update(birth) {
      var life = {CO2: 0, CH4: 0};
      d.years.forEach(function(y, i) { if (y >= birth) { life.CO2 += d.CO2[i]; life.CH4 += d.CH4[i]; } });
      life.CO2 /= d.total.CO2; life.CH4 /= d.total.CH4;
      chart.setOption({
        title: {text: 'Born in ' + birth + ': ' + pct(life.CO2) + ' of all CO2 and ' + pct(life.CH4) +
                      ' of all CH4 emissions came in your lifetime',
                subtext: 'Halfway point: ' + halfText('CO2', birth) + '; ' + halfText('CH4', birth)},
        series: [{markLine: {silent: true, symbol: 'none', lineStyle: {color: '#fff', type: 'solid', width: 1.5},
                             label: {formatter: '{b}', position: 'insideStartTop', color: '#666', fontSize: 11},
                             data: [10, 20, 30, 40, 50, 60, 70, 80, 90, 100].filter(function(a) { return birth + a <= d.last; })
                               .map(function(a) { return {xAxis: birth + a, name: 'age ' + a}; })},
                  markArea: {silent: true, itemStyle: {color: 'rgba(0,0,0,0.07)'},
                             label: {show: true, position: 'insideTopLeft', formatter: 'Your lifetime', color: '#666'},
                             data: [[{xAxis: birth}, {xAxis: d.last}]]}}, {}]
      });
    }
    update(d.birth);
    chart.getZr().on('click', function(ev) {
      var y = Math.round(chart.convertFromPixel({seriesIndex: 0}, [ev.offsetX, ev.offsetY])[0]);
      if (y >= d.first && y <= d.last) { if (embed) parent.postMessage({birth: y}, '*'); else update(y); }
    });
  }
", data = list(years = world$year, CO2 = world$CO2, CH4 = world$CH4,
               total = list(CO2 = sum(world$CO2), CH4 = sum(world$CH4)),
               birth = BIRTH, first = min(world$year), last = LAST_YEAR,
               half = as.list(setNames(halfway$year, halfway$gas))))

dir.create("charts/d_scrollytelling", showWarnings = FALSE, recursive = TRUE)
save_echart(e, "charts/d_scrollytelling/01_lifetime_share.html")
