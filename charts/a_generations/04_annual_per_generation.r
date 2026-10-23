# Actual annual global emissions over the life of a typical member of each generation, with their events marked.
#
# One standalone chart per generation (Silent to Gen Alpha), for the personalized scrollytelling page: the visitor
# enters a birth year, the page maps it to a generation (data/generations.csv) and shows that generation's chart.
# Each chart starts at the generation's midpoint birth year (same representative person as chart 02) and runs to
# 2024. Events from events.csv that happened during that lifetime are marked with a dashed line, a label and a
# dot on the CO2 line. All six charts share the same y axis and size, so they stay comparable when shown one at a
# time. Lines connect the actual yearly values: nothing is smoothed or averaged.
#
# Output files are named by the generation, lowercase with dashes: silent, boomer, gen-x, millennial, gen-z,
# gen-alpha. The page can build the path from the generation name, e.g. by_generation/gen-x.png.
# Data: data/emissions_global_year.csv, data/generations.csv, data/events.csv
# Run from the project root:  Rscript charts/a_generations/04_annual_per_generation.r
# Saves: charts/a_generations/by_generation/<generation>.png (static) and .html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as one PNG per generation.
# Step 2: interactive echarts4r version of each chart, saved as standalone HTML in the same folder.

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])   # same colors as charts 01 and 02
LAST_YEAR <- 2024
OUT_DIR   <- "charts/a_generations/by_generation"

caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9."

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt, CH4 = ch4_mtco2e)

# One representative birth year per generation (midpoint, rounded down), as in chart 02
generations <- read_csv("data/generations.csv", show_col_types = FALSE) |>
  mutate(birth_year = (start_birth_year + end_birth_year) %/% 2,
         label = paste0(generation, " (born ", birth_year, ")"),
         file  = str_replace_all(str_to_lower(generation), " ", "-"))

# Actual annual emissions from each birth year, in Gt CO2e per year
lifetimes <- generations |>
  reframe(world |> filter(year >= birth_year) |>
            mutate(CO2 = CO2 / 1000, CH4 = CH4 / 1000),
          .by = c(generation, label, file, birth_year)) |>
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "gt_year") |>
  mutate(gas = fct_relevel(gas, "CO2"))

# Events inside each generation's lifetime, with a dot on the CO2 line
events <- read_csv("data/events.csv", show_col_types = FALSE) |>
  select(year, event = label) |>
  cross_join(distinct(generations, generation, birth_year)) |>
  filter(year > birth_year) |>
  left_join(filter(lifetimes, gas == "CO2") |> select(generation, year, gt_year),
            by = c("generation", "year"))

# Same y range for every generation, so the six charts compare honestly
ytop <- max(lifetimes$gt_year)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Step 1: static ggplot ----
chart_static <- function(gen) {
  d  <- filter(lifetimes, generation == gen)
  ev <- filter(events, generation == gen)
  ends <- slice_max(d, year, by = gas)

  ggplot(d, aes(year, gt_year, color = gas)) +
    geom_vline(data = ev, aes(xintercept = year),
               linetype = "dashed", color = "grey70", linewidth = 0.4) +
    geom_text(data = ev, aes(x = year, y = ytop, label = event),
              inherit.aes = FALSE, angle = 90, hjust = 0, vjust = -0.5,
              size = 3.2, color = "grey40", family = font) +
    geom_line(linewidth = 1) +
    geom_point(data = ev, aes(y = gt_year), color = gasColors[["CO2"]], size = 2.5) +
    geom_text_repel(data = ends, aes(label = gas), hjust = 0, direction = "y",
                    nudge_x = 0.5, size = 4, family = font, segment.color = NA) +
    scale_color_manual(values = gasColors) +
    scale_x_continuous(expand = expansion(mult = c(0.04, 0.12)),
                       breaks = \(lims) {                       # whole years, none after the last data year
                         b <- round(scales::breaks_pretty()(lims))
                         unique(b[b <= LAST_YEAR])
                       }) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
    coord_cartesian(ylim = c(0, ytop * 1.5)) +   # headroom so event labels sit above the lines
    labs(x = NULL,
         y = "Annual emissions (Gt CO2e per year)",
         title = paste0("Annual global emissions since birth, ", unique(d$label)),
         caption = caption) +
    theme_minimal_hgrid(font_family = font) +   # no vertical grid: it would double up with the event lines
    theme(legend.position = "none",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
}

generations |>
  distinct(generation, file) |>
  pwalk(\(generation, file) {
    ggsave(file.path(OUT_DIR, paste0(file, ".png")), chart_static(generation),
           width = 8, height = 5, dpi = 300)
  })

# ---- Step 2: interactive echarts4r ----
# Same chart as the PNG, one HTML per generation, with the same y axis (ytop * 1.5, rounded up to a multiple of 10) so they stay comparable.
# Hover shows exact values and event descriptions; zoom and download buttons come from echarts_helpers.r.
event_info <- read_csv("data/events.csv", show_col_types = FALSE) |>
  select(year, label, description)

chart_interactive <- function(gen, file) {
  d  <- lifetimes |> filter(generation == gen) |> select(year, gas, gt_year) |>
    pivot_wider(names_from = gas, values_from = gt_year)
  ev <- events |> filter(generation == gen) |>
    select(year, gt_year) |> left_join(event_info, by = "year")
  title <- paste0("Annual global emissions since birth, ", distinct(filter(lifetimes, generation == gen), label)$label)

  lifetime_chart(d, ev, title, gasColors, caption, ytop = ceiling(ytop * 1.5 / 10) * 10, dots = TRUE) |>
    add_buttons(paste0("annual_emissions_", file)) |>
    save_echart(file.path(OUT_DIR, paste0(file, ".html")))
}

generations |>
  distinct(generation, file) |>
  pwalk(\(generation, file) chart_interactive(generation, file))
