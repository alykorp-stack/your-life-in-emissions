# Actual annual global emissions since an example birth year, with events annotated.
#
# Line chart of the real yearly values from the birth year on (nothing smoothed, averaged or summed). CO2 and CH4 share one panel. Events from events.csv are marked with dashed lines.
# Data: data/emissions_global_year.csv, data/events.csv
# Run from the project root:  Rscript charts/a_generations/01_annual_since_birth.r
# Saves: charts/a_generations/01_annual_since_birth.png (static) and charts/a_generations/01_annual_since_birth.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML.
#         Hover tooltips and event descriptions; lines are labeled directly (no legend toggling). The birth-year input that picks the generation needs Quarto OJS (echarts4r cannot take it).

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets

font       <- "sans"
hat        <- as.vector(ltc("hat"))
gasColors  <- c("CO2" = hat[6], "CH4" = hat[7])   # blue, teal (no red/green pair)
BIRTH_YEAR <- 1970                                # example; the site will take this as user input
OUT_PNG    <- "charts/a_generations/01_annual_since_birth.png"
OUT_HTML   <- "charts/a_generations/01_annual_since_birth.html"

caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9."

# Annual emissions from the birth year on, in Gt CO2e per year (data is in Mt)
since_birth <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  filter(year >= BIRTH_YEAR) |>
  transmute(year,
            CO2 = co2_mt / 1000,
            CH4 = ch4_mtco2e / 1000) |>
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "gt_year")

events <- read_csv("data/events.csv", show_col_types = FALSE) |>
  filter(year > BIRTH_YEAR)

ends <- since_birth |> slice_max(year, by = gas)
ytop <- max(since_birth$gt_year)

# ---- Step 1: static ggplot ----
p_static <- ggplot(since_birth, aes(year, gt_year, color = gas)) +
  geom_vline(data = events, aes(xintercept = year),
             linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_text(data = events, aes(x = year, y = ytop, label = label),
            inherit.aes = FALSE, angle = 90, hjust = 0, vjust = -0.5,
            size = 3.2, color = "grey40", family = font) +
  geom_line(linewidth = 1) +
  geom_text_repel(data = ends, aes(label = gas), hjust = 0, direction = "y",
                  nudge_x = 1, size = 4, family = font, segment.color = NA) +
  scale_color_manual(values = gasColors) +
  scale_x_continuous(breaks = seq(1970, 2020, 10),
                     expand = expansion(mult = c(0.02, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
  coord_cartesian(ylim = c(0, ytop * 1.4)) +   # headroom so event labels sit above the lines
  labs(x = NULL,
       y = "Annual emissions (Gt CO2e per year)",
       title = paste0("Annual global emissions since ", BIRTH_YEAR),
       caption = caption) +
  theme_minimal_hgrid(font_family = font) +   # no vertical grid: it would double up with the event lines
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(OUT_PNG, p_static, width = 8, height = 5, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# Hover shows exact values; hovering an event line shows its description. Lines are labeled at their ends, with no legend.
# Zoom and download buttons come from echarts_helpers.r (same look as the choropleth's).
p <- since_birth |>
  pivot_wider(names_from = gas, values_from = gt_year) |>
  lifetime_chart(events, paste0("Annual global emissions since ", BIRTH_YEAR), gasColors, caption) |>
  add_buttons("annual_emissions_since_birth")

save_echart(p, OUT_HTML)
