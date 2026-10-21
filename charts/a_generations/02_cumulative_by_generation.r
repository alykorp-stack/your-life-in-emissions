# Emissions lived through by a typical member of each generation (Silent to Gen Alpha).
#
# Vertical bar chart, one bar group per generation in generation order. A "typical member" is born in the
# midpoint of the generation's birth years. Two panels: the share of all emissions since 1850 that were
# emitted from that birth year to 2024, and the average emissions per year lived. The first favors older
# generations (more years); the second favors younger ones (all their years are high-emission years).
# Data: data/emissions_global_year.csv, data/generations.csv
# Run from the project root:  Rscript charts/a_generations/02_cumulative_by_generation.r
# Saves: charts/a_generations/02_cumulative_by_generation.png (static) and charts/a_generations/02_cumulative_by_generation.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive plotly version of the same chart, saved as standalone HTML.
#         Hover-only: a tooltip with the exact value on each bar (no legend toggling, zoom or pan).

library(tidyverse)
library(cowplot)
library(ltc)
library(plotly)
library(htmlwidgets)

font      <- "sans"
hat       <- as.vector(ltc("hat"))
gasColors <- c("CO2" = hat[6], "CH4" = hat[7])   # same colors as chart 01
LAST_YEAR <- 2024
OUT_PNG   <- "charts/a_generations/02_cumulative_by_generation.png"
OUT_HTML  <- "charts/a_generations/02_cumulative_by_generation.html"

caption <- "Data: PRIMAP-hist v2.2, Global Carbon Budget, EDGAR. Excludes land use. CH4 at GWP100 = 27.9. All emissions = 1850 to 2024."

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt, CH4 = ch4_mtco2e) |>
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "mt")

all_time <- world |> summarise(total = sum(mt), .by = gas)

# One representative birth year per generation (midpoint, rounded down)
generations <- read_csv("data/generations.csv", show_col_types = FALSE) |>
  mutate(birth_year = (start_birth_year + end_birth_year) %/% 2,
         years_lived = LAST_YEAR - birth_year + 1,
         label = fct_inorder(paste0(generation, "\n(born ", birth_year, ")")))

by_generation <- generations |>
  cross_join(distinct(world, gas)) |>
  mutate(lived = map2_dbl(birth_year, gas,
                          \(b, g) sum(world$mt[world$gas == g & world$year >= b]))) |>
  left_join(all_time, by = "gas") |>
  mutate(`Share of all emissions since 1850` = lived / total,
         `Average emissions per year lived (Gt CO2e)` = lived / years_lived / 1000) |>
  pivot_longer(starts_with(c("Share", "Average")), names_to = "metric", values_to = "value") |>
  mutate(metric = fct_inorder(metric),
         gas = fct_relevel(gas, "CO2"))

# ---- Step 1: static ggplot ----
# Two panels with different units, so two plots joined with plot_grid (one legend, shared at the top)
panel <- function(metric_name, y_labels, title) {
  by_generation |>
    filter(metric == metric_name) |>
    ggplot(aes(label, value, fill = gas)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    scale_fill_manual(values = gasColors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = y_labels) +
    labs(x = NULL, y = NULL, fill = NULL, title = title) +
    theme_minimal_hgrid(font_family = font, font_size = 12) +
    theme(legend.position = "none",
          plot.title.position = "plot",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
}

p_share <- panel("Share of all emissions since 1850", scales::percent,
                 "Share of all emissions since 1850")
p_rate  <- panel("Average emissions per year lived (Gt CO2e)", scales::comma,
                 "Average emissions per year lived (Gt CO2e)")

legend  <- get_legend(p_share + theme(legend.position = "top", legend.justification = "left"))
title   <- ggdraw() + draw_label("Emissions lived through by a typical member of each generation",
                                 fontfamily = font, fontface = "bold", size = 16, x = 0, hjust = 0)
sub     <- ggdraw() + draw_label("Emitted from the generation's midpoint birth year to 2024",
                                 fontfamily = font, size = 11, x = 0, hjust = 0)
foot    <- ggdraw() + draw_label(caption, fontfamily = font, size = 9, x = 0, hjust = 0)

p_static <- plot_grid(title, sub, legend, plot_grid(p_share, p_rate, nrow = 1), foot,
                      ncol = 1, rel_heights = c(0.08, 0.05, 0.06, 1, 0.05)) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(8, 12, 8, 12))

ggsave(OUT_PNG, p_static, width = 11, height = 5.5, dpi = 300)

# ---- Step 2: interactive plotly ----
# Hover-only: a tooltip with the exact value on each bar. No legend toggling, zoom or pan.
panel_ly <- function(metric_name, tick_format, hover_value, show_legend) {
  by_generation |>
    filter(metric == metric_name) |>
    mutate(label = str_replace(label, "\n", "<br>"),
           label = fct_inorder(label),
           who = paste0(generation, " (born ", birth_year, ")")) |>
    plot_ly(x = ~label, y = ~value, color = ~gas, colors = gasColors, type = "bar",
            customdata = ~who, showlegend = show_legend,
            hovertemplate = paste0("<b>%{customdata}</b><br>", hover_value, "<extra>%{fullData.name}</extra>")) |>
    layout(xaxis = list(title = "", fixedrange = TRUE),
           yaxis = list(title = "", tickformat = tick_format, rangemode = "tozero", fixedrange = TRUE))
}

p_share_ly <- panel_ly("Share of all emissions since 1850", ".0%",
                       "%{y:.1%} of all emissions since 1850", TRUE)
p_rate_ly  <- panel_ly("Average emissions per year lived (Gt CO2e)", ",",
                       "%{y:.1f} Gt CO2e per year lived", FALSE)

p <- subplot(p_share_ly, p_rate_ly, nrows = 1, margin = 0.06) |>
  layout(barmode = "group",
         title = list(text = "Emissions lived through by a typical member of each generation", x = 0),
         legend = list(orientation = "h", x = 0, y = 1.12, itemclick = FALSE, itemdoubleclick = FALSE),
         annotations = list(
           list(text = "Share of all emissions since 1850", x = 0, y = 1.05, xref = "paper",
                yref = "paper", xanchor = "left", showarrow = FALSE, font = list(size = 14)),
           list(text = "Average emissions per year lived (Gt CO2e)", x = 0.56, y = 1.05, xref = "paper",
                yref = "paper", xanchor = "left", showarrow = FALSE, font = list(size = 14)),
           list(text = caption, x = 0, y = -0.2, xref = "paper", yref = "paper",
                xanchor = "left", showarrow = FALSE, font = list(size = 10, color = "grey40"))),
         margin = list(t = 90, b = 100),
         paper_bgcolor = "white", plot_bgcolor = "white") |>
  config(displayModeBar = FALSE)

# saveWidget(selfcontained = TRUE) needs pandoc; use the copy bundled with Quarto if none is on the PATH
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = file.path(dirname(Sys.which("quarto")), "tools"))
}
saveWidget(p, file.path(getwd(), OUT_HTML), selfcontained = TRUE)
unlink(sub("[.]html$", "_files", OUT_HTML), recursive = TRUE)   # saveWidget leaves this behind on Windows
