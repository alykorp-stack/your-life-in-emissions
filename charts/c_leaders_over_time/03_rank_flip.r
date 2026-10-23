# Do rankings flip when we adjust for population?
#
# Slope chart comparing each featured country's annual total rank with its per capita rank in the latest year.
# Rank 1 = highest emitter, out of every country in the master table. The rank axis is logarithmic so the top ranks, where
# most of the featured countries sit, do not bunch together. Lines are colored by direction:
# blue = ranks higher per person than in total, red = ranks lower per person than in total.
# Featured countries = those of charts c1 and c2 (China, United States, India, United Kingdom, Russia,
# Qatar, Australia, Canada, Germany, Indonesia, Brazil, Nigeria).
# Data: data/emissions_country_year.csv (co2_rank_annual, co2_rank_per_capita)
# Run from the project root:  Rscript charts/c_leaders_over_time/03_rank_flip.r
# Saves: charts/c_leaders_over_time/03_rank_flip.png (static) and charts/c_leaders_over_time/03_rank_flip.html (interactive)
#
# Step 1: static ggplot (my-dataviz house style), saved as a PNG.
# Step 2: interactive echarts4r version of the same chart, saved as standalone HTML.
#         Hover shows both ranks and the values behind them; clicking a country in the legend hides or shows it.

library(tidyverse)
library(cowplot)
library(ltc)
library(ggrepel)
source("charts/a_generations/echarts_helpers.r")   # loads echarts4r and htmlwidgets; save_echart()

font     <- "sans"
hat      <- as.vector(ltc("hat"))
dirColors <- c("Ranks higher per person" = hat[6], "Ranks lower per person" = hat[3])
featured <- c("China", "United States", "India", "United Kingdom", "Russia",
              "Qatar", "Australia", "Canada", "Germany", "Indonesia", "Brazil", "Nigeria")

all <- read_csv("data/emissions_country_year.csv", show_col_types = FALSE)
YEAR <- max(all$year)
n_countries <- sum(all$year == YEAR & !is.na(all$co2_rank_annual))
caption <- paste0("Data: PRIMAP-hist v2.2, Global Carbon Budget. CO2 only, ", YEAR, ". Rank 1 = highest of ", n_countries, " countries. Log scale.")

d <- all |>
  filter(year == YEAR, country %in% featured) |>
  transmute(country, total = co2_rank_annual, per_person = co2_rank_per_capita,
            co2_mt, co2_per_capita,
            direction = if_else(per_person < total, "Ranks higher per person", "Ranks lower per person"))

# ---- Step 1: static ggplot ----
long <- d |>
  pivot_longer(c(total, per_person), names_to = "measure", values_to = "rank") |>
  mutate(measure = factor(measure, levels = c("total", "per_person"),
                          labels = c("Total emissions", "Emissions per person")),
         label = paste0(country, " (", rank, ")"))

p <- ggplot(long, aes(measure, rank, group = country, color = direction)) +
  geom_line(linewidth = 1, key_glyph = "path") +
  geom_point(size = 2.5, show.legend = FALSE) +
  geom_text_repel(data = filter(long, measure == "Total emissions"), aes(label = label),
                  hjust = 1, direction = "y", nudge_x = -0.06, size = 3.8, family = font, segment.color = NA, show.legend = FALSE) +
  geom_text_repel(data = filter(long, measure == "Emissions per person"), aes(label = label),
                  hjust = 0, direction = "y", nudge_x = 0.06, size = 3.8, family = font, segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = dirColors) +
  scale_x_discrete(position = "top", expand = expansion(add = c(1.1, 1.1))) +
  scale_y_continuous(transform = c("log10", "reverse")) +
  labs(x = NULL, y = NULL, color = NULL,
       title = paste0("Rankings of CO2 emitters, total vs per person, ", YEAR),
       caption = caption) +
  theme_minimal_hgrid(font_family = font) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 13),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("charts/c_leaders_over_time/03_rank_flip.png", p, width = 8, height = 6, dpi = 300)

# ---- Step 2: interactive echarts4r ----
# One line per country between the two measures; the left label sits on the first point, the right one on the second.
measures <- c("Total emissions", "Emissions per person")
wide <- d |> arrange(total)
e <- tibble(measure = measures, r = 0) |> e_charts(measure) |> e_line(r)   # placeholder series, replaced below
# One series per country for the lines, plus one invisible point per label. The labels sit at ranks nudged apart
# (in log space, since the axis is logarithmic) so they never overlap; the lines keep the true ranks.
RANK_MAX <- 250
spread <- function(rank, gap = 0.04) {
  u <- log(rank) / log(RANK_MAX)
  for (k in 1:500) {
    o <- order(u)
    for (j in seq_len(length(u) - 1)) {
      a <- o[j]; b <- o[j + 1]
      if (u[b] - u[a] < gap) { m <- (u[a] + u[b]) / 2; u[a] <- m - gap / 2; u[b] <- m + gap / 2 }
    }
  }
  exp(pmax(u, 0) * log(RANK_MAX))
}
wide$total_lab <- spread(wide$total)
wide$per_person_lab <- spread(wide$per_person)

line_series <- map(seq_len(nrow(wide)), \(i) {
  w <- wide[i, ]
  col <- dirColors[[w$direction]]
  list(type = "line", name = w$country, symbol = "circle", symbolSize = 9,
       lineStyle = list(width = 3, color = col), itemStyle = list(color = col),
       data = list(list(measures[1], w$total), list(measures[2], w$per_person)),
       tooltip = list(formatter = paste0("<b>", w$country, "</b><br>Total: rank ", w$total, " (", format(round(w$co2_mt), big.mark = ","),
                                         " Mt CO2)<br>Per person: rank ", w$per_person, " (", round(w$co2_per_capita, 1), " t per person)")))
})
# Label points share their country's name, so clicking the country in the legend hides its line and both labels together.
label_series <- map(seq_len(nrow(wide)), \(i) {
  w <- wide[i, ]
  col <- dirColors[[w$direction]]
  lab <- \(left) list(show = TRUE, position = if (left) "left" else "right", distance = 14, fontSize = 13, color = col,
                      formatter = paste0(w$country, " (", if (left) w$total else w$per_person, ")"))
  list(type = "scatter", name = w$country, symbolSize = 0, silent = TRUE, tooltip = list(show = FALSE),
       data = list(list(value = list(measures[1], w$total_lab), label = lab(TRUE)),
                   list(value = list(measures[2], w$per_person_lab), label = lab(FALSE))))
})
e$x$opts$series <- c(line_series, label_series)
e <- e |>
  e_x_axis(axisLabel = list(fontWeight = "bold", fontSize = 14), axisTick = list(show = FALSE),
           axisLine = list(show = FALSE), boundaryGap = c("38%", "38%"), position = "top") |>
  e_y_axis(type = "log", inverse = TRUE, min = 1, max = RANK_MAX, show = FALSE) |>
  e_tooltip(trigger = "item") |>
  e_toolbox(show = FALSE) |>
  e_title(paste0("Rankings of CO2 emitters, total vs per person, ", YEAR), left = 0) |>
  e_grid(top = 120, right = 20, bottom = 60, left = 20)
e$x$opts$legend <- list(data = as.list(wide$country), top = 34, left = 0, selectedMode = TRUE)
e$x$opts$graphic <- list(type = "text", left = 0, bottom = 6, style = list(text = paste0(caption, " Click a country in the legend to hide or show it."), fill = "#888", fontSize = 11))

save_echart(e, "charts/c_leaders_over_time/03_rank_flip.html")
