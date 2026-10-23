# Build data/events_impact.csv: world events and how global emissions behaved during each one, against its own pre-event trend.
#
# For each event window (start to end year) and each gas, the baseline is a log-linear trend (constant growth rate) fitted to
# the 10 years right before the window, projected through the window. pct_vs_trend = (actual - baseline) / baseline, summed
# over the window years. The baseline can itself contain an earlier shock (e.g. the 2012-2021 fit for 2022 includes COVID);
# that is a limit of a simple trend and is not corrected for.
# Policy milestones (kind = "policy") are only markers: a pre-event trend says nothing about their effect, so no impact is computed.
# This measures how emissions moved relative to trend while an event was going on. It does not show the event caused it.
#
# Data: data/emissions_global_year.csv
# Run from the project root:  Rscript R/event_impacts.r
# Writes: data/events_impact.csv (one row per event and gas; policy markers have gas = NA)
#         data/events_baseline_year.csv (the projected baseline for each event, gas and window year, for drawing it)

library(tidyverse)

OUT_FILE  <- "data/events_impact.csv"
BASE_FILE <- "data/events_baseline_year.csv"
FIT_YEARS <- 10

events <- tribble(
  ~event,                         ~start, ~end, ~kind,    ~category,    ~description,
  "World War I",                  1914,   1918, "event",  "geopolitics", "The First World War diverts industry and trade to the war effort and disrupts coal and shipping.",
  "Great Depression",             1929,   1933, "event",  "economy",     "A worldwide slump in industrial output and trade cuts energy demand.",
  "World War II",                 1939,   1945, "event",  "geopolitics", "Total war reshapes industry and energy use across most of the major economies.",
  "1973 oil crisis",              1973,   1975, "event",  "energy",      "OPEC oil embargo sharply raises oil prices and cuts energy demand in many importing countries.",
  "1979 oil crisis",              1979,   1983, "event",  "energy",      "The Iranian Revolution disrupts oil supply and prices roughly double; a deep recession follows.",
  "Fall of the USSR",             1991,   1995, "event",  "geopolitics", "The Soviet Union dissolves; industrial output and emissions in its successor states fall sharply.",
  "Kyoto Protocol",               1997,   1997, "policy", "policy",      "The first international treaty with binding emissions targets for rich countries is adopted.",
  "Asian financial crisis",       1997,   1999, "event",  "economy",     "Currency and banking crises hit East and Southeast Asia and slow industrial growth.",
  "China joins the WTO",          2001,   2007, "event",  "economy",     "China's entry into the World Trade Organization accelerates export-led growth and coal use.",
  "Global financial crisis",      2008,   2010, "event",  "economy",     "A worldwide recession cuts industrial activity; emissions dip in 2009 before rebounding.",
  "Paris Agreement",              2015,   2015, "policy", "policy",      "Nearly every country commits to national climate targets under the Paris Agreement.",
  "COVID-19 pandemic",            2020,   2020, "event",  "health",      "Lockdowns cut transport and industry; global emissions fall sharply for one year.",
  "Energy crisis after Ukraine",  2022,   2023, "event",  "energy",      "Russia's invasion of Ukraine disrupts gas and oil markets and raises energy prices."
)

world <- read_csv("data/emissions_global_year.csv", show_col_types = FALSE) |>
  transmute(year, CO2 = co2_mt, CH4 = ch4_mtco2e) |>
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "mt")
stopifnot(!anyNA(world$mt), all(world$mt > 0))

windows <- events |> filter(kind == "event")

# Baseline for one event and gas: the fitted trend projected over the window, one row per year
baseline <- function(ev_start, ev_end, g) {
  fit <- world |> filter(gas == g, year >= ev_start - FIT_YEARS, year < ev_start)
  stopifnot(nrow(fit) == FIT_YEARS)
  m <- lm(log(mt) ~ year, data = fit)
  world |> filter(gas == g, year >= ev_start, year <= ev_end) |>
    transmute(year, actual_mt = mt, baseline_mt = exp(predict(m, tibble(year = year))), growth = exp(coef(m)[["year"]]) - 1,
              fit_years = nrow(fit))
}

yearly <- windows |>
  cross_join(tibble(gas = c("CO2", "CH4"))) |>
  mutate(res = pmap(list(start, end, gas), baseline)) |>
  unnest(res)

impacts <- yearly |>
  summarise(baseline_mt = sum(baseline_mt), actual_mt = sum(actual_mt),
            pct_vs_trend = round(100 * (sum(actual_mt) - sum(baseline_mt)) / sum(baseline_mt), 1),
            trend_growth_pct = round(100 * first(growth), 2), fit_years = first(fit_years),
            .by = c(event, start, end, kind, category, description, gas))

yearly |>
  transmute(event, gas, year, baseline_mt = round(baseline_mt, 1)) |>
  write_csv(BASE_FILE)

bind_rows(impacts, events |> filter(kind == "policy")) |>
  mutate(across(c(baseline_mt, actual_mt), \(x) round(x, 1))) |>
  arrange(start, event) |>
  select(event, start, end, kind, category, gas, baseline_mt, actual_mt, pct_vs_trend, trend_growth_pct, fit_years, description) |>
  write_csv(OUT_FILE)
