# Build data/generations.csv: generation name and birth-year range.
# Used to map a visitor's birth year to a generation.
#
# Ranges follow Pew Research Center's definitions. Gen Alpha is open-ended at Pew; here it ends
# at 2024, the last year of emissions data. People born before 1928 are not mapped to a generation.
# Run from the project root:  Rscript R/generations.r

library(tidyverse)

OUT_FILE <- "data/generations.csv"

tribble(
  ~generation,  ~start_birth_year, ~end_birth_year,
  "Silent",     1928,              1945,
  "Boomer",     1946,              1964,
  "Gen X",      1965,              1980,
  "Millennial", 1981,              1996,
  "Gen Z",      1997,              2012,
  "Gen Alpha",  2013,              2024
) |>
  write_csv(OUT_FILE)
