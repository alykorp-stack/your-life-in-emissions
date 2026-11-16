# Home page greenhouse gas graphs (echarts4r)

Goal: show that CH4 is emitted in far smaller amounts than CO2 but each tonne warms much more. CO2 + CH4 only.

- [x] `data/gases.csv` (gas, gwp20, gwp100, lifetime_years, source) + entry in `data/README.md`
- [x] `charts/e_home/01_global_by_gas.r`: global lines 1850-2024, toggle by weight / by warming
- [x] `charts/e_home/02_tonne_for_tonne.r`: warming of 1 tonne of each gas, toggle 20 / 100 years
- [x] `charts/e_home/03_world_map.r`: echarts4r world map, gas + measure selects, year slider with play
- [x] `index.qmd`: embed the three charts, placeholder text
- [x] Verify: scripts run, HTML works in a browser, `quarto render` OK
- [x] New source (IPCC AR6 Table 7.SM.7) added to `organizing.qmd`

## Review

- CH4 share in 2024: 0.9% by weight, 19.7% by warming (printed by 01).
- Shared switch: `charts/e_home/toggle_helper.r` (`add_toggle()`), used by 01 and 02.
- Checked in headless Chrome: toggles swap data, legend hides a gas, tooltips, map selects keep each other's value when the slider moves, play advances years, zoom survives updates.
- Site at 1280px and 390px: no horizontal page scroll, Montserrat intact, all three iframes draw.
- Known gaps:
  - At phone width the charts' own controls crowd the titles (chart 1 title hidden behind buttons, map controls stack over the map).
  - Map HTML is 2.7 MB, mostly the year-by-country data.
  - `index.qmd` has two "Why CO2 and CH4?" headings (top and bottom).
  - The year axis in `charts/a_generations/echarts_helpers.r:94` and `charts/c_leaders_over_time/01_cumulative_featured.r:67` likely shows "1,970"-style labels (same `{value}` formatter fixed in 01). Not changed.
