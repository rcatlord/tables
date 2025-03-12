# League of Arab States

library(gt) ; library(tidyverse) ; library(countrycode)

tbl <- countrypops |> 
  filter(year == 2022,
         country_code_3 %in% c("DZA","BHR","COM","DJI","EGY","IRQ","JOR","KWT",
                               "LBN","LBY","MRT","MAR","OMN","PSE","QAT","SAU",
                               "SOM","SDN","SYR","TUN","ARE","YEM")) |> 
  mutate(continent_en = countrycode(country_code_3, "iso3c", "un.region.name"),
         continent_ar = ifelse(continent_en == "Africa", "أفريقيا", "آسيا")) |> 
  select(country_en = country_name, continent_en, population, country_ar = country_code_3, continent_ar) |> 
  arrange(desc(population)) |> 
  group_by(continent_en) |> 
  mutate(continent_ar = ifelse(row_number() == 1, continent_ar, "")) |> 
  ungroup() |> 
  gt(rowname_col = "country_en",
     groupname_col = "continent_en",
     row_group_as_column = TRUE) |> 
  tab_stubhead(label = "Continent") |> 
  fmt_country(columns = country_ar, locale = "ar") |> 
  fmt_number(columns = population, decimals = 1, suffixing = TRUE) |> 
  cols_label(
    population = "Population (2022)",
    country_ar = "",
    continent_ar = "قارة"
  ) |> 
  sub_missing(columns = "country_ar", missing_text = "") |> 
  tab_header(title = "Countries belonging to the League of Arab States") |> 
  tab_source_note(source_note = "Source: World Bank") |> 
  tab_options(
    column_labels.font.weight = "bold",
    stub_row_group.font.weight = "bold",
    data_row.padding = px(3)
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = continent_ar)
  ) |> 
  tab_style(
    style = cell_borders(sides = "left", color = "#d8d4d4", weight = px(2)),
    locations = cells_body(columns = c(country_ar,continent_ar))
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "#FFFFFF"),
    locations = cells_body(columns = "continent_ar")
  )

tbl |> gtsave("index.html")
tbl |> gtsave("tbl.png", expand = 10)
