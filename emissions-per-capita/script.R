# Per capita CO2 emissions
# Source: Our World in Data
# URL: https://ourworldindata.org/co2-and-greenhouse-gas-emissions

library(tidyverse) ; library(countrycode) ; library(gt)

df <- read.csv("co-emissions-per-capita.csv") |> 
  filter(Year == max(Year)) |> 
  mutate(
    status = case_when(
      dense_rank(desc(Annual.CO..emissions..per.capita.)) <= 10 ~ "Highest",
      dense_rank(Annual.CO..emissions..per.capita.) <= 10 ~ "Lowest",
      TRUE ~ NA
      ),
    Continent = countrycode(Code, "iso3c", "continent")) |> 
  filter(!is.na(status)) |> 
  group_by(status) |> 
  arrange(desc(Annual.CO..emissions..per.capita.))

tbl <- df |> 
  gt() |>  
  cols_hide(columns = Year) |> 
  fmt_number(columns = Annual.CO..emissions..per.capita., decimals = 2) |> 
  fmt_flag(columns = Code) |> 
  cols_merge(
    columns = c(Entity, Code),
    pattern = "{2} {1}"
    ) |> 
  cols_move(
    columns = Continent,
    after = Entity
    ) |>
  cols_label(
    Entity = "Country",
    Annual.CO..emissions..per.capita. = html("Emissions/<br>capita (t)")
    ) |> 
  cols_width(
    Entity ~ px(330)
  ) |> 
  cols_width(
    Continent ~ px(140)
    ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_borders(sides = "bottom", weight = px(3))
      ),
    locations = cells_column_labels()
    ) |> 
  # data_color(
  #   columns = Continent, 
  #   method = "factor",
  #   palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),
  #   apply_to = "text"
  #   ) |> 
  data_color(
    columns = Annual.CO..emissions..per.capita.,
    rows = grepl("Highest", status),
    method = "numeric",
    palette = "Greys"
    ) |> 
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_row_groups()
    ) |> 
  tab_header(title = html("<strong>Annual CO<sub>2</sub> emissions per capita<strong>, 2022")) |> 
  tab_source_note(source_note = "Data: ourworldindata.org/co2-and-greenhouse-gas-emissions") |> 
  tab_footnote(
    footnote = html("Carbon dioxide (CO<sub>2</sub>) emissions from fossil fuels and industry. Land-use change is not included."),
    locations = cells_title(groups = "title")
    ) |> 
  opt_footnote_marks(marks = "standard") |> 
  opt_table_font(font = list(google_font("Chivo"), default_fonts())) |> 
  tab_options(
    column_labels.border.top.width = px(5),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    row_group.background.color = "#deebf7",
    row_group.border.top.color = "#000000",
    row_group.border.bottom.color = "#FFFFFF",
    table.font.size = px(18),
    heading.title.font.size = px(24),
    heading.align = "left",
    footnotes.font.size = px(12),
    footnotes.padding = px(5),
    source_notes.font.size = px(12),
    data_row.padding = px(5),
    table.margin.left = px(5)
  )

gtsave(tbl, "emissions_per_capita.png")
