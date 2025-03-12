# Almodóvar films at Cannes Film Festival

library(gt) ; library(tidyverse)

tbl <- films |>  
  filter(director == "Pedro Almodóvar") |> 
  select(contains("title"), year, run_time, countries_of_origin, imdb_url) |> 
  gt() |>
  tab_header(title = "Films in Competition at Cannes directed by Pedro Almodóvar") |>
  tab_source_note(md("Data: **gt** package")) |> 
  fmt_country(columns = countries_of_origin, sep = ", ") |> 
  fmt_url(
    columns = imdb_url,
    label = fontawesome::fa("imdb", fill = "#323232")
  ) |> 
  cols_merge(
    columns = c(title, original_title),
    pattern = "{1}<< ({2})>>"
  ) |> 
  text_replace(
    locations = cells_body(columns = title),
    pattern = "\\((.*?)\\)",
    replacement = "<br><small><em>\\1</em></small>"
  ) |> 
  cols_label(
    title = "Film",
    year = "Year",
    run_time = "Length",
    countries_of_origin = "Country",
    imdb_url = "IMDB"
  ) |> 
  cols_align(align = "center", columns = imdb_url) |> 
  opt_table_font(font = list(google_font("Chivo"), default_fonts())) |> 
  opt_stylize(style = 6, color = "gray", add_row_striping = TRUE) |> 
  opt_vertical_padding(scale = 0.5) |> 
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.padding = px(15),
    heading.align = "left",
    source_notes.font.size = 12,
    source_notes.padding = px(10)
  )

gtsave(tbl, "tbl.png", expand = 10)
