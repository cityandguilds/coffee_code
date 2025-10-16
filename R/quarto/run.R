
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(quarto)


# Render a single quarto --------------------------------------------------

# To render a doc one at a time you need to change most arguments each time.
quarto_render(
  input = "quarto_template.qmd",
  output_file = "test.docx",
  execute_params = list(component_no = "0171-001", 
                        series = "Summer 2025")
)


# But it would be better to render multiple documents at once -------------

# Create a dataframe for purrr
table <- tibble(component_no = rep(c("0171-001", "0172-535", "8202-021"), each = 2),
                series = rep(c("Summer 2024", "Summer 2025"), times = 3)) |> 
  mutate(filename = paste0("file_", component_no, "_", series, ".docx")) 


pwalk(table, function(component_no, series, filename) {
  quarto_render(input = "quarto_template.qmd",
                output_file = filename,
                execute_params = list(component_no = component_no,
                                      series = series))
})


# It would be better to have the docs in a location of your choice --------

file_list <- list.files(path = here::here(), pattern = c(".docx$"))

# I don't want to lost my custom reference doc
# Returns all values in x that are not in y
file_list <- dplyr::setdiff(file_list, "custom-reference-doc.docx")

paste0("outputs/", file_list)

walk(file_list, ~file.copy(from = ., to = paste0("outputs/", .)))

file.remove(file_list)
