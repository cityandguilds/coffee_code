
my_plot <- function(df){
  df |> ggplot(aes(x = raw_mark)) +
    geom_histogram() +
    theme_minimal() +
    labs(x = "Mark", y = "Count")
}
