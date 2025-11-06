library(dplyr)

mtcars |>
  filter(cyl == 4) |>
  group_by(am) |>
  summarise(avg_mpg = mean(mpg))

calculate_average_mpg <- function(cyl) {
  mtcars |>
    filter(cyl == !!cyl) |> #! AI is not clever here
    group_by(am) |>
    summarise(avg_mpg = mean(mpg))
}

calculate_average_mpg(4)

library(microbenchmark)
?microbenchmark
microbenchmark(
  calculate_average_mpg(4),
  calculate_average_mpg_base(4)
)
microbenchmark(
  calculate_average_mpg(4),
  calculate_average_mpg_base(4),
  unit = "ms"
)

calculate_average_mpg <- function(cyl) {
  browser()
  mtcars |>
    filter(cyl == !!cyn) |>
    group_by(am) |>
    summarise(avg_mpg = mean(mpg))
}
calculate_average_mpg(4)
