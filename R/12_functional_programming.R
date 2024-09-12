
library(dplyr)

# example of how neat functions help with repeated tasks
mtcars |>
  count(cyl)

mtcars |>
  group_by(cyl) |>
  summarise(count = n()) |>
  ungroup()

identical(
  mtcars |>
    count(cyl),
  mtcars |>
    group_by(cyl) |>
    summarise(n = n()) |>
    ungroup() |>
    as.data.frame()
)

# str is a base R way to inspect your R objects
str(mtcars)
str(
  mtcars |>
    group_by(cyl) |>
    summarise(n = n()) |>
    ungroup()
)
str(mtcars |> count(cyl))

# you can type a function name without the parenthesis to see the code
count

# an example of writing a (very bad) convenience function
filter_cylgear3 <- function(cyl_num) {
  mtcars |>  dplyr::filter(cyl == cyl_num, gear > 3)
}

filter_cylgear3(cyl_num = 4)
filter_cylgear3(cyl_num = 6)

# documenting your function with Roxygen2
# install.packages("Roxygen2")

#' Filter mtcars based on number of cylinders and gears
#'
#' joiajonfjahh uaifihuiwhai o houiiyhaoifh haouihf ohahf fojhafhiuahofifhoii
#' haoiagh aghiuf h
#'
#' @param cyl_num An integer indicating the number of cylinders to filter for
#' @param gear_num An integer giving threshold above which to filter on the gear
#'   variable.
#'
#' @examples
#' filter_cylgear(cyl_num = 4, gear_num = 3)
#' filter_cylgear(cyl_num = 6, gear_num = 3)
filter_cylgear <- function(cyl_num, gear_num) {
  mtcars |>  dplyr::filter(cyl == cyl_num, gear > gear_num)
}
