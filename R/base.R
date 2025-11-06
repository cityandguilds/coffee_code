
subset_data <- mtcars[mtcars$cyl == 4, ]
aggregate(mpg ~ am, data = subset_data, FUN = mean)

calculate_average_mpg_base <- function(cyl) {
  subset_data <- mtcars[mtcars$cyl == cyl, ]
  aggregate(mpg ~ am, data = subset_data, FUN = mean)
}

calculate_average_mpg_base(4)

calculate_average_mpg_base <- function(cyl) {
  browser()
  subset_data <- mtcars[mtcars$cyl == cyn, ]
  aggregate(mpg ~ am, data = subset_data, FUN = mean)
}
calculate_average_mpg_base(4)
