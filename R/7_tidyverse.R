#### Thinking explicitly about the tidyverse

# https://tidyverse.tidyverse.org/articles/paper.html
# https://r4ds.hadley.nz/

# subsets ----------------------------------------------------------------------
mtcars
mtcars[mtcars$cyl == 4,]

# install.packages("dplyr")
library(dplyr)

mtcars |> filter(cyl == 4)

identical(
  mtcars[mtcars$cyl == 4,],
  mtcars |> filter(cyl == 4)
)

# new features -----------------------------------------------------------------
ourcars <- mtcars

ourcars$new_var <- "new"
ourcars

ourcars$is4 <- ifelse(ourcars$cyl == 4, TRUE, FALSE)
ourcars

identical(
  ourcars[ourcars$is4, ],
  ourcars |> filter(cyl == 4)
)
ourcars |> filter(is4)

mtcars |>
  mutate(
    new_var = "new",
    is4 = ifelse(cyl == 4, TRUE, FALSE)
  )
mtcars

newcars <- mtcars |>
  mutate(
    new_var = "new",
    is4 = ifelse(cyl == 4, TRUE, FALSE)
    )

identical(ourcars, newcars)

# dropping variables -----------------------------------------------------------
newcars[, 1:11]
newcars[, -c(12, 13)]

newcars |> select(mpg:carb)
newcars |> select(-is4, -new_var)
newcars |> select(-c(is4, new_var))

identical(
  newcars[, 1:11],
  newcars |> select(-is4, -new_var)
)
