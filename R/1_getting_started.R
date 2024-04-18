# Coffee and code - week 1
# getting started with R

# Sys.Date()
# [1] "2024-04-18"

# if you open this file in RStudio and want to execute the code, just insert
# your cursor into any line then hold `Ctrl` and hit enter to execute the line.

# vectors ----------------------------------------------------------------------
# a one-dimensional data structure
LETTERS
letters
1:10
1:100

## concatenate  ----------------------------------------------------------------
# a simple function for everyday use! Functions are called by typing the function
# name, then a pair of parentheses, and then some information for the function to
# act on.
c(2, 4, 6, 8, 10)

## assignment ------------------------------------------------------------------
# storing something (to a name)
evens <- c(2, 4, 6, 8, 10)
# alt- (alt-minus) keyboard shortcut for this operator
evens

## indexing --------------------------------------------------------------------
# how to get at values
LETTERS
LETTERS[5]
LETTERS[c(2, 4, 6, 8, 10)]
LETTERS[evens]

# matrices ---------------------------------------------------------------------
# a 2D data structure
matrix(data = 1:10, ncol = 2)
mymat <- matrix(data = 1:10, ncol = 2)
mymat[1,1]

# dataframes -------------------------------------------------------------------
# data tables (essentially extend matrices) where we can store and access data
mtcars
mtcars[1,1]
mtcars$mpg # returns a vector of values stored in the first field/variable in the mtcars dataframe
mtcars[["mpg"]] # equivalent to line above

# plotting ---------------------------------------------------------------------
plot(x = mtcars$cyl, y = mtcars$mpg) # dotplot
split(mtcars$mpg, mtcars$cyl) |> boxplot() # ah, that's better :)
