
# The last session in relation to R

# In this code, you will learn:
# - about the map() fun from the Purr package.
#   Purrr is one of those tidyverse packages.
#   At it’s core, purrr is all about iteration
# - import multiple .csv files in R


rm(list = ls())
library(tidyverse)

# apply a function to each element of a list or atomic vector
# map(.x, .f) is the main mapping function and returns a list
# map_df(.x, .f) returns a data frame
# map_dbl(.x, .f) returns a numeric (double) vector
# map_chr(.x, .f) returns a character vector
# map_lgl(.x, .f) returns a logical vector


###################################################
#define a data-frame 
data <- data.frame(x1=c(1, 2, 3),
             x2=c(4, 5, 6),
             x3=c(7, 8, 9))
# Calculate the mean of each element of the data-frame 
data %>% summarise(mean_x1=mean(x1),
                   mean_x2=mean(x2),
                   mean_x3=mean(x3))

# In one sweep, using the function map()
data %>% map(mean)  #output a list
data %>% map_dbl(mean) #output an atomic vector

# or 
map(.x=data, .f=mean)
map_dbl(.x=data, .f=mean)

# Calculate the mean of each element of the list 
my_list <- list(c(1,2,3),
                c(4,5,6),
                c(7,8,9))

# Find the mean of each elements of the list, tedious approach 
my_list %>% .[[1]] %>% mean
my_list %>% .[[2]] %>% mean
my_list %>% .[[3]] %>% mean

#using the map fun
my_list %>% map(mean) # the map apply the fun to each element of the list
my_list %>% map_dbl(mean)

# Q Add 10 to each element of the list
my_list %>% map(function(x){x+10})
my_list %>% map(~.x+10) #shorthand function


# 5 times, 3 random normal numbers
1:5 %>%
  map(rnorm, n = 3) %>%  # output a list
  map_dbl(mean)          # output an atomic vector

# using set_names() with character vectors
set_names(c("high", "low","verylow")) %>% map_chr(paste0, "_category")

# working with lists
favorite_desserts <- list(Sophia = "ice cream", Eliott = "jello", Karina = "chocolate cake")
favorite_desserts %>% map_chr(~ paste(.x, "rocks!"))

# split a data frame into pieces, fit a
# model to each piece, summarise and extract R^2
mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# if each element of the output is a data frame, use
# map_dfr to row-bind them together:
mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map_dfr(~ as.data.frame(t(as.matrix(coef(.)))))


library(gapminder)
# gapminder data info, apply the class() function to each column 
gapminder %>% 
  map_chr(class) # we use map:chr() since the output of the class() fun is a character

# unique/distinct items/values in each column 
gapminder %>% 
  map_dbl(n_distinct)

# apply both the class() and n_distinct() function and
# return the results in data frame
gapminder %>% 
  map_df(~data.frame(n_distinct = n_distinct(.x),
                     class = class(.x)))

# Note that we have lost the variable names/ the column names.
gapminder %>%
  map_df(~data.frame(n_distinct = n_distinct(.x),
                      class = class(.x)),
                     .id = "variable")

########################################
# Maps with multiple input objects
####################################
#For instance, what if you want to perform a map that iterates through two objects. 
# the map function that maps over two objects instead of 1 is called map2().

#The first two arguments are the two objects you want to iterate over, and the third is the function
map2(.x = object1, # the first object to iterate over
     .y = object2, # the second object to iterate over
     .f = Function(.x, .y))

# pmap() allows you to iterate over an arbitrary number of objects (i.e. more than two)

# Example
d=tribble(~p, ~q,
          10, 3,
          11, 6,
          12, 4,
          13, 1)
# For example for some reason, we want to calculate 3 times p minus q
d %>% mutate(z=3*p-q) # this is the obvious way

#Using map function 
d %>% mutate(z=map2(.x = p,
                    .y = q,
                    .f = ~{3*.x-.y}))

d %>% mutate(z=map2_dbl(.x = p,
                    .y = q,
                    .f = ~{3*.x-.y}))
#d %>% mutate(z=map2_dbl(p,q,function(p,q) {3*p-q}))



# Q. Create a list of plots that compare life expectancy and GDP per capita 
# for each continent/year combination

#First, you need to define a vector (or list) of continents and
# a paired vector (or list) of years that you want to iterate through.

# all distinct combinations of continents and years in the data
continent_year <- gapminder %>% 
                 distinct(continent, year)

# extract the continent and year pairs as separate vectors
continents <- continent_year %>% 
             pull(continent) %>% as.character

years <- continent_year %>% pull(year)

# the first in each vector (just for the first iteration, the first continent, and the first year)
.x <- continents[1]
.y <- years[1]

# make a scatter-plot of GDP vs life expectancy in all Asian countries for 1952
gapminder %>% 
  filter(continent == .x,
         year == .y) %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp)) +
  ggtitle(glue::glue(.x, " ", .y))

# A list of plots
plot_list <- map2(.x = continents, 
                  .y = years, 
                  .f = ~{
                    gapminder %>% 
                      filter(continent == .x,
                             year == .y) %>%
                      ggplot() +
                      geom_point(aes(x = gdpPercap, y = lifeExp)) +
                      ggtitle(glue::glue(.x, " ", .y))
                  })

plot_list[[1]]
plot_list[[60]]


#
# list columns and nested data frames
gapminder_nested <- gapminder %>% 
                    group_by(continent) %>% 
                    nest()

# extract the first entry from the data column
gapminder_nested$data[[1]]

# Using dplyr pluck() function, this can be written as
gapminder_nested %>% 
  pluck("data", 1)

#'Similarly, the 5th entry in the data column corresponds to 
#'the entire gapminder dataset for Oceania.
gapminder_nested %>% pluck("data", 5)

# Why we would ever want to nest our data frame, 
# while we can use dplyr function such as filter?

# mutate() are applied directly to the entire column (which is usually a vector)
tibble(vec_col = 1:10) %>%
  mutate(vec_sum = sum(vec_col))

# when the column is a list, vectorized functions donot know what to do with them, 
#and we get an error
tibble(list_col = list(c(1, 5, 7), 
                       5, 
                       c(10, 10, 11))) %>%
  mutate(list_sum = sum(list_col))

# to apply mutate functions to a list-column, use map
tibble(list_col = list(c(1, 5, 7), 
                       5, 
                       c(10, 10, 11))) %>%
  mutate(list_sum = map(list_col, sum))

# map() returns a list itself, the list_sum column is thus itself a list
tibble(list_col = list(c(1, 5, 7), 
                       5, 
                       c(10, 10, 11))) %>%
  mutate(list_sum = map(list_col, sum)) %>% 
  pull(list_sum)

# what if we wanted it to be a vector? use map_dbl()
tibble(list_col = list(c(1, 5, 7), 
                       5, 
                       c(10, 10, 11))) %>%
  mutate(list_sum = map_dbl(list_col, sum))

# mutate on average lifeExp per continent 
gapminder_nested

gapminder_nested %>% 
  mutate(avg_lifeExp = map_dbl(data, ~{mean(.x$lifeExp)}))

#Calculate the correlation b/n lifeExp and gdpPercap
gapminder_nested %>% 
  mutate(cor.test =map_dbl(data,~cor(.x$lifeExp,.x$gdpPercap)))

# Similar to, but without "data"
#mean 
gapminder %>% group_by(continent) %>% 
  summarise(lifeExp=mean(lifeExp))

#correlation 
gapminder %>% group_by(continent) %>% 
  summarise(cor.test=cor(lifeExp,gdpPercap))

#' The next example will demonstrate how to fit a model separately 
#' for each continent, and evaluate it, all within a single tibble.
# fit a model separately for each continent
gapminder_nested <- gapminder_nested %>% 
  mutate(lm_obj = map(data, ~lm(lifeExp ~ pop + gdpPercap + year, data = .x)))
gapminder_nested

#Where the first linear model (for Asia) is
gapminder_nested %>% pluck("lm_obj", 1)

# predict the response for each continent,
# using the corresponding linear model. 
gapminder_nested <- gapminder_nested %>% 
  mutate(pred = map2(lm_obj, data, function(.lm, .data) predict(.lm, .data)))
gapminder_nested

# calculate the correlation between observed and predicted response for each continent
gapminder_nested <- gapminder_nested %>% 
  mutate(cor = map2_dbl(pred, data, function(.pred, .data) cor(.pred, .data$lifeExp)))
gapminder_nested

# Advanced exercise
#' Fit a separate linear model for each continent without splitting up the data. 
#' Report the your results in data frame, where the data frame  should contain 
#' the continent, each term in the model for the continent, its linear model coefficient 
#' estimate, and standard error.
gapminder %>% 
  group_by(continent) %>% 
  nest() %>%
  mutate(lm_obj = map(data, ~lm(lifeExp ~ pop + year + gdpPercap, data = .))) %>%
  mutate(lm_tidy = map(lm_obj, broom::tidy)) %>%
  ungroup() %>%
  transmute(continent, lm_tidy) %>%
  unnest(cols = c(lm_tidy))


# More resources 
browseURL("https://www.rebeccabarter.com/blog/2019-08-19_purrr/")
browseURL("https://aosmith.rbind.io/2018/06/05/a-closer-look-at-replicate-and-purrr/")
browseURL("https://www.r-bloggers.com/2020/05/one-stop-tutorial-on-purrr-package-in-r/")
browseURL("http://ritsokiguess.site/docs/2018/04/19/purrr-and-map-an-introduction/")
browseURL("https://rpubs.com/cliex159/867722")
browseURL("http://joshuamccrain.com/tutorials/purrr/purrr_introduction.html")


################################
# Reading many csv file 
############################
rm(list = ls())
library(tidyverse)  
library(lubridate)
library(fs)
browseURL("https://www.rdocumentation.org/packages/fs/versions/1.5.0")
browseURL("https://cran.r-project.org/web/packages/vroom/vignettes/vroom.html")

# the location of the unzipped folder in data_dir.
data_dir <- "C:/Users/dki007/Desktop/many_csv_files"

# List the csv files 
fs::dir_ls(data_dir)

#Only csv file 
csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")
csv_files

# Importing one file 
readr::read_csv(csv_files[7])

# Importing all the files 
csv_files %>% 
  map_dfr(read_csv)

# Modify the setting 
csv_files %>% 
  map_dfr(read_csv, col_types = cols("Month_Year" = col_date(format = "%b-%y"))) 

# View the data
csv_files %>% 
  map_dfr(read_csv, col_types = cols("Month_Year" = col_date(format = "%b-%y"))) %>% View()

# Why some of the dates become NA's?
#Look at the format of the first date(i.e.,Aug-15 ) and last one(21-Nov)
csv_files %>% 
  map_dfr(read_csv) %>% View()

# Fix date parsing 
csv_files %>% 
  map_dfr(read_csv) %>% 
  mutate(Month_Year_1=my(Month_Year)) %>% 
  mutate(Month_Year_2 = if_else(is.na(Month_Year_1),ym(Month_Year),Month_Year_1)) 

# deselect Month_Year_1 and View the data
csv_files %>% 
  map_dfr(read_csv) %>% 
  mutate(Month_Year_1=my(Month_Year)) %>% 
  mutate(Month_Year_2 = if_else(is.na(Month_Year_1),ym(Month_Year),Month_Year_1)) %>% 
  select(-Month_Year_1) %>% View()

# Add a source indicator 
csv_files %>% 
  map_dfr(read_csv,.id = "source") %>% 
  mutate(Month_Year_1=my(Month_Year)) %>% 
  mutate(Month_Year_2 = if_else(is.na(Month_Year_1),ym(Month_Year),Month_Year_1)) %>% 
  select(-Month_Year_1)


#Here’s the code we used, all in one place.
data_dir %>% 
  dir_ls(regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source") %>% 
  mutate(Month_Year_1=my(Month_Year)) %>% 
  mutate(Month_Year_2 = if_else(is.na(Month_Year_1),ym(Month_Year),Month_Year_1)) %>%
  select(-Month_Year_1) %>% 
 View()











