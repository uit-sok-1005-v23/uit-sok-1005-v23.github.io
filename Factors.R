
# Factors
# the forcats package in tidyverse
rm(list = ls())
library(tidyverse)

library(gapminder)

data("gapminder")
gapminder
summary(gapminder)

# factor inspection
gapminder$continent
class(gapminder$continent)
levels(gapminder$continent)
nlevels(gapminder$continent)
str(gapminder$continent)

# to get a frequency table as a tibble
gapminder %>% 
  count(continent)

# from a factor, use forcats::fct_count()
fct_count(gapminder$continent)

# dropping unused levels
gapminder$country
nlevels(gapminder$country)

nordic_countries <- c("Norway", "Sweden", "Denmark", "Finland", "Iceland")

nordic <- gapminder %>%
  filter(country %in% nordic_countries)

nordic$country
nlevels(nordic$country) # same as above

nordic_dropped <- nordic %>% droplevels()
nordic_dropped$country
nlevels(nordic_dropped$country)

# use forcats::fct_drop() on a factor
nordic$country %>% 
  fct_drop() %>%
  levels()

# change order of the levels
# default order is alphabetical
gapminder$continent %>% levels()

# order by frequency
gapminder$continent %>% 
  fct_infreq() %>%
  levels()

# backwards
gapminder$continent %>% 
  fct_infreq() %>%
  fct_rev() %>% 
  levels()

# order nordic countries by median life expectancy
fct_reorder(nordic_dropped$country, nordic_dropped$lifeExp) %>% 
  levels() %>% head()

# order nordic countries according to minimum life exp instead of median
fct_reorder(nordic_dropped$country, nordic_dropped$lifeExp, min) %>% 
  levels() %>% head()

# backwards
fct_reorder(nordic_dropped$country, nordic_dropped$lifeExp, .desc = TRUE) %>% 
  levels() %>% head()

# we reorder factor levels because it often makes plots much better
nordic_dropped %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp)) %>%
  ggplot(aes(x = lifeExp, y = country)) + geom_point()

nordic_dropped %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp)) %>%
  ggplot(aes(x = lifeExp, y = fct_reorder(country, lifeExp))) + 
  geom_point()


# use fct_reorder2() when you have a line chart of a quantitative x against another quantitative y
# and your factor provides the color. This way the legend appears in some order as the data!
# Contrast the legend on the left with the one on the right.

nordic_dropped %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line()

nordic_dropped %>% 
  ggplot(aes(x = year, y = lifeExp,
             color = fct_reorder2(country, year, lifeExp))) +
  geom_line() + labs(color = "country")

# change order of the levels
nordic_dropped$country %>% levels()

nordic_dropped$country %>% 
  fct_relevel("Norway", "Sweden") %>% levels()

# recode the levels
nordic_dropped$country %>% levels()

nordic_dropped$country %>%
  fct_recode("Norge" = "Norway", 
             "Island" = "Iceland",
             "Sverige" = "Sweden", 
             "Danmark" = "Denmark") %>% levels()

# two data frames, each with data from two countries,
# dropping unused factor levels
df1 <- gapminder %>%
  filter(country %in% c("United States", "Mexico"), year > 2000) %>%
  droplevels()
df2 <- gapminder %>%
  filter(country %in% c("France", "Germany"), year > 2000) %>%
  droplevels()

# the country factors in df1 and df2 have different levels
levels(df1$country)
levels(df2$country)

# combine them?
df1$country
df2$country

c(df1$country, df2$country)

# use fct_c() to do this
fct_c(df1$country, df2$country)

# adding rows with factors
bind_rows(df1, df2) # dplyr

rbind(df1, df2) # base R

