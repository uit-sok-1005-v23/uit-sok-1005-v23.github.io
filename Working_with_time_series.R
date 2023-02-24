rm(list=ls())

library(Quandl)
library(tidyverse)

# Read Quandl data
browseURL("https://www.quandl.com/tools/r")

#########
# Daily crude oil prices
#########
browseURL("https://www.quandl.com/data/OPEC/ORB-OPEC-Crude-Oil-Price")

oil <- Quandl("OPEC/ORB")

# copy oil data
dframe <- oil
# as tibble
dframe <- as_tibble(dframe)
dframe

# Plots ok
plot(dframe, type="l")
dframe %>% ggplot(aes(x=Date, y=Value)) + geom_line() + theme_bw()

# Note the ordering of dates!
dframe 
tail(dframe)

# flip order on Date
dframe %>% arrange(Date) %>% head()

# Change order, so that "time" calculations are correct
# first row must be first time observation
dframe <- dframe %>% arrange(Date)

# How to measure returns
browseURL("https://en.wikipedia.org/wiki/Rate_of_return")

# Arithmetic returns, looses first observation
dframe %>% mutate(arit_ret = (Value - lag(Value))/lag(Value)) %>% head()

# Returns over time, geometric mean
# Note that we set the first observation (of this index) equal to 1
dframe %>% mutate(arit_ret = (Value - lag(Value))/lag(Value),
                  comp_arit_ret = c(1, cumprod(1+na.omit(arit_ret)))) %>% head()                  

# Logarithmic (or log-returns), commonly used in economic models
dframe %>% mutate(log_ret = log(Value) - log(lag(Value))) %>% head()
# or
dframe %>% mutate(log_ret = log(Value/lag(Value))) %>% head()
# or
dframe %>% mutate(log_ret = c(NA,diff(log(Value)))) %>% head()

# Compounding log returns
dframe %>% mutate(log_ret = log(Value/lag(Value)),
                  comp_log_ret = c(1, 1+cumsum(na.omit(log_ret)))) %>% head()


# Update dataframe with log returns
dframe <- dframe %>% mutate(log_ret = log(Value/lag(Value)),
                            comp_log_ret = c(1, 1+cumsum(na.omit(log_ret))))

# plot log returns
dframe %>% ggplot(aes(x=Date, y=log_ret)) + geom_line()

mosaic::favstats(~log_ret, data = dframe)

dframe %>% ggplot(aes(log_ret)) + geom_density()

# the daily nominal oil price since 2003
dframe %>% ggplot(aes(x=Date, y=comp_log_ret)) + geom_line()

# index comp_log_ret with the value of January 4th 2010 = 100
dframe[dframe$Date == "2010-01-04",]
dframe[dframe$Date == "2010-01-04","comp_log_ret"]$comp_log_ret

dframe %>% ggplot(aes(x=Date, y=100*comp_log_ret/dframe[dframe$Date == "2010-01-04","comp_log_ret"]$comp_log_ret)) + geom_line()

# or mutate new variable first
dframe <- dframe %>% mutate(new_comp_log_ret = 100*comp_log_ret/comp_log_ret[Date == "2010-01-04"]) 
dframe %>% ggplot(aes(x=Date, y=new_comp_log_ret)) + geom_line()


##########################
# Time series packages
##########################

# Remove dframe
rm(dframe)

ggplot(oil, aes(x=Date, y=Value)) + geom_line(color = "#20A0E0") + 
  ggtitle("Daily Crude Oil Prices") + theme_bw()

# Remember the order of dates!
oil %>% head()

# Change the order, so that calculations are correct.
# First row must be first time observation,
oil <- oil %>% arrange(Date)
head(oil)

#########
# Gold prices, US$ per troy ounce
#########
browseURL("https://www.quandl.com/data/LBMA/GOLD-Gold-Price-London-Fixing")

# The London Gold Fix involves gold dealers from London's five biggest bullion banks
# establishing a common transaction price for a large pool of purchase and sale orders.
# They do this twice each business day - first at 10:30am (the Morning Fix) and then
# again at 3pm (the Afternoon Fix).
gold <- Quandl("LBMA/GOLD")

# Arrange dates
gold <- gold %>% arrange(Date)

# Rename
names(gold) <- c("Date","USD_AM","USD_PM","GBP_AM","GBP_PM","EURO_AM","EURO_PM")

# make a copy
gold_Quandl <- gold

# Pick one price
gold <- gold %>% select(Date, USD_PM)

# daily gpld prices
ggplot(gold, aes(x=Date, y=USD_PM)) + geom_line(color = "#FC4E07") + 
  ggtitle("US$ per troy ounce") + theme_bw()


############################
# calculate monthly averages
############################

# Tsibble
browseURL("https://github.com/tidyverts/tsibble")
browseURL("https://tsibble.tidyverts.org/")

# Also tibbletime
browseURL("https://business-science.github.io/tibbletime/reference/rollify.html")
browseURL("https://blog.earo.me/2018/02/06/tsibble-or-tibbletime/")

library(tsibble)
# Coerce to a tsibble with as_tsibble() 
# To coerce a data frame to tsibble, we need to declare key and index.
# index: a variable that represents time
# key: identifying variables that define series

head(oil)
str(oil)

# Interval	Class
# --------------------
# Annual	integer/double
# Quarterly	yearquarter
# Monthly	yearmonth
# Weekly	yearweek
# Daily	Date/difftime
# Subdaily	POSIXt/difftime/hms

oil_month <- as_tsibble(oil, index = Date) %>% index_by(year_month=yearmonth(Date)) %>% # monthly aggregates
  summarise(oil_avg = mean(Value, na.rm = TRUE))

head(oil_month)
str(oil_month)

gold_month <- as_tsibble(gold, index = Date) %>% index_by(year_month=yearmonth(Date)) %>% # monthly aggregates
  summarise(gold_avg = mean(USD_PM, na.rm = TRUE))

head(gold_month)
str(gold_month)

# Merge by date, Oil is the shortest
dframe <- left_join(oil_month, gold_month, by="year_month")

head(dframe)
tail(dframe)

#############
# From wide to long to use in ggplot
# https://uc-r.github.io/tidyr
browseURL("https://blog.methodsconsultants.com/posts/data-pivoting-with-tidyr/")

dframe.long <- dframe %>% pivot_longer(-year_month,
                                       names_to = "commodity",
                                       values_to = "price")

head(dframe.long)
str(dframe.long)

dframe.long %>% ggplot(aes(x=year_month, y=price, col=commodity)) + geom_line()


##########################################################################
# Q: Rebase this plot using the Jan 2005 values=100
##########################################################################

# browseURL("https://pkg.earo.me/tsibble/")
# browseURL("https://cran.rstudio.com/web/packages/tsibble/vignettes/intro-tsibble.html")
# browseURL("https://cran.r-project.org/web/packages/tsibble/vignettes/window.html")

# Rolling 12 month correlation
browseURL("https://business-science.github.io/tibbletime/reference/rollify.html")

library(tibbletime)

# With 2 args, use the purrr syntax of ~ and .x, .y
# Rolling correlation 12 periods
cor_roll <- rollify(~cor(.x, .y), window = 12)

# use function
dframe <- dframe %>% mutate(cor_12 = cor_roll(oil_avg, gold_avg))

mean.cor <- cor(dframe$oil_avg, dframe$gold_avg)
mean.12.cor <- mean(dframe$cor_12, na.rm = TRUE)

dframe %>% ggplot(aes(x = year_month, y = cor_12)) + geom_line() + 
  geom_hline(yintercept=mean.cor, linetype="dashed", color = "red", size=2) +
  geom_hline(yintercept=mean.12.cor, linetype="dashed", color = "blue", size=2)

# 12 month moving average
# Turn the normal mean function into a rolling mean with a 12 row window
mean_roll_12 <- rollify(mean, window = 12)

dframe.long <- dframe.long %>% group_by(commodity) %>% 
  mutate(MA_12 = mean_roll_12(price))

dframe.long

dframe.long %>% ggplot(aes(x = year_month, y = MA_12, color = commodity)) + geom_line() 

# The broom equivalent for time series, sweep
browseURL("https://business-science.github.io/sweep/index.html")

browseURL("https://slides.earo.me/rstudioconf19/#1")


##########################################################################
# Q: Create a dataframe that is the weekly average prices and plot it
##########################################################################