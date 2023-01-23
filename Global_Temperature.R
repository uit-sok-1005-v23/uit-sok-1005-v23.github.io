
# Global Temperature
# A comment
rm(list=ls())

library(tidyverse)

df_lower <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
# We would like remove the comments in the bottom

head(df_lower)
tail(df_lower, 20)

# Selecting a vector using $
df_lower$Year

# Subsetting on a data frame
# df_lower[rows, columns]

#############
# base R
#############

# select coulmn 1
df_lower[ , 1]
# select column based on name
df_lower[ , "Year"]

# select both length and vector
df_lower[1:10 , 1]

# subset data on rows, correct but not dynamic!
df_lower[1:529,]
tail(df_lower[1:529, ])

# updated the data frame
 df<- df_lower[1:500, ]

# comments starts on $Year equal to "Year", write code that is dynamic
which(df_lower$Year %in% "Year")

# dynamic select
df_lower <- df_lower[1:which(df_lower$Year %in% "Year")-1, ]

# This works!
tail(df_lower)

# Work with the first 5 columns, base R
df_base <- df_lower[ , 1:5]
df_base 
# Create a Date variable, using base R
# uses the first of each month as day
df_base$Date <- as.Date(paste(df_base$Year, df_base$Mo, 1, sep="-"), format = "%Y-%m-%d")
head(df_base)

#paste(df_base$Year, df_base$Mo, 1, sep="-")

####################
### Tidyverse
####################

# Selecting on columns using tidyverse, pipe to console
df_lower %>% select(Year, Mo, Globe, Land, Ocean)

# Selecting on columns using tidyverse, create date variable, pipe to console
library(lubridate)

# Using mutate and lubridate::ymd to make a date variable, note no need for "format" option
df_lower %>% select(Year, Mo, Globe, Land, Ocean) %>% 
  mutate(Date = ymd(paste(df_lower$Year, df_lower$Mo, 1, sep="-")))

# Selecting on columns using tidyverse, create date variable, pipe to console, update dataframe
df_tidy <- df_lower %>% 
  mutate(Date = ymd(paste(df_lower$Year, df_lower$Mo, 1, sep="-"))) %>% 
  select(Date, Globe, Land, Ocean) 

df_tidy

df_tidy %>% mutate(Year = year(Date),
                   Month = month(Date),
                   Month2 = month(Date, label = TRUE, abbr = FALSE))

# update
df_tidy <- df_tidy %>% mutate(Year = year(Date),
                              Month = month(Date),
                              Month2 = month(Date, label = TRUE, abbr = FALSE))

df_tidy <- df_tidy %>% select(Year, Month, Date, Globe, Land, Ocean)
df_tidy 

#####################################
### Session 2 on Global temperature
#####################################

# looking at the structure of the data frame
# No numbers all characters, except date
str(df_base)
str(df_tidy)

# chr? mean characters or letters

# turning a character into a numeric, base R, tedious
df_base$Globe <- as.numeric(df_base$Globe)
str(df_base)
# num means numeric


# Use tidyverse
str(df_tidy)

# turning a character into a numeric, tidyverse, many variables at the same time
df_tidy <- df_tidy %>% mutate_at(vars(Globe, Land, Ocean), ~as.numeric(.))
str(df_tidy)

# even more general, all data wrangling in one sweep
df_lower %>% select(Year, Mo, Globe, Land, Ocean) %>% 
  mutate(Date = ymd(paste(df_lower$Year, df_lower$Mo, 1, sep="-"))) %>% 
  mutate(Year = year(Date), Month = month(Date)) %>%
  select(Year, Month, Date, Globe, Land, Ocean) %>%  # this makes the order of the variables
  mutate_if(is.character, ~as.numeric(.)) # note generic is.character means all character in data

# data manipulation, select on rows dplyr::filter()
df_tidy %>% filter(Year >= 2000)

# summarise using group_by
# we could assign it to a new data frame
df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>% summarise(Average.Temp=mean(Globe))
df_tidy %>% filter(Year > 1978)
df_tidy %>% filter(Year >= 1979)

# mutate, make new variable
df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>% mutate(Average.Temp=mean(Globe)) %>%
  ggplot(aes(x=Date, y=Globe)) + geom_line() +
  geom_line(aes(x=Date, y=Average.Temp), col="blue")

# help
?zoo::rollmean

df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>% mutate(Average.Temp=mean(Globe)) %>% 
  ggplot(aes(x=Date, y=Globe)) + geom_line() +
  geom_line(aes(y=zoo::rollmean(Globe, 13, fill=NA)), col="red", size=1.3) 

library(zoo)
df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>% mutate(Average.Temp=mean(Globe)) %>% 
  ggplot(aes(x=Date, y=Globe)) + geom_line() +
  geom_line(aes(y=rollmean(Globe, 13, fill=NA)), col="red", size=1.3) 


# change color of line, add points
df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>%  
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(col="lightblue") + geom_point(col="blue") +
  geom_line(aes(y=zoo::rollmean(Globe, 13, fill=NA)), col="red", size=1.3) 

# add labels, title
df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>% 
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(col="lightblue") + geom_point(col="lightblue") +
  geom_line(aes(y=zoo::rollmean(Globe, 13, fill=NA)), col="red", size=1.3) +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + 
  xlab("Time") 

# make a plot into an object
p1 <- df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>% 
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(col="lightblue") + geom_point(col="lightblue") +
  geom_line(aes(y=zoo::rollmean(Globe, 13, fill=NA)), col="red", size=1.3)

p1

p1 + ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Time") 

# One plot per year
df_tidy %>% filter(Year != 1978) %>% group_by(Year) %>% 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(col="lightblue") + geom_point(col="lightblue") +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Time") + facet_wrap(~Year)

# Keep the number of objects/data frames and variables under control!

# Monthly, one line per year, note that Year a numeric,
#has to be a factor, in order to make one line per year
df_tidy %>% filter(Year != 1978) %>% 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(aes(colour=as.factor(Year))) +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Month") 

df_tidy %>% filter(Year != 1978) %>% 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(aes(colour=Year)) +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Month")

# Task: How do we remove as.factor(Year) into Year?
df_tidy %>% filter(Year != 1978) %>% 
  mutate(Year=as.factor(Year)) %>% 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(aes(colour=Year)) +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Month") 


# Add the other temperatures, note no "labels"
df_tidy %>% filter(Year != 1978) %>%
  ggplot(aes(x=Date, y=Globe)) + geom_line(col="blue") +
  geom_line(aes(x=Date, y=Land), col="red") +
  geom_line(aes(x=Date, y=Ocean), col="orange")


# Wide data to long data
df_tidy # wide
df_tidy %>% filter(Year != 1978) %>% select(Date, Globe, Land, Ocean) %>% 
  pivot_longer(-Date, names_to = "Location", values_to = "Temperature") # long

# same as above, but with labels on location
df_tidy %>% filter(Year != 1978) %>% select(Date, Globe, Land, Ocean) %>% 
  pivot_longer(-Date, names_to = "Location", values_to = "Temperature") %>% 
  ggplot(aes(x=Date, y=Temperature, group=Location)) + 
  geom_line(aes(color=Location)) 


# Make one "long" plot of the averages per year, add a trend line (regression)
df_tidy %>% filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Globe=mean(Globe)) %>% 
  ggplot(aes(x=Year, y=Globe)) + geom_line() +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Year") +
  geom_smooth(method = lm, se = FALSE)


# Analysis - Global temperature
library(broom)

 df_tidy %>% filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Globe=mean(Globe)) %>%
  ungroup() %>% 
  do(model = lm(Globe ~ Year, data = .)) -> fit

 fit <- tidy(fit$model[[1]])

fit
0.2/fit$estimate[2] # years for average global temperature to increase 0.2 degree C

# Data Science?
el.nino.years <- c(1982,1983,1986,1987,1991,1992,1993,1994,1997,1998,2002,2003,2006,2007,2015,2016)

df_tidy %>% filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Globe=mean(Globe)) %>%
  ungroup() %>% 
  ggplot(aes(x=Year, y=Globe)) + geom_line(col="blue") +
  geom_vline(xintercept = as.numeric(el.nino.years), linetype=4) +
  ggtitle("Yearly global temperature and el nino years")

# Replicate the analysis above with the yearly sea temperature?

df_tidy %>% filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Ocean=mean(Ocean)) %>%
  ungroup() %>% 
  ggplot(aes(x=Year, y=Ocean)) + geom_line(col="red") +
  geom_vline(xintercept = as.numeric(el.nino.years), linetype=4) +
  ggtitle("Yearly ocean temperature and el nino years")
