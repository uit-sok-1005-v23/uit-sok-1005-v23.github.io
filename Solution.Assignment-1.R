
rm(list=ls())

library(tidyverse)
df_lower <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

head(df_lower)
tail(df_lower,14)

# subsetting a data frame on rows
df_lower <- df_lower[1:which(df_lower$Year %in% "Year")-1,]
tail(df_lower)
# looking at the structure of the data frame
str(df_lower)

df_lower
df_lower %>% select(Year,Mo,Globe)
df_lower %>% select(starts_with("Ocean"))

df_lower <- df_lower %>% select(Year,Mo,Globe)
df_lower
# turning a character into a numeric
df_lower$Globe <- as.numeric(df_lower$Globe)

# Create a date variable
df_lower$date <- as.Date(paste(df_lower$Year, df_lower$Mo, 1,sep="-"), "%Y-%m-%d")

df_lower
# Make a base R plot
plot(df_lower$date, df_lower$Globe, type = "l", col="red",
     main="This is my beautiful plot!", xlab="Date", ylab="Temperature in C")

ggplot(data = df_lower, aes(x=date, y=Globe)) + geom_line(color="blue")

p <- ggplot(data = df_lower, aes(x=date, y=Globe))
p + geom_line()


# Download the mid troposphere data to 
# Find the average lower and mid temperature per month
df_mid <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")

df_mid <- df_mid[1:which(df_mid$Year %in% "Year")-1,] %>% select(Year, Mo,Globe)

# turning a character into a numeric
df_mid$Globe <- as.numeric(df_mid$Globe)

# Create a date variable
df_mid$date <- as.Date(paste(df_mid$Year, df_mid$Mo, 1,sep="-"), "%Y-%m-%d")

# Merge or combine the two data frame 
df_lower;df_mid
head(merge(df_lower,df_mid, by = "date"),10)
head(merge(df_lower,df_mid, by = c("Year","Mo")),10)

# Wide data frame
df_lower;df_mid
df <- left_join(df_lower,df_mid, by="date")
df

# Any redundant variables?
df$Year.x <- NULL
df$Mo.x <- NULL
df

names(df) <- c("Globe.Lower.Trop","date","Year","Month","Globe.Mid.Trop")
names(df)
df

# calculate the mean 
mutate(df, meanTemp = (Globe.Lower.Trop+Globe.Mid.Trop)/2,
       mean.Temp = rowMeans(select(df, starts_with("Globe"))))


#####################################################

# Create a long data frame from the 4 data sets
# use a common variable name, eg, measurement using the mutate fn.
rm(list=ls())

df_1 <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
df_2 <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
df_3 <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
df_4 <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

df_1 <- df_1[1:which(df_1$Year %in% "Year")-1,c(1:3)]
df_2 <- df_2[1:which(df_2$Year %in% "Year")-1,c(1:3)]
df_3 <- df_3[1:which(df_3$Year %in% "Year")-1,c(1:3)]
df_4 <- df_4[1:which(df_4$Year %in% "Year")-1,c(1:3)]

df_1 <- df_1 %>% mutate(measurement="Lower_Troposphere")
df_2 <- df_2 %>% mutate(measurement="Mid_Troposphere")
df_3 <- df_3 %>% mutate(measurement="Tropopause")
df_4 <- df_4 %>% mutate(measurement="Lower_Stratosphere")

df_1;df_2;df_3;df_4

longdf <- bind_rows(df_1,df_2,df_3,df_4)

longdf$date <- as.Date(paste(longdf$Year, longdf$Mo, 1, sep="-"), "%Y-%m-%d")
longdf 

longdf$Globe <- as.numeric(longdf$Globe)

ggplot(data = longdf, aes(x=date, y=Globe, group=measurement)) + 
  geom_line(aes(col= measurement))


###################################
# calculate the average of the four 

longdf

# first remove the obs in 1978
longdf %>%
  filter(Year!=1978) %>%
  group_by(measurement) %>%
  mutate(Average = zoo::rollmean(Globe, 13, fill = NA, align = "right")) %>% 
  filter(date > "1979-12-01") %>%
  ggplot(aes(x = date, y= Average, col= measurement))+ geom_line()
 


# calculate the average-grand and plot all the five averages in one plot
longdf %>%
  filter(Year!=1978) %>%
  group_by(measurement) %>%
  mutate(Average = zoo::rollmean(Globe, 13, fill = NA, align = "right")) %>%
  filter(date > "1979-12-01") %>% 
  select(date, measurement,Average) %>%  
  pivot_wider(names_from = measurement,values_from = Average) %>% 
  mutate(Average_grand  = (Lower_Troposphere+Mid_Troposphere+ Tropopause+ Lower_Stratosphere)/4,
         Average_grand2 = rowMeans(select(., -date))) %>% 
  select(-Average_grand2) %>% 
  pivot_longer(-date, names_to = "Location",values_to = "Average") %>% 
  ggplot(aes(x=date, y=Average, col=Location))+geom_line()



####################
# Another try ####
####################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(zoo)

# Here are the url's
# In essence, we want to do the same operations 4 times
# "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"
# "https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt"
# "https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt"
# "https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
# "Lower-Troposphere"
# "Mid-Troposphere"
# "Tropopause"
# "Lower-Stratosphere"

# what to do to write more general code?
# note that the data is now a "."
test <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt") 
test <- test %>% .[1:which(.$Year %in% "Year")-1, ]
tail(test,15)

#str(test)

# selecting on columns and mutate
test <- test %>% .[ , 1:3] %>% 
  mutate(Date = ymd(paste(.$Year, .$Mo, 1, sep="-"))) %>% 
  select(Date, Globe) %>% 
  mutate_if(is.character, ~as.numeric(.)) %>%
  mutate(Globe=rollmean(Globe, 13, fill=NA, align="right"))

head(test, 15)

rm(test)

# Lets create our own function that does the procedure, even more general, without an object name.
# replace the data object with . in the pipe

scrape <- function(url) {
  return(read_table2(url) %>% # read data frame
           .[1:which(.$Year %in% "Year")-1, ] %>% # subsetting on rows
           .[ , 1:3] %>%  # selecting on columns
           mutate(Date = ymd(paste(.$Year, .$Mo, 1, sep="-"))) %>% 
           select(Date, Globe) %>% 
           mutate_if(is.character, ~as.numeric(.)) %>%
           mutate(Globe=rollmean(Globe, 13, fill=NA, align="right")))
}

lt <- scrape("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
head(lt, 15)

rm(lt, scrape)

# now think how we would like to have the final long data frame
#browseURL("https://www.youtube.com/watch?v=sLF31AY25so")
# need to have a variable identifying the where the temperature was measured

scrape.bake <- function(url, location) {
  return(read_table2(url) %>%
           .[1:which(.$Year %in% "Year")-1, ] %>%
           .[ , 1:3] %>% 
           mutate(Date = ymd(paste(.$Year, .$Mo, 1, sep="-"))) %>% 
           select(Date, Globe) %>% 
           mutate_if(is.character, ~as.numeric(.)) %>%
           mutate(Globe=rollmean(Globe, 13, fill=NA, align="right"),
                  Location=paste0(location)))
}


lot <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", "Lower-Troposphere")
mit <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt", "Mid-Troposphere")
trp <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt", "Tropopause")
los <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt", "Lower-Stratosphere")

head(lot, 15)

#bind all the 4 data frame 
longdf <- bind_rows(lot,mit,trp,los)

head(longdf,20)

# Plot
longdf %>%
  mutate(Year=year(Date)) %>% 
  filter(Year >= 1980) %>%
  ggplot(aes(x=Date, y=Globe, group=Location)) +
  geom_line(aes(color=Location)) +
  ylab(expression("Temperature ("*~degree*C*")")) +
  xlab("") +
  labs(title = "Global Temperature Development",
       subtitle = "Temperature departure from 1981-2010 average",
       caption = "Series are 12-month moving averages, right aligned.") +
  theme_linedraw()



