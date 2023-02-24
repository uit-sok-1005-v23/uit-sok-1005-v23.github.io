
# Source. R for data science book

library(tidyverse)
library(lubridate)
#install.packages("nycflights13")
library(nycflights13)

#' Current date or date-time 
today()
now()

# There are three ways we are likely to create a date/time

### 1). From strings 

ymd("2022-12-17")
#'We can also change the order of the letters in ymd()
#' to match which information came first (year, month, or day). 
dmy("17-December-2022")
dmy("28 Feb, 2023")
dmy("17st-12-2022")

mdy("December 17st,2022")

#'These functions also take unquoted numbers
dmy(17122022)
myd(12202217)

#' If we do choose to only write a series of number,
#'  then our string can be unquoted as well.
dmy("17122022")

# Task. Use the appropriate lubridate function to parse each of the following dates
c("August 19 (2015)", "July 1 (2015)")
mdy(c("August 19 (2015)", "July 1 (2015)"))

# Q. What happens if you parse a string that contains invalid dates?
ymd(c("2010-10-10", "bananas"))


#' We can also generate hour, minute, and 
#' second information by using ymd_hms(). Different than
#' the functions above, we cannot change the order of “hms.”

ymd_hms("20221217,21:33:45")
ymd_hms("2022/12/17,21/33/45")

ymd_hm("20221217,21:33")
ymd_h("20221217,21")

#You can also force the creation of a date-time from 
#a date by supplying a timezone:

#' In R, the default time zone is Coordinated Universal
#' Time, or UTC for short. But we can also change the 
#' time zone of our date/time data by using the tz 
#' argument like so:
ymd(20170131, tz = "UTC")
ymd("2022/12/17", tz = "UTC")
ymd_hms("2021-06-20-5-49-34", tz = "America/Vancouver")

#' full list of time zones that R recognizes
head(OlsonNames(), 20)

#' check our current time zone.
Sys.timezone()

#' we can also tell R directly what our time zone is. 
#' For example, if we are in Vancouver, BC, Canada right now,
#' we would write the following code:
Sys.setenv(tZ = "America/Vancouver")
Sys.timezone()

#' confirm if the correct time zone has been set
today() #only the current date
now()  # gives us both the date and time of our current location


# You may want to switch between a date-time and a date.
#'We can actually convert date data to datetime and vice versa 
#'using as_datetime() and as_date()
as_datetime(today())
as_date(now())

## from datetime to date
#  “Unix Epoch”, 1970-01-01
as_date(365 * 10 + 2)


### 2). From individual components 

#'Instead of a single string, sometimes you’ll have the individual 
#'components of the date-time spread across multiple columns. 
#'For example see in the following data frame:
head(flights)
#View(flights)
flights %>% select(year,month,day,hour, minute)
#'As we can see, all of these columns contain data 
#'regarding date and time.
#'Unfortunately,because the information about datetime is divided up
#'into different columns, R does not recognize it as date/time data. 
#'What we need to do is combine and convert all of these columns
#' into datetime.
  
#'To do this, we can use the function: 
#'make_date() for dates, or make_datetime() for date-times:
  
flights %>% 
  select(year,month,day,hour, minute) %>% 
  mutate(departure = make_datetime(year,month,day,hour,minute),
         departure_date = make_date(year, month,day))

#'Now we know how to combine information of different columns to 
#'form one cohesive one, what about the time columns, such as dep_time,
#'arr_time, sched_dep_time, sched_arr_time ? 
flights %>% 
  select(year,month,day,hour, minute, dep_time,arr_time,sched_dep_time,sched_arr_time)
#'How can we turn values like 517 into 5:17:00? Once again,
#' make_datetime() is the answer!

#'create a new function that will help us make the process easier
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
  #The last two arguments will give us the hour and minutes of the day.  
  # %/% is the integral division operator, retains integer 
  # %% is the modulo operator (retains the reminder)
  #For example, 517 %/% 100 = 5 and 517 %% 100 is 17, so combined, 517 becomes 5:17!
}                                                             


flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

#'Another simpler way to use make_datetime() is to insert the
#' numerical values of the year, month, day, and time directly.
make_datetime_100(2023,02,01,1430)


#### 3). Retrieving Information from Date/Time Data

#You can pull out individual parts of the date
#with the accessor functions 

datetime <- ymd_hms("2016-07-08 12:34:56")
datetime

month(datetime, label = TRUE)
month(datetime, label = FALSE)
wday(datetime,label = TRUE)
yday(datetime) # the number of days since January 01.
hour(datetime)
minute(datetime)
second(datetime)

#
year(ymd(20221217))
month(ymd(20221217))
day(ymd(20221217))
hour(ymd(20221217))
second(ymd(20221217))


# We can use wday() to see that more flights depart
# during the week than on the weekend:
  
  flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

  