
rm(list = ls())

#' Date/time data are data that conveys information about, 
#' date and/or time! There are three relevant data types 
#' when we talk about date/time data: 
#' 1. Date - only has the date (e.g. 2023-03-21) 
#' 2. Time - only has the time (e.g. 19:45:00) 
#' 3. Datetime - has both the date and time (e.g. 2023-03-21 19:45:00)

# date/time classes
# three date/time classes are built-in in R; Date, POSIXct, and POSIXlt

# Date()
#' this is the class to use if you have only dates, 
#' but no times, in your data

# create a date:
# the default format is yyyy-mm-dd
dt1 <- as.Date("2020-10-11")
dt1
str(dt1)
class(dt1)
#The default format is yyyy-mm-dd
# non-standard formats must be specified:
# see list of format symbols:
#browseURL("https://www.statmethods.net/input/dates.html")
dt2 <- as.Date("04/20/2020", format = "%m/%d/%Y")
dt2

dt3 <- as.Date("October 6, 2020", format = "%B %d, %Y")
dt3

#convert between character and time objects.
?strptime #convert characters to time objects.
?strftime #converts time objects to characters
#browseURL("https://www.statology.org/strptime-strftime-r/")


#### Calculations with dates:
#Just like any other types of data on R, we can conduct
#basic arithmetic operations using date/time data.

# find the difference between dates:
dt1-dt2

difftime(dt1, dt2, units = "weeks")
difftime(dt1, dt2, units = "hours")

dt2-dt1

# add or subtract days:
dt1 + 10
dt1 - 10

# create a vector of dates and find the intervals between them:
three.dates <- as.Date(c("2017-08-22", "2018-09-20", "2019-10-06"))
three.dates
diff(three.dates)

#create a sequence of dates:
six.weeks <- seq(dt1, length = 6, by = "week") #1 week 7 days
six.weeks

six.weeks <- seq(dt1, length = 6, by = 14)
six.weeks

six.weeks <- seq(dt1, length = 6, by = "2 weeks") #14 days
six.weeks

# see the internal integer representation
unclass(dt1)#Dates are represented as the number of days since 1970-01-01,
dt1 - as.Date("1970-01-01")

#### posixct
# If you have times in your data, this is usually the best class to use.

# create some POSIXct objects:
tm1 <- as.POSIXct("2020-07-25 07:35:26")
tm1

tm2 <- as.POSIXct("25072020 08:32:07", format = "%d%m%Y %H:%M:%S")
tm2

# specify the time zone:
tm3 <- as.POSIXct("2020-08-12 11:42:03", tz = "GMT")
tm3

# some calculations with times
# compare times:
tm2 > tm1

# add or subtract seconds:
tm1 + 30
tm1 - 30

# find the difference between times:
tm2 - tm1

# automatically adjusts for daylight savings time:
as.POSIXct("2022-03-29 23:32:07") - as.POSIXct("2022-03-30 03:45:26")

# get the current time (in POSIXct by default):
Sys.time()

# see the internal integer representation:
unclass(tm1)

difftime(tm1, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), units = "secs")

# posixlt
# This class enables easy extraction of specific components of a time.
# (as.POSIXct stand for calender time and as.POSIXlt stands for local time.
# as.POSIXlt also helps one remember that POXIXlt objects are lists.)

# create a time:
tm1.lt <- as.POSIXlt("2020-07-25 08:55:26")
tm1.lt

unclass(tm1.lt)
unlist(tm1.lt)

# extract components of a time object:
tm1.lt$sec
tm1.lt$wday

# truncate or round off the time:
trunc(tm1.lt, "days")
trunc(tm1.lt, "mins")

# chron
# this class is a good option when you donot need to deal with timezones
# it requires the package chron
require(chron)

# create some times:
tm1.c <- as.chron("2020-07-25 09:55:26")
tm1.c

tm2.c <- as.chron("07/25/20 10:32:07", "%m/%d/%y %H:%M:%S")
tm2.c

# extract just the date:
dates(tm1.c)

#compare times:
tm2.c > tm1.c

# add days:
tm1.c + 10

# calculate the difference between times:
tm2.c - tm1.c

difftime(tm2.c, tm1.c, units = "hours")

# does not adjust for daylight savings time:
as.POSIXct("2019-03-29 23:32:07") - as.POSIXct("2019-03-30 03:45:26")
as.chron("2019-03-29 23:32:07") - as.chron("2019-03-30 03:45:26")

# detach the chron package as it will interfere with lubridate later in this script.
detach("package:chron", unload = TRUE)

# summary of date/time classes
# When you just have dates, use Date.
# When you have times, POSIXct is usually the best,
# but POSIXlt enables easy extraction of specific components
# and chron is simplest when you donâ€™t need to deal with timezones and daylight savings time.


# manipulating times and dates
# lubridate
# This package is a wrapper for POSIXct with more intuitive syntax.

require(lubridate)

# create a time:
tm1.lub <- ymd_hms("2020-07-25 08:35:26")
tm1.lub

tm2.lub <- mdy_hm("07/25/20 09:32")
tm2.lub

tm3.lub <- ydm_hm("2020-25-07 9:00am")
tm3.lub

tm4.lub <- dmy("25072020")
tm4.lub

# some manipulations: extract components:
year(tm1.lub)

week(tm1.lub)

wday(tm1.lub, label = TRUE)

wday(tm1.lub, label = FALSE)

hour(tm1.lub)

tz(tm1.lub)

tm2.lub
second(tm2.lub) <- 7
tm2.lub

# converting to decimal hours can facilitate some types of calculations:
tm1.dechr <- hour(tm1.lub) + minute(tm1.lub)/60 + second(tm1.lub)/3600
tm1.dechr

# lubridate distinguishes between four types of objects:
#'  - instants, 
#'  - intervals, 
#'  - durations, and
#'  -  periods

# an instant is a specific moment in time
# intervals, durations, and periods are all ways of recording time spans

# dates and times parsed in lubridate are instants:
is.instant(tm1.lub)

# round an instant:
round_date(tm1.lub, "minute")

round_date(tm1.lub, "day")

# get the current time or date as an instant:
now()

today()

# Note that lubridate uses UTC time zones as default.

# see an instant in a different time zone:
with_tz(tm1.lub, "America/Los_Angeles")
with_tz(tm1.lub, "America/New_York")

# change the time zone of an instant (keeping the same clock time):
force_tz(tm1.lub, "America/New_York")

# some calculations with instants. Note that the units are seconds:
tm2.lub - tm1.lub

tm2.lub > tm1.lub

tm1.lub + 30

# an interval is the span of time that occurs between two specified instants.
in.call <- as.interval(tm1.lub, tm2.lub)
in.call

# check whether a certain instant occured with a specified interval:
tm3.lub %within% in.call

tm4.lub %within% in.call

# determine whether two intervals overlap:
daytime <- as.interval(ymd_hm("2020-07-25 06:03"), ymd_hm("2020-07-25 20:23"))
daytime

int_overlaps(in.call, daytime)

# a duration is a time span not anchored to specific start and end times
# it has an exact, fixed length, and is stored internally in seconds

# create some durations:
ten.minutes <- dminutes(10)
ten.minutes

five.days <- ddays(5)
five.days

one.year <- dyears(1)
one.year

as.duration(in.call)

# arithmetics with durations:
tm1.lub - ten.minutes

five.days + dhours(12)

ten.minutes/as.duration(in.call)

# a period is a time span not anchored to specific start and end times,
# and measured in units larger than seconds with inexact lengths

# create some periods:
three.weeks <- weeks(3)
three.weeks

four.hours <- hours(4)
four.hours

# arithmetics with periods:
tm4.lub + weeks(9)

holiday <- months(1) + days(5)
holiday

# estimate only: convert to intervals for accuracy
three.weeks/holiday

# convert character date into date format 'dd/mm/yyyy'
chrDates <- c("25/07/2020", "19/09/2020")
str(chrDates)
# base
dates1 <- as.Date(chrDates, "%d/%m/%Y")
str(dates1)
# or lubridate
dates2 <- dmy(chrDates)
str(dates2)

# more resources
browseURL("https://cran.r-project.org/web/packages/anytime/vignettes/anytime-introduction.pdf")

browseURL("http://en.wikibooks.org/wiki/R_Programming/Times_and_Dates")

# lubridate:
browseURL("https://lubridate.tidyverse.org/")  

# also see the R help file at ?Sys.timezone