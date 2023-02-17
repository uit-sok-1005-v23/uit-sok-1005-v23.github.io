
rm(list=ls())

library(tidyverse)
library(lubridate)

#' From reading list:
#' Chiu, David
#' R for data science cookbook:
#' over 100 hands-on recipes to effectively solve real-world data problems using
#' the most popular R packages and techniques
superstore <- read_csv("https://github.com/ywchiu/rcookbook/raw/master/chapter7/superstore_sales.csv", 
                       col_types = cols(`Order Date` = col_date(format = "%Y/%m/%d")))
View(superstore)
#. This is superstore sales data in different province of Canada.

str(superstore)
names(superstore)

superstore$`Order ID`

#' Change names, do not like "-" in variable names
names(superstore) <- c("Order_ID", "Order_Date","Order_Quantity","Sales","Profit","Unit_Price",
                       "Province","Customer_Segment","Product_Category")

str(superstore)
head(superstore)

#' Exploratory Data Analysis
#install.packages("DataExplorer") 
library(DataExplorer)

plot_str(superstore)
plot_missing(superstore)
plot_histogram(superstore)
plot_density(superstore)
plot_correlation(superstore, type = 'continuous')
# For character/factors
plot_bar(superstore)  

#' Summarize the total sales amount by year, month, and province
#' All days in a month set to 1 - long data
superstore %>% select(Sales, Province, Order_Date) %>% 
  group_by(Year_Month = ymd(strftime(Order_Date,"%Y/%m/01")), Province) %>%  
  summarise(Total_Sales = sum(Sales)) 
#or 
superstore %>% select(Sales, Province, Order_Date) %>% 
  mutate(Year_Month = ymd(strftime(Order_Date,"%Y/%m/01"))) %>% group_by(Year_Month, Province) %>%  
  summarise(Total_Sales = sum(Sales))

# From long to wide, use spread(data, key, value), retired!
# superstore %>% select(Sales, Province, Order_Date) %>% http://127.0.0.1:12565/graphics/plot_zoom_png?width=2544&height=1338
#   group_by(Year_Month = as.Date(strftime(Order_Date,"%Y/%m/01")), Province) %>% 
#   summarise(Total_Sales = sum(Sales)) %>% spread(., Province, Total_Sales) 

# From long to wide, use pivot_wider(data, key, value)
superstore %>% select(Sales, Province, Order_Date) %>% 
  group_by(Year_Month = as.Date(strftime(Order_Date,"%Y/%m/01")), Province) %>%  # as.Date() here instead of ymd, same 
  summarise(Total_Sales = sum(Sales)) %>% 
  pivot_wider(names_from = Province, values_from=Total_Sales) 

#' Make a plot of the total sales in Alberta and Youkon in 2010 and 2011
superstore %>% select(Sales, Province, Order_Date) %>% 
  group_by(Year_Month = ymd(strftime(Order_Date,"%Y/%m/01")), Province) %>% 
  summarise(Total_Sales = sum(Sales)) %>% filter(Province %in% c("Alberta","Yukon")) %>% 
  filter(Year_Month >= '2010-01-01' & Year_Month <= '2011-12-01') %>% 
  ggplot(., aes(x=Year_Month, y=Total_Sales, color=Province)) + geom_line() +
  xlab("Year Month") + ylab("Sale Amount") + ggtitle("Monthly Total Sale Amount By Province")


#' Changing aesthetics
p <- superstore %>% select(Sales, Province, Order_Date) %>% 
  group_by(Year_Month = as.Date(strftime(Order_Date,"%Y/%m/01")), Province) %>%  #why as.Date here again?
  summarise(Total_Sales = sum(Sales)) %>% filter(Province %in% c("Alberta","Yukon")) %>% 
  filter(Year_Month >= '2010-01-01' & Year_Month <= '2011-12-01') %>% 
  ggplot(., aes(x=Year_Month, y=Total_Sales, color=Province))    
  

p  # blank canvas

p + geom_line(linetype="dashed", size=2) + xlab("Year Month") + ylab("Sale Amount") + ggtitle('Change Linetype and Size')
p + geom_bar(stat = "identity", aes(fill=Province) , position = "stack") + xlab("Year Month")  + ylab("Sale Amount") + ggtitle('Stack Position') # bar on top of the other
p + geom_bar(stat = "identity", aes(fill=Province), position = "fill")   +  xlab("Year Month") + ylab("Sale Amount") + ggtitle('Fill Position')
p + geom_bar(stat = "identity", aes(fill=Province), position = "dodge")  + xlab("Year Month")  + ylab("Sale Amount") + ggtitle('Dodge Position')
p + geom_bar(stat = "identity", aes(fill=Province), position = "dodge")  + xlab("Year Month")  + ylab("Sale Amount") + ggtitle('Dodge Position') +
  scale_fill_brewer(palette=2) + ggtitle('Refill Bar Colour')

p + geom_point(size=3) + xlab("Year Month") + ylab("Sale Amount") + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("Year Month") + ylab("Sale Amount") + geom_smooth() + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("Year Month") + ylab("Sale Amount") +geom_smooth(se=FALSE) + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("Year Month") + ylab("Sale Amount") + geom_smooth(method=lm,se=FALSE) + ggtitle('Adding Linear Regression')
p + geom_point(size=3) + xlab("Year Month") + ylab("Sale Amount") + geom_point(stat = "summary", fun.y = "mean", colour = "red", size = 4) + ggtitle('Adding Mean Points')

p + geom_point(aes(size=Total_Sales)) + scale_size_continuous(range=c(1,10)) +xlab("Year Month") + ylab("Sale Amount") + ggtitle('Resize The Point')
p + geom_point(aes(colour=Total_Sales), size=5) + scale_color_gradient() +xlab("Year Month") + ylab("Sale Amount")  +ggtitle('Repaint The Point in Gradient Color')

p + geom_point(size = 5) + facet_wrap(~Province) +xlab("Year Month") + ylab("Sale Amount") + ggtitle('Create Multiple Subplots by Province')
p + geom_point(size = 5) + facet_wrap(~Province, ncol=1) +xlab("Year Month") + ylab("Sale Amount") + ggtitle('Multiple Subplots in Vertical Direction')

p + geom_point(size=5) + theme_bw()+xlab("Year Month") + ylab("Sale Amount")  +ggtitle('theme_bw Example')
p + geom_point(size=5) + theme_dark()+ xlab("Year Month") + ylab("Sale Amount") +ggtitle('theme_dark Example')

p + geom_point(size=5) + scale_color_manual(values=c("#E69F00", "chartreuse")) +
  theme(
    axis.text = element_text(size = 12),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "yellow"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "blue")
  ) + xlab("Year Month") + ylab("Sale Amount") +ggtitle('Customized Theme')

library(grid)
grid.newpage()

plot1 <- p + geom_point(size=5) +xlab("Year Month") + ylab("Sale Amount") + ggtitle('Scatter Plot')
plot2 <- p + geom_line(size=3) + xlab("Year Month") + ylab("Sale Amount") + ggtitle('Line Chart')

pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp =viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp =viewport(layout.pos.row = 1, layout.pos.col = 2))

library(gridExtra)
grid.arrange(plot1,plot2, ncol=2)


# ----------------------------------------------

#' Min and Max date
summarise(superstore, min(Order_Date))
summarise(superstore, max(Order_Date))

#' Find weekday of date
superstore %>% select(Order_Date) %>% mutate(weekday = wday(Order_Date, label=TRUE),
                                             weekdayno = wday(Order_Date))

#' Q: Find average sales by weekday, but exclude Saturdays and Sundays.
#' Call the table weekdays
superstore %>% select(Sales, Order_Date) %>% mutate(weekday = wday(Order_Date, label=TRUE)) %>% 
  filter(!wday(Order_Date) %in% c(1, 7)) %>% group_by(weekday) %>%
  summarise(Average_Sales=mean(Sales))

# Average sales on Saturdays and Sundays
superstore %>% select(Sales, Order_Date) %>% mutate(weekday = wday(Order_Date, label=TRUE)) %>% 
  filter(!wday(Order_Date) %in% c(2,3,4,5,6)) %>% group_by(weekday) %>%
  summarise(Average_Sales=mean(Sales))

#' Q: Find average sales by weekday and Customer Segment. Make one column per Customer Segment.
#' Call the table weekdays
superstore %>% select(Sales, Order_Date, Customer_Segment) %>% mutate(weekday = wday(Order_Date, label=TRUE)) %>% 
  group_by(weekday, Customer_Segment) %>% summarise(Average_Sales=mean(Sales)) %>% 
  spread(Customer_Segment, Average_Sales)

#'Replace spread with pivot_wider
superstore %>% select(Sales, Order_Date, Customer_Segment) %>% mutate(weekday = wday(Order_Date, label=TRUE)) %>% 
  group_by(weekday, Customer_Segment) %>% summarise(Average_Sales=mean(Sales)) %>% 
  pivot_wider(names_from = Customer_Segment, values_from=Average_Sales) 

#' Your boss loves excel (sigh), and would like a spreadsheet with the weekdays and segments report in it.
#browseURL("https://cran.r-project.org/web/packages/openxlsx/index.html")

library(openxlsx)

weekdays <- superstore %>% select(Sales, Order_Date) %>% mutate(weekday = wday(Order_Date, label=TRUE)) %>% 
  filter(!wday(Order_Date) %in% c(1, 7)) %>% group_by(weekday) %>%
  summarise(Average_Sales=mean(Sales))

segments <- superstore %>% select(Sales, Order_Date, Customer_Segment) %>% mutate(weekday = wday(Order_Date, label=TRUE)) %>% 
  group_by(weekday, Customer_Segment) %>% summarise(Average_Sales=mean(Sales)) %>% 
  pivot_wider(names_from = Customer_Segment, values_from=Average_Sales) 

#' Make sure you use a relevant folder on your computer
getwd()
#setwd("filepath")
setwd("C:/Users/dki007/OneDrive - UiT Office 365/SOK-1005V23/")
dir()

sheets <- list(weekdays = weekdays, segments = segments)
openxlsx::write.xlsx(sheets, file = "dataset.xlsx")

#' Use the segments data frame and calculate the market share per customer segment and day
# segments %>%
#   pivot_longer(-weekday, names_to = "province", values_to = "Average_sale") %>%
#   group_by(weekday) %>% mutate(share = 100*Average_sale/sum(Average_sale)) %>%
#   pivot_wider(-Average_sale,names_from = province, values_from = share)


#' Find average profit per customer segment and product category in 2011, for all provinces except
#' Nunavut, Newfoundland and Manitoba.
#' What segment produced the highest profit?

superstore %>% group_by(Province) %>% tally() 

superstore %>% select(Profit, Customer_Segment, Product_Category, Order_Date, Province) %>% 
  mutate(Year = year(Order_Date)) %>% filter(Year==2011) %>% 
  filter(!Province %in% c("Nunavut", "Newfoundland", "Manitoba")) %>% 
  group_by(Customer_Segment, Product_Category) %>% 
  summarise(Average_Profit = mean(Profit)) %>% arrange(-Average_Profit)

#' What customer segment and product category has the highest correlation between
#' unit price and sales?

# superstore %>% 
#   group_by(Customer_Segment) %>% 
#   select(Sales,Unit_Price) %>% 
#   summarize(cor(Sales,Unit_Price))
# superstore %>% 
#   group_by(Product_Category) %>% 
#   select(Sales,Unit_Price) %>% 
#   summarize(cor(Sales,Unit_Price))

#browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html")

superstore %>% select(Unit_Price, Sales, Customer_Segment, Product_Category) %>% 
  group_by(Customer_Segment, Product_Category) %>% nest() -> nested 

nested

library(broom)

nested %>% 
  mutate(
    test = map(data, ~ cor.test(.x$Unit_Price, .x$Sales)), # S3 list-col
    tidied = map(test, tidy)) %>% 
  unnest(tidied)

#' Make a scatter plot of it
nested$group <- 1:length(nested$Customer_Segment)

library(glue)

many_plots <- nested %>% 
  mutate(plot = map2(data, 
                     group,
                     ~ ggplot(data = .x, aes(x = Unit_Price, y = Sales)) +
                       ggtitle(glue("Group {.y}")) + geom_point()))


print(many_plots$plot) 

#install.packages("cowplot")
library(cowplot)
cowplot::plot_grid(plotlist = many_plots$plot)



#Task: Replicate the code for three provinces, i.e., Nunavu, British Columbia, Ontirruwa,  
superstore$Province
