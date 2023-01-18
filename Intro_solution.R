
##############################
#'  Forelesning 1 
##############################
#' `Objective`: learn/review basic commands from `dplyr`
#' 
#' R makes use of the # or #' signs to add comments, 
#' so that you and others can understand what the R code 
#' is about.

##########################################################
##########################################################
######
#' `dplyr- Basic data manipulation`
#' browseURL("https://r4ds.had.co.nz/transform.html")
#' `filter()` - Pick observations (rows) by their values   
#' `select()` -  Pick variables (columns) by their names  
#' `mutate()` - Create new variables (columns) with functions of existing variables  
#' `arrange()`-  Reorder rows using  
#' `summarise()` - Collapse many values down to a single summary 
#' `rename()`  - Rename columns (variables):
#' `group_by()` - Group rows by columns (variables)
#' 
##########################################################
#############################################################
########


rm (list = ls())
library(tidyverse)
# Use the gapminder data-set from the gapminder package
library(gapminder) 
data("gapminder")


#--------Data Exploration----------

str(gapminder)
glimpse(gapminder)

head(gapminder,10) 
tail(gapminder,10)

summary(gapminder)

#' Remember, since this dataset is provided by a package, 
#' you can get more information about the variables with 
?gapminder
help("gapminder")

#' `select()` columns 

# Selecting a vector(column) using $
gapminder$country

###############
#' Base R
################

#select column 1 
gapminder[,1]
#select column based on name 
gapminder[,"country"]
gapminder[,c("country","year","pop")]

#################
# Tidyverse 
###################
#Selecting on columns using tidyverse 
select(gapminder,country)

# Some Questions:

#' Q0. From the gapminder dataset, select country, year & pop 

select(gapminder, country,year, pop)

#' new way (using pipe, %>% (shift+ctrl+M); then
gapminder %>% select(country,year,pop)

#' the symbol %>% means: use this as the first argument
#' in the next command.
# x %>% f(y) is the same as f(x,y)!

#' piping allows us to split up the code,
#' executing one command at a time.

# selecting columns that start with a letter c
gapminder %>% select(starts_with("c"))

# selecting columns that ends with a letter Exp
gapminder %>% select(country,year, ends_with("Exp"))

# deselecting 
gapminder %>% select(-continent,-lifeExp,-gdpPercap)


#' `filter()` rows 

#' old way: function_verb(object, option)
gapminder[gapminder$country=="Norway",]

filter(gapminder, country == "Norway")

#' new way: using the pipe, %>% 
gapminder %>%
  filter(country == "Norway")

#' Q1: From the gapminder dataset, filter out the data from Norway before and including 1977.

#combine filter functions 
gapminder %>% 
  filter(country == "Norway") %>% 
  filter(year <= 1977) 

#' & = AND
gapminder %>% 
  filter(country == "Norway" & 
           year <= 1977) 

#' , = AND
gapminder %>% 
  filter(country == "Norway" ,
         year <= 1977) 



#' Q2: Filter the data from Norway or Sweden before and
#' including 1970.

gapminder %>% 
  filter(year <= 1970) %>% 
  filter(country == "Norway" | country == "Sweden") 

#' or 
 gapminder %>%
   filter(year <= 1970,country == "Norway" | country == "Sweden")

 # or 
#' combine using %in% and a vector (same as above)
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden")) 

# or 
Norway_Sweden <- c("Norway", "Sweden")
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% Norway_Sweden) 



#' Q3. Filter the data from Norway, Sweden, or Denmark 
#' before and including 1970. And select the variables
#' country, year, gdpPercap

#' filter, & select a variable
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>%
  select(country, year, gdpPercap)


#' Q4. Following Q3. Let us say you do not like 
#' long variable names such as "gdpPercap".  
#' Rename "gdpPercap" by gdp_pc        

#' rename a variable
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>%
  select(country, year, gdpPercap) %>%
  rename(gdp_pc=gdpPercap)

#' Q5. Following Q4. Arrange rows according to 
#' ascending order of "gdp_pc" using the function 
#' arrange()  

#' ascending
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp_pc=gdpPercap) %>% 
  arrange(gdp_pc)


#' Q6. Following Q5. Arrange rows according to
#' descending order of "gdp_pc"

#' descending
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp_pc=gdpPercap) %>% 
  arrange(desc(gdp_pc))

#' Q7. Arrange rows according to ascending order 
#' of "gdp_pc", within each year. 
#'(i.e., within each year, arrange gdp_pc)
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>% 
  rename(gdp_pc=gdpPercap) %>% 
  arrange(year, gdp_pc)


#' `mutate()` - Create new variables (columns) with functions of existing variables  

#'Q8.Data from Norway.   
#' Create a new variable that is GDP from gdpPercap & pop
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop)

#' Q9. Data from Norway. 
#' Create a new variable called "gdpNOK" that is GDP per per billion NOK (1 000 000 000 NOK) 
#' (1 USD=9 NOK)
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9)

#' change order of columns(rearranging data as: country, continent, year, gdpNok, ...)
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9) %>% 
  select(country:year, gdpNOK, everything()) # change the order of the columns


#' Q10.Use mutate and ifelse to Categorise "gdpNOK" into 3 categories,
#'  (i.e., less than or equal to 999, between 1000 and 1999, and greater than 2000).

gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9,
         gdpcat = ifelse(gdpNOK <= 999, "less than a billion",
                         ifelse(gdpNOK > 1000 & gdpNOK <= 1999, "between a billion and two billions",
                                ifelse(gdpNOK > 2000, "larger than two billions", NA))))

#' or
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9,
         gdpcat = ifelse(gdpNOK <= 999, "less than a billion",
                         ifelse(between(gdpNOK, 1000, 1999), "between a billion and two billions",
                                ifelse(gdpNOK >= 2000, "larger than two billions", NA))))


#'  `summarise() or summarize()` 


#' Q11. Calculate the average lifExp of all three Nordic countries
#' (i.e., Norway, Sweden, Denmark)

gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  summarise(mean(lifeExp))

# Assign a name to the calculated mean lifeEXP, call it avlfexp
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  summarise(av_lfExp = mean(lifeExp))


#' Q12. Calculate the average lifExp of the three countries,per country. 
#' (i.e., average per country, name variable )
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  group_by(country) %>% 
  summarise(av_lfexp=mean(lifeExp))

#' Q13. Calculate mean life expectancy per country 
#' (i.e., for every country in the gapminder data set)
gapminder %>% 
  group_by(country) %>%
  summarise(avlfexp=mean(lifeExp))

#' Q14. Calculate mean life expectancy per continent.
gapminder %>% 
  group_by(continent) %>%
  summarise(avlfexp=mean(lifeExp))

#' Q15. calculate mean life expectancy by continent & country

gapminder %>% 
  group_by(continent, country) %>%
  summarise(avlfexp=mean(lifeExp))

#' Q16. Calculate mean life expectancy by continent & add min and max lifeExp
gapminder %>%
  group_by(continent) %>% 
  summarise(avlfexp=mean(lifeExp), min=min(lifeExp), max=max(lifeExp))


#' Visualization

# Q17. Scatter plot of gdpPercap vs lifeExp 

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(alpha=0.4) +
  ggtitle("gdpPercap vs life Expectancy")


# Q18. Scatter plot of gdpPercap vs lifeExp by Continent
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color=continent, shape=continent)) + 
  geom_point(alpha=0.4) +
  ggtitle("Life Expectancy and GDP by Continent")


#' scatter plot per continent, separate graphs for each continent 
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  facet_wrap(~continent, ncol=3) +
  ggtitle("Life Expectancy and GDP by Continent")


#' histogram
gapminder %>%
  ggplot(aes(x = lifeExp)) + 
  geom_histogram(binwidth=2) +
  facet_wrap(~continent, ncol=3) +
  ggtitle("Life Expectancy by Continent")


#' 3 countries
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  ggplot(aes(x = gdpPercap,y = lifeExp, colour = country)) + geom_point()

#' more bells and whistles
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = country)) + geom_point() + 
  geom_text(aes(label=as.character(year), hjust=0, vjust=1)) + geom_line() 




# Some extra 

#' Data structure
#' You can represent the same underlying data 
#' in multiple ways.But they are not equally easy to use.
#' One dataset, the tidy dataset, will be much easier
#' to work with inside the tidyverse.

#' Starting point, our "long" data
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

#' Task. Change the above data to wider format, i.e.,take year as a key variable here
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  pivot_wider(names_from = year,
              values_from = gdp)

#' Task. Change the data to wide format, i.e., take countries as a key variable
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  pivot_wider(names_from = country,
              values_from = gdp)

#' wide, countries as variables & back again (long)
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  pivot_wider(names_from = country,
              values_from = gdp) %>% 
  pivot_longer(-year,
               names_to = "country",
               values_to = "gdp")


#' joining dataframes  

#' first dataframe, gdp
df1 <- gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

#' second dataframe, pop
df2 <- gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, pop)

df1 ; df2

#' this works
left_join(df1, df2)

#' however, this is good practice
left_join(df1, df2, by=c("year","country"))

#' third dataframe, lifeExp
df3 <- gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, lifeExp)

df3

#' adding a third data frame
left_join(df1, df2, by=c("year","country")) %>% 
  left_join(df3, by=c("year","country"))

#' remove dataframes
rm(df1,df2,df3)

#' first dataframe, some rows
df1 <- gapminder %>%
  filter(year <= 1980) %>%
  filter(country == "Norway") %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

#' second dataframe, some more rows, overlap with first dataframe
df2 <- gapminder %>%
  filter(country == "Norway") %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

df1 ; df2

#' third dataframe, with duplicate rows
df3 <- bind_rows(df1,df2)
df3

#' remove duplicate rows
df3 %>% distinct()

#' remove dataframes
rm(df1,df2,df3)



#' Find the correlation for all countries
browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html")

#' sorted ascending
gapminder %>%
  group_by(country) %>%
  summarise(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(correlation)

#' sorted descending
gapminder %>%
  group_by(country) %>%
  summarise(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(desc(correlation))

#' top two and bottom two
gapminder %>%
  filter(country %in% c("Kuwait","Madagascar","France","Austria")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point()

#' scale is important in plots
gapminder %>%
  filter(country %in% c("Madagascar")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point()

library(broom)
#' note that reference to data and variables is .$
gapminder %>%
  group_by(country) %>%
  do(tidy(cor.test(.$lifeExp, .$gdpPercap)))

#' calculate the percentage and logaritmic growth & their compounds (cumulative change)
gapminder %>%
  filter(country == "Norway") %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(perc_diff = (gdp-lag(gdp))/lag(gdp),
         log_diff = c(NA, diff(log(gdp))),
         comp_perc = c(1, 1+cumsum(na.omit(perc_diff))),
         comp_log = c(1, 1+cumsum(na.omit(log_diff))))

#' what if we wanted to change 1982 to the base year (=100) for comp_log?
df1 <- gapminder %>%
  filter(country == "Norway") %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(log_diff = c(NA, diff(log(gdp))),
         comp_log = c(1, 1+cumsum(na.omit(log_diff))))

df2 <- df1 %>% filter(year==1982)

df1 %>%
  mutate(rebase=100*comp_log/df2$comp_log)

rm(df1,df2)

#' calculate the logaritmic growth in gdp for all countries
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>% head(.,20)

#' what country has the highest and lowest average logaritmic growth in gdp?  

#' lowest
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>%
  group_by(country) %>% 
  summarise(meanGDP=mean(na.omit(log_diff))) %>% arrange(meanGDP)

#' highest
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>%
  group_by(country) %>% 
  summarise(meanGDP=mean(na.omit(log_diff))) %>% arrange(desc(meanGDP))

#' density plot
gapminder %>%
  select(continent, country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>% 
  ggplot() + 
  geom_density(aes(x = log_diff, group=continent, fill=continent), alpha=0.3) + 
  ggtitle("Average GDP by Continent") +
  xlim(c(-0.25,0.5)) +
  xlab("% average growth in GDP")



