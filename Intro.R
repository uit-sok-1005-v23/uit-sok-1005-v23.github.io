
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


# Some Questions:

#' Q0. From the gapminder dataset, select country, year & pop 







#' Q1: From the gapminder dataset, filter out the data from Norway before and including 1977.







#' Q2: Filter the data from Norway or Sweden before and including 1970.







#' Q3. Filter the data from Norway, Sweden, or Denmark before and including 1970.







#' Q4. Following Q3. Let us say you do not like long variable names such as "gdpPercap".  
#' Rename "gdpPercap" by gdp_pc        








#' Q5. Following Q4. Arrange rows according to ascending order of "gdp_pc"








#' Q6. Following Q5. Arrange rows according to descending order of "gdp_pc"







#' Q7. Arrange rows according to ascending order of "gdp_pc", within each year. 






#'Q8.Data from Norway.   
#' Create a new variable that is GDP  from gdpPercap & pop






#' Q9. Data from Norway. 
#' Create a new variable called "gdpNOK" that is GDP per per billion NOK (1 000 000 000 NOK) 
#' (1 USD=9 NOK)






#' Q10.Use mutate and ifelse to Categorise "gdpNOK" into 3 categories,
#'  (i.e., less than or equal to 999, between 1000 and 1999, and greater than 2000).







#' Q11. Calculate the average lifExp of all three Nordic countries
#' (i.e., Norway, Sweden, Denmark)









#' Q12. Calculate the average lifExp of the three countries,per country. 









#' Q13. Calculate mean life expectancy per country 
#' (i.e., for every country in the gapminder data set)








#' Q14. Calculate mean life expectancy per continent.








#' Q15. calculate mean life expectancy by continent & country








#' Q16. Calculate mean life expectancy by continent & add min and max lifeExp








# Q17. Scatter plot of gdpPercap vs lifeExp 








# Q18. Scatter plot of gdpPercap vs lifeExp by Continent









# Q19. Scatter plot of gdpPercap vs lifeExp by Continent. Use different shapes per continent



