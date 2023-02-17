
# Learning outcomes:
# to work more on dplyr functions to manipulate data 
# Apply additional dplyr functions to manipulate data and data frames for analysis

rm(list = ls())
library(tidyverse)
#load data
data("starwars")

# copying 
sw <- starwars
??sw 

###----- Missing data: ----- 

# Q1. How many missing (NA) values does sw contain?

# Q2. Which variable (column) has the most missing values?


#Q3. Replace all missing values of `hair_color` (in the variable `sw$hair_color`) by "bald": 


# Q4. Which individuals come from an unknown (missing) homeworld 
# but have a known birth_year or mass? 


###-------Gender issues-----

# Q5. How many humans are contained in sw overall and by gender?



# Q6. How many and which individuals are neither male nor female?


# Q7. Of which species are there at least 2 different gender values?



# Q8. compute a summary table tb that counts the frequency of each gender in sw,
# and then create a bar chart that shows the gender distribution.


# Q9. Use ggplot2 on the raw data of sw to create a bar chart that shows the gender distribution.




###----- Popular homes and heights----

# Q10. From which homeworld do the most indidividuals (rows) come from? 


#' Q11.  What is the mean height of all individuals with orange eyes
#' from the most popular homeworld? 



# Q12. What is the mass and homeworld of the smallest droid?



###----- Size and mass issues----

#  Q13.Compute the median, mean, and standard deviation of `height` for all droids.


# Q14. Compute the average height and mass by species and save the result as `h_m`:



# Q 15. Use `h_m` to list the 3 species with the smallest individuals
# (in terms of mean height)?



# Q.16. Use `h_m` to list the 3 species with the heaviest individuals 
# (in terms of median mass)?



# Q17. How many individuals come from the 3 most frequent (known) species?



# Q 18. Which individuals are more than 20% lighter (in terms of mass) 
# than the average mass of individuals of their own homeworld?



# Have a look at the page 
 browseURL("https://www.rebeccabarter.com/blog/2019-01-23_scoped-verbs/")

#' Scoped verbs:
#' 
#'   select_if()
#'   rename_if() 
#'   mutate_if()
#'   summarise_if(), ....

#'  rename_at()
#'  summarise_at() 
#'  mutate_at(), ....
#' 
#'   mutate_all()
#'   rename_all() 
#'   summarise_all(),...

#  The _if() scoped variant: perform an operation on variables that satisfy a logical criteria
#' The _at() scoped variant: perform an operation only on variables specified by name
#' The _all() scoped variant:perform an operation on all variables at once
