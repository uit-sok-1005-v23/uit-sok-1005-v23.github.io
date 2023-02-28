
# Learning outcomes:
# to work more on dplyr functions to manipulate data 
# Apply additional dplyr functions to manipulate data and data frames for analysis


rm(list = ls())
library(tidyverse)

#load data. 
data("starwars")
#This data set is about characters in the Star Wars movie saga
#browseURL("https://www.rpubs.com/JackAbbey/554917")
??starwars

#copy the data
sw <- starwars
# Basic data properties 
sw  #tibble 
dim(sw) # 87 rows (denoting individuals) x 13 columns (variables) 
head(sw)

nrow(sw)
ncol(sw)
names(sw)

###----- Missing data: ----- 

# Q1. How many missing (NA) values does sw contain?
is.na(sw)
sum(is.na(sw)) # 101 missing values.

# Q2. Which variable (column) has the most missing values?
colSums(is.na(sw))  
max(colSums(is.na(sw)))# birth_year has 44 missing values
colMeans(is.na(sw)) # amounting to 50.1% of all cases. 

## Q3. Replace all missing values of `hair_color` (in the variable `sw$hair_color`) by "bald": 
sw$hair_color
sw$hair_color[is.na(sw$hair_color)] <- "bald"
sw$hair_color

## Q4. Which individuals come from an unknown (missing) homeworld 
# but have a known birth_year or mass? 
sw$homeworld

sw %>% 
  filter(is.na(homeworld), !is.na(mass) | !is.na(birth_year))


###-------Gender issues-----

# Q5. How many humans are contained in sw overall and by gender?
sw %>% filter(species=="Human") %>% 
  group_by(gender) %>% count # Answer: 35 Humans in total: 9 females, 26 male.


# Q6. How many and which individuals(rows?) are neither male nor female?
# sw %>%
#   filter(gender != "male", gender != "female")

sw %>%
  filter(sex != "male", sex != "female")

# Q7. Of which species are there at least 2 different gender values?

sw %>%
  group_by(species) %>%
  summarise(n_gender_vals = n_distinct(gender)) %>%
  filter(n_gender_vals >= 2)

# alternative (and shorter) solution:
sw %>%
  group_by(species, gender) %>%
  count() %>%  # table shows species by gender: 
  group_by(species) %>%  # Which species appear more than once in this table? 
  count() %>%
  filter(n > 1)

# Q8. compute a summary table tb that counts the frequency of each gender in sw,
# and then create a bar chart that shows the gender distribution.
# Summary table:
tb <- sw %>%
  group_by(gender) %>%
  count()

tb
#From summary table tb
tb %>% ggplot(aes(x = gender, y = n)) +
  geom_bar(aes(fill = gender), stat = "identity") + 
  # Set labels, colors, and theme: 
  labs(title = "Gender distribution of the sw Universe", 
       x = "Gender", y = "Frequency") 

# Q9. Use ggplot2 on the raw data of sw to create a bar chart 
#     that shows the gender distribution.
sw %>% ggplot(aes(x = gender)) +
  geom_bar(aes(fill = gender)) + 
  # Set labels, colors, and theme: 
  labs(title = "Gender distribution of the sw Universe",  
       x = "Gender", y = "Frequency") 



###----- Popular homes and heights----

#Q10.From which homeworld do the most individuals (rows) come from? 
sw
sw %>%
  group_by(homeworld) %>%
  count() %>%
  arrange(desc(n))

#'Q11. What is the mean height of all individuals with orange eyes
#'     from the most popular homeworld? 
names(sw)
sw %>% 
  filter(homeworld == "Naboo", eye_color == "orange") %>%
  summarise(n = n(),
            mn_height = mean(height))
  
## Note: 
sw %>% 
  filter(eye_color == "orange") # => 8 individuals


# Q12.What is the mass and homeworld of the smallest droid?
sw
sw %>% 
  filter(species == "Droid") %>%
  arrange(height)


###----- Size and mass issues----

# Q13. Compute the median, mean, and standard deviation of `height` for all droids.
sw %>%
  filter(species == "Droid") %>%
  summarise(md_height = median(height, na.rm = TRUE),
            mn_height = mean(height, na.rm = TRUE),
            sd_height = sd(height, na.rm = TRUE),
            n = n(),
            not_NA_h = sum(!is.na(height)))

# Q14. Compute the average height and mass by species and save the result as `h_m`:
h_m <- sw %>%
  group_by(species) %>%
  summarise(n = n(),
            mn_height = mean(height, na.rm = TRUE),
            not_NA_h = sum(!is.na(height)),
            md_mass = median(mass, na.rm = TRUE),
            not_NA_m = sum(!is.na(mass)))
h_m

# Q15. Use `h_m` to list the 3 species with the smallest individuals
# (in terms of mean height)?
h_m %>% arrange(mn_height) %>% slice(1:3)


# Q16. Use `h_m` to list the 3 species with the heaviest individuals 
# (in terms of median mass)?
h_m %>% arrange(desc(md_mass)) %>%  slice(1:3)


# Q17. How many individuals come from the 3 most frequent (known) species?
sw %>%
  group_by(species) %>%
  count %>%
  arrange(desc(n)) %>%
  filter(n > 1)


# Q.18. Which individuals are more than 20% lighter (in terms of mass) 
# than the average mass of individuals of their own homeworld?
sw %>%
  select(name, homeworld, mass) %>%
  group_by(homeworld) %>%
  mutate(n_notNA_mass = sum(!is.na(mass)),  
         mn_mass = mean(mass, na.rm = TRUE),
         lighter = mass < (mn_mass - (.20 * mn_mass))) %>%
  filter(lighter == TRUE)



# Have a look at the page 
browseURL("https://www.rebeccabarter.com/blog/2019-01-23_scoped-verbs/")

#-----------------------------------------------------------------------
#' Scoped verbs:
#  The _if() scoped variant: perform an operation on variables that satisfy a logical criteria
#'   select_if()
#'   rename_if() 
#'   mutate_if()
#'   summarise_if()
#'   
#' The _at() scoped variant: perform an operation only on variables specified by name
#'  rename_at()
#'  summarise_at() 
#'  mutate_at()
#'  
#'  
#' The _all() scoped variant:perform an operation on all variables at once
#'  rename_all() 
#'  mutate_all()
#'  summarise_all()
#'-----------------------------------------------------------------------------  

#' `select_if()`
# extract the numeric columns of the tibble 
sw %>% select_if(is.numeric)
sw %>% select_if(is.character)

#' We could also apply use more complex logical statements,
#' For example by selecting columns that have at least one missing value.
sw %>% 
  # select columns with at least one NA
  # the expression evaluates to TRUE if there is one or more missing values
  select_if(~sum(is.na(.x)) > 0) 

#or
 #sw %>% select_if(function(.)sum(is.na(.))>0)
 #sw %>% select_if(function(x)sum(is.na(x))>0)



#' `rename_if()`
#We could rename columns that satisfy a logical expression using rename_if(). 
# Rename/add a num_ prefix to all numeric column names.
sw %>% select_if(is.numeric)

sw %>%
  # only rename numeric columns by adding a "num_" prefix
  rename_if(is.numeric, ~paste0("num_", .x)) %>% select_if(is.numeric)

#paste0("Hello", "World","today")
#or 
# sw %>%
#   # only rename numeric columns by adding a "num_" prefix
#   rename_if(is.numeric, function(.x)paste0("num_", .x)) %>% select_if(is.numeric)



#' `mutate_if()`
#' We could similarly use mutate_if() to mutate columns that satisfy specified logical conditions. 
# For instance, we can... 

# Convert any variable of type factor to character 
sw %>% mutate_if(is.character, as.factor)



#' `summarise_if()`
#' summarise columns that satisfy the specified logical conditions.

#For instance, if you want to calculate the mean value of all numeric columns,
#we would need to write out the mean fun three separate times. 
sw %>% 
  summarise(mean_height = mean(height, na.rm =TRUE),
            mean_mass = mean(mass, na.rm =TRUE),
            mean_birth_year = mean(birth_year, na.rm =TRUE))

#Using the scoped verb summarise_if
sw %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

sw %>% 
  summarise_if(is.numeric, ~mean(.x, na.rm = TRUE))



#' `The _at scoped fun`
#' The _at() scoped variant allows you to perform an operation 
#' only on variables specified by name 
sw %>% mutate_at(c("height","mass"), funs(mod=./10))
sw %>% mutate_at(c("height","mass"), ~(mod=./10))

sw %>% 
  summarise_all(n_distinct)



######################
## across() 
###################
#' Scoped verbs have now essentially been superseded by across().
#' It is easier to work with across() fun than scoped verbs

browseURL("https://www.rebeccabarter.com/blog/2020-07-09-across/")

# summarise_if() will summarise columns that satisfy the specified logical conditions.
sw %>% 
  summarise_if(is.numeric, median, na.rm = TRUE)

# using across() fun
sw %>% 
  summarise(across(where(is.numeric),
                   .fns = median,
                   na.rm = TRUE))
#'The formula approach gives us the ability to combine functions with arguments
#'Using this, you start with the ~ to signify “as a function of” and then put 
#'wherever your column name would normally as a .x.
sw %>% 
  summarise(across(where(is.numeric),
                  ~median(.x, na.rm = TRUE))) # also ,x = . 

# Example with no list but argument
sw %>% 
  summarise(across(where(is.numeric),
                   .fns = list(median, mean), 
                   na.rm = TRUE))  
# Example with list and argument
sw %>% 
  summarise(across(where(is.numeric),
                   .fns = list(median = median, mean = mean), 
                   na.rm = TRUE)) 


#Find all rows where no variable has missing values:
sw %>% filter(across(everything(), ~!is.na(.x)))
