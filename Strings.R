
# How to deal with strings 

# text
# manipulating character vectors

library(tidyverse)

# character vector sample
?fruit

fruit

# detect or filter on a target string
str_detect(fruit, pattern = "fruit")

# subset on string
str_subset(fruit, pattern = "fruit")

# make data frame of character vector
fruit.df <- data.frame(fruit=fruit)
fruit.df

# in a data frame
fruit.df %>% 
  filter(str_detect(fruit, "fruit"))

# same as
fruit.df %>% 
  filter(str_detect(fruit, "fruit")==TRUE)

# string splitting by delimiter, note that we get a list back
str_subset(fruit, pattern = "fruit") %>%
  str_split(pattern = " ")

# if you know the number of text pieces, you can use str_split_fixed() and get a character matrix back
str_subset(fruit, pattern = "fruit") %>%
  str_split_fixed(pattern = " ", n = 2)

# if text in a data frame, separate()
tibble(fruit = str_subset(fruit, pattern = "fruit")) %>%
  separate(fruit, into = c("pre", "post"), sep = " ")

# but know the pattern!
tibble(fruit = c(str_subset(fruit, pattern = "fruit"),"fruit loop rules")) %>%
  separate(fruit, into = c("pre", "post"), sep = " ")

tibble(fruit = c(str_subset(fruit, pattern = "fruit"),"fruit loop rules")) %>%
  separate(fruit, into = c("pre", "post", "third"), sep = " ")

# the length of the character vector
str_subset(fruit, pattern = "fruit") %>% length()

# number of characters in string
str_subset(fruit, pattern = "fruit") %>% str_length()

# snip substrings based on character position
str_subset(fruit, pattern = "fruit") %>% 
  str_sub(1, 3)

# the start and end arguments are vectorised
tibble(fruit = str_subset(fruit, pattern = "fruit")) %>%
  head() %>% 
  mutate(snip = str_sub(fruit, 1:6, 3:8)) # a sliding 3-character window on the first 6 obs

# str_sub() also works for assignment
x <- head(fruit, 3)
x

# replace first 3 characters
str_sub(x, 1, 3) <- "AAA"
x

# collapse a vector
head(fruit) %>% 
  str_c(collapse = ", ")

# combine a character vector by catenating multiple vectors
str_c(fruit[1:4], fruit[5:8], sep = " & ")

# element-wise catenation can be combined with collapsing
str_c(fruit[1:4], fruit[5:8], sep = " & ", collapse = ", ")

# if vectors are variables in a data frame, tidyr::unite() 
fruit_df <- tibble(
  fruit1 = fruit[1:4],
  fruit2 = fruit[5:8]
)

fruit_df

# discard origin
fruit_df %>% 
  unite("flavor_combo", fruit1, fruit2, sep = " & ")

# keep origin
fruit_df %>% 
  unite("flavor_combo", fruit1, fruit2, sep = " & ", remove = FALSE)

# substring replacement
str_subset(fruit, pattern = "fruit") %>% 
  str_replace(pattern = "fruit", replacement = "THINGY")

# replacing NA, use str_replace_na()
melons <- str_subset(fruit, pattern = "melon")
melons
melons[2] <- NA
melons

str_replace_na(melons, "UNKNOWN MELON")

# if NA variable is in a data frame, you can use tidyr::replace_na()
tibble(melons) %>% 
  replace_na(replace = list(melons = "UNKNOWN MELON"))

rm(list=ls())

# regular expressions
#browseURL("https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html")

# load gapminder
library(gapminder)

# store the 142 unique country names
countries <- levels(gapminder$country)

# frequently your string tasks cannot be expressed in terms of a fixed string,
# but can be described in terms of a pattern
# regular expressions, aka "regex", are the standard way to specify these patterns
# in regex, specific characters and constructs take on special meaning in order to 
# match multiple strings

# the first metacharacter is the period ., which stands for any single character,
# except a newline (which by the way, is represented by \n)
# the regex a.b will match all countries that have an a, followed by any single character,
# followed by b, note that regex are case sensitive
str_subset(countries, pattern = "i.a")

# anchors can be included to express where the expression must occur within the string
# the ^ indicates the beginning of string and $ indicates the end
str_subset(countries, pattern = "i.a$")

str_subset(countries, pattern = "^Z")

str_subset(countries, pattern = "^S.i")

# the metacharacter \b indicates a word boundary and \B indicates NOT a word boundary
# this is our first encounter with something called "escapingâ€
# we need to prepend a second backslash to use these sequences in regex in R
str_subset(fruit, pattern = "melon")

str_subset(fruit, pattern = "\\bmelon")

str_subset(fruit, pattern = "\\Bmelon")

# character classes are usually given inside square brackets, [] 
# but a few come up so often that we have a metacharacter for them, 
# such as \d for a single digit

# match ia at the end of the country name, preceded by one of the characters in the class
str_subset(countries, pattern = "[nls]ia$")

# or
str_subset(countries, pattern = "[lns]ia$")

# use ^ to negate the class
str_subset(countries, pattern = "[^nls]ia$")

# all
str_subset(countries, pattern = ".ia$")

# two more general ways to match whitespace
# the \s metacharacter and the POSIX class [:space:]
# use extra backslash \ to escape \s 
# the POSIX class has to be surrounded by two sets of square brackets

# repeat from above
str_subset(fruit, pattern = "fruit") %>% 
  str_split_fixed(pattern = " ", n = 2)

# alternative 1
str_subset(fruit, pattern = "fruit") %>%
  str_split_fixed(pattern = "\\s", n = 2)

# alternative 2
str_subset(fruit, pattern = "fruit") %>%
  str_split_fixed(pattern = "[[:space:]]", n = 2)

# country names that contain punctuation
str_subset(countries, "[[:punct:]]")

# how many characters are allowed to match
# any string with an l eventually followed by an e
str_subset(fruit, pattern = "l.*e") # * means	0 or more intervening character

str_subset(fruit, pattern = "l.+e") # + means	1 or more intervening character

matches <- str_subset(fruit, pattern = "l.*e") # the most general

# the strings that no longer match:
# all have a literal le with no preceding l and no following e.
list(match = intersect(matches, str_subset(fruit, pattern = "l.+e")),
     no_match = setdiff(matches, str_subset(fruit, pattern = "l.+e")))

# change the quantifier from * to ? to require at most one intervening character
# in the strings that no longer match,
# the shortest gap between l and following e is at least two characters
list(match = intersect(matches, str_subset(fruit, pattern = "l.?e")),
     no_match = setdiff(matches, str_subset(fruit, pattern = "l.?e")))

# remove the quantifier and allow for no intervening characters
# the strings that no longer match lack a literal le
list(match = intersect(matches, str_subset(fruit, pattern = "le")),
     no_match = setdiff(matches, str_subset(fruit, pattern = "le")))

# escaping
# there are certain characters with special meaning in regex,
# including $ * + . ? [ ] ^ { } | ( ) \

# what if you really need the plus sign to be a literal plus sign and not a regex quantifier?
# need to escape it by prepending a backslash

# before a regex is interpreted as a regular expression,
# it is also interpreted by R as a string
# and backslash is used to escape there as well
# so you need to preprend two backslashes in order to match a literal plus sign in a regex

# to escape quotes inside quotes
cat("Do you use \"airquotes\" much?")

# eliminating the need for these escapes is exactly why some use
# double quotes inside single quotes and vice versa  

# to insert newline (\n) or tab (\t)
cat("before the newline\nafter the newline")

cat("before the tab\tafter the tab")

# escapes in regular expressions
# several gapminder country names contain a period
str_subset(countries, "[[:punct:]]") # using a POSIX class

str_subset(countries, pattern = ".") # does not work

# using two backslashes to escape the period
str_subset(countries, pattern = "\\.")

# a square bracket
x <- c("la di da!", "X is distributed U[0,1]")
x

str_subset(x, pattern = "\\[")

# use str_to_lower() to change the text to lower case
browseURL("https://r4ds.had.co.nz/strings.html")