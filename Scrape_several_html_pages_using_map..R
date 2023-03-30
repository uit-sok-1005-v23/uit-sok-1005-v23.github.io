rm(list=ls())

library(rvest)
library(tidyverse)
library(rlist)

# The URL's

# static course
browseURL("http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list")

course <- list("SOK-3008-1","SOK-3020-1","SOK-1005-1")

semester <- 
  list(y = c(20, 21, 22),
       s = c("v","h"),
       sep = c("")) %>% 
  cross() %>% map(lift(paste)) 

semester

df <- expand.grid(semester,course)
df

# empty list
url <- list()

for (i in 1:length(df$Var1)) {
  url[i] <- paste("http://timeplan.uit.no/emne_timeplan.php?sem=",df[i,1],"&module%5B%5D=",df[i,2],"&View=list", sep="")
}

url

# use function
scrape <- function(url) {
  return(read_html(unlist(url)) %>% 
           html_nodes(., 'table') %>% 
           html_table(., header=FALSE, fill=TRUE, trim=TRUE)) }

# map the function on the list of url's
dlist <- map(url, safely(scrape))

dlist[[3]]

dlist[[3]][[1]]

dframe <- dlist %>% map('result') %>% map_df(., bind_rows)

# use function
scrape2 <- function(url) {
  return(read_html(unlist(url)) %>% 
           html_nodes(., 'table') %>% 
           html_table(., header=FALSE, fill=TRUE, trim=TRUE) %>% 
           list.stack(.)) }

# map the function on the list of url's
dlist2 <- map(url, safely(scrape2))

dframe2 <- dlist2 %>% map('result') %>% map_df(., bind_rows)
