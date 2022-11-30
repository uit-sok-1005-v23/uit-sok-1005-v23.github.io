library(haven)
friberg_gronqvist_wine_data <- read_dta("friberg_gronqvist_wine_data.dta")

friberg_gronqvist_wine_data

library(mosaic)
library(tidyverse)

tally(~year, data=friberg_gronqvist_wine_data)
tally(~name, data=friberg_gronqvist_wine_data)
tally(~segm, data=friberg_gronqvist_wine_data)

friberg_gronqvist_wine_data <- friberg_gronqvist_wine_data %>% 
  mutate(Month_Yr = format(as.Date(date), "%Y-%m"))

friberg_gronqvist_wine_data %>% group_by(segm, Month_Yr) %>% 
  summarise(q=sum(litre), x=sum(litre*price)) -> dframe

dframe <- dframe %>% mutate(p=x/q)

# From long to wide
dframe %>% select(segm, Month_Yr, q) %>% tidyr::spread(segm, q) -> qdf
dframe %>% select(segm, Month_Yr, x) %>% tidyr::spread(segm, x) -> xdf
dframe %>% select(segm, Month_Yr, p) %>% tidyr::spread(segm, p) -> pdf

names(qdf) <- c("Month_Yr","q.red","q.spa","q.whi")
names(xdf) <- c("Month_Yr","x.red","x.spa","x.whi")
names(pdf) <- c("Month_Yr","p.red","p.spa","p.whi")

left_join(qdf,xdf, by="Month_Yr") %>% left_join(.,pdf, by="Month_Yr") -> DF

head(DF)



