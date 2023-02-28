

rm(list=ls())

# load packages
library(tidyverse)
library(rvest)
library(janitor)
library(ggrepel)

#browseURL("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"

webpage <- read_html(url)
webpage
str(webpage) # list with "nothing special in it!"

# there are 2 div table's
html_nodes(webpage, "div table")

html_table(html_nodes(webpage, "div table")[[1]])
html_table(html_nodes(webpage, "div table")[[2]])

# [[1]] is the one we want, 
table <- 
  html_table(html_nodes(webpage, "div table")[[1]]) %>% 
  as_tibble()

#View(table)
head(table)

table <- table %>% 
  row_to_names(row_number = 1) %>% # remove the names/first row
  clean_names() %>% 
  separate(wltp_tall, into = c("wltp", "kwh"), sep = "/") %>% 
  separate(wltp, into = c("wltp","km"), sep = " ") %>%
  select(-km) %>% 
  separate(kwh, into = c("kwh", "kwh_tex"), sep = " ") %>%
  select(-kwh_tex) %>% 
  separate(stopp, into = c("stop","km"), sep = " ") %>% 
  select(-km) %>% 
  separate(avvik, into = c("avvik","percent"), sep = " ") %>% 
  select(-percent) %>% 
  mutate_at(vars(wltp, kwh,stop, avvik),~as.numeric(str_replace(., ",", ".")))

names(table)[1]<- "model"
table

###############################
# Alternative 2
##########################
dframe <- html_table(html_nodes(webpage, "div table")[[1]]) %>% 
  as_tibble()

dframe <-
  dframe %>% 
  row_to_names(row_number = 1) %>% # remove the names/first row
  clean_names() %>% 
  separate(wltp_tall, c("wltp","kwh"), sep = "/") %>%
  mutate(wltp=str_remove_all(wltp, "[km]"),
         stopp=str_remove_all(stopp, "[km]"),
         kwh=str_remove_all(kwh, "[kWh]"),
         avvik=str_remove_all(avvik, "[%]"),
         kwh=str_replace(kwh, pattern = ",", replacement = "."),
         avvik=str_replace(avvik, pattern = ",", replacement = "."),
         stopp=str_replace(stopp, pattern = "x", replacement = ""),
         avvik=str_replace(avvik, pattern = "x", replacement = "")) %>% 
  mutate_at(c("wltp","kwh","stopp","avvik"), ~as.numeric(.))

names(dframe)[1] <- "modell" 

dframe


dframe %>% 
  ggplot(aes(x=wltp, y=stopp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour="red") +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed() +
  geom_label_repel(aes(label = modell)) +
  theme_classic()

dframe %>% 
  ggplot(aes(x=wltp, y=stopp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour="red") +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed() +
  geom_label_repel(aes(label = modell),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()


dframe %>% 
  ggplot(aes(x=wltp, y=stopp, label=modell)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour="red") +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed() +
  geom_text_repel(min.segment.length = 0, seed = 45, box.padding = 0.5) +
  theme_classic()

dframe %>% 
  ggplot(aes(x=wltp, y=stopp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour="red") +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed() +
  theme_classic()


dframe %>% 
  ggplot(aes(x=wltp, y=stopp)) +
  geom_point() +
  xlim(200,600) +
  ylim(200,600) +
  geom_abline(intercept = 0, slope = 1, colour="red") +
  coord_fixed() +
  theme_classic()


summary(lm(stopp ~ wltp, data = dframe))
