library(tidyverse)
library(dplyr)
library(readxl)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

data0 <- read_excel("data/synthesis_HKH-copy.xlsx") %>% 
  select(-35:-39)

#Combining and Unique--------
data1 <- data0 %>% 
  select(4, 18, 19)
data1 <- as.data.frame(lapply(data1, tolower)) #all lower case
data1$Auth_keywords <- gsub(" ", "", data1$Auth_keywords)
data1$Index_keywords <- gsub(" ", "", data1$Index_keywords)

data1.a <- data1 %>% 
  select(1,2) %>% 
  na.omit() %>% 
  group_by(Year) %>% 
  summarise(a_key = paste(Auth_keywords, collapse = ";")) %>% 
  ungroup()

data1.i <- data1 %>% 
  select(1,3) %>% 
  na.omit() %>% 
  group_by(Year) %>% 
  summarise(i_key = paste(Index_keywords, collapse = ";")) %>% 
  ungroup()

data2 <- merge(data1.a, data1.i, by="Year", all=T)
data2$Combine <- with(data2, paste0(a_key, i_key))

data3 <- data2 %>% 
  select(1, 4)
data3$Combine <- gsub("NA", "", data3$Combine)

data3.s <- data3 %>% 
  separate_rows(Combine, sep = ";")
data3.s <- data3.s %>% 
  separate_rows(Combine, sep = ",")

#Selecting unique rows
data3.uni <- data3.s %>%
  group_by(Year) %>% 
  distinct()

#Filtering by Year--------
  

