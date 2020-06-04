#Library-------
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)

data0 <- read_excel("data/synthesis_HKH-copy.xlsx") %>% 
  select(-35:-39)

data1 <- data0 %>% 
  select(1, 4, 15) %>% 
  mutate(id = seq(1, 905)) %>% 
  select(4,2,3) %>% 
  na.omit() 

data2 <- data1 %>% 
  separate_rows(Affiliations, sep=";")

data2$Affiliations <- word(data2$Affiliations, -1)

data3 <- data2 %>% 
  group_by(id, Year) %>% 
  summarise(aff = paste(Affiliations, collapse = ",")) %>% 
  ungroup()
