library(tidyverse)
library(readxl)
library(readr)

#Importing excel ----------
foundation <- read_excel("data/pillars-raw.xlsx", 
                        sheet = "Foundation ")
funct <- read_excel("data/pillars-raw.xlsx", 
                    sheet = "Function ")
application <- read_excel("data/pillars-raw.xlsx", 
                          sheet = "Application ")
engagement <- read_excel("data/pillars-raw.xlsx", 
                          sheet = "Engagement ")
drivers <- read_excel("data/pillars-raw.xlsx", 
                          sheet = "Drivers ")

foundation$word <- gsub(",", "", foundation$word)
foundation$word <- gsub('"', '', foundation$word)
funct$word <- gsub(",", "", funct$word)
funct$word <- gsub('"', '', funct$word)
application$word <- gsub(",", "", application$word)
application$word <- gsub('"', '', application$word)
engagement$word <- gsub(",", "", engagement$word)
engagement$word <- gsub('"', '', engagement$word)
drivers$word <- gsub(",", "", drivers$word)
drivers$word <- gsub('"', '', drivers$word)


#Merge with dataset ------------
all <- read_csv("output/keywords-allyear.csv") %>% 
  select(-1)

names(foundation)[1] <- "Combine"
foundation <- foundation %>% 
  mutate(Pillar = "Foundation")
f1 <- merge(all, foundation, by="Combine") %>% 
  group_by(Combine, Pillar) %>% 
  tally()





