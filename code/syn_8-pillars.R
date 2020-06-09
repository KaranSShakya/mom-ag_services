library(tidyverse)
library(readxl)
library(readr)

#Function -----------
tally.all <- function(x){
  x %>% 
    select(1, 3) %>% 
    group_by(Combine) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n))
}

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
  select(-1) %>% 
  group_by(Year, Combine) %>% 
  tally() %>% 
  ungroup()

names(foundation)[1] <- "Combine"
foundation <- foundation %>% 
  mutate(Pillar = "Foundation")
f.merge <- merge(all, foundation, by="Combine") 
f.all <- tally.all(f.merge)

names(funct)[1] <- "Combine"
funct <- funct %>% 
  mutate(Pillar = "Function")
fu.merge <- merge(all, funct, by="Combine")
fu.all <- tally.all(fu.merge)

names(application)[1] <- "Combine"
application <- application %>% 
  mutate(Pillar = "Application")
a.merge <- merge(all, application, by="Combine")
a.all <- tally.all(a.merge)

names(engagement)[1] <- "Combine"
engagement <- engagement %>% 
  mutate(Pillar = "Engagement")
e.merge <- merge(all, engagement, by="Combine")
e.all <- tally.all(e.merge)

names(drivers)[1] <- "Combine"
drivers <- drivers %>% 
  mutate(Pillar = "Drivers")
d.merge <- merge(all, drivers, by="Combine")
d.all <- tally.all(d.merge)

# write.csv(f.all, file="output/pillars-f.csv")
# write.csv(fu.all, file="output/pillars-fu.csv")
# write.csv(a.all, file="output/pillars-a.csv")
# write.csv(e.all, file="output/pillars-e.csv")
# write.csv(d.all, file="output/pillars-d.csv")





