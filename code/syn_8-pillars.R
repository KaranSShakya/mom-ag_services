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
  select(-1) %>% 
  group_by(Year, Combine) %>% 
  tally() %>% 
  ungroup()
names(foundation)[1] <- "Combine"
names(funct)[1] <- "Combine"
names(application)[1] <- "Combine"
names(engagement)[1] <- "Combine"
names(drivers)[1] <- "Combine"

#All Year Freq ---------------
#Function
tally.all <- function(x){
  x %>% 
    select(1, 3) %>% 
    group_by(Combine) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n))
}


foundation <- foundation %>% 
  mutate(Pillar = "Foundation")
f.merge <- merge(all, foundation, by="Combine") 
f.all <- tally.all(f.merge)

funct <- funct %>% 
  mutate(Pillar = "Function")
fu.merge <- merge(all, funct, by="Combine")
fu.all <- tally.all(fu.merge)

application <- application %>% 
  mutate(Pillar = "Application")
a.merge <- merge(all, application, by="Combine")
a.all <- tally.all(a.merge)

engagement <- engagement %>% 
  mutate(Pillar = "Engagement")
e.merge <- merge(all, engagement, by="Combine")
e.all <- tally.all(e.merge)

drivers <- drivers %>% 
  mutate(Pillar = "Drivers")
d.merge <- merge(all, drivers, by="Combine")
d.all <- tally.all(d.merge)

# write.csv(f.all, file="output/pillars-f.csv")
# write.csv(fu.all, file="output/pillars-fu.csv")
# write.csv(a.all, file="output/pillars-a.csv")
# write.csv(e.all, file="output/pillars-e.csv")
# write.csv(d.all, file="output/pillars-d.csv")










#Year by Year ------------
application <- application %>% 
  mutate(pillar = "App")
drivers <- drivers %>% 
  mutate(pillar = "Dri")
engagement <- engagement %>% 
  mutate(pillar = "Eng")
foundation <- foundation %>% 
  mutate(pillar = "Fou")
funct <- funct %>% 
  mutate(pillar = "Fun")

#Function
merge.pillar <- function(x){
  x1 <- merge(all, x, by="Combine") %>% 
    filter(n>1) %>% 
    arrange(Year, desc(n))
}

pi.app <- merge.pillar(application)
pi.dri <- merge.pillar(drivers)
pi.eng <- merge.pillar(engagement)
pi.fou <- merge.pillar(foundation)
pi.fun <- merge.pillar(funct)

write.csv(pi.app, file="output/pillar-app.csv")
write.csv(pi.dri, file="output/pillar-dri.csv")
write.csv(pi.eng, file="output/pillar-eng.csv")
write.csv(pi.fou, file="output/pillar-fou.csv")
write.csv(pi.fun, file="output/pillar-fun.csv")
 
 








