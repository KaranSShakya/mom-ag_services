#Library------
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)

key <- read_csv("output/keywords_combined_all.csv") %>%   
  ungroup() %>% 
  select(-1) %>% 
  mutate(id=seq(1:12127)) %>% 
  select(3,1,2)
names(key)[3] <- "Keyword"

#
#Group division-----
# found <- c("cropdiversity", "livestock", "geneticdiversity", "landraces", 
#   "paddylandraces", "ricelandraces", "traditionalcrops", "cropproduction",
#   "cropimprovement", "germplasm")
# 
# func <- c("ecosystemservice", "ecosystemservices", "traditionalknowledge",
#           "integratedfarming", "integratedapproach", "integratedfarmingsystems",
#           "integratedapproach", "sustainableintensification", "conservation", 
#           "conservationagriculture", "ecologicalagriculture", "	integratedpestmanagement",
#           "soilnutrient", "watermanagement")
# 
# appli <- c("foodsecurity", "agribusiness", "foodsupply", "agritourism",
#            "livelihood", "livelihoods", "farmincome", "nutrition")
# 
# engage <- c("sustainability", "sustainableagriculture", "adaptation",
#             "resilience", "ecosystemresilience")
# 
# drive <- c("climatechange", "landusechange", "pesticide", "pesticides")

#Group Import-------
found <- read_excel("data/keyword-groups.xlsx", 
                    sheet = "foundation")
func <- read_excel("data/keyword-groups.xlsx", 
                    sheet = "functional")
appli <- read_excel("data/keyword-groups.xlsx", 
                    sheet = "application")
engage <- read_excel("data/keyword-groups.xlsx", 
                    sheet = "engagement")
drivers <- read_excel("data/keyword-groups.xlsx", 
                    sheet = "drivers")

#Merge
key.f <- merge(key, found, by="Keyword")
key.fu <- merge(key, func, by="Keyword")
key.a <- merge(key, appli, by="Keyword")
key.e <- merge(key, engage, by="Keyword")
key.d <- merge(key, drivers, by="Keyword")

remove(found, func, appli, engage, drivers)





#Function test--------
key.f1 <- key.f %>% 
  select(3,1) %>% 
  group_by(Year) %>% 
  tally()
names(key.f1)[2] <- "foundation"

key_count <- function(x){
  x1 <- x %>% 
    select(3,1) %>% 
    group_by(Year) %>% 
    tally()
}

key.fu1 <- key_count(key.fu)
names(key.fu1)[2] <- "functional"
key.a1 <- key_count(key.a)
names(key.a1)[2] <- "application" 
key.d1 <- key_count(key.d)
names(key.d1)[2] <- "drivers"
key.e1 <- key_count(key.e)
names(key.e1)[2] <- "engagement"

remove(key.a, key.d, key.e, key.f, key.fu)



#Merge all--------
all <- merge(key.f1, key.fu1, by="Year", all=T)
all <- merge(all, key.a1, by="Year", all=T)
all <- merge(all, key.e1, by="Year", all=T)
all <- merge(all, key.d1, by="Year", all=T)

all[is.na(all)] <- "0"

all.l <- all %>% 
  gather(key="Attribute", value="Freq", 2:6)

all.l$Freq <- as.numeric(as.character(all.l$Freq))

all.final <- all.l %>% 
  group_by(Year, Attribute) %>% 
  summarise(Sum = sum(Freq)) %>% 
  ungroup()

all.select <- all.final %>% 
  filter(Year >= 1996 & Year <= 2019) %>% 
  filter(Attribute != "drivers")

pillars <- ggplot(all.select, aes(x=Year, y=Sum))+
  geom_line(aes(color=Attribute))+
  labs(y="Frequency of Keywords", color="Keyword Attribute")+
  theme_bw(base_size = 11)+
  scale_y_continuous(breaks = seq(0, 35, 5))+
  scale_x_continuous(limits = c(1996, 2019), breaks = seq(1996, 2018, 2))
  
all.drivers <- all.final %>% 
  filter(Year >= 1996 & Year <= 2019) %>% 
  filter(Attribute == "drivers")

pillars.drivers <- ggplot(all.drivers, aes(x=Year, y=Sum))+
  geom_line(aes(color=Attribute))+
  labs(y="Frequency of Keywords", color="Keyword Attribute")+
  scale_color_manual(values=c("yellow3"))+
  theme_bw(base_size = 11)+
  scale_y_continuous(breaks = seq(0, 35, 5))+
  scale_x_continuous(limits = c(1996, 2019), breaks = seq(1996, 2018, 2))

geom_area <- ggplot(all.final, aes(x=Year, y=Sum))+
  geom_area(aes(color=Attribute))+
  labs(y="Count of Keywords")
