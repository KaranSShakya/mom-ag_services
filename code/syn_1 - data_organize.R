library(tidyverse)
library(readxl)

#Import excel
data0 <- read_excel("data/synthesis_HKH-copy.xlsx") %>% 
  select(-35:-39)

#By year
data.year <- data0 %>% 
  select(4) %>%
  group_by(Year) %>% 
  tally()

#By country
unique(data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-NP", "Nepal", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-CN", "China", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-IN", "India", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-BD", "Bangladesh", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-BU", "Bhutan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-PK", "Pakistan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH_NP", "Nepal", data0$Geog_unit)
data0$Geog_unit <- gsub("NKH-NP", "Nepal", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH_BD", "Bangladesh", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-AF", "Afghanistan", data0$Geog_unit)
data0$Geog_unit <- gsub("Otheers", "Others", data0$Geog_unit)
data0$Geog_unit <- gsub("HKh-PK", "Pakistan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKh-IN", "India", data0$Geog_unit)
data0$Geog_unit <- gsub("HH-PK", "Pakistan", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-MM", "Myanmar", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH-MN", "Myanmar", data0$Geog_unit)
data0$Geog_unit <- gsub("HKh_CN", "China", data0$Geog_unit)
data0$Geog_unit <- gsub("HKH_IN", "India", data0$Geog_unit)
unique(data0$Geog_unit)

data.country <- data0 %>% 
  select(34) %>% 
  group_by(Geog_unit) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate('% of Total' = (n/905)*100)
names(data.country)[1] <- "Country"
names(data.country)[2] <- "Count"
data.country$`% of Total` <- round(data.country$`% of Total`, 1)


