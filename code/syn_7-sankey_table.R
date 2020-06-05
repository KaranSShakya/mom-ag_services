library(tidyverse)

#Year - Collaboration ------------
source("code/syn_5-institute.r")
remove(data.c, data0, data1, data2, data3, inst_collaboration, inst.95_00,
       inst.01_05, inst.06_10, inst.11_15, inst.16_20, inst.merge, inst.y, inst0)

#Year - HKH Country (Geog Unit) -------------
data0 <- read_excel("data/synthesis_HKH-final.xlsx")
names(data0)[34] <- "Geog"
data0$Geog <- gsub("HKH-NP", "Nepal", data0$Geog)
data0$Geog <- gsub("HKH-CN", "China", data0$Geog)
data0$Geog <- gsub("HKH-IN", "India", data0$Geog)
data0$Geog <- gsub("HKH-BD", "Bangladesh", data0$Geog)
data0$Geog <- gsub("HKH-BU", "Bhutan", data0$Geog)
data0$Geog <- gsub("HKH-PK", "Pakistan", data0$Geog)
data0$Geog <- gsub("HKH_NP", "Nepal", data0$Geog)
data0$Geog <- gsub("NKH-NP", "Nepal", data0$Geog)
data0$Geog <- gsub("HKH_BD", "Bangladesh", data0$Geog)
data0$Geog <- gsub("HKH-AF", "Afghanistan", data0$Geog)
data0$Geog <- gsub("Otheers", "Others", data0$Geog)
data0$Geog <- gsub("HKh-PK", "Pakistan", data0$Geog)
data0$Geog <- gsub("HKh-IN", "India", data0$Geog)
data0$Geog <- gsub("HH-PK", "Pakistan", data0$Geog)
data0$Geog <- gsub("HKH-MM", "Myanmar", data0$Geog)
data0$Geog <- gsub("HKH-MN", "Myanmar", data0$Geog)
data0$Geog <- gsub("HKh_CN", "China", data0$Geog)
data0$Geog <- gsub("HKH_IN", "India", data0$Geog)
data0$Geog <- gsub("HKH", "NA", data0$Geog)

data1 <- data0 %>% 
  select(4, 34) %>% 
  group_by(Year, Geog) %>% 
  tally() %>% 
  ungroup()

data1.1 <- data1 %>% 
  filter(Year >= 1996 & Year <= 2000) %>% 
  select(-1) %>% 
  group_by(Geog) %>% 
  summarise(sum95 = sum(n)) %>% 
  ungroup()
  
data1.2 <- data1 %>% 
  filter(Year >= 2001 & Year <= 2005) %>% 
  select(-1) %>% 
  group_by(Geog) %>% 
  summarise(sum01 = sum(n)) %>% 
  ungroup()

data1.3 <- data1 %>% 
  filter(Year >= 2006 & Year <= 2010) %>% 
  select(-1) %>% 
  group_by(Geog) %>% 
  summarise(sum06 = sum(n)) %>% 
  ungroup() %>% 
  na.omit()

data1.4 <- data1 %>% 
  filter(Year >= 2011 & Year <= 2015) %>% 
  select(-1) %>% 
  group_by(Geog) %>% 
  summarise(sum11 = sum(n)) %>% 
  ungroup() 

data1.5 <- data1 %>% 
  filter(Year >= 2016 & Year <= 2020) %>% 
  select(-1) %>% 
  group_by(Geog) %>% 
  summarise(sum16 = sum(n)) %>% 
  ungroup() 

data2 <- merge(data1.1, data1.2, by="Geog")
data2 <- merge(data2, data1.3, by="Geog")
data2 <- merge(data2, data1.4, by="Geog")
data2 <- merge(data2, data1.5, by="Geog")

names(data2)[1] <- "HKH_Country"
names(data2)[2] <- "1995-2000"
names(data2)[3] <- "2001-2005"
names(data2)[4] <- "2006-2010"
names(data2)[5] <- "2011-2015"
names(data2)[6] <- "2016-2020"

year_hkh <- data2 %>% 
  gather(key="Year", value="Country_count", 2:6) %>% 
  select(2,1,3)

remove(data1, data1.1, data1.2, data1.3, data1.4, data1.5, data0, data2)





#First 3 Merge --------------
names(inst.final)[3] <- "Collaboration_count"
