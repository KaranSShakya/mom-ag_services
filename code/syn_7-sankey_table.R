library(tidyverse)
library(splitstackshape)

#Year - Collaboration ------------
source("code/syn_5-institute.r")
remove(data.c, data0, data1, data2, data3, inst_collaboration, inst.95_00,
       inst.01_05, inst.06_10, inst.11_15, inst.16_20, inst.merge, inst.y, inst0)
remove(inst.final)

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

data2 <- merge(data1.1, data1.2, by="Geog", all=T)
data2 <- merge(data2, data1.3, by="Geog", all=T)
data2 <- merge(data2, data1.4, by="Geog", all=T)
data2 <- merge(data2, data1.5, by="Geog", all=T)

data2[is.na(data2)] <- "0"

names(data2)[1] <- "HKH_Country"
names(data2)[2] <- "1995-2000"
names(data2)[3] <- "2001-2005"
names(data2)[4] <- "2006-2010"
names(data2)[5] <- "2011-2015"
names(data2)[6] <- "2016-2020"

year.country <- data2 %>% 
  gather(key="Year", value="Country_count", 2:6) %>% 
  select(2,1,3) 
year.country$Country_count <- as.numeric(year.country$Country_count)

remove(data1, data1.1, data1.2, data1.3, data1.4, data1.5, data0, data2)

year.country1  <- year.country %>%  
  uncount(Country_count) %>% 
  na.omit()

remove(year.country)

#data.frame(dat[rep(seq_len(dim(dat)[1]), dat$count), 2, drop = FALSE], row.names=NULL)

#Country - Collaboration -------------
inst0 <- read_csv("output/hkh-institution_unedited.csv") %>% 
  select(-1,-2)

inst.sep <- inst0 %>% 
  separate_rows(aff, sep = ",") %>% 
  group_by(Year, aff) %>%
  count(col) %>% 
  ungroup()

inst.sep1 <- inst.sep %>% 
  filter(Year >= 1995 & Year <= 2000) %>% 
  select(-1) %>% 
  group_by(aff, col) %>% 
  summarise(sum95 = sum(n)) %>% 
  ungroup()

inst.sep2 <- inst.sep %>% 
  filter(Year >= 2001 & Year <= 2005) %>% 
  select(-1) %>% 
  group_by(aff, col) %>% 
  summarise(sum01 = sum(n)) %>% 
  ungroup()

inst.sep3 <- inst.sep %>% 
  filter(Year >= 2006 & Year <= 2010) %>% 
  select(-1) %>% 
  group_by(aff, col) %>% 
  summarise(sum06 = sum(n)) %>% 
  ungroup()

inst.sep4 <- inst.sep %>% 
  filter(Year >= 2011 & Year <= 2015) %>% 
  select(-1) %>% 
  group_by(aff, col) %>% 
  summarise(sum11 = sum(n)) %>% 
  ungroup()

inst.sep5 <- inst.sep %>% 
  filter(Year >= 2016 & Year <= 2020) %>% 
  select(-1) %>% 
  group_by(aff, col) %>% 
  summarise(sum16 = sum(n)) %>% 
  ungroup()

inst.merge <- merge(inst.sep1, inst.sep2, by=c("aff", "col"), all=T)
inst.merge <- merge(inst.merge, inst.sep3, by=c("aff", "col"), all=T)
inst.merge <- merge(inst.merge, inst.sep4, by=c("aff", "col"), all=T)
inst.merge <- merge(inst.merge, inst.sep5, by=c("aff", "col"), all=T)

names(inst.merge)[1] <- "HKH_Country"
names(inst.merge)[3] <- "1995-2000"
names(inst.merge)[4] <- "2001-2005"
names(inst.merge)[5] <- "2006-2010"
names(inst.merge)[6] <- "2011-2015"
names(inst.merge)[7] <- "2016-2020"

inst.fall <- inst.merge %>% 
  gather(key="Year", value="value", 3:7)
inst.fall[is.na(inst.fall)] <- "0"

remove(inst.merge, inst.sep, inst.sep1, inst.sep2, inst.sep3, inst.sep4,
       inst.sep5, inst0)

#Combining Year-Country-Collaboration --------
axis3 <- merge(inst.fall, year.country1, by=c("Year", "HKH_Country"), all=T)

axis3$Year <- as.factor(axis3$Year)
axis3$HKH_Country <- as.factor(axis3$HKH_Country)
axis3$col <- as.factor(axis3$col)

names(axis3)[2] <- "Country"
names(axis3)[3] <- "Collaboration"
names(axis3)[4] <- "Frequency"

axis3.c <- axis3 %>% 
  filter(Country == "Afghanistan" | Country=="Bangladesh" | 
         Country == "Bhutan" | Country == "China" | Country == "India" | 
         Country == "Myanmar" | Country == "Nepal" | Country == "Pakistan")

axis3.c$Collaboration <- gsub("w", "Within HKH", axis3.c$Collaboration)
axis3.c$Collaboration <- gsub("e", "Outside HKH", axis3.c$Collaboration)
axis3.c$Collaboration <- gsub("q", "Sole", axis3.c$Collaboration)

remove(axis3, inst.fall, year.country1)

               

