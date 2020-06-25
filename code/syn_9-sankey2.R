library(tidyverse)
library(readxl)
library(readr)

data0 <- read_excel("data/synthesis_HKH-final.xlsx") %>% 
  mutate(Keyid = seq(1, 838, 1))

data1 <- data0 %>% 
  select(4,34,15,18,19,42)

remove(data0)

#Geog Unit Fix ----
names(data1)[2] <- "Geog_unit"
unique(data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-NP", "Nepal", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-CN", "China", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-IN", "India", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-BD", "Bangladesh", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-BU", "Bhutan", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-PK", "Pakistan", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH_NP", "Nepal", data1$Geog_unit)
data1$Geog_unit <- gsub("NKH-NP", "Nepal", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH_BD", "Bangladesh", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-AF", "Afghanistan", data1$Geog_unit)
data1$Geog_unit <- gsub("Others", "Others", data1$Geog_unit)
data1$Geog_unit <- gsub("HKh-PK", "Pakistan", data1$Geog_unit)
data1$Geog_unit <- gsub("HKh-IN", "India", data1$Geog_unit)
data1$Geog_unit <- gsub("HH-PK", "Pakistan", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-MM", "Myanmar", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH-MN", "Myanmar", data1$Geog_unit)
data1$Geog_unit <- gsub("HKh_CN", "China", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH_IN", "India", data1$Geog_unit)
data1$Geog_unit <- gsub("HK_PK", "Pakistan", data1$Geog_unit)
data1$Geog_unit <- gsub("HKH", "Others", data1$Geog_unit)
data1$Geog_unit <- gsub("NA", "Others", data1$Geog_unit)
unique(data1$Geog_unit)


#Keywords Combined ----
names(data1)[4] <- "Auth_keywords"
names(data1)[5] <- "Index_keywords"

comb0 <- data1 %>% 
  select(4,5,6)

comb0 <- as.data.frame(lapply(comb0, tolower)) #lower case
comb0$Auth_keywords <- gsub(" ", "", comb0$Auth_keywords) #remove space
comb0$Index_keywords <- gsub(" ", "", comb0$Index_keywords)

comb0 <- comb0 %>% 
  separate_rows(Auth_keywords, sep=";") %>% 
  separate_rows(Index_keywords, sep=";") %>% 
  mutate(Identical = Auth_keywords == Index_keywords)

comb0$Identical <- comb0$Identical %>% 
  replace_na("NO")

comb0 <- comb0 %>% 
  filter(Identical != "TRUE") %>% 
  select(-4)

comb.a <- comb0 %>% 
  select(1,3) %>% 
  group_by(Auth_keywords, Keyid) %>% 
  distinct(Auth_keywords, .keep_all = T) %>% 
  ungroup() %>% 
  group_by(Keyid) %>% 
  summarise(a_key = paste(Auth_keywords, collapse = ";")) %>% 
  ungroup()

comb.i <- comb0 %>% 
  select(2,3) %>% 
  group_by(Index_keywords, Keyid) %>% 
  distinct(Index_keywords, .keep_all = T) %>% 
  ungroup() %>% 
  group_by(Keyid) %>% 
  summarise(i_key = paste(Index_keywords, collapse = ";")) %>% 
  ungroup()

comb1 <- merge(comb.a, comb.i, by="Keyid")
comb1$Combine <- with(comb1, paste0(a_key, i_key))
comb1 <- comb1 %>% 
  select(-2,-3)
comb1$Combine <- gsub("NA", "", comb1$Combine)
comb1$Keyid <- as.numeric(comb1$Keyid)

data2 <- merge(data1, comb1, by="Keyid") %>% 
  select(-5,-6)

remove(comb0, comb1, comb.a, comb.i)


#Affiliations ----
inst0 <- read_csv("output/hkh-institution_unedited.csv") %>% 
  select(-1)
names(inst0)[1] <- "Keyid"
names(inst0)[3] <- "Institution"

inst1 <- inst0 %>% 
  select(1,4)

data3 <- merge(data2, inst1, by="Keyid", all=T) %>% 
  select(-4)

remove(inst0, inst1, data1, data2)


#Year Intervals ----
data3$Year_int <- NA

data3$Year_int <- ifelse(data3$Year <= 2000, "1996-2000",
                         ifelse(data3$Year >= 2001 & data3$Year <= 2005, "2001-2005",
                         ifelse(data3$Year >= 2006 & data3$Year <= 2010, "2006-2010",
                         ifelse(data3$Year >= 2011 & data3$Year <= 2015, "2011-2015",
                                "2016-2020"))))
data3$Year_int <- as.factor(data3$Year_int)



#Collaboration Text ----
names(data3)[5] <- "Collaboration"
data3$Collaboration <- as.factor(data3$Collaboration)

data3$Collaboration <- recode(data3$Collaboration, q = "Sole Country", 
       w = "Two or more HKH Countries",
       e = "HKH and International Countries",
       r = "Only International Countries")
