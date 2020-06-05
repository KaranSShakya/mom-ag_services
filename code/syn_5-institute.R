#Library-------
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(readr)

data0 <- read_excel("data/synthesis_HKH-final.xlsx") %>% 
  mutate(keyid = seq(1, 838))

data1 <- data0 %>% 
  select(42, 4, 15) %>% 
  na.omit() 

data2 <- data1 %>% 
  separate_rows(Affiliations, sep=";")

data2$Affiliations <- word(data2$Affiliations, -1)

data3 <- data2 %>% 
  group_by(keyid, Year) %>% 
  summarise(aff = paste(Affiliations, collapse = ",")) %>% 
  ungroup()

#write.csv(data3, file="output/hkh-institution_unedited.csv")

inst0 <- read_csv("output/hkh-institution.csv")

inst.y <- inst0 %>% 
  select(2,4) %>% 
  group_by(Year, collaboration) %>% 
  tally() %>% 
  ungroup()

inst.95_00 <- inst.y %>% 
  filter(Year >= 1995 & Year <= 2000) %>% 
  select(-1) %>% 
  group_by(collaboration) %>% 
  summarise(sum_95 = sum(n))

inst.01_05 <- inst.y %>% 
  filter(Year >= 2001 & Year <= 2005) %>% 
  select(-1) %>% 
  group_by(collaboration) %>% 
  summarise(sum_01 = sum(n))

inst.06_10 <- inst.y %>% 
  filter(Year >= 2006 & Year <= 2010) %>% 
  select(-1) %>% 
  group_by(collaboration) %>% 
  summarise(sum_06 = sum(n))

inst.11_15 <- inst.y %>% 
  filter(Year >= 2011 & Year <= 2015) %>% 
  select(-1) %>% 
  group_by(collaboration) %>% 
  summarise(sum_11 = sum(n))

inst.16_20 <- inst.y %>% 
  filter(Year >= 2016 & Year <= 2020) %>% 
  select(-1) %>% 
  group_by(collaboration) %>% 
  summarise(sum_16 = sum(n))

inst.merge <- merge(inst.95_00, inst.01_05, by="collaboration")
inst.merge <- merge(inst.merge, inst.06_10, by="collaboration")
inst.merge <- merge(inst.merge, inst.11_15, by="collaboration")
inst.merge <- merge(inst.merge, inst.16_20, by="collaboration")

names(inst.merge)[1] <- "Collaboration_type"
names(inst.merge)[2] <- "1995-2000"
names(inst.merge)[3] <- "2001-2005"
names(inst.merge)[4] <- "2006-2010"
names(inst.merge)[5] <- "2011-2015"
names(inst.merge)[6] <- "2016-2020"

inst.final <- inst.merge %>% 
  gather(key="Year", value="count", 2:6)
inst.final$Collaboration_type <- as.factor(inst.final$Collaboration_type)
inst.final$Collaboration_type <- factor(inst.final$Collaboration_type, 
                                        levels = c("q", "w", "e", "r"))

inst_collaboration <- ggplot(inst.final, aes(x=Year, y=count, fill=Collaboration_type))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(x="Year", y="Frequency", fill="Collaboration Type")+
  theme_bw(base_size = 10)+
  scale_y_continuous(breaks = seq(0, 300, 25))+
  scale_fill_discrete(labels=c("Sole HKH Country", "Two or more HKH Countries",
                               "HKH and International Countries", 
                               "Only International Countrues"))

data.c <- data3 %>% 
  separate_rows(aff, sep=",") %>% 
  group_by(aff) %>% 
  tally()


  








