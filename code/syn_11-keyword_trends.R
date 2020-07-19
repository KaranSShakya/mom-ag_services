# Library ----
library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)

# Importing Group ----
a1 <- read_excel("data/keyword-trends.xlsx", 
                             sheet = "Genetic")
a2 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Agronomic")
a3 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Agriculture")
a4 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Ecosystem")
a5 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Soil")
a6 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Water")
a7 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Livlihood")
a8 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Food")
a9 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Social")
a10 <- read_excel("data/keyword-trends.xlsx", 
                 sheet = "Support")
a11 <- read_excel("data/keyword-trends.xlsx", 
                  sheet = "Sustainability")
a12 <- read_excel("data/keyword-trends.xlsx", 
                  sheet = "Agrobiodiversity")
a13 <- read_excel("data/keyword-trends.xlsx", 
                  sheet = "Drivers")

# Importing Base Keywords + Join ----
base0 <- read_csv("output/keywords_combined_all.csv")

base1 <- base0 %>% 
  select(-1)

names(base1)[2] <- "Keyword"

a1$Keyword <- gsub(" ", "", a1$Keyword)
a2$Keyword <- gsub(" ", "", a2$Keyword)
a3$Keyword <- gsub(" ", "", a3$Keyword)
a4$Keyword <- gsub(" ", "", a4$Keyword)
a5$Keyword <- gsub(" ", "", a5$Keyword)
a6$Keyword <- gsub(" ", "", a6$Keyword)
a7$Keyword <- gsub(" ", "", a7$Keyword)
a8$Keyword <- gsub(" ", "", a8$Keyword)
a9$Keyword <- gsub(" ", "", a9$Keyword)
a10$Keyword <- gsub(" ", "", a10$Keyword)
a11$Keyword <- gsub(" ", "", a11$Keyword)
a12$Keyword <- gsub(" ", "", a12$Keyword)
a13$Keyword <- gsub(" ", "", a13$Keyword)

base2 <- merge(base1, a1, by="Keyword", all=T)
base2 <- merge(base2, a2, by="Keyword", all=T)
base2 <- merge(base2, a3, by="Keyword", all=T)
base2 <- merge(base2, a4, by="Keyword", all=T)
base2 <- merge(base2, a5, by="Keyword", all=T)
base2 <- merge(base2, a6, by="Keyword", all=T)
base2 <- merge(base2, a7, by="Keyword", all=T)
base2 <- merge(base2, a8, by="Keyword", all=T)
base2 <- merge(base2, a9, by="Keyword", all=T)
base2 <- merge(base2, a10, by="Keyword", all=T)
base2 <- merge(base2, a11, by="Keyword", all=T)
base2 <- merge(base2, a12, by="Keyword", all=T)
base2 <- merge(base2, a13, by="Keyword", all=T)

remove(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, base0)

# Tally Each ----
base3 <- base2 %>% 
  select(-1)

Gen <- base3 %>% 
  select(1,2) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup()

Agr <- base3 %>% 
  select(1,3) %>% 
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup()

names(Gen)[2] <- "Genetic"
names(Agr)[2] <- "Agronomic"

Gen.agr <- merge(x=Gen, y=Agr, by="Year", all=T) %>% 
  gather(key="Attribute", value="n", 2:3)

Ag <- base3 %>% 
  select(1,4) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup()
names(Ag)[2] <- "Agriculture"

Soil <- base3 %>% 
  select(1,6) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup()
names(Soil)[2] <- "Soil"

Water <- base3 %>% 
  select(1,7) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup()
names(Water)[2] <- "Water"

Agroecosystem <- merge(x=Ag, y=Soil, by="Year", all=T)
Agroecosystem <- merge(x=Agroecosystem, y=Water, by="Year", all=T)

# Relative Tally ----
year <- base1 %>% 
  select(1) %>% 
  group_by(Year) %>% 
  tally()

Gen.year <- merge(x=Gen, y=year, by="Year") %>% 
  mutate(Relative = (Genetic/n)*100)
names(Gen.year)[4] <- "Gen_relative"

Agroecology <- merge(x=Gen.year, y=Agr, by="Year", all=T)
Agroecology.r <- Agroecology %>% 
  mutate(Agr_relative = (Agronomic/n)*100) %>% 
  filter(Year < 2020)

Agroecosystem <- merge(x=Agroecosystem, y=year, by="Year", all=T)
Agroecosystem.r <- Agroecosystem %>%   
  mutate(Ag_relative = (Agriculture/n)*100, Soil_relative = (Soil/n)*100,
         Water_relative = (Water/n)*100) %>% 
  filter(Year < 2020)

#
# GGplot (Test) ----
plot.simple <- ggplot(Gen, aes(x=Year, y=Genetic))+
  geom_line()+
  geom_smooth(method = "loess", se=F)+
  ggtitle("Genetic")+
  labs(x="")+
  theme_classic(base_size = 12)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold", size=14))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))

plot.simple2 <- ggplot(Agr, aes(x=Year, y=Agronomic))+
  geom_line()+
  geom_smooth(method = "loess", se=F)+
  ggtitle("Agronomic Practices")+
  labs(x="")+
  theme_classic(base_size = 12)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold", size=14))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 20))

plot.combine <- ggplot(Gen.agr, aes(x=Year, y=n, color=Attribute))+
  geom_smooth(method = "loess", se=F)+
  ggtitle("Agroecology")+
  labs(x="", color="")+
  theme_classic(base_size = 12)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold", size=14),
        legend.position = c(0.2, 0.7))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 20))

# GGplot Final ----
agroecology.plot <- ggplot(Agroecology.r, aes(x=Year))+
  geom_smooth(aes(y=Genetic), color="green4", se=F, size=2)+
  geom_smooth(aes(y=Agronomic), color="seagreen2", se=F, size=2)+
  geom_smooth(aes(y=Gen_relative), color="green4", se=F, linetype = "dashed", size=1)+
  geom_smooth(aes(y=Agr_relative), color="seagreen2", se=F, linetype = "dashed", size=1)+
  scale_y_continuous("Absolute Trend", 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)"))+
  ggtitle("Agroecology")+
  labs(x="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))

agroecosystem.plot <- ggplot(Agroecosystem.r, aes(x=Year))+
  geom_smooth(aes(y=Agriculture), color="dodgerblue2", se=F, size=2)+
  geom_smooth(aes(y=Soil), color="turquoise", se=F, size=2)+
  geom_smooth(aes(y=Water), color="lightslateblue", se=F, size=2)+
  geom_smooth(aes(y=Ag_relative), color="dodgerblue2", se=F, linetype = "dashed", size=1)+
  geom_smooth(aes(y=Soil_relative), color="turquoise", se=F, linetype = "dashed", size=1)+
  geom_smooth(aes(y=Water_relative), color="lightslateblue", se=F, linetype="dashed", size=1)+
  scale_y_continuous("Absolute Trend", 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)"))+
  ggtitle("Agroecosystem")+
  labs(x="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))
