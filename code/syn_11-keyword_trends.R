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

## Tally Each ----
base3 <- base2 %>% 
  select(-1)

Gen <- base3 %>% 
  select(1,2) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)

Agr <- base3 %>% 
  select(1,3) %>% 
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)

names(Gen)[2] <- "Genetic"
names(Agr)[2] <- "Agronomic"

Ag <- base3 %>% 
  select(1,4) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Ag)[2] <- "Agriculture"

Eco <- base3 %>% 
  select(1,5) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Eco)[2] <- "Ecosystem"


Soil <- base3 %>% 
  select(1,6) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Soil)[2] <- "Soil"

Water <- base3 %>% 
  select(1,7) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Water)[2] <- "Water"

Agroecosystem <- merge(x=Ag, y=Soil, by="Year", all=T)
Agroecosystem <- merge(x=Agroecosystem, y=Water, by="Year", all=T)
Agroecosystem <- merge(x=Agroecosystem, y=Eco, by="Year", all=T)

Livelihood <- base3 %>% 
  select(1,8) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Livelihood)[2] <- "Livelihood"

Food <- base3 %>% 
  select(1,9) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Food)[2] <- "Food_Security"

Well <- merge(x=Livelihood, y=Food, by="Year", all=T)

Social <- base3 %>% 
  select(1,10) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Social)[2] <- "Social"

Supp <- base3 %>% 
  select(1,11) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Supp)[2] <- "Support"

Sust <- base3 %>% 
  select(1,12) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Sust)[2] <- "Sustainability"

Resi <- merge(x=Social, y=Supp, by="Year", all=T)
Resi <- merge(x=Resi, y=Sust, by="Year", all=T)

Drivers <- base3 %>% 
  select(1,14) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Drivers)[2] <- "Drivers"

Agrobio <- base3 %>% 
  select(1,13) %>%
  na.omit() %>% 
  group_by(Year) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(Year < 2020)
names(Agrobio)[2] <- "Agrobio"

# Relative Tally ----
Year <- base1 %>% 
  select(1) %>% 
  group_by(Year) %>% 
  tally() 

Gen.year <- merge(x=Gen, y=Year, by="Year", all=T) %>% 
  mutate(Gen_relative = (Genetic/n)*100)

Agr.year <- merge(x=Agr, y=Year, by="Year", all=T) %>% 
  mutate(Agr_relative = (Agronomic/n)*100)

Agroecology.r <- merge(x=Gen.year, y=Agr.year, by="Year", all=T)

Agroecosystem <- merge(x=Agroecosystem, y=Year, by="Year", all=T)

Agroecosystem.r <- Agroecosystem %>%   
  mutate(Ag_relative = (Agriculture/n)*100, Soil_relative = (Soil/n)*100,
         Water_relative = (Water/n)*100, Eco_relative = (Ecosystem/n)*100) 

Well.r <- merge(x=Well, y=Year, by="Year", all=T)
Well.r[is.na(Well.r)] <- 0
Well.r <- Well.r %>% 
  filter(Year < 2020) %>% 
  mutate(Liv_relative=(Livelihood/n)*100, Food_relative=(Food_Security/n)*100)

Resi.r <- merge(x=Resi, y=Year, by="Year", all=T)
Resi.r[is.na(Resi.r)] <- 0
Resi.r <- Resi.r %>% 
  mutate(Social_relative=(Social/n)*100, Supp_relative=(Support/n)*100,
         Sust_relative=(Sustainability/n)*100) %>% 
  filter(Year < 2020)

Drivers.r <- merge(x=Drivers, y=Year, by="Year", all=T)
Drivers.r[is.na(Drivers.r)] <- 0
Drivers.r <- Drivers.r %>% 
  filter(Year < 2020) %>% 
  mutate(Drivers_relative=(Drivers/n)*100)

Agrobio.r <- merge(x=Agrobio, y=Year, by="Year", all=T)
Agrobio.r <- Agrobio.r %>% 
  filter(Year < 2020) %>% 
  mutate(Agrobio_relative=(Agrobio/n)*100)
  
#
# GGplot (Test) ----
# plot.simple <- ggplot(Gen, aes(x=Year, y=Genetic))+
#   geom_line()+
#   geom_smooth(method = "loess", se=F)+
#   ggtitle("Genetic")+
#   labs(x="")+
#   theme_classic(base_size = 12)+
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         plot.title = element_text(hjust=0.5, face="bold", size=14))+
#   scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))
# 
# plot.simple2 <- ggplot(Agr, aes(x=Year, y=Agronomic))+
#   geom_line()+
#   geom_smooth(method = "loess", se=F)+
#   ggtitle("Agronomic Practices")+
#   labs(x="")+
#   theme_classic(base_size = 12)+
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         plot.title = element_text(hjust=0.5, face="bold", size=14))+
#   scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 20))
# 
# plot.combine <- ggplot(Gen.agr, aes(x=Year, y=n, color=Attribute))+
#   geom_smooth(method = "loess", se=F)+
#   ggtitle("Agroecology")+
#   labs(x="", color="")+
#   theme_classic(base_size = 12)+
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         plot.title = element_text(hjust=0.5, face="bold", size=14),
#         legend.position = c(0.2, 0.7))+
#   scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 20))

## GGplot Final ----
#Agroecology----
Agroecology.color <- c("Genetics" = "seagreen2",
                       "Agronomic Practices" = "green4")
plot1 <- ggplot(Agroecology.r, aes(x=Year))+
  geom_smooth(aes(y=Genetic, color="Genetics"), se=F, size=2)+
  geom_smooth(aes(y=Agronomic, color="Agronomic Practices"), se=F, size=2)+
  geom_smooth(aes(y=Gen_relative, color="Genetics"), se=F, linetype = "dashed", size=1)+
  geom_smooth(aes(y=Agr_relative, color="Agronomic Practices"), se=F, linetype = "dashed", size=1)+
  scale_y_continuous("Absolute Trend", breaks = c(0, 100, 50), 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)",
                                         breaks = c(0, 100, 50)))+
  ggtitle("Agroecology")+
  labs(x="", colour="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14),
        legend.position = c(0.3, 0.8))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))+
  scale_color_manual(values=Agroecology.color)+
  geom_hline(yintercept = 50, linetype="dashed", color="grey", size=0.3)

#Agroecosystem----
Agroecosystem.color <- c("Agriculture"="dodgerblue3", 
                         "Soil"="sandybrown",
                         "Water"="aquamarine3",
                         "Ecosystem"="yellowgreen")
plot2 <- ggplot(Agroecosystem.r, aes(x=Year))+
  geom_smooth(aes(y=Agriculture, color="Agriculture"), se=F, size=1.2)+
  geom_smooth(aes(y=Soil, color="Soil"), se=F, size=1.2)+
  geom_smooth(aes(y=Water, color="Water"), se=F, size=1.2)+
  geom_smooth(aes(y=Ecosystem, color="Ecosystem"), se=F, size=1.2)+
  geom_smooth(aes(y=Ag_relative, color="Agriculture"), se=F, linetype = "dashed", size=0.8)+
  geom_smooth(aes(y=Soil_relative, color="Soil"), se=F, linetype = "dashed", size=0.8)+
  geom_smooth(aes(y=Water_relative, color="Water"), se=F, linetype="dashed", size=0.8)+
  geom_smooth(aes(y=Eco_relative, color="Ecosystem"), se=F, linetype="dashed", size=0.8)+
  scale_y_continuous("Absolute Trend", breaks = c(0,70,35), 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)",
                     breaks=c(0,70,35)))+
  ggtitle("Agroecosystem")+
  labs(x="", color="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14),
        legend.position = c(0.3, 0.88))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))+
  scale_color_manual(values=Agroecosystem.color)+
  geom_hline(yintercept = 50, linetype="dashed", color="grey", size=0.3)

#Well-Being----
Well.color <- c("Livelihood"="red1", 
                "Food Security"="tan2")
plot3 <- ggplot(Well.r, aes(x=Year))+
  geom_smooth(aes(y=Livelihood, color="Livelihood"), se=F, size=2)+
  geom_smooth(aes(y=Food_Security, color="Food Security"), se=F, size=2)+
  geom_smooth(aes(y=Liv_relative, color="Livelihood"), se=F, linetype="dashed", size=1)+
  geom_smooth(aes(y=Food_relative, color="Food Security"), se=F, linetype = "dashed", size=1)+
  scale_y_continuous("Absolute Trend", breaks = c(0,50,25), 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)",
                                         breaks=c(0,50,25)))+
  ggtitle("Well-Being")+
  labs(x="", color="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14),
        legend.position = c(0.3, 0.65))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))+
  scale_color_manual(values=Well.color)+
  geom_hline(yintercept = 50, linetype="dashed", color="grey", size=0.3)

#Resilience----
Resilience.color <- c("Social Engagement"="plum2", 
                "Support Interventions"="khaki4", "Sustainability"="yellow2")
plot4 <- ggplot(Resi.r, aes(x=Year))+
  geom_smooth(aes(y=Social, color="Social Engagement"), se=F, size=2)+
  geom_smooth(aes(y=Support, color="Support Interventions"), se=F, size=2)+
  geom_smooth(aes(y=Sustainability, color="Sustainability"), se=F, size=2)+
  geom_smooth(aes(y=Social_relative, color="Social Engagement"), se=F, linetype="dashed", size=1)+
  geom_smooth(aes(y=Supp_relative, color="Support Interventions"), se=F, linetype = "dashed", size=1)+
  geom_smooth(aes(y=Sust_relative, color="Sustainability"), se=F, linetype = "dashed", size=1)+
  scale_y_continuous("Absolute Trend", breaks = c(0,100,50), limits = c(0,100), 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)",
                                         breaks=c(0,100,50)))+
  ggtitle("Resilience Support")+
  labs(x="", color="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14),
        legend.position = c(0.3, 0.8))+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))+
  scale_color_manual(values=Resilience.color)+
  geom_hline(yintercept = 50, linetype="dashed", color="grey", size=0.3)

#Drivers----
Drivers.color <- c("Drivers of Change"="cadetblue4")
plot5 <- ggplot(Drivers.r, aes(x=Year))+
  geom_smooth(aes(y=Drivers, color="Drivers of Change"), se=F, size=2)+
  geom_smooth(aes(y=Drivers_relative, color="Drivers of Change"), se=F, linetype="dashed", size=1)+
  scale_y_continuous("Absolute Trend", breaks = c(0,60,30), limits = c(0,70), 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)",
                                         breaks=c(0,60,30)))+
  ggtitle("Drivers of Change")+
  labs(x="", color="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14),
        legend.position = "none")+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))+
  scale_color_manual(values=Drivers.color)+
  geom_hline(yintercept = 50, linetype="dashed", color="grey", size=0.3)

#Agrobiodiversity----
Agrobio.color <- c("Agrobiodiversity Extent"="magenta3")
plot6 <- ggplot(Agrobio.r, aes(x=Year))+
  geom_smooth(aes(y=Agrobio, color="Agrobiodiversity Extent"), se=F, size=2)+
  geom_smooth(aes(y=Agrobio_relative, color="Agrobiodiversity Extent"), se=F, linetype="dashed", size=1)+
  scale_y_continuous("Absolute Trend", breaks = c(0,180,90), limits = c(0,180), 
                     sec.axis = sec_axis(~.*1, name = "Relative Trend (%)",
                                         breaks=c(0,180,90)))+
  ggtitle("Drivers of Change")+
  labs(x="", color="")+
  theme_classic(base_size = 12)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14),
        legend.position = "none")+
  scale_x_continuous(limits = c(1995, 2020), breaks=c(1995, 2020, 15))+
  scale_color_manual(values=Agrobio.color)+
  geom_hline(yintercept = 50, linetype="dashed", color="grey", size=0.3)
