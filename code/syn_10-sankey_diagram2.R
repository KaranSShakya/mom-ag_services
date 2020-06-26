library(tidyverse)
library(readxl)
library(ggplot2)
library(ggalluvial)
library(alluvial)
library(ggrepel)

sankey0 <- read_excel("data/synthesis_sankey.xlsx")

sankey1 <- sankey0 %>% 
  select(-1)

#Year interval ----
sankey1$Year_int <- NA

sankey1$Year_int <- ifelse(sankey1$Year <= 2000, "1996-2000",
                         ifelse(sankey1$Year >= 2001 & sankey1$Year <= 2005, "2001-2005",
                                ifelse(sankey1$Year >= 2006 & sankey1$Year <= 2010, "2006-2010",
                                       ifelse(sankey1$Year >= 2011 & sankey1$Year <= 2015, "2011-2015",
                                              "2016-2020"))))
sankey1$Year_int <- as.factor(sankey1$Year_int)

#Geog unit ----
names(sankey1)[2] <- "Geog_unit"
sankey1$Geog_unit <- gsub("NP", "Nepal", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("CN", "China", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("OTH", "Other", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("IN", "India", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("BD", "Bangladesh", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("MN", "Myanmar", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("BU", "Bhutan", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("HH-PK", "Pakistan", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("PK", "Pakistan", sankey1$Geog_unit)
sankey1$Geog_unit <- gsub("AF", "Afghanistan", sankey1$Geog_unit)

sankey1$Geog_unit <- as.factor(sankey1$Geog_unit)

sankey1$Geog_unit <- factor(sankey1$Geog_unit, levels = c("Afghanistan", 
                              "Bangladesh", "Bhutan", "China",
                              "India", "Myanmar", "Nepal", "Pakistan", "HKH",
                              "Other"))
  
#Collaboration ----
names(sankey1)[3] <- "Collaboration"
sankey1$Collaboration <- as.factor(sankey1$Collaboration)

sankey1$Collaboration <- recode(sankey1$Collaboration, 'INT' = "International Only",
                                'HKH-INT' = "HKH and International", 
                                'HKH' = "HKH Only",
                                'HKH-HKH' = "HKH and HKH",
                                'HKH_INT' = "HKH and International",
                                'HH-INT' = "HKH and International")

#Publication ----
names(sankey1)[4] <- "Publication"

sankey1$Publication <- as.factor(sankey1$Publication)

sankey1$Publication <- recode(sankey1$Publication, 'Article' = "Article",
                              'Review' = "Review",
                              'Article in Press' = "Article",
                              'Book' = "Book",
                              'Book Chapter' = "Book")

#Pillars ----
sankey1$Pillars <- as.factor(sankey1$Pillars)

sankey1$Pillars <- recode(sankey1$Pillars, FUN = "Functions",
                          ENG = "Engagement",
                          FOU = "Foundation",
                          APP = "Application",
                          App = "Application",
                          ENF = "ENF",
                          COM = "COM")

#Sankey diagram ----
sankeyF <- sankey1 %>% 
  select(-1)

#sankey.draft1
sankey1 <- ggplot(sankeyF, aes(axis1 = Year_int, axis2 = Geog_unit,
                    axis3 = Collaboration, axis4 = Publication, 
                    axis5 = Pillars))+
  geom_alluvium(aes(fill=Geog_unit), knot.pos = 0.3)+
  labs(fill="Country")+
  geom_stratum(alpha = 0.1)+
  theme_minimal(base_size = 10)+
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 11))+
  scale_fill_manual(values = c("aquamarine4", "yellow3", "thistle1",
                               "salmon2", "skyblue2", "sandybrown",
                               "palegreen3", "plum2", "grey54",
                               "grey42"))+
  scale_x_discrete(limits = c("Year", "Country", "Collaboration",
                              "Publication", "Pillars"), 
                   expand = c(0.01, 0.01))+
  geom_text(stat = "stratum", infer.label = TRUE, size=3)

#sankey.draft2
sankey2 <- ggplot(sankeyF, aes(axis1 = Year_int, axis2 = Geog_unit,
                    axis3 = Collaboration, axis4 = Publication, 
                    axis5 = Pillars))+
  geom_alluvium(aes(fill=Geog_unit), knot.pos = 0.3)+
  labs(fill="Country")+
  geom_stratum(alpha = 0.1)+
  theme_minimal(base_size = 10)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11))+
  scale_fill_manual(values = c("aquamarine4", "yellow3", "thistle1",
                               "salmon2", "skyblue2", "sandybrown",
                               "palegreen3", "plum2", "grey54",
                               "grey42"))+
  scale_x_discrete(limits = c("Year", "Country", "Collaboration",
                              "Publication", "Pillars"), 
                   expand = c(0.01, 0.01))

#sankey.draft3
sankey3 <- ggplot(sankeyF, aes(axis1 = Year_int, axis2 = Geog_unit,
                    axis3 = Collaboration, axis4 = Publication, 
                    axis5 = Pillars))+
  geom_alluvium(aes(fill=Geog_unit), knot.pos = 0.3)+
  labs(fill="Country")+
  geom_stratum()+
  theme_minimal(base_size = 10)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11))+
  scale_fill_manual(values = c("aquamarine4", "yellow3", "thistle1",
                               "salmon2", "skyblue2", "sandybrown",
                               "palegreen3", "plum2", "grey54",
                               "grey42"))+
  scale_x_discrete(limits = c("Year", "Country", "Collaboration",
                              "Publication", "Pillars"), 
                   expand = c(0.01, 0.01))

#ggsave(filename = "sankey1_nolabels2.png", plot=sankey3, device = "png", dpi=300)
