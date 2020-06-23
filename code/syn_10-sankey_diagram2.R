library(tidyverse)
library(ggplot2)
library(ggalluvial)
library(alluvial)

source("code/syn_9-sankey2.R")

sankey <- data3 %>% 
  select(3,5,6)

#Sankey diagram ----
ggplot(sankey, aes(axis1 = Year_int, axis2 = Geog_unit,
                    axis3 = Collaboration))+
  geom_alluvium(aes(fill=Geog_unit))+
  geom_stratum(alpha = 0.1)
