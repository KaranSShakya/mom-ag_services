library(tidyverse)
library(ggplot2)

source("code/int_1-data_organize.r")
attach(data0)

levels(data0$Rank) <- c("Cancels", "Constraints", "Consistent", "Reinforces", 
               "Indivisible")

dot1 <- ggplot(data0, aes(x=Int_abb, y=Sdg_abb, color=Rank))+
  geom_point()+
  scale_color_manual(values = c("red2", "orange2", "royalblue2", 
                                "palegreen2", "green4"))+
  facet_grid(rows = vars(Scale), scales="free")+
  theme_bw(base_size = 10)+
  labs(x="Interventions (Abbreviated)", y="SDGs (Abbreviated)", color="Response")
  
