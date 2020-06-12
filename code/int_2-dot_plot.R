library(tidyverse)
library(ggplot2)

source("code/int_1-data_organize.r")
attach(abb1)

levels(data0$Rank) <- c("Cancels", "Constraints", "Consistent", "Reinforces", 
               "Indivisible")

dot1 <- ggplot(data0, aes(x=Int_abb, y=Sdg_abb, color=Rank))+
  geom_point()+
  scale_color_manual(values = c("red2", "orange2", "royalblue2", 
                                "palegreen2", "green4"))+
  facet_grid(rows = vars(Scale), scales="free")+
  theme_bw(base_size = 10)+
  labs(x="Interventions (Abbreviated)", y="SDGs (Abbreviated)", color="Response")

abb1$Rank <- as.factor(abb1$Rank)
levels(abb1$Rank) <- c("Cancels", "Constraints", "Consistent", "Reinforces", 
                        "Indivisible")

abb1$Sdg_abb <- as.factor(abb1$Sdg_abb)
#Sdg_abb reorder----------
abb1$Sdg_abb <- factor(abb1$Sdg_abb, levels = c("1.1",
"1.2",
"2a",
"2.2",
"2.3",
"2.4",
"5 c",
"5.5",
"6 b",
"6.4",
"6.5",
"7.1",
"8.1",
"8.2",
"8.3",
"8.5",
"8.8",
"8.9",
"9.1",
"9.3",
"9.4",
"10.1",
"10.2",
"11.4",
"11.5",
"12.2",
"12.3",
"12.8",
"13.1",
"13.2",
"15 a",
"15.1",
"15.2",
"15.3",
"15.4",
"15.5",
"15.6",
"15.7",
"15.8",
"15.9",
"16.6",
"16.7",
"17.1",
"17.16",
"17.17",
"17.18",
"17.6",
"17.7",
"17.8",
"17.9"))

dot2 <- ggplot(abb1, aes(x=Int_abb, y=Sdg_abb, color=Rank))+
  geom_point()+
  scale_color_manual(values = c("red2", "orange2", "royalblue2", 
                                "palegreen2", "green4"))+
  theme_bw(base_size = 10)+
  labs(x="Interventions (Abbreviated)", y="SDGs (Abbreviated)", color="Response")


detach(abb1)
