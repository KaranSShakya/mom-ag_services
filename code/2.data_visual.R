#Library
library(tidyverse)
library(ggthemes)
library(ggrepel)

#Importing dataset
survey <- read_csv("data/survey.csv")
names(survey)[5] <- "farming"

#Current Basic----------------------------
cur <- survey %>% 
  select(-7,-8,-9)

#Filter avg-provisioning
cur.p <- cur %>% 
  filter(eco_type=="Provisioning") %>% 
  select(2,5,6) %>% 
  group_by(eco_ser, farming) %>% 
  summarise(avg_res=mean(current)) %>% 
  ungroup()
  
ggplot(cur.p, aes(x=avg_res, y=eco_ser, color=farming, group=farming))+
  geom_point(size=0.2)+
  geom_path()

cur.pa <- cur %>% 
  filter(eco_type=="Provisioning") %>% 
  select(2,6) %>% 
  group_by(eco_ser) %>% 
  summarise(avg_res=mean(current)) %>% 
  ungroup()

ggplot(cur.pa, aes(x=avg_res, y=eco_ser))+
  geom_point(size=1)+
  labs(title="Average of all Farming Systems", x="Ranking", y="Ecosystem Services")+
  scale_x_continuous(limits = c(0,5))

