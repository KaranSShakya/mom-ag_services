#Library
library(tidyverse)
library(ggthemes)
library(ggrepel)

#Importing dataset
survey <- read_csv("data/survey.csv")
names(survey)[5] <- "farming"

#Current Basic ----------------------------
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



#Current-Demand same plot ----------------
cur_dem <- survey %>% 
  select(2,6,7,10) %>% 
  group_by(eco_ser, eco_type) %>% 
  summarize(avg_c=mean(current), avg_d=mean(demand)) %>% 
  ungroup() %>% 
  na.omit()
cur_dem$avg_c <- round(cur_dem$avg_c, 1)

cur_dem$avg_d <- round(cur_dem$avg_d, 0)
cur_dem$avg_d <- factor(cur_dem$avg_d)
levels(cur_dem$avg_d) <- c("decreases", "stays the same", "increases")

filter(cur_dem, eco_type=="Provisioning") %>% 
  ggplot(aes(x=avg_c, y=eco_ser, color=avg_d))+
  geom_point(shape=15)+
  scale_x_continuous(limits = c(1,5))+
  labs(color="Demand in Future", title="Provisioning",
       x="Current", y="Ecosystem Services")+
  scale_color_manual(values = c("red2", "royalblue", "green3"))

filter(cur_dem, eco_type=="Cultural") %>% 
  ggplot(aes(x=avg_c, y=eco_ser, color=avg_d))+
  geom_point(shape=15)+
  scale_x_continuous(limits = c(1,5))+
  labs(color="Demand in Future", title="Provisioning",
       x="Current", y="Ecosystem Services")+
  scale_color_manual(values = c("red2", "royalblue", "green3"))

filter(cur_dem, eco_type=="Regulating") %>% 
  ggplot(aes(x=avg_c, y=eco_ser, color=avg_d))+
  geom_point(shape=15)+
  scale_x_continuous(limits = c(1,5))+
  labs(color="Demand in Future", title="Provisioning",
       x="Current", y="Ecosystem Services")+
  scale_color_manual(values = c("red2", "royalblue", "green3"))  
  
  
  
  



#Current-Demand same plot - revised --------------
cur_dem <- survey %>% 
  select(2,6,7,10) %>% 
  group_by(eco_ser, eco_type) %>% 
  summarize(avg_c=mean(current), avg_d=mean(demand)) %>% 
  ungroup() %>% 
  na.omit()
cur_dem$avg_c <- round(cur_dem$avg_c, 1)

cur_dem$avg_d <- round(cur_dem$avg_d, 0)
cur_dem$avg_d <- factor(cur_dem$avg_d)
levels(cur_dem$avg_d) <- c("decreases", "stays the same", "increases")

cur_dem <- cur_dem %>% 
  group_by(eco_ser) %>% 
  arrange(eco_type)

#scale_x_continuous(limits = c(1,5))+
 # labs(color="Demand in Future", title="Provisioning",
  #     x="Current", y="Ecosystem Services")+
  #scale_color_manual(values = c("red2", "royalblue", "green3"))
  
  
