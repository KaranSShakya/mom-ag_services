library(tidyverse)
library(readxl)
library(ggplot2)
library(ggfortify)
library(gridExtra)

pca0 <- read_excel("data/modified.xlsx")

pca1 <- pca0 %>% 
  select(-1,-2,-3,-5)

pca2 <- pca1 %>% 
  remove_rownames() %>% 
  column_to_rownames(var="Farm_abb")

label_true <- autoplot(prcomp(pca2), data=pca1, label=T)+
  theme_bw()+
  labs(x="Principle Component 1", y="Principle Component 2")

label_false <- autoplot(prcomp(pca2), data=pca1, label=F)+
  theme_bw()+
  labs(x="Principle Component 1", y="Principle Component 2")

pca.table <- pca0 %>% 
  select(1:4)

