library(tidyverse)
library(readxl)
library(factoextra)

pca0 <- read_excel("data/pca_agrobiodiversity.xlsx", 
                   sheet = "Edited")
pca1 <- pca0 %>%
  select(-1) %>% 
  remove_rownames() %>% 
  column_to_rownames(var="Acronym")

res.pca <- prcomp(pca1, scale=T)

fviz_pca_ind(res.pca, repel = T)
fviz_pca_ind(res.pca, repel = T, label=F)

biplot <- fviz_pca_biplot(res.pca, repel = T)

ggsave(filename = "biplot_agrobiodiversity_draft1.png", plot=biplot, device = "png", dpi=300)
