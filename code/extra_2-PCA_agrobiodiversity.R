library(tidyverse)
library(readxl)
library(factoextra)

pca0 <- read_excel("data/pca_agrobiodiversity.xlsx", 
                   sheet = "Edited")
pca1 <- pca0 %>%
  select(-1,-4) %>% 
  remove_rownames() %>% 
  column_to_rownames(var="Acronym")

res.pca <- prcomp(pca1, scale=T)

fviz_pca_ind(res.pca, repel = T)
fviz_pca_ind(res.pca, repel = T, label=F)

biplot2 <- fviz_pca_biplot(res.pca, repel = T) 

#ggsave(filename = "biplot_agrobiodiversity_draft2.png", plot=biplot2, device = "png", dpi=300)

#plot(res.pca, type="lines")

#screeplot(res.pca, type="lines")

scree <- fviz_eig(res.pca, choice="eigenvalue",
         addlabels = TRUE,
         geom="line")

ggsave(filename = "scree_draft1.png", plot=scree, device = "png", dpi=300)







