library(tidyverse)
library(ggplot2)
library(ggalluvial)
library(alluvial)


source("code/syn_7-sankey_table.r")

sankey.test <- ggplot(year_hkh, aes(axis1 = Year, axis2 = HKH_Country, y=Country_count))+
  geom_alluvium()+
  geom_stratum()+
  geom_text(stat = "stratum", infer.label = TRUE)+
  theme_minimal(base_size = 10)







# inst.final$Year <- as.factor(inst.final$Year)
# inst.final$Collaboration_type <- as.factor(inst.final$Collaboration_type)
# names(inst.final)[1] <- "Collaboration"
# names(inst.final)[3] <- "Count"
# 
# inst.final <- inst.final %>% 
#   select(2,1,3)
# 
# inst.final$Collaboration <- gsub("q", "Sole HKH Country", inst.final$Collaboration)
# inst.final$Collaboration <- gsub("w", "Two or more HKH Countries", 
#                                  inst.final$Collaboration)
# inst.final$Collaboration <- gsub("e", "HKH and International Countries", 
#                                  inst.final$Collaboration)
# inst.final$Collaboration <- gsub("r", "Only International Countrues", 
#                                  inst.final$Collaboration)
# 
# sankey.test <- alluvial(inst.final[,1:2], freq = inst.final$Count, 
#          hide = inst.final$Count == 0,
#          border = ifelse(inst.final$Count == "Yes", "orange", "grey"),
#          cex = 0.5)

