##################
# Landscape MSOM 
##################

library(dplyr)
library(tidyr)

canaan <- read.csv("Data/Landscape/CVNWR_transects.csv", header = T, stringsAsFactors = F)
capital <- read.csv("Data/Landscape/NCRlotic_all.csv", header = T)
shenandoah <- read.csv("Data/Landscape/Shen_snp12.csv", header = T)

str(canaan)

can <- canaan %>%
  mutate(Transect = paste(Name, Transect, Year, sep = "_")) %>%
  group_by(Transect, Species, Age) %>%
  select(Transect, Pass, Species, Age, Caught) %>%
  mutate(Pass = paste0("p", Pass)) %>%
  spread(Pass, Caught)

# # can <- canaan %>%
#   mutate(Transect = paste(Name, Transect, Year, sep = "_")) %>%
#   group_by(Transect, Species, Age) %>%
#   select(Transect, Pass, Species, Age, Caught) %>%
#   mutate(Pass = paste0("p", Pass)) %>%
#   spread(Pass, Caught)
  