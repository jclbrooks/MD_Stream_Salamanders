##################
# Landscape MSOM 
##################

library(dplyr)
library(tidyr)

# Read in data
canaan <- read.csv("Data/Landscape/CVNWR_transects.csv", header = T, stringsAsFactors = F)
capital <- read.csv("Data/Landscape/NCRlotic_all.csv", header = T, stringsAsFactors = F)
shenandoah <- read.csv("Data/Landscape/Shen_snp12.csv", header = T)

str(canaan)
str(capital)
str(shenandoah)

# Format data: transect ID - species - age - pass/visit 1- pass/visit 2 - pass/visit - 3
# made all same format, column names
can <- canaan %>%
  mutate(Transect = paste(Name, Transect, Year, sep = "_")) %>%
  group_by(Transect, Species, Age) %>%
  select(Transect, Pass, Species, Age, Caught) %>%
  mutate(Pass = paste0("p", Pass)) %>%
  spread(Pass, Caught)
colnames(can) <- c("transect", "species", "age", "pass1", "pass2", "pass3", "pass4")

cap <- capital %>%
  mutate(Transect = paste(PointName, Visit, sep = "_v"),
         pass4 = "NULL") %>% # added pass4 column to match canaan dataframe
  group_by(Transect, SpeciesCode, SAgeID) %>%
  select(Transect, SpeciesCode, SAgeID, PassCount1, PassCount2, PassCount3, pass4)
colnames(cap) <- c("transect", "species", "age", "pass1", "pass2", "pass3", "pass4")

# list <- c(shenandoah$Site, shenandoah$Species, shenandoah$Age)
# add_count(shenandoah, name = "count")

she <- shenandoah  %>%
  group_by(Site, Species, Age, Pass) %>%
  select(Site, Species, Age, Pass) %>%
  summarise(count = n()) %>%
  mutate(Pass = paste0("pass", Pass)) %>%
  spread(Pass, count)
# shenandoah[452,]
# shenandoah[453,]
# shenandoah[467,]
# shenandoah$X.

max_pass <- shenandoah  %>%
  group_by(Site, Date) %>%
  summarise(max_pass = max(Pass, na.rm = TRUE))


# Remove NULLs from capitals data
na <- cap[which(cap$species == "NULL"),]
cap1 <- cap[-which(cap$species == "NULL"),]
cap <- cap1
 