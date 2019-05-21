####################################
# Landscape MSOM locations for GIS #
####################################


canaan_loc <- read.csv("Data/Landscape/CVNWR_transects.csv", header = T, stringsAsFactors = F)
capital <- read.csv("Data/Landscape/NCRlotic_all.csv", header = T, stringsAsFactors = F)
shenandoah <- read.csv("Data/Landscape/Shen_snp12.csv", header = T, stringsAsFactors = FALSE)

str(canaan) # need to join name of site with lat/lon
str(capital) # has lat/lon in it, need to clip it down
str(shenandoah) # need to join name of site with lat/lon


can_loc <- canaan %>%
  select(-, )



























