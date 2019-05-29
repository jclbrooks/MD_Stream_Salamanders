###########
#MSOM landscape characteristics
#############

# Reorganizing Shenandoah data to just unique site names to send to Evan Grant to figure out how to deal with empty lat/long

# Shenandoah locations
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

NCR <- read.csv("Data/Landscape/NCRlotic_all.csv", header=T, stringsAsFactors = F)

str(NCR)

NCRnew <- NCR %>%
  select(UnitName, PointName, Latitude, Longitude, SDate) %>%
  dplyr::distinct(PointName, .keep_all = T) #%>%
  #mutate(Latitude = ifelse(Latitude < 0, NA, Latitude))

str(NCRnew) 

#write.csv(NCRnew, "Data/Landscape/NCR_locations.csv")


###############################################################################

#Read in landscape data

cvnwr <- read.csv("landscape_characteristics/CVNWR_locations_intersected.csv", header = T, stringsAsFactors = F)
restored <- read.csv("landscape_characteristics/MDE_Salamander_Sites_intersected.csv", header = T, stringsAsFactors = F)  
ncrlotic <- read.csv("landscape_characteristics/NCRlotic_locations_intersected.csv", header = T, stringsAsFactors = F) 
shenandoah <- read.csv("landscape_characteristics/Shenandoah_locations_intersected.csv", header = T, stringsAsFactors = F)
#rerence <-  read.csv("Landscape_ecology/landscape_characteristics/Sites_intersect.csv", header = T, stringsAsFactors = F)  


# Re-structure data
str(cvnwr) ######################## PROBLEMS
cvnwr <- cvnwr %>%
  dplyr::select(Name, Year, Transect__, NEAR_FID) %>%
  mutate(site = paste(Name, Year, Transect__, sep="_"), #Name = Name_Year_Transect
         UnitName = "Canaan Valley National Wildlife Refuge") %>% 
  select(UnitName, site, NEAR_FID)
colnames(cvnwr) <- c("region", "transect", "featureid")


str(restored)
restored <- restored %>%
  mutate(unit_name = "Savage River State Forest") %>%
  select(unit_name, Site_Name, NEAR_FID) 
colnames(restored) <- c("region", "transect", "featureid")


str(ncrlotic)
ncrlotic <- ncrlotic %>%
  select(UnitName, PointName, NEAR_FID) %>%
  dplyr::distinct(PointName, .keep_all = T)
colnames(ncrlotic) <- c("region", "transect", "featureid")


str(shenandoah)
shenandoah <- shenandoah %>%
  mutate(unit = "Shenandoah National Park") %>%
  select(unit, Object_ID, NEAR_FID)
colnames(shenandoah) <- c("region", "transect", "featureid")


#Bind data all together
landscape02 <- bind_rows(ncrlotic, shenandoah)
head(landscape02)
tail(landscape02)

landscape05 <- bind_rows(cvnwr, restored)
head(landscape05)
tail(landscape05)


# Read in characteristics file
characteristics02 <- read_csv("Landscape_ecology/covariates_02/Catchments02.csv", 
                              col_types = list(featureid = col_character(),
                                               variable = col_character(),
                                               value = col_double(),
                                               zone = col_character(),
                                               riparian_distance_ft = col_logical()))

# Shenandoah, NCR, Sites
characteristics05 <- read_csv("Landscape_ecology/covariates_05/Catchments05.csv", 
                              col_types = list(featureid = col_character(),
                                               variable = col_character(),
                                               value = col_double(),
                                               zone = col_character(),
                                               riparian_distance_ft = col_logical()))

# Remove extra featureids
featureids02_keepers <- unique(landscape02$featureid)
featureids05_keepers <- unique(landscape05$featureid)

# str(characteristics02)
# library(stringr)
# ids <- characteristics02 %>%
#   dplyr::select(featureid) %>%
#   distinct() %>%
#   mutate(featureid = str_sub(string = featureid, start = 4, end = 9))
# unique(characteristics05$featureid)

characteristics02 <- characteristics02 %>%
  dplyr::filter(featureid %in% featureids02_keepers)

characteristics05 <- characteristics05 %>%
  dplyr::filter(featureid %in% featureids05_keepers)

# CVNWR, Restored

str(landscape02)
landscape02$featureid <- as.character(landscape02$featureid)
landscape05$featureid <- as.character(landscape05$featureid)
str(characteristics02)

combine02 <- landscape02 %>%
  left_join(characteristics02)
summary(combine02)
head(combine02)
str(combine02)
str(landscape02)
combine02$variable


combine05 <- landscape05 %>%
  left_join(characteristics05)
head(combine02)
str(combine02)
str(landscape02)
combine02$variable





