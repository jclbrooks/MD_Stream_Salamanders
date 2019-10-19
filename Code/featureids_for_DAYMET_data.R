### Feature ID's for DAYMET Data ###


library(readr)
library(dplyr)
library(lubridate)

huc02_landscape <- read_csv("landscape_characteristics/HUC02_sites_landscape_characteristics.csv")
# National Capitals Region, Shenandoah, and W MD reference streams

huc05_landscape <- read_csv("landscape_characteristics/HUC05_sites_landscape_characteristics.csv")
# Canaan Valley NWR and W MD restored streams

landscape_characteristics <- bind_rows(huc02_landscape, huc05_landscape)

landscape_characteristics <- landscape_characteristics %>%
  mutate(region = ifelse(region == "C & O Canal National Historic Park", "Capital", region),
         region = ifelse(region == "Rock Creek National Park", "Capital", region),
         region = ifelse(region == "Prince William Forest National Park", "Capital", region),
         region = ifelse(region == "Shenandoah National Park", "Shenandoah", region),
         region = ifelse(region == "Canaan Valley National Wildlife Refuge", "Canaan", region),
         region = ifelse(region == "Savage River State Park", "Savage", region),
         transect = ifelse(region == "Canaan", gsub(pattern = "*_.*_", replacement = "", x = transect), transect)) %>%
  select(region, transect, featureid) %>%
  distinct() %>%
  arrange(region, transect, featureid)

unique(landscape_characteristics$transect)








canaan <- read.csv("Data/Landscape/CVNWR_transects.csv", header = TRUE, stringsAsFactors = FALSE)
capital <- read.csv("Data/Landscape/NCRlotic_all.csv", header = TRUE, stringsAsFactors = FALSE)
shenandoah <- read.csv("Data/Landscape/Shen_snp12.csv", header = TRUE, stringsAsFactors = FALSE)
wmaryland <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", header = TRUE, stringsAsFactors = FALSE)


head(canaan)
head(capital)
head(shenandoah)
head(wmaryland)

summary(canaan)

canaan <- canaan %>%
  select(Name, Transect, Date) %>%
  distinct() %>%
  arrange(Name, Transect, Date) %>%
  mutate(Transect = ifelse(is.na(Transect), 0, Transect),
         Transect = paste0(Name, Transect),
         Date = mdy(Date)) %>%
  select(Date, Transect) 

colnames(canaan) <- c("date", "transect")


cap <- capital %>%
  mutate(#Transect = paste(PointName, Visit, sep = "_v"),
         pass4 = NA_real_,
         region = "Capital") %>% # added pass4 column to match canaan dataframe
  group_by(PointName, SpeciesCode, SAgeID) %>%
  select(region, PointName, SDate, Visit, SpeciesCode, SAgeID, PassCount1, PassCount2, PassCount3, pass4)
colnames(cap) <- c("region", "transect", "date", "visit", "species", "age", "pass1", "pass2", "pass3", "pass4")
na <- cap[which(cap$species == "NULL"),]
cap1 <- cap[-which(cap$species == "NULL"),]
cap <- cap1
capital1 <- cap %>%
  select(date, transect) %>%
  mutate(date = mdy(date)) %>%
  distinct()


shenandoah <- shenandoah %>%
  select(Date, Site) %>%
  mutate(Date = mdy(Date)) %>%
  distinct()

colnames(shenandoah) <- c("date", "transect")
  


wmaryland1 <- wmaryland %>%
  select(date, stream, transect) %>%
  mutate(date = mdy(date),
         stream = ifelse(stream == "POPLICKTRIB", "PopLick", stream),
         stream = ifelse(stream == "ALEX", "Alexander Run", stream),
         stream = ifelse(stream == "ELKLICK", "ElkLick", stream),
         stream = ifelse(stream == "MILL", "Mill", stream),
         stream = ifelse(stream == "BLUELICK", "BlueLick", stream),
         stream = ifelse(stream == "WSHALEN", "West Shale North", stream),
         stream = ifelse(stream == "KOCH", "Koch", stream),
         stream = ifelse(stream == "DUNGHILL", "Bowser-Dung Hill", stream),
         stream = ifelse(stream == "BEARHILL", "Maynardier Ridge at Bear Hill", stream)) %>%
  distinct() %>%
  mutate(stream_trans = paste0(stream, "_", transect)) %>%
  select(date, stream_trans)

colnames(wmaryland1) <- c("date", "transect")

str(canaan)
str(capital1)
str(shenandoah)
str(wmaryland1)

# as.character(canaan$date)
# as.character(capital$date)
# as.character(shenandoah$date)
# as.character(wmaryland1$date)


#trans <- bind_rows(canaan, capital1, shenandoah, wmaryland)
trans <- rbind(canaan, capital1, shenandoah)

str(trans)
summary(trans)

str(landscape_characteristics)
summary(landscape_characteristics)

wmd_transects <- wmaryland1 %>%
  tidyr::separate(transect, into = c("transect", "transect_num"), sep = "_") %>%
  left_join(landscape_characteristics) %>%
  mutate(transect = paste0(transect, "_", transect_num),
         region = "WMaryland") %>%
  select(-transect_num) %>%
  distinct()
  
transects <- trans %>%
  left_join(landscape_characteristics)

str(transects)
summary(transects)

all_transects <- bind_rows(transects, wmd_transects) %>%
  filter(!is.na(region))

#write.csv(x = all_transects, file = "featureids_for_DAYMET_data.csv", row.names = FALSE)

