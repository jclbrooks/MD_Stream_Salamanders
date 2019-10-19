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



capital <- capital %>%
  select(SDate, PointName) %>%
  mutate(SDate = mdy(SDate)) %>%
  distinct()

colnames(capital) <- c("date", "transect")


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
str(capital)
str(shenandoah)
str(wmaryland1)

# as.character(canaan$date)
# as.character(capital$date)
# as.character(shenandoah$date)
# as.character(wmaryland1$date)


trans <- bind_rows(canaan, capital, shenandoah, wmaryland)

str(trans)
summary(trans)

str(landscape_characteristics)
summary(landscape_characteristics)

transects <- trans %>%
  left_join(landscape_characteristics)

str(transects)
summary(transects)

#write.csv(x = transects, file = "featureids_for_DAYMET_data.csv", row.names = FALSE)

