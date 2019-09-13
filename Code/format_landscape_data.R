

library(dplyr)
library(tidyr)
library(readr)


#----- Format Landscape Characteristics Data ----

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
         region = ifelse(region == "Savage River State Forest", "Savage", region),
         transect = ifelse(region == "Canaan", gsub(pattern = "*_.*_", replacement = "", x = transect), transect)#,
         #transect = ifelse(region == "Canaan", substr(transect, 1, nchar(transect) - 7), transect)
  )

# changes colname "transect" to "stream" to match landscape_occ

#-----Pick variables to potentially use ----
vars <- c("agriculture", "elevation", "forest", "impervious", "AreaSqKM")

landscape_vars <- landscape_characteristics %>%
  filter(zone == "local",
         variable %in% vars) %>%
  # mutate(loc = paste0(.$region, "_", .$transect)) %>%
  select(region, transect, variable, value) %>%
  distinct() %>%
  # pivot_wider(names_from = variable, values_from = value, values_fill = NA)
  spread(key = variable, value = value) %>%
  mutate(Capital = ifelse(region == "Capital", 1, 0),
         Canaan = ifelse(region == "Canaan", 1, 0),
         Shenandoah = ifelse(region == "Shenandoah", 1, 0),
         WMaryland = ifelse(region == "Savage", 1, 0)) %>%
  select(-region)

#-----Create a matrix that shows whether a species' range overlaps each transect----


spec_dist <- landscape_vars %>%
  ungroup() %>%
  select(transect, Capital, Canaan, Shenandoah, WMaryland) %>%
  mutate(DFUS = 1,
         DMON = NA_real_,
         DOCH = NA_real_,
         EBIS = 1,
         GPOR = NA_real_,
         EGUT = NA_real_,
         ELON = NA_real_,
         PRUB = 1,
         DMON = ifelse(Capital == 1, 0, 1),
         DMON = ifelse(transect == "COST22-1", 1, DMON),
         DOCH = ifelse(WMaryland == 1 | Canaan == 1, 1, 0),
         GPOR = ifelse(Capital == 1, 0, 1)) ### need to finish EGUT and ELON 
         




# modeldata <- landscape_occ %>%
#   left_join(landscape_vars)

modeldata <- landscape_vars %>%
  right_join(landscape_occ)

str(modeldata)
summary(modeldata)

unique(landscape_occ$transect) %in% unique(landscape_vars$transect)

unique(landscape_occ$transect)[!(unique(landscape_occ$transect) %in% unique(landscape_vars$transect))] # names of transects in the occupancy that are not in landscape variables

unique(landscape_occ[,1:2])
unique(landscape_characteristics[,1:2])
# length(unique(paste(landscape_characteristics[,1], landscape_characteristics[,2])))
# unique_char <- unique(landscape_characteristics[,1:2])
# unique(landscape_occ$transect)
# unique(landscape_characteristics$transect)



