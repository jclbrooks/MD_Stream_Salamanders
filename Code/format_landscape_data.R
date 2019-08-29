

library(dplyr)
library(tidyr)



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
         region = ifelse(region == "Savage River State Park", "Savage", region),
         transect = ifelse(region == "Canaan", gsub(pattern = "*_.*_", replacement = "", x = transect), transect)#,
         #transect = ifelse(region == "Canaan", substr(transect, 1, nchar(transect) - 7), transect)
  )

# changes colname "transect" to "stream" to match landscape_occ

#-----Pick variables to potentially use ----
vars <- c("agriculture", "elevation", "forest", "impervious", "AreaSqKM")

landscape_vars <- landscape_characteristics %>%
  filter(zone == "local",
         variable %in% vars)%>%
  # mutate(loc = paste0(.$region, "_", .$transect)) %>%
  select(region, transect, variable, value) %>%
  distinct() %>%
  # pivot_wider(names_from = variable, values_from = value, values_fill = NA)
  spread(key = variable, value = value)

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



