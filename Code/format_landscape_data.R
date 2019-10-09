

library(dplyr)
library(tidyr)
library(readr)


#----- Format Landscape Characteristics Data ----

huc02_landscape <- read_csv("landscape_characteristics/HUC02_sites_landscape_characteristics.csv")
# National Capitals Region, Shenandoah, and W MD reference streams

huc05_landscape <- read_csv("landscape_characteristics/HUC05_sites_landscape_characteristics.csv")
# Canaan Valley NWR and W MD restored streams

landscape_characteristics <- bind_rows(huc02_landscape, huc05_landscape)

landscape_characteristics1 <- landscape_characteristics %>%
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

landscape_vars <- landscape_characteristics1 %>%
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
southofpotomac_prw <- landscape_characteristics %>%
  filter(region == "Prince William Forest National Park") %>%
  select(transect) %>%
  distinct()
southofpotomac_list <- as.list(southofpotomac_prw)

prw <- c("PRW101-1R","PRW101-2L","PRW101-3R", "PRW101-4L", "PRW114-1R", "PRW114-2L", "PRW114-3R", "PRW114-4L", "PRW127-1R", "PRW127-2L", "PRW127-3L", "PRW127-4R", "PRW146-1L", "PRW146-2R", "PRW146-A1", "PRW146-A2", "PRW157-1",  "PRW157-2", "PRW157-3",  "PRW157-4",  "PRW157-A1", "PRW157-A2", "PRW172-1R", "PRW172-2L", "PRW172-A1", "PRW172-A2", "PRW226-1R", "PRW226-2L", "PRW226-3R", "PRW226-4L", "PRW226-A1", "PRW226-A2", "PRW227-1L", "PRW227-2R", "PRW227-3L", "PRW227-4L", "PRW227-A1", "PRW227-A2", "PRW247-1R", "PRW247-2L", "PRW247-3R", "PRW247-4L", "PRW262-1",  "PRW262-2",  "PRW262-3", "PRW262-4",  "PRW263-1",  "PRW263-2", "PRW263-3","PRW263-4",  "PRW263-A1", "PRW263-A2", "PRW301-1R", "PRW301-2L", "PRW301-3R", "PRW301-4L", "PRW314-1R", "PRW314-2L", "PRW314-3L", "PRW314-4R", "PRW315-1L", "PRW315-2R", "PRW315-3L", "PRW315-4R", "PRW331-1R", "PRW331-2L", "PRW331-3L", "PRW331-4L", "PRW370-1", "PRW370-2",  "PRW370-3",  "PRW370-4", "PRW371-1",  "PRW371-2",  "PRW371-3", "PRW371-4",  "PRW424-1L", "PRW424-2R", "PRW424-3L", "PRW424-4R", "PRW516-1L", "PRW516-2R", "PRW516-3L", "PRW516-4R")
as.list(prw)
# These are all sites south of the Potomac in the Prince William Forest National Park, the only other sites south of the Potomac are COST28-2 and COST8-3, but the lat/lon that was given is questionable, we might leave these two sites out

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
         GPOR = ifelse(Capital == 1, 0, 1),
         EGUT = ifelse(transect == c("PRW101-1R","PRW101-2L","PRW101-3R", "PRW101-4L", "PRW114-1R", "PRW114-2L", "PRW114-3R", "PRW114-4L", "PRW127-1R", "PRW127-2L", "PRW127-3L", "PRW127-4R", "PRW146-1L", "PRW146-2R", "PRW146-A1", "PRW146-A2", "PRW157-1",  "PRW157-2", "PRW157-3",  "PRW157-4",  "PRW157-A1", "PRW157-A2", "PRW172-1R", "PRW172-2L", "PRW172-A1", "PRW172-A2", "PRW226-1R", "PRW226-2L", "PRW226-3R", "PRW226-4L", "PRW226-A1", "PRW226-A2", "PRW227-1L", "PRW227-2R", "PRW227-3L", "PRW227-4L", "PRW227-A1", "PRW227-A2", "PRW247-1R", "PRW247-2L", "PRW247-3R", "PRW247-4L", "PRW262-1",  "PRW262-2",  "PRW262-3", "PRW262-4",  "PRW263-1",  "PRW263-2", "PRW263-3","PRW263-4",  "PRW263-A1", "PRW263-A2", "PRW301-1R", "PRW301-2L", "PRW301-3R", "PRW301-4L", "PRW314-1R", "PRW314-2L", "PRW314-3L", "PRW314-4R", "PRW315-1L", "PRW315-2R", "PRW315-3L", "PRW315-4R", "PRW331-1R", "PRW331-2L", "PRW331-3L", "PRW331-4L", "PRW370-1", "PRW370-2",  "PRW370-3",  "PRW370-4", "PRW371-1",  "PRW371-2",  "PRW371-3", "PRW371-4",  "PRW424-1L", "PRW424-2R", "PRW424-3L", "PRW424-4R", "PRW516-1L", "PRW516-2R", "PRW516-3L", "PRW516-4R", "COST28-2", "COST8-3" , 1, 0)))
         




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



