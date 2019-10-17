#### creating a file with all lat/lon for all of landscape occ model sites


can <- read.csv("Data/Landscape/CVNWR_locations_intersected.csv", header = T, stringsAsFactors = F)
she <- read.csv("Data/Landscape/Shenandoah_locations_intersected.csv", header = T, stringsAsFactors = F)
ncr <- read.csv("Data/Landscape/NCRlotic_locations_intersected.csv", header = T, stringsAsFactors = F)
other <- read.csv("Data/Landscape/missing_sites_locations.csv", header = T, stringsAsFactors = F)
wmd <- read.csv("Data/WMD_sites_lat_lon_coords.csv", header = T, stringsAsFactors = F)
df_occ <- readRDS("Data/Derived/combined_detailed_occ.rds")

can1 <- can %>%
  mutate(region = "Canaan",
         site = paste0(Name, Transect__))%>%
  select(region, site, Lat, Long)
colnames(can1) <- c("region", "site", "lat", "lon")
summary(can1)
head(can1)

she1 <- she %>%
  select(Object_ID, POINT_X, POINT_Y) %>%
  mutate(region = "Shenandoah")
colnames(she1) <- c("site", "lon", "lat", "region")
summary(she1)
head(she1)

ncr1 <- ncr %>%
  mutate(region_1 = "Capital") %>%
  select(region_1, PointName, Latitude, Longitude) %>%
  distinct()
colnames(ncr1) <- c("region", "site", "lat", "lon")
summary(ncr1)
head(ncr1)

other1 <- other
colnames(other1) <- c("region", "site", "lat", "lon")
summary(other1)
head(other1)

wmd1 <- wmd %>%
  mutate(region = "WMaryland")
colnames(wmd1) <- c("site", "lat", "lon", "region")
summary(wmd1)
head(wmd1)


all_sites <- bind_rows(can1, she1, ncr1, other1, wmd1) %>%
  distinct() %>%
  select(-region)
colnames(all_sites) <- c("transect", "lat", "lon")

all_coords <- df_occ %>%
  left_join(all_sites, by = "transect") %>%
  select(transect, lat, lon) %>%
  distinct()
colnames(all_sites) <- c("site", "lat", "lon")
  
head(all_coords)



# write.csv(all_coords, "Data/all_sites_coords.csv")



