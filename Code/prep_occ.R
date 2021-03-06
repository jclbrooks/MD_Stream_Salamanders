library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
# library(fastDummies)

df_occ <- readRDS("Data/Derived/combined_detailed_occ.rds")
df_covariates <- readRDS(file = "Data/Derived/landscape.rds") # can use this or the one Jacey created
df_hucs <- readRDS(file = "Data/Derived/hucs.rds") # can just left_join into covariate data
tempData <- readRDS(file = "Data/Derived/daymet_daily.rds") # daily covariates are going to need to be prepped and spread - do in separate script
climate_data_means <- readRDS(file = "Data/Derived/daymet_means.rds") # annual means by
featureids_matched <- read_csv("featureids_for_DAYMET_data.csv")
spp_dist <- read_csv("Data/species_ranges.csv") %>%
  rename(stream = transect) # transect mislabled as stream for western maryland

# fix the naming inconsistencies
transect_stream <- read_csv("Data/Location_Transect_Stream_Features_processed.csv") %>%
  select(stream = Stream, Transect) %>%
  distinct() %>%
  mutate(stream = ifelse(stream == "POPLICKTRIB", "PopLick", stream),
         stream = ifelse(stream == "ALEX", "Alexander Run", stream),
         stream = ifelse(stream == "ELKLICK", "ElkLick", stream),
         stream = ifelse(stream == "MILL", "Mill", stream),
         stream = ifelse(stream == "BLUELICK", "BlueLick", stream),
         stream = ifelse(stream == "WSHALEN", "West Shale North", stream),
         stream = ifelse(stream == "KOCH", "Koch", stream),
         stream = ifelse(stream == "DUNGHILL", "Bowser-Dung Hill", stream),
         stream = ifelse(stream == "BEARHILL", "Maynardier Ridge at Bear Hill", stream)) %>%
  mutate(transect = paste0(stream, "_", Transect))

# filter out sites with NA because of bad lat lon; also done below (line 84) for df_occ_all and spp_dist 
featureids_matched <- featureids_matched %>%
  filter(!transect %in% c("COST28-2", "Fairway18-1", "Picnic 21-1", "COST8-3",  "COST21-3", "West Shale North_1D", "PRW227-A2", "PRW263-A1"))

df_occ <- df_occ %>%
  filter(!transect %in% c("COST28-2", "Fairway18-1", "Picnic 21-1", "COST8-3", "COST21-3", "West Shale North_1D", "PRW227-A2", "PRW263-A1"))

# fix the stream vs. transect naming issues to be consistent and work with other data
spp_dist <- featureids_matched %>%
  select(-date) %>%
  distinct() %>%
  left_join(transect_stream) %>%
  mutate(stream = ifelse(region == "WMaryland", stream, transect)) %>%
  left_join(spp_dist) 

filter(spp_dist, WMaryland == 1) # check that worked as expected

spp_dist <- spp_dist %>%
  select(-Transect, -stream)

# filter out sites with NA because of bad lat lon
spp_dist <- spp_dist %>%
  filter(!transect %in% c("COST28-2", "Fairway18-1", "Picnic 21-1", "COST21-3", "COST8-3", "West Shale North_1D", "PRW227-A2", "PRW263-A1")) 

# write out object for later use
saveRDS(spp_dist, "Data/Derived/spp_dist.rds")



str(df_occ)
summary(df_occ) # missing dates :( - deal with this using the "Data/featureids_for_DAYMET_data.csv" :)

# Reduce (summarize) occupancy data to species (not stage/age)
df_occ1 <- df_occ %>%
  mutate(year = year(date)) %>%
  mutate(year = if_else(region == "WMaryland", 2018, year)) %>%
  dplyr::select(transect, region, year, species, pass1, pass2, pass3, pass4) %>% # maybe include other variables as covariates
  group_by(transect, region, year, species) %>%
  summarise_all(max) 

summary(df_occ1)

# Rearrange covariates so they are in the same order (by featureid)
df_featureid <- featureids_matched %>%
  select(transect, featureid) %>%
  distinct()

df_occ_all <- df_occ1 %>%
  left_join(df_featureid) # %>%
  # filter(!is.na(featureid)) 
  
summary(df_occ_all)

df_covariates1 <- df_covariates %>%
  filter(zone == "local",
         is.na(riparian_distance_ft)) %>% # decide zone local or upstream
  select(featureid, zone, AreaSqKM, devel_hi, developed, elevation, forest, impervious, slope_pcnt, impound_area, surfcoarse)

# select just daily weather data of interest






# combine data and sort by region (make western MD the first or the last for ease of looping through differently)
df_occ_all <- df_occ_all %>%
  left_join(df_covariates1) %>%
  left_join(df_hucs) %>%
  # left_join(tempData) %>%
  left_join(climate_data_means) %>%
  filter(!is.na(featureid)) %>%
  ungroup() %>%
  mutate(transect_num = as.integer(as.factor(transect))) # %>%
  # dummy_cols(., select_columns = "region") # don't need region in the model, just use HUCs
  
summary(df_occ_all) # check for NA and reasonable values
str(df_occ_all)


### Combos for expanding combinations for JAGS loops 

# get numbers of transects, passes, and years for 3D array
species <- unique(df_occ_all$species)

n_sites <- length(unique(df_occ_all$transect_num))
n_passes <- 4
n_years <- length(unique(df_occ_all$year))

n_sites * n_years
n_sites * n_years * length(species)

n_huc12 <- length(unique(df_occ_all$huc12))
n_huc10 <- length(unique(df_occ_all$huc10))

# occ1 should be expanded to 3546 to make into complete array - might work as build array?

transect_num <- unique(df_occ_all$transect_num)
year <- unique(df_occ_all$year)

combos <- expand_grid(transect_num, year, species) %>%
  arrange(species, transect_num, year) %>%
  ungroup()


############## STILL NEED TO PUT IN DAILY COVARIATES INTO THE 3D ARRAYS BELOW - will only work if add western maryland separately or have pass in long format with dates to join in with daily covariates then spread 

#---- potential easy strategy would be to get for non-maryland as a vector with a left join by featureid and date, the replicate that 4 times into a matrix that's the same for each pass. Then create maryland separately and bind_rows. I have all the daily covariates in the tempData object (should rename - used old code)

# needed matching gransect numbers for ordering later
trans_num <- df_occ_all %>%
  select(region, transect, transect_num)

# non-Western Maryland sites daily covariates
ids <- featureids_matched %>%
  select(-date) %>%
  distinct()
  
non_wmd <- df_occ %>%
  # select(-date) %>%
  left_join(ids, by = c("transect", "region")) %>%
  select(date, region, transect, featureid) %>%
  distinct() %>%
  filter(!region == "WMaryland") %>%
  left_join(tempData) %>%
  left_join(trans_num) %>%
  distinct() %>%
  select(date, transect, region, transect_num, tmax, tmin, prcp, airTempLagged1, temp5p, temp7p, prcp2, prcp7, prcp30) %>%
  mutate(year = year(date)) %>%
  arrange(transect_num, date) %>%
  ungroup() # not sure if airTempLagged1, temp5p, temp7p, prcp2, prcp7, prcp30 are daily cov - yes they are

summary(non_wmd) 

prcp7_non <- non_wmd %>%
  select(year, region, transect, transect_num, prcp7) %>%
  mutate(pass1 = prcp7,
         pass2 = prcp7,
         pass3 = prcp7,
         pass4 = prcp7) %>%
  select(-prcp7)
  

# Need to expand to all years, standardize, and fill NA with 0

# non_wmd_3d <- array(non_wmd, c(dim(non_wmd), 4)) #don't think this is working, size of array looks fine but numbers are not...

# Western Maryand sites daily covariates
wmd <- df_occ %>%
  select(-date) %>%
  left_join(featureids_matched, by = c("transect", "region")) %>%
  select(date, region, transect, featureid) %>%
  distinct() %>%
  filter(region == "WMaryland") %>%
  left_join(tempData) %>%
  left_join(trans_num) %>%
  distinct() %>%
  select(date, transect, region, transect_num, tmax, tmin, prcp, airTempLagged1, temp5p, temp7p, prcp2, prcp7, prcp30) %>%
  mutate(year = year(date)) %>%
  arrange(transect_num, date) %>%
  ungroup() # not sure if airTempLagged1, temp5p, temp7p, prcp2, prcp7, prcp30 are daily cov - yes

summary(wmd) 

# need visit (name pass) to spread by
wmd$pass <- NA_integer_
wmd[1, "pass"] <- 1

for(i in 2:nrow(wmd)) {
  wmd$pass[i] <- ifelse(wmd$transect[i] == wmd$transect[i-1], wmd$pass[i-1] + 1, 1)
}

prcp7_wmd <- wmd %>%
  group_by(region, transect, transect_num, year) %>%
  select(region, transect, transect_num, year, pass, prcp7) %>%
  pivot_wider(names_from = pass, values_from = prcp7, names_prefix = "pass")
# Need to handle other years!!!!!!
# Need to combine with non-wmd data, expand to all years, standardize, and fill NA with 0 

str(prcp7_non)
str(prcp7_wmd)

# combine regions
prcp7 <- bind_rows(prcp7_non, prcp7_wmd)

# standardize

prcp7_std <- combos %>%
  select(-species) %>%
  distinct() %>%
  left_join(distinct(trans_num)) %>%
  left_join(prcp7) %>%
  ungroup() %>%
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  select(pass1, pass2, pass3, pass4) %>%
  as.matrix()

# prcp7_std[prcp7_std > 200] <- 200 # problematic mass rainfall or data errors. Cut off at 200 
prcp7_std <- (prcp7_std - mean(prcp7_std, na.rm = TRUE)) / sd(prcp7_std, na.rm = TRUE)
prcp7_std[is.na(prcp7_std)] <- 0

summary(prcp7_std)

saveRDS(prcp7_std, "Data/Derived/prcp7_std.rds")

# example code
# dfus_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
# for(t in 1:n_years) {
#     dfus_3d[ , , t] <- dfus %>% 
#       filter(year == year[t]) %>%
#       dplyr::select(starts_with("pass")) %>%
#       as.matrix()
# }






covs <- df_occ_all %>% # probelm with 2018 with no pass 1?
  select(-pass1, -pass2, -pass3, -pass4, -species, - year) %>%
  distinct() %>%
  left_join(spp_dist) %>% # add species range restrictions to covariates
  arrange(desc(region), transect_num)

head(covs) # western maryland at the beginning

# save covariates for occupancy
saveRDS(covs, "Data/Derived/covs.rds")

# Separate by species and make into 3D array with transect x pass x year



combos <- combos %>%
  left_join(distinct(trans_num))

# DFUS
dfus <- combos %>%
  left_join(df_occ_all) %>%
  ungroup() %>%
  dplyr::filter(species == "DFUS") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)

# dfus$region_num <- NA_integer_
# dfus$region_num[1] <- 1
# 
# for(i in 2:nrow(dfus)) {
#   dfus$region_num[i] <- ifelse(dfus$region[i] == dfus$region[i-1], dfus$region_num[i-1], dfus$region_num[i-1] + 1)
# }
#   
# summary(dfus)
#   
Year <- sort(year)
# regions <- unique(dfus$region)
# n_regions <- length(regions)

dfus_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
# for(i in 1:n_sites) {
for(t in 1:n_years) {
    dfus_3d[ , , t] <- dfus %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}
# }

saveRDS(dfus_3d, "Data/Derived/dfus_3d.rds")

# DMON
dmon <- combos %>%
  left_join(df_occ_all) %>%
  dplyr::filter(species == "DMON") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)
  
summary(dmon)
  
dmon_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    dmon_3d[ , , t] <- dmon %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(dmon_3d, "Data/Derived/dmon_3d.rds")

#DOCH
doch <- combos %>%
  left_join(df_occ_all) %>%
  dplyr::filter(species == "DOCH") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)
  
summary(doch)
  
doch_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    doch_3d[ , , t] <- doch %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(doch_3d, "Data/Derived/doch_3d.rds")

#EBIS
ebis <- combos %>%
  left_join(df_occ_all) %>%
  dplyr::filter(species == "EBIS") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)
  
summary(ebis)
  
ebis_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    ebis_3d[ , , t] <- ebis %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(ebis_3d, "Data/Derived/ebis_3d.rds")

#EGUT
egut <- combos %>%
  left_join(df_occ_all) %>%
  dplyr::filter(species == "EGUT") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)
  
summary(egut)
  
egut_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    egut_3d[ , , t] <- egut %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(egut_3d, "Data/Derived/egut_3d.rds")

#ELON
elon <- combos %>%
  left_join(df_occ_all) %>%
  dplyr::filter(species == "ELON") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)
  
summary(elon)
  
elon_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    elon_3d[ , , t] <- elon %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(elon_3d, "Data/Derived/elon_3d.rds")

#GPOR
gpor <- combos %>%
  left_join(df_occ_all) %>%
  dplyr::filter(species == "GPOR") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)
  
summary(gpor)
  
gpor_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    gpor_3d[ , , t] <- gpor %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(gpor_3d, "Data/Derived/gpor_3d.rds")

#PRUB
prub <- combos %>%
  left_join(df_occ_all) %>%
  dplyr::filter(species == "PRUB") %>% # probelm with 2018 with no pass 1?
  select(region, transect_num, year, pass1, pass2, pass3, pass4) %>%
  arrange(desc(region), transect_num, year) %>%
  data.frame(. , stringsAsFactors = FALSE)
  
summary(prub)
  
prub_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    prub_3d[ , , t] <- prub %>% 
      filter(year == Year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(prub_3d, "Data/Derived/prub_3d.rds")

#---------------cleaning---------------------

rm(list = ls())
gc()

# unload packages?


