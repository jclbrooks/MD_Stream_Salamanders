
library(dplyr)
library(tidyr)
library(lubridate)

df_occ <- readRDS("Data/Derived/combined_detailed_occ.rds")

str(df_occ)
summary(df_occ) # missing dates :(

df_occ %>%
  filter(is.na(date))

# Reduce (summarize) occupancy data to species (not stage/age)

df_occ1 <- df_occ %>%
  mutate(transect_num = as.integer(as.factor(transect)),
         year = year(date)) %>%
  mutate(year = if_else(region == "WMaryland", 2018, year)) %>%
  dplyr::select(transect_num, year, species, pass1, pass2, pass3, pass4) %>% # maybe include other variables as covariates
  group_by(transect_num, year, species) %>%
  summarise_all(max)

summary(df_occ1)

# get numbers of transects, passes, and years for 3D array
species <- unique(df_occ1$species)

n_sites <- length(unique(df_occ1$transect_num))
n_passes <- 4
n_years <- length(unique(df_occ1$year))

n_sites * n_years
n_sites * n_years * length(species)

# occ1 should be expanded to 3546 to make into complete array - might work as build array?

transect_num <- unique(df_occ1$transect_num)
year <- unique(df_occ1$year)

combos <- expand_grid(transect_num, year, species) %>%
  arrange(species, transect_num, year)


# Separate by species and make into 3D array with transect x pass x year

dfus <- combos %>%
  left_join(df_occ1) %>%
  dplyr::filter(species == "DFUS") # probelm with 2018 with no pass 1?
  
summary(dfus)
  
dfus_3d <- array(NA_integer_, c(n_sites, n_passes, n_years))
for(t in 1:n_years) {
    dfus_3d[ , , t] <- dfus %>% 
      filter(year == year[t]) %>%
      dplyr::select(starts_with("pass")) %>%
      as.matrix()
}

saveRDS(dfus_3d, "Data/Derived/dfus_3d.rds")



