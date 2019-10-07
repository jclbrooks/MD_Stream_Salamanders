# Retrieve data from postgres database
#
# requires working directory
#
# returns three RData files: observed temperature time series, landscape/landuse, and climate data from daymet
#
# usage: $ Rscript derive_metrics.R <input ??? json> <output temperatureData rdata> <output covariateData rdata> <output climateData rdata>
# example: $ Rscript retrieve_db.R ./wd??? ./temperatureData.RData ./covariateData.RData ./climateData.RData

library(dplyr)
library(tidyr)
library(lubridate)
library(RPostgreSQL)
# library(ggplot2)
# library(devtools)
# install_github("Conte-Ecology/conteStreamTemperature", quiet = TRUE)
# library(conteStreamTemperature)
# library(jsonlite)
library(readr)
library(zoo) # rollsum, rollapply
library(stringr)



featureid_df <- read_csv("Data/featureids_for_DAYMET_data.csv")
str(featureid_df)

#-----------------------fetch covariates-----------------------
featureids <- as.integer(unique(featureid_df$featureid))

# connect to database source
db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

# fetch covariates
# featureid |  variable  | value | zone  | riparian_distance_ft 
cov_fetch <- c("agriculture", "alloffnet", "allonnet", "AreaSqKM", "devel_hi", "devel_low", "devel_med", "developed", "devel_opn", "drainageclass", "elevation", "forest", "fwsopenwater", "fwswetlands", "herbaceous", "hydrogroup_a", "hydrogroup_ab", "hydrogroup_cd", "hydrogroup_d1", "hydrogroup_d4", "impervious", "openoffnet", "openonnet", "percent_sandy", "slope_pcnt", "surfcoarse", "tree_canopy", "undev_forest", "water", "wetland")

start.time <- Sys.time()
tbl_covariates <- tbl(db, 'covariates') %>%
  dplyr::filter(featureid %in% featureids & variable %in% cov_fetch)

df_covariates_long <- dplyr::collect(tbl_covariates, n = Inf)

Sys.time() - start.time

df_covariates <- df_covariates_long %>%
  tidyr::spread(variable, value) %>% # convert from long to wide by variable
  mutate(impound_area = AreaSqKM * allonnet / 100) %>%
  data.frame(., stringsAsFactors = FALSE)

summary(df_covariates)

# need to organize covariates into upstream or local by featureid
# upstream <- df_covariates %>%
#   dplyr::group_by(featureid) %>%
#   dplyr::filter(zone == "upstream",
#                 is.na(riparian_distance_ft)) %>%
#   # dplyr::select(-zone, -location_id, -location_name) %>%
#   # dplyr::summarise_each(funs(mean)) %>% # needed???
#   dplyr::rename(forest_all = forest)
# 
# # Get upstream riparian forest
# riparian_200 <- df_covariates %>%
#   dplyr::group_by(featureid) %>%
#   dplyr::select(featureid, forest, zone, riparian_distance_ft) %>%
#   dplyr::filter(zone == "upstream",
#                 riparian_distance_ft == 200)
# 
# # create covariateData input dataset
# covariateData <- riparian_200 %>%
#   dplyr::select(-riparian_distance_ft) %>%
#   dplyr::left_join(upstream)

str(df_covariates)

#---------------------HUC data-------------------------

con <- dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "ecosheds.org",
  port = 5432,
  user = options('SHEDS_USERNAME'),
  password = options('SHEDS_PASSWORD'),
  dbname = "sheds"
)

dbListTables(con)

db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

tbl_hucs <- tbl(db, 'catchment_huc12') %>%
  dplyr::filter(featureid %in% featureids)

df_hucs <- dplyr::collect(tbl_hucs, n = Inf)

df_hucs <- df_hucs %>%
  mutate(huc10 = str_sub(huc12, end = -3)) # not sure why 3, space?

length(unique(df_hucs$huc12))
length(unique(df_hucs$huc10))

#---------------------daymet climate data-------------------------
start.time <- Sys.time()

featureids <- featureids[!is.na(featureids)]

con <- dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "ecosheds.org",
  port = 5432,
  user = options('SHEDS_USERNAME'),
  password = options('SHEDS_PASSWORD'),
  dbname = "sheds"
)

sql <- glue::glue_sql("select * from get_daymet_featureids('{<<featureids*>>}');", .open = "<<", .close = ">>", .con = con)
rs <- dbSendQuery(con, sql)
daymet_df <- dbFetch(rs)
dbClearResult(rs)

table(daymet_df$featureid)
str(daymet_df)

##### need 2018 data


#-------------------- Restructure and combine data----------------
climateData <- data.frame(daymet_df, stringsAsFactors = FALSE)

tempData <- climateData %>%
  dplyr::mutate(site = as.numeric(as.factor((featureid))),
                year = year(date),
                dOY = yday(date),
                airTemp = (tmax + tmin) / 2) # Add seasons to summarize by later??

# create separate dataframe with just longer term norms related to occupancy
# summarize by year (or seasons first?) and then means across years
climate_data_means <- tempData %>%
  group_by(featureid, year) %>%
  select(featureid, year, tmax, airTemp, prcp, swe) %>%
  summarise(tmax = max(tmax), airTemp = mean(airTemp), prcp_mo = sum(prcp)/12, swe = sum(swe)) %>%
  ungroup() %>%
  select(-year) %>%
  group_by(featureid) %>%
  summarise_all(mean) # just overall wetter or cooler sites?

summary(climate_data_means)

# detection weather variables - rolling means
# filter to years with observed data?
tempData <- tempData %>%
  group_by(site, year) %>%
  arrange(site, date) %>%
  mutate(airTempLagged1 = lag(airTemp, n = 1, fill = NA),
         #airTempLagged2 = lag(airTemp, n = 2, fill = NA),
         #prcpLagged1 = lag(prcp, n = 1, fill = NA),
         #prcpLagged2 = lag(prcp, n = 2, fill = NA),
         #prcpLagged3 = lag(prcp, n = 3, fill = NA),
         #temp5 = rollsum(x = airTemp, 5, align = "right", fill = NA),
         temp5p = rollapply(data = airTempLagged1, 
                            width = 5, 
                            FUN = mean, 
                            align = "right", 
                            fill = NA, 
                            na.rm = T),
         temp7p = rollapply(data = airTempLagged1, 
                            width = 7, 
                            FUN = mean, 
                            align = "right", 
                            fill = NA, 
                            na.rm = T),
         prcp2 = rollsum(x = prcp, 2, align = "right", fill = NA),
         prcp7 = rollsum(x = prcp, 7, align = "right", fill = NA),
         prcp30 = rollsum(x = prcp, 30, align = "right", fill = NA))



#----------------save files---------------------
saveRDS(df_covariates, file = "Data/Derived/landscape.rds")
saveRDS(df_hucs, file = "Data/Derived/hucs.rds")
saveRDS(tempData, file = "Data/Derived/daymet_daily.rds")
saveRDS(climate_data_means, file = "Data/Derived/daymet_means.rds")

# library(foreign)
# write.dbf(data.frame(df_featureid_200, stringsAsFactors = FALSE), file = paste0(data_dir, "/featureid_list_20160602.dbf"))

#---------------cleaning---------------------

rm(list = ls())
gc()

# unload packages?

