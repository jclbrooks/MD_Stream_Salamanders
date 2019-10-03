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
  tidyr::spread(variable, value) # convert from long to wide by variable
summary(df_covariates)

# need to organize covariates into upstream or local by featureid
upstream <- df_covariates %>%
  dplyr::group_by(featureid) %>%
  dplyr::filter(zone == "upstream",
                is.na(riparian_distance_ft)) %>%
  # dplyr::select(-zone, -location_id, -location_name) %>%
  # dplyr::summarise_each(funs(mean)) %>% # needed???
  dplyr::rename(forest_all = forest)

# Get upstream riparian forest
riparian_200 <- df_covariates %>%
  dplyr::group_by(featureid) %>%
  dplyr::select(featureid, forest, zone, riparian_distance_ft) %>%
  dplyr::filter(zone == "upstream",
                riparian_distance_ft == 200)

# create covariateData input dataset
covariateData <- riparian_200 %>%
  dplyr::select(-riparian_distance_ft) %>%
  dplyr::left_join(upstream)

str(covariateData)

# covariateData <- left_join(covariateData,
#                            dplyr::select(df_locations, location_id, location_name, latitude, longitude, featureid)) %>%
#   dplyr::mutate(location_name=factor(location_name))
# summary(covariateData)


#---------------------daymet climate data-------------------------
start.time <- Sys.time()
# cat(paste0("Make daymet query: ", start.time), file = logFile, append = TRUE, sep = "\n")

tbl_daymet <- tbl(db, 'daymet') %>%
  dplyr::filter(featureid %in% featureids)

df_daymet_long <- dplyr::collect(tbl_daymet, n = Inf)

Sys.time() - start.time

str(df_daymet_long)



#---------------------daymet climate data-------------------------
start.time <- Sys.time()
cat(paste0("Make daymet query: ", start.time), file = logFile, append = TRUE, sep = "\n")

# too big/slow to pull through R so make the query and export that. The resulting sql script can then be run via command line or within a bash script or make file


featureids_string <- paste(featureids[!is.na(featureids)], collapse=', ')

years <- 2000:2018
years_string <- paste(years, collapse=', ')

qry <- paste0("COPY(SELECT featureid, date_part('year', date) as year, date, tmax, tmin, prcp, dayl, srad, swe FROM daymet WHERE featureid IN (", featureids_string, ") AND date_part('year', date) IN (",years_string, ") ) TO STDOUT CSV HEADER;")  #

if(!file.exists(file.path(getwd(), "Code"))) dir.create(file.path(getwd(), "Code"))
cat(qry, file = "Code/daymet_query.sql")


# psql -f <subdirectory>/code/daymet_query.sql -d sheds -w > <subdirectory>/daymet_results.csv

# create a PostgreSQL instance and create one connection.
m <- dbDriver("PostgreSQL")

con <- dbConnect(m, options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'), dbname="sheds", host='felek.cns.umass.edu')

dbListTables(con)
dbListFields(con, "daymet_daily")

rs <- dbSendQuery(con, qry)

df2 <- fetch(rs, n = -1)
dbHasCompleted(rs)
dbClearResult(rs)
dbListTables(con)   

#-------------------- Restructure and combine data----------------
climateData <- data.frame(df_daymet_long, stringsAsFactors = FALSE)

tempData <- climateData %>%
  dplyr::mutate(site = as.numeric(as.factor((featureid))),
                dOY = yday(date),
                airTemp = (tmax + tmin) / 2)

# Order by group and date
tempData <- tempData[order(tempData$site,tempData$year,tempData$dOY), ]

# For checking the order of tempDataSync
tempData$count <- 1:length(tempData$year)

tempData <- tempData[order(tempData$count),] # just to make sure tempDataSync is ordered for the slide function

# moving means instead of lagged terms in the future
tempData <- tempData %>%
  group_by(site, year) %>%
  arrange(site, year, dOY) %>%
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

tempDataBP <- temperatureData %>%
  dplyr::filter(!is.na(featureid)) %>%
  left_join(dplyr::mutate(data.frame(unclass(tempData)), date = as.Date(date))) %>%
  left_join(covariateData, by = c("featureid")) %>%
  dplyr::mutate(site = as.character(featureid),
                impoundArea = AreaSqKM * allonnet / 100)


