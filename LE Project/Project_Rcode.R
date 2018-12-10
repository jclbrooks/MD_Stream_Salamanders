### Project Landscape Ecology
### Jacey Brooks

library("maptools")
library(rgdal) # shapefiles
library(raster) # rasters
library(biomod2) # SDM package
library(colorRamps) # build color ramps for mapping
library(dplyr)

location = read.csv("Project/LOCATION.csv", header = TRUE)
sal = read.csv("Project/Brooks_FSU_herps.csv", header = TRUE)

location <-  location[, c(1,5,6)]
sal <-  sal[, c(1,4,5,6,7,8,9,10)]

mdshp <- readOGR(dsn = "Project/shp", layer = "Maryland_Waterbodies__Rivers_and_Streams_Detailed")
            
#streamsal <- merge(location, sal, "SITEYR")    

# write a csv for mbssallpoints
mbssallpoints <- readOGR(dsn = "Project/shp", layer = "MBSS_allpoints_albers")
head(mbssallpoints@data)
projection(mbssallpoints)
write.csv(mbssallpoints@data, "Project/data/mbssallpoints.csv", row.names = FALSE)
mbssallpoints$Longitude8 <- ifelse(mbssallpoints$Longitude8 > 0, mbssallpoints$Longitude8*-1, mbssallpoints$Longitude8)


# Load rasters, by watershed
NBlistRasts <- list.files("Project/data/mbssData/mbssData/02070002/tif", pattern=".tif", full.names=T, recursive=T)
NBranchpred <- stack(NBlistRasts[grep("/data/mbssData/mbssData/02070002/tif/", NBlistRasts)])

YlistRasts <- list.files("Project/data/mbssData/mbssData/05020006/tif", pattern=".tif", full.names=T, recursive=T)
Youghpred <- stack(YlistRasts[grep("/data/mbssData/mbssData/05020006/tif/", YlistRasts)])

TClistRasts <- list.files("Project/data/mbssData/mbssData/02070003/tif", pattern=".tif", full.names=T, recursive=T)
TownCreekpred <- stack(TClistRasts[grep("/data/mbssData/mbssData/02070003/tif/", TClistRasts)])

ClistRasts <- list.files("Project/data/mbssData/mbssData/02070004/tif", pattern=".tif", full.names=T, recursive=T)
Conocopred <- stack(ClistRasts[grep("/data/mbssData/mbssData/02070004/tif/", ClistRasts)])

# Load in shp files
NBranchshp <- readOGR(dsn = "Project/shp", layer = "02070002_mbss")
Youghshp <- readOGR(dsn = "Project/shp", layer = "05020006_mbss")
TownCreekshp <- readOGR(dsn = "Project/shp", layer = "02070003_mbss")
Conocoshp <- readOGR(dsn = "Project/shp", layer = "02070004_mbss")

# Load occupancy data, by species
sal = read.csv("Project/Brooks_FSU_herps.csv", header = TRUE)
loc <- mbssallpoints[,c(1,6,7)]
DFUS <-  sal[, c(1,5)]
EBIS <-  sal[, c(1,6)]
PRUB <-  sal[, c(1,7)]
ELON <-  sal[, c(1,8)]
DMON <-  sal[, c(1,9)]
DOCH <-  sal[, c(1,10)]

DFUSloc <- merge(DFUS, loc, by = "SITEYR")
DFUSloc <- na.omit(DFUSloc)

EBISloc <- merge(EBIS, loc, by = "SITEYR")
EBISloc <- na.omit(EBISloc)

PRUBloc <- merge(PRUB, loc, by = "SITEYR")
PRUBloc <- na.omit(PRUBloc)

ELONloc <- merge(ELON, loc, by = "SITEYR")
ELONloc <- na.omit(ELONloc)

DMONloc <- merge(DMON, loc, by = "SITEYR")
DMONloc <- na.omit(DMONloc)

DOCHloc <- merge(DOCH, loc, by = "SITEYR")
DOCHloc <- na.omit(DOCHloc)

# plot all maryland streams and points from location
crs.geo <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
salloc <- merge(sal, loc, by = "SITEYR")
streamsal <- salloc[,c(1,5,6,7,8,9,10,19,20)]
# coordinates(salloc) <- c("Longitude8", "Latitude83")
# projection(salloc) <- crs.geo
# projection(salloc)
projection(mdshp) <- crs.geo
projection(mdshp)

plot(streamsal$Longitude8, streamsal$Latitude83,
     pch = 20,
     cex = 2,
     col = "red",
     xlab = "Longitude (decimal degrees)",
     ylab = "Latitude (decimal degrees)")
plot(mdshp, 
     col = "darkblue",
     add = T)

# Formatting data
DFUSvec <- as.vector(DFUSloc$DFUS)
DFUSvec[1] <- 1.0
# coordinates(DFUSloc) <- c("Longitude8", "Latitude83")
# projection(DFUSloc) <- crs.geo
# projection(DFUSloc)

initialStateDFUS <- BIOMOD_FormatingData(resp.var=as.vector(DFUSloc$DFUS), 
                                         resp.name="DFUS",
                                         resp.xy=cbind(DFUSloc$Longitude8, DFUSloc$Latitude83),
                                         expl.var=addLayer(NBranchpred, 
                                                           which(names(NBranchpred) == c("slp_loc", 
                                                                                         "str_len_loc", 
                                                                                         "cc_loc", 
                                                                                         "for_loc",
                                                                                         "sicl_loc",
                                                                                         "sat_loc"))),
                                         PA.nb.rep=1, PA.nb.absences=2000)

# develop model, by species
salModClim <- BIOMOD_Modeling(initialStateDFUS, models="GLM", DataSplit = 75, NbRunEval = 5
                           # models.options=BIOMOD_ModelingOptions(RF = list(ntree=500)) #### do I need this??
                           )

salProjClim <- BIOMOD_Projection(modeling.output=salModClim,
                            new.env=dropLayer(saPreds, 
                            which(names(saPreds)=="hum_impact")), 
                            proj.name="South America")


projList <- list.files(paste(getwd(), "/Sinvicta.SAclim", sep=""), ".grd", 
                       full.names=T, recursive=T)
saProjRast <- stack(projList[grep("proj_South America_Sinvicta.SAclim", projList)])
par(mfrow=c(1,2))
plot(saProjRast[[1]]/1000, axes=F, main="GLM-Prediction", col=color.system)
#points(saAnts$longitude, saAnts$latitude, pch=21, col=rgb(0,0,0),
#       bg=rgb(1,1,1,0.5), cex=0.5) 
plot(saRange, add=T, lwd=3)

plot(saProjRast[[2]]/1000, axes=F, legend=T, main="RF-Prediction", col=color.system)
#points(saAnts$longitude, saAnts$latitude, pch=21, col=rgb(0,0,0),
#       bg=rgb(1,1,1,0.5), cex=0.5) 
plot(saRange, add=T, lwd=3)







# make occupancy data spatial objects, by species
coordinates(DFUSloc) <- c("Longitude8", "Latitude83")
projection(mbsssal) <- crs.geo
projection(mbsssal)

# Plot Maryland
# CRy<-c(35,40) # this is on y axis = latitude, neg values because extend into southern hemisphere
# CRx<-c(270,290) # this is on x axis = longitude, this goes from 0 to 360
# m<-system.file("share/gshhs_c.b",package="maptools") 
# CR<-Rgshhs(m,xlim=CRx,ylim=CRy,level=1) # clips the file, don't want to map entire world
# plot(CR$SP,col="grey",xaxs="i",yaxs="i",axes=TRUE, las=1)

# MD shp

# mdshp <- readOGR(dsn = "Project/shp", layer = "Maryland_Waterbodies__Rivers_and_Streams_Detailed")
# str(mdshp)
# 
# plot(mdshp, axes=F)
# points(streamsal$Longitude83, streamsal$Latitude83, pch=50, col=rgb(0,0,0),
#        bg=rgb(1,1,1,0.5), cex=0.5) 
# # DOESN"T WORK




location = read.csv("Project/LOCATION.csv", header = TRUE)
sal = read.csv("Project/Brooks_FSU_herps.csv", header = TRUE)
location <-  location[, c(1, 5, 6)]
sal <-  sal[, c(1, 4, 5, 6, 7, 8, 9, 10)]
streamsal <- merge(location, sal, "SITEYR")



projection(mdshp)
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
extent(mdshp)
# class:Extent
# xmin:-79.48721
# xmax:-75.04921
# ymin:37.91198
# ymax:39.72302
mmm <- crop(mdshp, extent = c(-79.5, -78, 37.92, 39.73))

plot(location$Longitude83, location$Latitude83)
plot(mdshp, add = # Fri Dec 07 11:25:24 2018 ------------------------------+)
)
         
plot(mdshp, add = T)
       
plot(location$Longitude83, location$Latitude83)
plot(mdshp, add = T)
plot(location$Longitude83,
     location$Latitude83,
     pch = 20,
     col = "red")
plot(mdshp, add = T)












head(mbssallpoints)

# merge mbss coord with sal loc
loc <- mbssallpoints[,c(1,6,7)]
sal <-  sal[, c(1,4,5,6,7,8,9,10)]
mbsssal <- merge(sal, loc, by = "SITEYR")


# make mbsssal a spatial object
coordinates(mbsssal) <- c("Longitude8", "Latitude83")
projection(mbsssal) <- crs.geo
projection(mbsssal)






# Formatting data
initialStateSal <- BIOMOD_FormatingData(resp.var=as.vector(mbsssal$DFUS, na.rm = TRUE), 
                                       resp.name="streamsal",
                                       resp.xy=cbind(saAstreamsal$Longitude8, streamsal$Latitude83),
                                       expl.var=dropLayer(saPreds, 
                                       which(names(saPreds)=="hum_impact")),
                                       PA.nb.rep=1, PA.nb.absences=2000)
# should I do this by species???
# should my predictor variables be in a tif file??




salModClim <- BIOMOD_Modeling(initialStateSal, models="GLM", DataSplit = 75, NbRunEval = 5
                           # models.options=BIOMOD_ModelingOptions(RF = list(ntree=500)) #### do I need this??
                           )

salProjClim <- BIOMOD_Projection(modeling.output=salModClim,
                            new.env=dropLayer(saPreds, 
                            which(names(saPreds)=="hum_impact")), 
                            proj.name="South America")


projList <- list.files(paste(getwd(), "/Sinvicta.SAclim", sep=""), ".grd", 
                       full.names=T, recursive=T)
saProjRast <- stack(projList[grep("proj_South America_Sinvicta.SAclim", projList)])
par(mfrow=c(1,2))
plot(saProjRast[[1]]/1000, axes=F, main="GLM-Prediction", col=color.system)
#points(saAnts$longitude, saAnts$latitude, pch=21, col=rgb(0,0,0),
#       bg=rgb(1,1,1,0.5), cex=0.5) 
plot(saRange, add=T, lwd=3)

plot(saProjRast[[2]]/1000, axes=F, legend=T, main="RF-Prediction", col=color.system)
#points(saAnts$longitude, saAnts$latitude, pch=21, col=rgb(0,0,0),
#       bg=rgb(1,1,1,0.5), cex=0.5) 
plot(saRange, add=T, lwd=3)



modelEvalsal <- get_evaluations(salaModClim)
modelEvalsal

# build data object for model evaluation
evalSA_NA <- cbind(Sinvicta.SAclim=get_formal_data(naInvictaModClim,'resp.var'), 
              get_formal_data(naInvictaModClim,'expl.var'))
# convert NA's to 0's - not sure why biomod formats this way
evalSA_NA[,1][which(is.na(evalSA_NA[,1]))] <- 0
# evaluate SA model using NA data
evaluate(saInvictaModClim, evalSA_NA, stat=c('TSS'))