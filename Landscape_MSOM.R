##################
# Landscape MSOM 
##################

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(devtools)

if(packageVersion("tidyr") < "0.8.3.9000") devtools::install_github("tidyverse/tidyr") # ensure tidyr version with pivot_wider

library(tidyr)

########################################
###### SALAMANDER OCCUPANCY DATA #######
########################################

# Read in data
canaan <- read.csv("Data/Landscape/CVNWR_transects.csv", header = TRUE, stringsAsFactors = FALSE)
capital <- read.csv("Data/Landscape/NCRlotic_all.csv", header = TRUE, stringsAsFactors = FALSE)
shenandoah <- read.csv("Data/Landscape/Shen_snp12.csv", header = TRUE, stringsAsFactors = FALSE)
wmaryland <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", header = TRUE, stringsAsFactors = FALSE)

str(canaan)
str(capital)
str(shenandoah)
str(wmaryland)

# Format data: region - transect ID - species - age - pass/visit 1- pass/visit 2 - pass/visit - 3
# make all same format, column names

#----- Canaan Valley National Wildlife Refuge Dataset -----

can <- canaan %>%
  mutate(Transect = ifelse(is.na(Transect), 0, Transect),
         Transect = paste0(Name, Transect),
         Date = mdy(Date)) %>%
  group_by(Transect, Species, Age) %>%
  select(Transect, Pass, Species, Age, Caught, Date)

max_pass_can <- can %>%
  ungroup() %>%
  group_by(Transect, Date) %>%
  summarize(max_pass = max(Pass),
            visit = NA_integer_) %>%
  arrange(Transect, Date) %>%
  ungroup() 


max_pass_can$visit[1] <- 1
for(i in 2:nrow(max_pass_can)) {
  if(max_pass_can$Transect[i] == max_pass_can$Transect[i-1]) {
    max_pass_can$visit[i] <- max_pass_can$visit[i-1] + 1
  } else {
    max_pass_can$visit[i] <- 1
  }
}

just_pass <- max_pass_can %>%
  filter(visit == 1)

combos <- can %>%
  dplyr::ungroup() %>%
  mutate(Species = ifelse(Species == "DOCR", "DOCH", Species)) %>%
  tidyr::expand(nesting(Transect, Date), Species, Age, Pass) %>%
  dplyr::filter(Species %in% c("GPOR", "DFUS", "EBIS", "DMON", "DOCH"),
                Age %in% c("A", "L")) %>%
  dplyr::arrange(Transect, Date, Species, Age, Pass) %>%
  dplyr::left_join(max_pass_can) 

can2 <- combos %>%
  left_join(can) %>%
  # group_by(Site) %>%
  mutate(Caught = ifelse(Pass <= max_pass & is.na(Caught), 0, Caught)) %>%
  arrange(Transect, Date, Species, Age, Pass)

# check the size of the combos vs resulting dataframe
length(unique(paste(can$Transect, can$Date))) * 5 * 2 * 4


# Convert counts to binary
can2$obs <- can2$Caught
can2[can2$obs > 1 & !is.na(can2$obs), "obs"] <- 1
summary(can2)



#--------- need to add date below and check if expanded for species-larvae-*age* combos for each transect -----------#
###### It did not spread for all species-age combos at all sites, something wrong with spread(), can't get pivot_wider() to load

can3 <- can2 %>%
  ungroup() %>%
  select(-visit, -Caught) %>%
  group_by(Transect, Date, Species, Age) %>%
  # select(-region) %>%
  mutate(Pass = paste0("p", Pass)) %>%
  tidyr::pivot_wider(names_from = Pass, values_from = obs) %>%
  mutate(region = "Canaan") %>%
  #spread(Pass, Caught) %>%  #### This doesn't spread correctly, it leaves out some species that need to be at all sites (even if not found)
  ungroup() %>%
  mutate(year = year(Date)) %>%
  select(region, Transect, Date, Species, Age, p1, p2, p3, p4) %>%
  as.data.frame(. , stringsAsFactors = FALSE) %>%
  arrange(region, Transect, Species, Age)

# Redo the naming
colnames(can3) <- c("region", "transect", "date", "species", "age", "pass1", "pass2", "pass3", "pass4")




#------- National Capitals Region Dataset ------

cap <- capital %>%
  mutate(#Transect = paste(PointName, Visit, sep = "_v"),
         pass4 = NA_real_,
         region = "Capital") %>% # added pass4 column to match canaan dataframe
  group_by(PointName, SpeciesCode, SAgeID) %>%
  select(region, PointName, SDate, Visit, SpeciesCode, SAgeID, PassCount1, PassCount2, PassCount3, pass4)
colnames(cap) <- c("region", "transect", "date", "visit", "species", "age", "pass1", "pass2", "pass3", "pass4")

# Remove NULLs from capitals data
na <- cap[which(cap$species == "NULL"),]
cap1 <- cap[-which(cap$species == "NULL"),]
cap <- cap1

cap[cap == "NULL"] <- NA_integer_

cap <- cap %>%
  arrange(region, transect, date, species, age) %>% 
  mutate(pass1 = as.numeric(pass1),
         pass2 = as.numeric(pass2),
         pass3 = as.numeric(pass3),
         pass4 = as.numeric(pass4),
         age = ifelse(age == "juvenile" | age == "adult", "A", age), # add together
         age = ifelse(age == "larva" | age == "metamorphosing", "L", age)) %>%
  group_by(region, transect, date, visit, species, age) %>%
  summarise_all(.funs = sum) %>%
  ungroup() %>%
  # select(-region) %>%
  as.data.frame(. , stringsAsFactors = FALSE) 


max_pass_cap <- cap %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("pass"), names_to = "pass", values_to = "count") %>%
  mutate(pass = gsub(pattern = "pass*", replacement = "", x = pass)) %>%
  filter(!is.na(count)) %>%
  select(transect, date, visit, pass) %>%
  group_by(transect, date) %>%
  mutate(max_pass = max(pass)) %>%
  arrange(transect, date, visit) %>%
  ungroup() %>%
  mutate(date = mdy(date),
         visit_old = as.integer(visit),
         pass = as.integer(pass),
         max_pass = as.integer(max_pass)) %>%
  mutate(year = year(date)) %>%
  group_by(transect, year, pass) %>%
  mutate(visit_1 = ifelse(date == min(date), 1, 0)) %>%
  distinct() %>%
  arrange(transect, date) %>%
  filter(visit_1 == 1) %>%
  select(-visit_old) %>%
  ungroup()


combos_cap <- cap %>%
  dplyr::ungroup() %>%
  mutate(species = ifelse(species == "ebis", "EBIS", species)) %>%
  tidyr::expand(nesting(transect, date, visit), species, age) %>%
  # nesting(Transect, Date, Species)
  dplyr::filter(species %in% c("DFUS", "EBIS", "PRUB", "ELON", "EGUT"),
                age %in% c("A", "L")) %>%
  dplyr::arrange(transect, date, species, age)

length(unique(cap$transect))

length(unique(paste0(cap$transect, "_", cap$date)))
length(unique(cap$species))
length(unique(cap$age))

# desired rows (before filtering to first visit each year)
rows_cap <- length(unique(paste0(cap$transect, "_", cap$date))) * 5 * 2

cap2 <- combos_cap %>%
  ungroup() %>%
  left_join(ungroup(cap)) %>%
  mutate(date = mdy(date))

rows_cap == nrow(cap2)

visit_passes <- max_pass_cap %>%
  select(transect, date, max_pass) %>%
  group_by(transect, date) %>%
  summarise_all(max) %>%
  ungroup()

cap3 <- cap2 %>%
  ungroup() %>%
  left_join(ungroup(visit_passes)) %>%
  # filter(pass == 1 | is.na(pass)) %>%
  mutate(pass1 = ifelse(1 <= max_pass & is.na(pass1), 0, pass1),
         pass2 = ifelse(2 <= max_pass & is.na(pass2), 0, pass2),
         pass3 = ifelse(3 <= max_pass & is.na(pass3), 0, pass3),
         pass4 = ifelse(4 <= max_pass & is.na(pass4), 0, pass4),
         region = "Capital") %>%
  arrange(transect, date, species, age) %>%
  distinct() %>%
  select(region, transect, date, species, age, pass1, pass2, pass3, pass4)


# cap3 <- combos_cap %>%
#   left_join(she) %>%
#  # group_by(Site) %>%
#   mutate(count = ifelse(Pass <= max_pass & is.na(count), 0, count),
#          Year = 2012) %>%
#   arrange(Site, Date, Species, Age, Pass, visit)


# ------------------------------- need max pass for each transect-date combo to separate 0 from NA  ------------------------ #





#----- Shenandoah National Park Dataset ----

# list <- c(shenandoah$Site, shenandoah$Species, shenandoah$Age)
# add_count(shenandoah, name = "count")

she <- shenandoah  %>%
  mutate(Date = mdy(Date),
         Age = ifelse(Age == "J", "A", Age)) %>%
  filter(Pass %in% 1:5,
         Age != "") %>%
  group_by(Site, Date, Species, Age, Pass) %>%
  select(Site, Date, Species, Age, Pass) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(Year = year(Date),
         Age = ifelse(Age == "l", "L", Age))

max_pass <- she %>%
  ungroup() %>%
  group_by(Site, Date) %>%
  summarize(max_pass = max(Pass),
            visit = NA_integer_) %>%
  arrange(Site, Date) %>%
  ungroup() 
  

max_pass$visit[1] <- 1
for(i in 2:nrow(max_pass)) {
  if(max_pass$Site[i] == max_pass$Site[i-1]) {
    max_pass$visit[i] <- max_pass$visit[i-1] + 1
  } else {
    max_pass$visit[i] <- 1
  }
}

just_pass <- max_pass %>%
  filter(visit == 1) %>%
  select(-Date)

# filter to just first visit to each site
she <- she %>%
  filter(visit == 1) # filter combo site-date in just pass one filter(site-date %in% unique(max_pass$site-date))

  #Pass = paste0("p", Pass)
  
# desired output length for combos
length(unique(paste(she$Site, she$Date))) * length(unique(she$Species)) * length(unique(she$Age)) * length(unique(she$Pass))

combos <- she %>%
  expand(nesting(Site, Date), Age, Species, Pass) %>%
  left_join(just_pass) 

she2 <- combos %>%
  left_join(she) %>%
 # group_by(Site) %>%
  mutate(count = ifelse(Pass <= max_pass & is.na(count), 0, count),
         Year = 2012) %>%
  arrange(Site, Date, Species, Age, Pass, visit)

she2 <- she2[-2338,]

# Convert counts to binary (detection/nondetection)
she2$obs <- she2$count
she2[she2$obs > 1 & !is.na(she2$obs), "obs"] <- 1
summary(she2)
# ------------ This needs to be spread into individual passes instead of one pass column ------------------#


#------------- Repeat this for cannan and NCR -----------




######################
# ROW 713 AND 714 NEED TO BE COMBINED? OR 713 NEEDS TO BE DELETED
######################



# spread canaan dataset
she3 <- she2 %>%
  mutate(Pass = paste0("p", Pass)) %>%
  select(-max_pass, -visit, -count, -Year) %>%
  tidyr::pivot_wider(names_from =  Pass, values_from = obs) %>%
  mutate(region = "Shenandoah") %>%
  filter(Species != "PCIN") %>%
  select(region, Site, Date, Species, Age, p1, p2, p3, p4, p5) %>% # these pass names may cause problems
  as.data.frame(. , stringsAsFactors = FALSE) 
colnames(she3) <- c("region", "transect", "date", "species", "age", "pass1", "pass2", "pass3", "pass4", "pass5")







#--- Western Maryland Dataset ---#

# Rearrange data into long format
df <- wmaryland %>%
  mutate(stream = ifelse(stream == "POPLICKTRIB", "PopLick", stream),
         stream = ifelse(stream == "ALEX", "Alexander Run", stream),
         stream = ifelse(stream == "ELKLICK", "ElkLick", stream),
         stream = ifelse(stream == "MILL", "Mill", stream),
         stream = ifelse(stream == "BLUELICK", "BlueLick", stream),
         stream = ifelse(stream == "WSHALEN", "West Shale North", stream),
         stream = ifelse(stream == "KOCH", "Koch", stream),
         stream = ifelse(stream == "DUNGHILL", "Bowser-Dung Hill", stream),
         stream = ifelse(stream == "BEARHILL", "Maynardier Ridge at Bear Hill", stream),
         trans = paste0(stream, "_", transect)) %>%
  group_by(trans, stream, transect, visit) %>%
  tidyr::gather(sp_stage, count, -date, -trans, - stream, -transect, -type, -up_down, -dist, -visit, -time_min, -air, -water, -pH, -DO, -EC, -TDS, -observers) %>%
  tidyr::separate(sp_stage, into = c("species", "stage"), sep = 4) %>%
  filter(species != "tota",
         !is.na(count)) %>%
 # mutate(type = ifelse(type == "res", up_down, type)) %>%
  select(date, stream, transect, visit, trans, species, stage, count) %>%
  ungroup()


# Convert counts to binary (detection/nondetection)
df$obs <- df$count
df[df$obs > 1 & !is.na(df$obs), "obs"] <- 1
summary(df)

# Remove PRUB from df
prub <- df[which(df$species == "PRUB"),]
df2 <- df[-which(df$species == "PRUB"),]
df <- df2


max_visit_df <- df %>%
  ungroup() %>%
  group_by(stream, transect) %>%
  summarize(max_pass = max(visit),
            visit = NA_integer_) %>%
  ungroup() %>%
  mutate(trans = paste0(stream, "_", transect))


max_visit_df$visit[1] <- 1
for(i in 2:nrow(max_visit_df)) {
  if(max_visit_df$trans[i] == max_visit_df$trans[i-1]) {
    max_visit_df$visit[i] <- max_visit_df$visit[i-1] + 1
  } else {
    max_visit_df$visit[i] <- 1
  }
}

just_visit <- max_visit_df %>%
  select(trans, max_pass) 
colnames(just_visit) <- c("trans", "max_visit")


# desired output length for combos
length(unique(df$trans)) * length(unique(df$species)) * length(unique(df$stage)) * length(unique(df$visit))

combos_df <- df %>%
  ungroup() %>%
  select(date, trans, visit, species, stage, obs) %>%
  expand(nesting(trans), stage, species, visit) %>%
  left_join(just_visit) %>%
  select(trans, species, stage, visit, max_visit) %>%
  arrange(trans, species, stage, visit)

df2 <- combos_df %>%
  left_join(df) %>%
  mutate(date = mdy(date)) %>%
  arrange(trans, species, stage, visit)

# spread dataset
df3 <- df2 %>%
  ungroup() %>%
  mutate(visit = paste0("v", visit)) %>%
  select(-max_visit, -stream, -transect, -count, -date) %>% # did not include date because it separates the counts into separate rows for each visit because each visit was done on a different day
  tidyr::pivot_wider(names_from =  visit, values_from = obs) %>%
  mutate(region = "WMaryland") %>%
  as.data.frame(. , stringsAsFactors = FALSE) %>%
  mutate(date = NA) %>%
   select(region, trans, date, species, stage, v1, v2, v3, v4) # these are VISITS NOT PASSES
colnames(df3) <- c("region", "transect", "date", "species", "age", "pass1", "pass2", "pass3", "pass4")

# array with matching dates and transect-visit, not sure if this is needed yet.....
date_df <- df %>%
  select(date, trans, visit)






#---- Combine all salamander data -----

landscape_N <- bind_rows(can3, cap3, she3, df3)

##### Like Shen replace the NA if <= max pass with 0 #####

spec <- c("DMON", "DOCH", "GPOR", "DFUS", "DOCR", "EBIS", "PRUB", "ELON", "EGUT")

landscape_occ <- landscape_N %>%
  mutate(pass1 = ifelse(pass1 > 0, 1, 0),
         pass2 = ifelse(pass2 > 0, 1, 0),
         pass3 = ifelse(pass3 > 0, 1, 0),
         pass4 = ifelse(pass4 > 0, 1, 0),
         pass5 = ifelse(pass5 > 0, 1, 0),
         canaan = ifelse(region == "Canaan", 1, 0),
         capital = ifelse(region == "Capital", 1, 0),
         shenandoah = ifelse(region == "Shenandoah", 1, 0),
         wmaryland = ifelse(region == "wmaryland", 1, 0),
         age = ifelse(age == "juvenile" | age == "recently metamorphosed" | age == "adult" | age == "metamorphosing", "A", age),
         age = ifelse(age == "" | age == "  ", NA, age),
         age = ifelse(age == "larva", "L", age)) %>%
  filter(species %in% spec,
         !transect %in% c("MRC2T1", "PR300", "MRC3TL", "PR"),
         !transect %in% c("Cortland1", "Picnic 21-1", "Picnic 21-2")) %>% # this line needs to be deleted when the csv is fixed
  mutate(#transect = ifelse(region == "Canaan", substr(transect, 1, nchar(transect) - 5), transect),
         #transect = ifelse(transect == "Camp 70-Yellow Creek_NA", "Camp 70-Yellow Creek", transect),
         #transect = ifelse(region == "Canaan", gsub(pattern = "*_", replacement = "", x = transect), transect),
         #transect = ifelse(region == "Capital", substr(transect, 1, nchar(transect) - 3), transect),
         transect = ifelse(region == "Capital", gsub(pattern = "_v.$", replacement = "", x = transect), transect),
         transect = ifelse(region == "Capital", gsub(pattern = "_vNULL", replacement = "", x = transect), transect),
         stream = transect) %>%
  separate(col = "transect", into = c("transect", "transect#"), sep = "_")
## WARNING: HARMLESS - just says that there are a lot of NAs filled into the stream column because it is conditional on the region = "wmaryland"

# Remove "PR" transect from landscape_occ (wasn't working in line 441)
landscape_occ_pr <- landscape_occ[-which(landscape_occ$transect == "PR"),]
landscape_occ <- landscape_occ_pr




########################################
##### LANDSCAPE CHARACTERISTICS ########
########################################

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

################# Pick variables to potentially use ##########
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



########################################
########### CODE FOR JAGS ##############
########################################

n_trans <- length(unique(landscape_occ$transect))
n_species <- length(unique(landscape_occ$species)) # I
n_streams <- length(unique(landscape_occ$transect)) # J
n_visits <- length(unique(sal$visit)) # K
n_stages <- length(unique(occ$stage)) # L

########### temporary hack ##########3
n_visits <- 5
n_stages <- 2 
###########################

# y[j,k,i,l]

# Make 4-D observation array
# Order everything the same way!!!!!!!!!
modeldata <- modeldata %>%
  arrange(age, species, transect)
str(modeldata)

occ_array <- array(NA, dim = c(n_trans, n_visits, n_species, n_stages))

species_observed <- unique(modeldata$species)
transects <- unique(modeldata$transect)
stages <- unique(modeldata$age)

# foo <- occ[which(occ$species == species_observed[1] & occ$stage == stages[1]), c("v1", "v2", "v3", "v4")]

# for(j in 1:n_sites) {
# for (k in 1:n_visits) {
for (i in 1:n_species) {
  for (l in 1:n_stages) {
    occ_array[1:n_trans, 1:n_visits, i, l] <- as.matrix(modeldata[which(modeldata$species == species_observed[i] & modeldata$age == stages[l]), c("pass1", "pass2", "pass3", "pass4", "pass5")])
  }
}
#   }
# }
str(occ_array)
  
data_list = list(y = occ_array, n_trans = n_trans, n_visits = n_visits, n_species = n_species, n_stages = n_stages,
                forest = (modeldata$forest - mean(modeldata$forest, na.rm = TRUE)) / sd(modeldata$forest, na.rm = TRUE), 
                area = as.numeric(scale(modeldata$AreaSqKM)),
                ec = as.numeric(df_trans$ec_s),
                water = as.matrix(select(water, v1, v2, v3, v4)),
                time_min = as.matrix(select(time_min, v1, v2, v3, v4)))


# MSOM - beta1: ec , alpha1: water - add covariance with occupancy and detection
sink("Code/JAGS/multi_spp_occ3.txt")
cat("
    model {
    
    #PRIORS
    
    # Set species loop
    for(i in 1:n_species) {
    
    b0[i] ~ dnorm(mu.b0, tau.b0) 
    b1[i] ~ dnorm(mu.b1, tau.b1)

    a0.exp[i] <- mu.a0 + (rho * sigma.p / sigma.psi) * (b0[i] - mu.b0)    
    a0[i] ~ dnorm(a0.exp[i], var.p) # expected mean detection for each species over mean covariate conditions
    a1[i] ~ dnorm(mu.a1, tau.a1)
    }
    
    #HYPERPRIORS
    mu.b0 ~ dnorm(0, 0.01)
    # sigma.psi ~ dt(0, pow(3, -2), 1)T(0, ) # half cauchy with sd = 3
    sigma.psi ~ dunif(0, 5)
    tau.b0 <- 1 / sigma.psi / sigma.psi
    # tau.b0 ~ dgamma(0.1, 0.1)
    # psi.mean ~ dunif(0, 1) # real prob scale mean occupancy among species
    # mu.b0 <- log(psi.mean / (1 - psi.mean)) # mean occupancy among species on logit scale
    
    mu.b1 ~ dnorm(0, 0.01)
    tau.b1 ~ dgamma(0.1, 0.1)
    
    mu.a0 ~ dnorm(0, 0.01)
    sigma.p ~ dunif(0, 10)
    # sigma.p ~ dt(0, pow(5, -2), 1)T(0, ) # half cauchy with sd = 1 # informative prior pulling things close to the center
    tau.a0 <- 1 / sigma.p / sigma.p
    # tau.a0 ~ dgamma(0.1, 0.1)
    var.p <- tau.b0 / (1.-pow(rho, 2))

    rho ~ dunif(-1, 1) # prior correlation between occupancy and detection
    
    mu.a1 ~ dnorm(0, 0.01)
    tau.a1 ~ dgamma(0.1, 0.1)
    
    # array transect (j) x visit (k) x species (i) x stage (l)
    # Estimation of Z matrix (true occurrence for species i at site j)	
    for(i in 1:n_species) {	
      for(j in 1:n_trans) { 
        for(l in 1:n_stages) {
          logit(psi[j,i,l]) <- b0[i] + b1[i]*ec[j]
    
          Z[j,i,l] ~ dbern(psi[j,i,l])
    
          # Estimate detection for species i and stage l at transect j during visit k
          for (k in 1:n_visits) {
            logit(p[j,k,i,l]) <- a0[i] + a1[i] * water[j, k]
            mu.p[j,k,i,l] <- p[j,k,i,l] * Z[j,i,l]
            y[j,k,i,l] ~ dbern(mu.p[j,k,i,l])
    
          }
        }
      }
    }
    
    ###### Derived quantities #######
    
    
    for(i in 1:n_species) {
    for(j in 1:n_trans) {
    occ_spp_stage[j, i] <- max(Z[ j, i, ])
    }
    occ_sp[i] <- sum(occ_spp_stage[ , i])        # Number of occupied transects by this species
    }
    
    for(j in 1:n_trans) {
    for(i in 1:n_species) {
    occ_trans_sp[j, i] <- max(Z[j, i, ])
    }
    occ_trans[j] <- sum(occ_trans_sp[j, ])      # Number of occurring species at each transect
    }
    
    # mean detection by species and stage
    for(i in 1:n_species) {
    for(l in 1:n_stages) {
    p_sp[i, l] <- sum(p[ , , i, l])
    }
    }
    
    # overall mean detection
    p_mean <- mean(p)
    
    
    }
    ", fill=TRUE)
sink()


#Parameters monitored
params <-c('p', 'Z', 'psi', 'a0','a1', 'b0','b1', "mu.b0", "tau.b0", "mu.a0", "tau.a0", "mu.b1", "tau.b1", "mu.a1", "tau.a1", "occ_sp", "occ_trans", "p_mean", "rho", "sigma.psi", "sigma.p")


# MCMC settings
testing <- TRUE
if(testing) {
  saved.per.chain <- 1000
  nc <- 3         #num. chains (this is a standard number of chains)
  nb <- 500    #burn-in; draws from Markov chain that are discarded
  nt <- 1        #thin chains to save diskspace / reduce autocorrelation among repeated draws
  ni <- saved.per.chain*nt + nb  #iterations (draws from posterior dist.)
} else {
  saved.per.chain <- 2500 # might have to make larger
  nc <- 4         #num. chains (this is a standard number of chains)
  nb <- 50000    #burn-in; draws from Markov chain that are discarded
  nt <- 4        #thin chains to save diskspace / reduce autocorrelation among repeated draws (might have to make 40)
  ni <- saved.per.chain*nt + nb  #iterations (draws from posterior dist.)
}



###################################################################
#Call JAGS from R using rjags and setup to run on multiple cores

library(jagsUI)

# beta1: pH, alpha1: time_min
out1 <- jags(data = data_list, inits = inits, params, model.file = "Code/JAGS/multi_spp_occ2.txt", 
            n.chains = nc,
            n.burnin = nb,
            n.iter = ni,
            parallel = TRUE,
            bugs.format = TRUE)



#out <- out1

summary(out1, parameters = c("mu.b0", "tau.b0", "mu.b1", "tau.b1", "mu.a0", "tau.a0", "mu.a1", "tau.a1"))

traceplot(out4, parameters = c("rho",
                              "mu.b0",
                              "mu.a0",
                              "sigma.psi",
                              "sigma.p",
                              "a0"
                              ,"b0"
                              ))

traceplot(out1, parameters = c("mu.b0", "tau.b0", "mu.a0", "tau.a0", "a0", "b0"))
traceplot(out4, parameters = c("mu.b0", "tau.b0", "mu.a0", "tau.a0", "a0", "b0"))

 