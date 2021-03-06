##################
# Landscape MSOM 
##################

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(devtools)

if(packageVersion("tidyr") < "0.8.99.9000") devtools::install_github("tidyverse/tidyr") # ensure tidyr version with pivot_wider

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

combos_can <- can %>%
  dplyr::ungroup() %>%
  mutate(Species = ifelse(Species == "DOCR", "DOCH", Species)) %>%
  tidyr::expand(nesting(Transect, Date), Species, Age, Pass) %>%
  dplyr::filter(Species %in% c("GPOR", "DFUS", "EBIS", "DMON", "DOCH"),
                Age %in% c("A", "L")) %>%
  dplyr::arrange(Transect, Date, Species, Age, Pass) %>%
  dplyr::left_join(max_pass_can) 

can2 <- combos_can %>%
  left_join(can) %>%
  # group_by(Site) %>%
  mutate(Caught = ifelse(Pass <= max_pass & is.na(Caught), 0, Caught)) %>%
  arrange(Transect, Date, Species, Age, Pass)

# check the size of the combos_can vs resulting dataframe
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
  arrange(region, Transect, Date, Species, Age)

# Redo the naming
colnames(can3) <- c("region", "transect", "date", "species", "age", "pass1", "pass2", "pass3", "pass4")

# Save detailed occupancy data for canaan
if(!dir.exists("Data/Derived")) dir.create("Data/Derived", recursive = TRUE)
saveRDS(can3, "Data/Derived/canaan_detailed_occ.rds")


#----- National Capitals Region Dataset ------

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
  right_join(ungroup(visit_passes)) %>%
  # filter(pass == 1 | is.na(pass)) %>%
  mutate(pass1 = ifelse(1 <= max_pass & is.na(pass1), 0, pass1),
         pass2 = ifelse(2 <= max_pass & is.na(pass2), 0, pass2),
         pass3 = ifelse(3 <= max_pass & is.na(pass3), 0, pass3),
         pass4 = ifelse(4 <= max_pass & is.na(pass4), 0, pass4),
         region = "Capital") %>%
  arrange(transect, date, species, age) %>%
  distinct() %>%
  select(region, transect, date, species, age, pass1, pass2, pass3, pass4)

# reduce from counts to occupancy
cap4 <- cap3 %>%
mutate(pass1 = ifelse(pass1 >= 1, 1, pass1),
       pass2 = ifelse(pass2 >= 1, 1, pass2),
       pass3 = ifelse(pass3 >= 1, 1, pass3),
       pass4 = ifelse(pass4 >= 1, 1, pass4),
       date = ymd(date))

# cap3 <- combos_cap %>%
#   left_join(she) %>%
#  # group_by(Site) %>%
#   mutate(count = ifelse(Pass <= max_pass & is.na(count), 0, count),
#          Year = 2012) %>%
#   arrange(Site, Date, Species, Age, Pass, visit)


# Save detailed occupancy data for the national capitals region
saveRDS(cap3, "Data/Derived/ncr_detailed_occ.rds")


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
# she <- she %>%
#   filter(visit == 1) # filter combo site-date in just pass one filter(site-date %in% unique(max_pass$site-date))

  #Pass = paste0("p", Pass)
  
# desired output length for combos_she
length(unique(paste(she$Site, she$Date))) * length(unique(she$Species)) * length(unique(she$Age)) * length(unique(she$Pass))

combos_she <- she %>%
  tidyr::expand(nesting(Site, Date), Age, Species, Pass) %>%
  left_join(just_pass) 

she2 <- combos_she %>%
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


# Save detailed occupancy data for the national capitals region
saveRDS(she3, "Data/Derived/shen_detailed_occ.rds")




#----- Western Maryland Dataset ----

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


# desired output length for combos_df
length(unique(df$trans)) * length(unique(df$species)) * length(unique(df$stage)) * length(unique(df$visit))

combos_df <- df %>%
  ungroup() %>%
  select(date, trans, visit, species, stage, obs) %>%
  tidyr::expand(nesting(trans), stage, species, visit) %>%
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


# Save detailed occupancy data for western maryland
saveRDS(df3, "Data/Derived/westmd_detailed_occ.rds")

# array with matching dates and transect-visit, not sure if this is needed yet.....
date_df <- df %>%
  select(date, trans, visit)






#----- Combine all salamander occ data -----

landscape_N <- bind_rows(can3, cap3, she3, df3)

##### Like Shen replace the NA if <= max pass with 0 

spec <- c("DMON", "DOCH", "GPOR", "DFUS", "DOCR", "EBIS", "PRUB", "ELON", "EGUT")

landscape_occ <- landscape_N %>%
  mutate(pass1 = ifelse(pass1 > 0, 1, pass1),
         pass2 = ifelse(pass2 > 0, 1, pass2),
         pass3 = ifelse(pass3 > 0, 1, pass3),
         pass4 = ifelse(pass4 > 0, 1, pass4),
         pass5 = ifelse(pass5 > 0, 1, pass5),
         canaan = ifelse(region == "Canaan", 1, 0),
         capital = ifelse(region == "Capital", 1, 0),
         shenandoah = ifelse(region == "Shenandoah", 1, 0),
         wmaryland = ifelse(region == "WMaryland", 1, 0),
         age = ifelse(age == "juvenile" | age == "recently metamorphosed" | age == "adult" | age == "metamorphosing", "A", age),
         age = ifelse(age == "" | age == "  ", NA, age),
         age = ifelse(age == "larva", "L", age)) %>%
  filter(species %in% spec,
         !transect %in% c("MRC2T1", "PR300", "MRC3TL", "PR")) %>% 
  mutate(#transect = ifelse(region == "Canaan", substr(transect, 1, nchar(transect) - 5), transect),
         #transect = ifelse(transect == "Camp 70-Yellow Creek_NA", "Camp 70-Yellow Creek", transect),
         #transect = ifelse(region == "Canaan", gsub(pattern = "*_", replacement = "", x = transect), transect),
         #transect = ifelse(region == "Capital", substr(transect, 1, nchar(transect) - 3), transect),
         transect = ifelse(region == "Capital", gsub(pattern = "_v.$", replacement = "", x = transect), transect),
         transect = ifelse(region == "Capital", gsub(pattern = "_vNULL", replacement = "", x = transect), transect),
         stream = transect) %>%
  separate(col = "transect", into = c("transect", "transect_num"), sep = "_") %>%
  select(region, stream, date, species, age, pass1, pass2, pass3, pass4, pass5, canaan, capital, shenandoah, wmaryland)
## WARNING: HARMLESS - just says that there are a lot of NAs filled into the stream column because it is conditional on the region = "wmaryland"

colnames(landscape_occ) <- c("region", "transect", "date", "species", "age", "pass1", "pass2", "pass3", "pass4", "pass5", "canaan", "capital", "shenandoah", "wmaryland")

# Remove "PR" transect from landscape_occ (wasn't working in line 441)
# landscape_occ_pr <- landscape_occ[-which(landscape_occ$transect == "PR"),]
# landscape_occ <- landscape_occ_pr

summary(landscape_occ)
unique(landscape_occ$age)
unique(landscape_occ$species)

# Save detailed occupancy data for western maryland
saveRDS(landscape_occ, "Data/Derived/combined_detailed_occ.rds")

#---------------cleaning---------------------

rm(list = ls())
gc()

# unload packages?
