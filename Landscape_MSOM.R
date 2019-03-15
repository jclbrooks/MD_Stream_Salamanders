##################
# Landscape MSOM 
##################

library(dplyr)
library(tidyr)
library(lubridate)

# Read in data
canaan <- read.csv("Data/Landscape/CVNWR_transects.csv", header = T, stringsAsFactors = F)
capital <- read.csv("Data/Landscape/NCRlotic_all.csv", header = T, stringsAsFactors = F)
shenandoah <- read.csv("Data/Landscape/Shen_snp12.csv", header = T, stringsAsFactors = FALSE)

str(canaan)
str(capital)
str(shenandoah)

# Format data: transect ID - species - age - pass/visit 1- pass/visit 2 - pass/visit - 3
# made all same format, column names
can <- canaan %>%
  mutate(Transect = paste(Name, Transect, Year, sep = "_")) %>%
  group_by(Transect, Species, Age) %>%
  select(Transect, Pass, Species, Age, Caught) %>%
  mutate(Pass = paste0("p", Pass)) %>%
  spread(Pass, Caught)
colnames(can) <- c("transect", "species", "age", "pass1", "pass2", "pass3", "pass4")

cap <- capital %>%
  mutate(Transect = paste(PointName, Visit, sep = "_v"),
         pass4 = "NULL") %>% # added pass4 column to match canaan dataframe
  group_by(Transect, SpeciesCode, SAgeID) %>%
  select(Transect, SpeciesCode, SAgeID, PassCount1, PassCount2, PassCount3, pass4)
colnames(cap) <- c("transect", "species", "age", "pass1", "pass2", "pass3", "pass4")

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
  filter() # filter combo site-date in just pass one filter(site-date %in% unique(max_pass$site-date))
  left_join(max_pass) # %>%
  # filter(visit == 1)

combos <- she %>%
  expand(nesting(Site, Year, Age, Species), Pass) 

she <- combos %>%
  left_join(she) %>%
  mutate(count = ifelse(Pass <= max_pass))


# if(she$count = "NULL" && she$Pass <= max_pass) {0
# } else {
#   "NA"
# }


# Remove NULLs from capitals data
na <- cap[which(cap$species == "NULL"),]
cap1 <- cap[-which(cap$species == "NULL"),]
cap <- cap1
 