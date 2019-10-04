#########################################
# Exploratory graphs for W Maryland data
# Results of GLMMs found in `run_glm.R`

# Organizing Data
library(tidyr)
library(dplyr)
library(ggplot2)

sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)


# ---- Add together larvae and adults ----

spec <- sal %>%
  ungroup() %>%
  dplyr::select(-type, -up_down, -dist, - visit, -total, -observers, -time_min, -air, -pH, -water, -DO, -EC, -TDS) %>%
  group_by(date, stream, transect) %>%
  pivot_longer(cols = c(DFUSL, DFUSA, DOCHL, DOCHA, DMONL, DMONA, EBISL, EBISA, GPORL, GPORA, PRUBL, PRUBA), names_to = "species" ) %>%
  mutate(spec = substr(species, start = 1, stop = 4)) %>%
  select(-species) %>%
  group_by(date, stream, transect, spec) %>%
  summarise_all(sum) %>%
  pivot_wider(names_from = spec, values_from = value)

sallies <- sal %>%
  left_join(spec) %>%
  select(-DFUSA, -DFUSL, -DOCHL, -DOCHA, -DMONL, -DMONA, -EBISL, -EBISA, -GPORL, -GPORA, -PRUBL, -PRUBA) %>%
  mutate(transect = paste0(stream, "_", transect),
         transect_num = as.integer(as.factor(transect)))


# Exploratory Graphs

# Plot statistically significant relationships, listed below
# Plot DMON ~ water
# Plot DOCH ~ water
# Plot EBIS ~ air
# Plot EBIS ~ pH
# Plot EBIS ~ EC

# Plot DMON ~ air
dmon = ggplot(sallies) 

dmon + aes(water,DMON) + geom_point() + geom_smooth(method = "glm") + ylab("DMON Counts per Plot") + xlab("Water Temperature (°C)") + theme_classic()


# Plot DOCH ~ water
doch = ggplot(sallies) 

doch + aes(water, DOCH) + geom_point() + geom_smooth(method = "glm") + ylab("DOCH Counts per Plot") + xlab("Water Temperature (°C)") + theme_classic()


# Plot EBIS ~ air
ebis = ggplot(sallies) 

ebis + aes(air, EBIS) + geom_point() + geom_smooth(method = "glm") + ylab("EBIS Counts per Plot") + xlab("Air Temperature (°C)") + theme_classic() 

# Plot EBIS ~ pH
ebis = ggplot(sallies) 

ebis + aes(pH, EBIS) + geom_point() + geom_smooth(method = "glm") + ylab("EBIS Counts per Plot") + xlab("pH") + theme_classic()

# Plot EBIS ~ EC
ebis = ggplot(sallies) 

ebis + aes(EC, EBIS) + geom_point() + geom_smooth(method = "glm") + ylab("EBIS Counts per Plot") + xlab("Electrical Conductivity (μS/cm)") + theme_classic()


