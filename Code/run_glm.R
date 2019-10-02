### Western Maryland Abundance Modelling
### Jacey Brooks

### GLM or GLMM by stream??


library(tidyr)
library(dplyr)
library(lme4)
library(ggplot2)

sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)

# ---- Separate adults and larvae analyses ----

# Desmognathus ochrophaeus ADULTS
doch_adult <- sal %>%
  group_by(stream, transect, visit) %>%
  select(stream, transect, visit, DOCHA) %>%
  spread(visit, value = DOCHA)

summary(doch_adult)

upstream <- sal %>%
  group_by(stream, transect) %>%
  select(stream, transect, up_down, type) %>%
  distinct() %>%
  mutate(up = ifelse(type == "res" & up_down == "UP", 1, 0))

downstream <- sal %>%
  group_by(stream, transect) %>%
  select(stream, transect, up_down, type) %>%
  distinct() %>%
  mutate(down = ifelse(type == "res" & up_down == "DOWN", 1, 0))



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


# ---- Exploratory GLMs ----

# Checking for correlation
library(corrplot)
cov <- sallies %>%
  select(air, pH, water, EC, TDS) 
cov <- na.omit(cov)
corrplot(corr = cov)
cor(cov)
plot(cor(cov))
## EC and TDS are highly correlated, EC and TDS are somewhat correlated with water temp


# DFUS
dfus <- glm(DFUS ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dfus)
plot(dfus)

dfus <- glm(DFUS ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(dfus)
plot(dfus)  
  
dfus <- glm(DFUS ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(dfus)
plot(dfus)  
  
dfus <- glm(DFUS ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(dfus)
plot(dfus)  
  
dfus <- glm(DFUS ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(dfus)
plot(dfus)  
  
dfus <- glm(DFUS ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dfus)
plot(dfus)  
  
# EBIS
ebis <- glm(EBIS ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(ebis)
plot(ebis)

ebis <- glm(EBIS ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(ebis)
plot(ebis)
##################################### SIGNIFICANT, residuals biased, somewhat autocorrelated
  
ebis <- glm(EBIS ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(ebis)
plot(ebis)
##################################### SIGNIFICANT, residuals not awesome
  
ebis <- glm(EBIS ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(ebis)
plot(ebis)  
##################################### SIGNIFICANT, residuals bad
  
ebis <- glm(EBIS ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(ebis)
plot(ebis)  
##################################### SIGNIFICANT, residuals bad
  
ebis <- glm(EBIS ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(ebis)
plot(ebis)  


# DMON
dmon <- glm(DMON ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dmon)
plot(dmon)

dmon <- glm(DMON ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(dmon)
plot(dmon)  
  
dmon <- glm(DMON ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(dmon)
plot(dmon)  
################## SIGNIFICANT, residuals skewed
  
dmon <- glm(DMON ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(dmon)
plot(dmon)  
  
dmon <- glm(DMON ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(dmon)
plot(dmon)  
################# sIGNIFICANT, but skewed
  
dmon <- glm(DMON ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dmon)
plot(dmon)  
################# SIGNIFICANT, but residuals not awesome, similar to EC glm


# GPOR
gpor <- glm(GPOR ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(gpor)
plot(gpor)

gpor <- glm(GPOR ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(gpor)
plot(gpor)  
  
gpor <- glm(GPOR ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(gpor)
plot(gpor)  
############## SIGNIFICANT, residuals are skewed
  
gpor <- glm(GPOR ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(gpor)
plot(gpor) 
############### SIGNIFICANT, residuals aren't terrible but not great
  
gpor <- glm(GPOR ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(gpor)
plot(gpor)  
  
gpor <- glm(GPOR ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(gpor)
plot(gpor)  



