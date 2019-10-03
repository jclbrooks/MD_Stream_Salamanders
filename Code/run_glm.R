### Western Maryland Abundance Modelling
### Jacey Brooks

### glm or GLMM by stream??


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


# Checking for correlation
library(corrplot)
cov <- sallies %>%
  select(air, pH, water, EC, TDS) 
cov <- na.omit(cov)
corrplot(corr = cov)
cor(cov)   # can also use vif() variance inflation factor, toss out anything above 10, ideal <3
plot(cor(cov))
## EC and TDS are highly correlated, EC and TDS are somewhat correlated with water temp


# ---- Exploratory GLMMs ----

par(mfrow=c(1,3))

# DFUS
dfus1 <- glmer(DFUS ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dfus1)
plot(dfus1)
qqnorm(residuals(dfus1))
boxplot(residuals(dfus1))
hist(residuals(dfus1))

dfus2 <- glmer(DFUS ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(dfus2)
plot(dfus2)
qqnorm(residuals(dfus2))
boxplot(residuals(dfus2))
hist(residuals(dfus2))
  
dfus3 <- glmer(DFUS ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(dfus3)
plot(dfus3)  
qqnorm(residuals(dfus3))
boxplot(residuals(dfus3))
hist(residuals(dfus3))
  
dfus4 <- glmer(DFUS ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(dfus4)
plot(dfus4)  
qqnorm(residuals(dfus4))
boxplot(residuals(dfus4))
hist(residuals(dfus4))
  
dfus5 <- glmer(DFUS ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(dfus5)
plot(dfus5) 
qqnorm(residuals(dfus5))
boxplot(residuals(dfus5))
hist(residuals(dfus5))
  
dfus6 <- glmer(DFUS ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dfus6)
plot(dfus6)  
qqnorm(residuals(dfus6))
boxplot(residuals(dfus6))
hist(residuals(dfus6))
  
# EBIS
ebis1 <- glmer(EBIS ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(ebis1)
plot(ebis1)
qqnorm(residuals(ebis1))
boxplot(residuals(ebis1))
hist(residuals(ebis1))

ebis2 <- glmer(EBIS ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(ebis2)
plot(ebis2)
qqnorm(residuals(ebis2))
boxplot(residuals(ebis2))
hist(residuals(ebis2))
##################################### SIGNIFICANT, residuals biased, somewhat autocorrelated
################### NOT BAD
  
ebis3 <- glmer(EBIS ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(ebis3)
plot(ebis3)
qqnorm(residuals(ebis3))
boxplot(residuals(ebis3))
hist(residuals(ebis3))
  
ebis4 <- glmer(EBIS ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(ebis4)
plot(ebis4) 
qqnorm(residuals(ebis4))
boxplot(residuals(ebis4))
hist(residuals(ebis4))
##################################### SIGNIFICANT, residuals not terrible, some clustering
  
ebis5 <- glmer(EBIS ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(ebis5)
plot(ebis5)  
qqnorm(residuals(ebis5))
boxplot(residuals(ebis5))
hist(residuals(ebis5))
  
ebis6 <- glmer(EBIS ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(ebis6)
plot(ebis6)  
qqnorm(residuals(ebis6))
boxplot(residuals(ebis6))
hist(residuals(ebis6))


# DMON
dmon1 <- glmer(DMON ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dmon1)
plot(dmon1)
qqnorm(residuals(dmon1))
boxplot(residuals(dmon1))
hist(residuals(dmon1))

dmon2 <- glmer(DMON ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(dmon2)
plot(dmon2)
qqnorm(residuals(dmon2))
boxplot(residuals(dmon2))
hist(residuals(dmon2))
############### significant, residuals are extreme
  
dmon3 <- glmer(DMON ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(dmon3)
plot(dmon3)  
qqnorm(residuals(dmon3))
boxplot(residuals(dmon3))
hist(residuals(dmon3))
  
dmon4 <- glmer(DMON ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(dmon4)
plot(dmon4)  
qqnorm(residuals(dmon4))
boxplot(residuals(dmon4))
hist(residuals(dmon4))
################# significant, residuals extreme
  
dmon5 <- glmer(DMON ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(dmon5)
plot(dmon5) 
qqnorm(residuals(dmon5))
boxplot(residuals(dmon5))
hist(residuals(dmon5))
  
dmon6 <- glmer(DMON ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(dmon6)
plot(dmon6)  
qqnorm(residuals(dmon6))
boxplot(residuals(dmon6))
hist(residuals(dmon6))


# GPOR
gpor1 <- glmer(GPOR ~ air + pH + water + DO + EC + TDS + (1 | transect_num), family = poisson, data = sallies)
summary(gpor1)
plot(gpor1)
qqnorm(residuals(gpor1))
boxplot(residuals(gpor1))
hist(residuals(gpor1))

gpor2 <- glmer(GPOR ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(gpor2)
plot(gpor2)  
qqnorm(residuals(gpor2))
boxplot(residuals(gpor2))
hist(residuals(gpor2))
############### significant residuals are clustered and extreme
  
gpor3 <- glmer(GPOR ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(gpor3)
plot(gpor3) 
qqnorm(residuals(gpor3))
boxplot(residuals(gpor3))
hist(residuals(gpor3))
  
gpor4 <- glmer(GPOR ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(gpor4)
plot(gpor4) 
qqnorm(residuals(gpor4))
boxplot(residuals(gpor4))
hist(residuals(gpor4))
  
gpor5 <- glmer(GPOR ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(gpor5)
plot(gpor5)  
qqnorm(residuals(gpor5))
boxplot(residuals(gpor5))
hist(residuals(gpor5))
  
gpor6 <- glmer(GPOR ~ TDS + (1 | transect_num), family = poisson, data = sallies)
summary(gpor6)
plot(gpor6) 
qqnorm(residuals(gpor6))
boxplot(residuals(gpor6))
hist(residuals(gpor6))



