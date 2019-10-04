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
library(car)
cov <- sallies %>%
  select(air, pH, water, EC, TDS) 
cov <- na.omit(cov)
corrplot(corr = cov)
cor(cov)   # can also use vif() variance inflation factor, toss out anything above 10, ideal <3
plot(cor(cov))
## EC and TDS are highly correlated, EC and TDS are somewhat correlated with water temp
vif(dfus1)


# ---- Exploratory GLMMs ----

par(mfrow=c(1,3))

# DFUS
dfus1 <- glmer(DFUS ~ air + EC + (1 | transect_num), family = poisson, data = sallies)
summary(dfus1)
plot(dfus1)
qqnorm(residuals(dfus1))
boxplot(residuals(dfus1))
hist(residuals(dfus1))
# removed pH from model because it wouldn't converge with it in it
# none of the covariates are statistically significant

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

dfus4 <- glmer(DFUS ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(dfus4)
plot(dfus4)
qqnorm(residuals(dfus4))
boxplot(residuals(dfus4))
hist(residuals(dfus4))
  
# DMON
dmon1 <- glmer(DMON ~ air + pH + EC + (1 | transect_num), family = poisson, data = sallies)
summary(dmon1)
plot(dmon1)
qqnorm(residuals(dmon1))
boxplot(residuals(dmon1))
hist(residuals(dmon1))
### air is statistically significant, the residuals look good!
###--- PLOT ---###

# DOCH
doch1 <- glmer(DOCH ~ air + pH + EC + (1 | transect_num), family = poisson, data = sallies)
summary(doch1)
plot(doch1)
qqnorm(residuals(doch1))
boxplot(residuals(doch1))
hist(residuals(doch1))
# only use air because pH and EC cause the model's parameters to be on the boudnary of the feasible parameter space, look up ?isSingular for explanation

doch2 <- glmer(DOCH ~ air + (1 | transect_num), family = poisson, data = sallies)
summary(doch2)
plot(doch2)
qqnorm(residuals(doch2))
boxplot(residuals(doch2))
hist(residuals(doch2))
# runs fine but is not significant

doch3 <- glmer(DOCH ~ water + (1 | transect_num), family = poisson, data = sallies)
summary(doch3)
plot(doch3)
qqnorm(residuals(doch3))
boxplot(residuals(doch3))
hist(residuals(doch3))
# push parameters to boundary but is significant, residuals are not great
###--- PLOT ---###

doch4 <- glmer(DOCH ~ pH + (1 | transect_num), family = poisson, data = sallies)
summary(doch4)
plot(doch4)
qqnorm(residuals(doch4))
boxplot(residuals(doch4))
hist(residuals(doch4))
# push parameters to boundary, is not significant

doch5 <- glmer(DOCH ~ EC + (1 | transect_num), family = poisson, data = sallies)
summary(doch5)
plot(doch5)
qqnorm(residuals(doch5))
boxplot(residuals(doch5))
hist(residuals(doch5))
# push parameters to boundary, is not significant

# EBIS
ebis1 <- glmer(EBIS ~ air + pH + EC + (1 | transect_num), family = poisson, data = sallies)
summary(ebis1)
plot(ebis1)
qqnorm(residuals(ebis1))
boxplot(residuals(ebis1))
hist(residuals(ebis1))
# all significant, some clustering in residuals but not terrible
###--- PLOT ---###

# GPOR
gpor1 <- glmer(GPOR ~ air + pH + EC + (1 | transect_num), family = poisson, data = sallies)
summary(gpor1)
plot(gpor1)
qqnorm(residuals(gpor1))
boxplot(residuals(gpor1))
hist(residuals(gpor1))
# only intercept is significant

# PRUB
prub1 <- glmer(PRUB ~ air + pH + EC + (1 | transect_num), family = poisson, data = sallies)
summary(prub1)
plot(prub1)
qqnorm(residuals(prub1))
boxplot(residuals(prub1))
hist(residuals(prub1))
# response is constant since there was just one count




