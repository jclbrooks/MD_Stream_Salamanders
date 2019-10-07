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
  mutate(stream_trans = paste0(stream, "_", transect),
         transect_num = as.integer(as.factor(transect)),
         date = mdy(date))

width <- read.csv("Data/Date_Location_Transect_Visit_Data_Physical_Stream_Features_processed.csv", stringsAsFactors = FALSE, header = T)

width1 <- width %>%
  select(-Section, -X) %>%
  group_by(Date, Stream, Transect, Visit) %>%
  summarise(mean(Wetted),
            mean(Depth))
  
width2 <- width1[-1,] %>%
  ungroup()# %>%
 # mutate(Date = mdy(Date))

str(width2)
colnames(width2) <- c("date", "stream", "transect", "visit", "mean_wetted_width", "mean_depth")
as.data.frame(width2)

sal_all <- sallies %>%
  left_join(width2, by = c("stream", "transect", "visit")) %>%
  select(-date.y, -DO)


# Checking for correlation
library(corrplot)
library(car)
cov <- sal_all %>%
  select(air, pH, water, EC, TDS, mean_wetted_width, mean_depth) 
cov <- na.omit(cov)
corrplot(corr = cov)
cor(cov)   # can also use vif() variance inflation factor, toss out anything above 10, ideal <3
plot(cor(cov))
## EC and TDS are highly correlated, EC and TDS are somewhat correlated with water temp
vif(dfus1)


# ---- Exploratory GLMMs ----

par(mfrow=c(1,3))

# DFUS
dfus1 <- glmer(DFUS ~ air + pH + EC + mean_wetted_width + mean_depth + (1 | transect_num), family = poisson, data = sal_all)
summary(dfus1)
plot(dfus1)
qqnorm(residuals(dfus1))
boxplot(residuals(dfus1))
hist(residuals(dfus1))
# removed pH from model because it wouldn't converge with it in it
# none of the covariates are statistically significant

dfus4 <- glmer(DFUS ~ water + (1 | transect_num), family = poisson, data = sal_all)
summary(dfus4)
plot(dfus4)
qqnorm(residuals(dfus4))
boxplot(residuals(dfus4))
hist(residuals(dfus4))
  
# DMON
dmon1 <- glmer(DMON ~ air + pH + EC + (1 | transect_num), family = poisson, data = sal_all)
summary(dmon1)
plot(dmon1)
qqnorm(residuals(dmon1))
boxplot(residuals(dmon1))
hist(residuals(dmon1))
### air is statistically significant, the residuals look good!
###--- PLOT ---###

dmon2 <- glmer(DMON ~ air + pH + EC + mean_wetted_width + mean_depth + (1 | transect_num), family = poisson, data = sal_all)
summary(dmon2)
plot(dmon2)
qqnorm(residuals(dmon2))
boxplot(residuals(dmon2))
hist(residuals(dmon2))
### depth is statistically significant, the residuals look okay
###--- PLOT ---###

dmon3 <- glmer(DMON ~ water + (1 | transect_num), family = poisson, data = sal_all)
summary(dmon3)
plot(dmon3)
qqnorm(residuals(dmon3))
boxplot(residuals(dmon3))
hist(residuals(dmon3))
### water is significant, residuals look okay



# DOCH
doch1 <- glmer(DOCH ~ air + pH + EC + mean_wetted_width + mean_depth + (1 | transect_num), family = poisson, data = sal_all)
summary(doch1)
plot(doch1)
qqnorm(residuals(doch1))
boxplot(residuals(doch1))
hist(residuals(doch1))
# only use air because pH and EC cause the model's parameters to be on the boudnary of the feasible parameter space, look up ?isSingular for explanation

doch3 <- glmer(DOCH ~ water + (1 | transect_num), family = poisson, data = sal_all)
summary(doch3)
plot(doch3)
qqnorm(residuals(doch3))
boxplot(residuals(doch3))
hist(residuals(doch3))
# wwater is significant, residuals are not great
###--- PLOT ---###



# EBIS
ebis1 <- glmer(EBIS ~ air + pH + EC + mean_wetted_width + mean_depth + (1 | transect_num), family = poisson, data = sal_all)
summary(ebis1)
plot(ebis1)
qqnorm(residuals(ebis1))
boxplot(residuals(ebis1))
hist(residuals(ebis1))
# air, pH, EC significant, some clustering in residuals but not terrible
###--- PLOT ---###

ebis2 <- glmer(EBIS ~ water + (1 | transect_num), family = poisson, data = sal_all)
summary(ebis2)
plot(ebis2)
qqnorm(residuals(ebis2))
boxplot(residuals(ebis2))
hist(residuals(ebis2))
### water is significant, residuals look good

# GPOR
gpor1 <- glmer(GPOR ~ air + pH + EC + mean_wetted_width + mean_depth + (1 | transect_num), family = poisson, data = sal_all)
summary(gpor1)
plot(gpor1)
qqnorm(residuals(gpor1))
boxplot(residuals(gpor1))
hist(residuals(gpor1))
# mean_wetted_width, mean_depth is significant, residuals okay

gpor2 <- glmer(GPOR ~ water + (1 | transect_num), family = poisson, data = sal_all)
summary(gpor2)
plot(gpor2)
qqnorm(residuals(gpor2))
boxplot(residuals(gpor2))
hist(residuals(gpor2))
# water not significa

# PRUB
prub1 <- glmer(PRUB ~ air + pH + EC + mean_wetted_width + mean_depth + (1 | transect_num), family = poisson, data = sal_all)
summary(prub1)
plot(prub1)
qqnorm(residuals(prub1))
boxplot(residuals(prub1))
hist(residuals(prub1))
# response is constant since there was just one count




