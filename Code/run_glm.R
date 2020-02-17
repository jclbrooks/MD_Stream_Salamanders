### Western Maryland Abundance Modelling
### Jacey Brooks

### glm or GLMM by stream??


library(tidyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(lubridate)
library(AICcmodavg)

sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)
substrate <- read.csv("C:/Users/Jacey/Documents/FSU/Research/R_code/MD_Stream_salamanders/Data/Location_Transect_Stream_Features_processed.csv", stringsAsFactors = FALSE)

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

sub <- substrate %>%
  select(-Date, - Visit, - Section, -X) %>%
  group_by(Stream, Transect) %>%
  summarise_all(max, na.rm = TRUE) %>%
  mutate(Bankfull = ifelse(Bankfull == (-Inf), NA, Bankfull),
         S.C = ifelse(S.C == (-Inf), NA, S.C),
         Sand = ifelse(Sand == (-Inf), NA, Sand),
         Gravel = ifelse(Gravel == (-Inf), NA, Gravel),
         Cobble = ifelse(Cobble == (-Inf), NA, Cobble),
         Boulder = ifelse(Boulder == (-Inf), NA, Boulder),
         Debris = ifelse(Debris == (-Inf), NA, Debris),
         perc_s.c = (S.C/25)*100,
         perc_sand = (Sand/25)*100,
         perc_gravel = (Gravel/25)*100,
         perc_cobble = (Cobble/25)*100,
         perc_boulder = (Boulder/25)*100,
         perc_debris = (Debris/25)*100) %>%
  select(-S.C, -Sand, -Gravel, -Cobble, -Boulder, -Debris)
colnames(sub) <- c("stream", "transect", "bankfull", "silt_clay", "sand", "gravel", "cobble", "boulder", "debris")


sal_all <- sallies %>%
  left_join(width2, by = c("stream", "transect", "visit")) %>%
  left_join(sub) %>%
  select(-date.y, -DO)


# Checking for correlation
library(corrplot)
library(car)
cov <- sal_all %>%
  select(air, pH, water, EC, TDS, mean_wetted_width, mean_depth, cobble) 
cov <- na.omit(cov)
corrplot(corr = cov)
cor(cov, use = "complete.obs")   # can also use vif() variance inflation factor, toss out anything above 10, ideal <3
plot(cor(cov))
## EC and TDS are highly correlated, EC and TDS are somewhat correlated with water temp
vif(dfus1)


# ---- Exploratory GLMMs ----
# water temp vs water chemistry

par(mfrow=c(1,3))

# DFUS
dfus = list()

dfus[[1]] <- glmer(DFUS ~ water + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

# sal_all$pH <- scale(sal_all$pH)
#sal_all$EC <- scale(sal_all$EC)

dfus[[2]] <- glmer(DFUS ~ pH + EC + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water + mean_wetted_width + cobble", "pH + EC + mean_wetted_width + cobble")
aictab(cand.set = dfus, modnames = model_names)

summary(dfus[[2]])
plot(dfus[[2]])
qqnorm(residuals(dfus[[2]]))
boxplot(residuals(dfus[[2]]))
hist(residuals(dfus[[2]]))
# none of the variables are significant, residuals aren't great

  
# DMON
dmon = list()

dmon[[1]] <- glmer(DMON ~ water + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

dmon[[2]] <- glmer(DMON ~  pH + EC + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water + mean_wetted_width + cobble", "pH + EC + mean_wetted_width + cobble")
aictab(cand.set = dmon, modnames = model_names)

summary(dmon[[2]])
plot(dmon[[2]])
qqnorm(residuals(dmon[[2]]))
boxplot(residuals(dmon[[2]]))
hist(residuals(dmon[[2]]))


### pH is statistically significant, the residuals look okay
###--- PLOT ---###

# DOCH
doch = list()

doch[[1]] <- glmer(DOCH ~ water + mean_wetted_width + cobble  + (1 | transect_num), family = poisson, data = sal_all)

doch[[2]] <- glmer(DOCH ~ pH + EC + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water + mean_wetted_width + cobble", "pH + EC + mean_wetted_width + cobble")
aictab(cand.set = doch, modnames = model_names)

### both models failed to converge


# EBIS
ebis = list()

ebis[[1]] <- glmer(EBIS ~ water + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

ebis[[2]] <- glmer(EBIS ~ pH + EC + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water + mean_wetted_width + cobble", "pH + EC + mean_wetted_width + cobble")
aictab(cand.set = ebis, modnames = model_names)

summary(ebis[[2]])
plot(ebis[[2]])
qqnorm(residuals(ebis[[2]]))
boxplot(residuals(ebis[[2]]))
hist(residuals(ebis[[2]]))
### nothing is significant, residuals look GREAT

# GPOR
gpor = list()

gpor[[1]] <- glmer(GPOR ~ water + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

gpor[[2]] <- glmer(GPOR ~ pH + EC + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water + mean_wetted_width + cobble", "pH + EC + mean_wetted_width + cobble")
aictab(cand.set = gpor, modnames = model_names)

summary(gpor[[1]])
plot(gpor[[1]])
qqnorm(residuals(gpor[[2]]))
boxplot(residuals(gpor[[2]]))
hist(residuals(gpor[[2]]))
# none are significant, residuals are okay

# PRUB
prub = list()

prub[[1]] <- glmer(PRUB ~ water + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)
prub[[2]] <- glmer(PRUB ~ pH + EC + mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water + mean_wetted_width + cobble", "pH + EC + mean_wetted_width + cobble")
aictab(cand.set = prub, modnames = model_names)

# response is constant since there was just one count


##################################################################
# Exploring water temp vs water chemistry vs physical features


# DFUS
dfus = list()

dfus[[1]] <- glmer(DFUS ~ water + (1 | transect_num), family = poisson, data = sal_all)

# sal_all$pH <- scale(sal_all$pH)
#sal_all$EC <- scale(sal_all$EC)

dfus[[2]] <- glmer(DFUS ~ pH + EC +  (1 | transect_num), family = poisson, data = sal_all)

dfus[[3]] <- glmer(DFUS ~ mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water", "pH + EC", "mean_wetted_width + cobble")
aictab(cand.set = dfus, modnames = model_names)

summary(dfus[[3]])
plot(dfus[[3]])
qqnorm(residuals(dfus[[3]]))
boxplot(residuals(dfus[[3]]))
hist(residuals(dfus[[3]]))
# none of the variables are significant, residuals aren't great

  
# DMON
dmon = list()

dmon[[1]] <- glmer(DMON ~ water + (1 | transect_num), family = poisson, data = sal_all)

dmon[[2]] <- glmer(DMON ~ pH + EC +  (1 | transect_num), family = poisson, data = sal_all)

dmon[[3]] <- glmer(DMON ~ mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water", "pH + EC", "mean_wetted_width + cobble")
aictab(cand.set = dmon, modnames = model_names)

summary(dmon[[3]])
plot(dmon[[3]])
qqnorm(residuals(dmon[[3]]))
boxplot(residuals(dmon[[3]]))
hist(residuals(dmon[[3]]))

saveRDS(dmon[[3]], "Results/dmon_glmm_results.rds")
### pH is statistically significant, the residuals look okay
###--- PLOT ---###

# DOCH
doch = list()

doch[[1]] <- glmer(DOCH ~ water + (1 | transect_num), family = poisson, data = sal_all)

doch[[2]] <- glmer(DOCH ~ pH + EC +  (1 | transect_num), family = poisson, data = sal_all)

doch[[3]] <- glmer(DOCH ~ mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water", "pH + EC", "mean_wetted_width + cobble")
aictab(cand.set = doch, modnames = model_names)

### both models failed to converge


# EBIS
ebis = list()

ebis[[1]] <- glmer(EBIS ~ water + (1 | transect_num), family = poisson, data = sal_all)

ebis[[2]] <- glmer(EBIS ~ pH + EC +  (1 | transect_num), family = poisson, data = sal_all)

ebis[[3]] <- glmer(EBIS ~ mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water", "pH + EC", "mean_wetted_width + cobble")
aictab(cand.set = ebis, modnames = model_names)

summary(ebis[[2]])
plot(ebis[[2]])
qqnorm(residuals(ebis[[2]]))
boxplot(residuals(ebis[[2]]))
hist(residuals(ebis[[2]]))

saveRDS(ebis[[2]], "Results/ebis_glmm_results.rds")

# GPOR
gpor = list()

gpor[[1]] <- glmer(GPOR ~ water + (1 | transect_num), family = poisson, data = sal_all)

gpor[[2]] <- glmer(GPOR ~ pH + EC +  (1 | transect_num), family = poisson, data = sal_all)

gpor[[3]] <- glmer(GPOR ~ mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water", "pH + EC", "mean_wetted_width + cobble")
aictab(cand.set = gpor, modnames = model_names)

summary(gpor[[3]])
plot(gpor[[3]])
qqnorm(residuals(gpor[[3]]))
boxplot(residuals(gpor[[3]]))
hist(residuals(gpor[[3]]))

saveRDS(gpor[[3]], "Results/gpor_glmm_results.rds")

# PRUB
prub = list()

prub[[1]] <- glmer(PRUB ~ water + (1 | transect_num), family = poisson, data = sal_all)

prub[[2]] <- glmer(PRUB ~ pH + EC +  (1 | transect_num), family = poisson, data = sal_all)

prub[[3]] <- glmer(PRUB ~ mean_wetted_width + cobble + (1 | transect_num), family = poisson, data = sal_all)

model_names = c("water", "pH + EC", "mean_wetted_width + cobble")
aictab(cand.set = prub, modnames = model_names)

# response is constant since there was just one count

####################
# Effect of up_down
dfus_up <- glm(DFUS ~ up_down, family = poisson, data = sal_all)
summary(dfus_up)
plot(dfus_up)
hist(dfus_up)

dmon_up <- glm(DMON ~ up_down, family = poisson, data = sal_all)
summary(dmon_up)
plot(dmon_up)
hist(dmon_up)

doch_up <- glm(DOCH ~ up_down, family = poisson, data = sal_all)
summary(doch_up)
plot(doch_up)
hist(doch_up)

ebis_up <- glm(EBIS ~ up_down, family = poisson, data = sal_all)
summary(ebis_up)
plot(ebis_up)
hist(ebis_up)
# SIGNIFICANT

gpor_up <- glm(GPOR ~ up_down, family = poisson, data = sal_all)
summary(gpor_up)
plot(gpor_up)
hist(gpor_up)

prub_up <- glm(PRUB ~ up_down, family = poisson, data = sal_all)
summary(prub_up)
plot(prub_up)
hist(prub_up)

####################
# Effect of type
dfus_up <- glm(DFUS ~ type, family = poisson, data = sal_all)
summary(dfus_up)
plot(dfus_up)
hist(dfus_up)

dmon_up <- glm(DMON ~ type, family = poisson, data = sal_all)
summary(dmon_up)
plot(dmon_up)
hist(dmon_up)
# SIGNIFICANT

doch_up <- glm(DOCH ~ type, family = poisson, data = sal_all)
summary(doch_up)
plot(doch_up)
hist(doch_up)
# SIGNIFICANT

ebis_up <- glm(EBIS ~ type, family = poisson, data = sal_all)
summary(ebis_up)
plot(ebis_up)
hist(ebis_up)

gpor_up <- glm(GPOR ~ type, family = poisson, data = sal_all)
summary(gpor_up)
plot(gpor_up)
hist(gpor_up)
# SIGNIFICANT

prub_up <- glm(PRUB ~ type, family = poisson, data = sal_all)
summary(prub_up)
plot(prub_up)
hist(prub_up)
