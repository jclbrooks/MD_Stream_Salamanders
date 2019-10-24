library(jagsUI)
library(dplyr)
library(tidyr)
library(psych) # for the pairs scatterplot


# from Royle and Dorazio page 314

testing <- TRUE
if(testing) {
  na = 500
  nb = 1000
  ni = 2000 + nb
  nt = 1
  nc = 3
} else {
  na = 2000
  nb = 20000
  ni = 100000
  nt = 12
  nc = 3
}

if(!dir.exists("Results")) dir.create("Results")

dfus_3d <- readRDS("Data/Derived/dfus_3d.rds")

prcp7_std <- readRDS(("Data/Derived/prcp7_std.rds"))

# covariates are going to need to be in the same order (by featureid) as occupancy data - do this in a separate script. Left join them and then organize and separate
# df_covariates <- readRDS(file = "Data/Derived/landscape.rds")
# df_hucs <- readRDS(file = "Data/Derived/hucs.rds")
# tempData <- readRDS(file = "Data/Derived/daymet_daily.rds") # daily covariates are going to need to be prepped and spread - do in separate script
# climate_data_means <- readRDS(file = "Data/Derived/daymet_means.rds")

covs <- readRDS("Data/Derived/covs.rds")


##### Autologistic Occupancy ######

# psi and p can't both have means (intercepts) that vary randomly
# vary p randomly?
# psi fixed mean prob (intercept) that only varies by region
# problem if p varies randomly by site and pass

sink("Code/JAGS/dynamic_autologistic_occ2.txt")
cat("
    model {
      
      # Priors    
      
      # mu_psi ~ dnorm(0, pow(1.5, -2))
      # sd_psi ~ dt(0, pow(2,-2), 1)T(0, ) # half cauchy distribution with scale? = 1.5? (or 1.5^2 - look up defition of cauchy scale)
      
      # mu_p ~ dnorm(0, pow(1.5, -2))
      # sd_p ~ dt(0, pow(2,-2), 1)T(0, )
      
      #for(i in 1:n_huc12) {
      #  b0_psi[i] ~ dnorm(mu_psi, sd_psi)
      # }
      
      
      a0_p ~ dnorm(0, pow(1.5, -2)) # fixed intercept
      a1 ~ dnorm(0, pow(1.5, -2))
      a2 ~ dnorm(0, pow(1.5, -2))T( , 0)  # behavioral effect for reduced detect with removal
      
      
      for(i in 1:36) { # first 36 records are in western maryland
        # a0_p[i] ~ dnorm(mu_p, sd_p)
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
          logit(p[i,j,t]) <- a0_p + a1 * prcp7[i, j] # add covariates surfcoarse? cobble? precip, precip*area, airtemp + b4 * precip[i] + b5 * area[i] ****consider separate fixed effect a0_p by region****************
          }
        }
      } 
      
            for(i in 37:n_sites) {
        # a0_p[i] ~ dnorm(mu_p, sd_p)
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
          logit(p[i,j,t]) <- a0_p + a2 * j + a1 * prcp7[i, j] #  add covariates surfcoarse? cobble? precip, precip*area, airtemp + b4 * precip[i] + b5 * area[i]
          }
        }
      } 
      
      mu_b0 ~ dnorm(0, pow(1.5, -2))
      b1 ~ dnorm(0, pow(1.5, -2))
      b2 ~ dnorm(0, pow(1.5, -2))
      b3 ~ dnorm(0, pow(1.5, -2))
      b4 ~ dnorm(0, pow(1.5, -2))
      b5 ~ dnorm(0, pow(1.5, -2))
      b6 ~ dnorm(0, pow(1.5, -2))
      mu_b6 ~ dnorm(0, pow(1.5, -2))
      
      sd_b0 ~ dt(0, pow(2, -2), 1)T(0, )
      sd_b6 ~ dt(0, pow(1.5, -2), 1)T(0, )

# sd_b0 ~ dunif(0, 5)
# sd_b6 ~ dunif(0, 5)

      for(h in 1:n_huc12) {
        b0[h] ~ dnorm(mu_b0, sd_b0)
        # b6[h] ~ dnorm(mu_b6, sd_b6)
      }
      
      # Process model
      for(i in 1:n_sites) { # sites or transects or streams?
        logit(psi[i, 1]) <- b0[huc[i]] + b1 * forest[i] + b2 * slope[i] + b3 * air_mean[i] + b5 * precip[i] # multiple the whole thing by vector of species range + b4 * air_mean[i] * air_mean[i]
        Z[i, 1] ~ dbern(psi[i, 1] * zeta[i])
        
        for(t in 2:n_years) {
          logit(psi[i,t]) <- b0[huc[i]] + b1 * forest[i] + b2 * slope[i] + b3 * air_mean[i] + b5 * precip[i] + b6 * Z[i, t-1] #  b6[huc[i]] * Z[i, t-1] # trouble getting convergence for b6 varying by huc - trying larger huc and more informative prior - or just use region instead + b4 * air_mean[i] * air_mean[i]
          Z[i, t] ~ dbern(psi[i, t] * zeta[i]) # makes psi the suitability and psi * zeta the prob of occupancy
        }
      }
      
      # Observation model - need to separate for visits vs. passes
      for(i in 1:n_sites) {
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
            mu_y[i,j,t] <- Z[i, t] * p[i,j,t]
            y[i,j,t] ~ dbern(mu_y[i,j,t])
          }
        }
      }
      
      # Derived parameters?
      mean_psi <- mean(psi[ , ])
      mean_p <- mean(p[ , 1, ])
      
      for(t in 1:n_years) {
      Z_sum[t] <- sum(Z[ , t])
      }
      
      # for(i in 1:n_sites) { 
        for(t in 2:n_years) {
          turnover[t] <- sum(abs(Z[ , t] - Z[ , t-1])) / n_sites
        }
      # }
      
      # calculate average turnover rates per region across years?
      
      } # end model

    ", fill=TRUE)
sink()



# make data list for JAGS
# pairs(covs[ , c("forest", "slope_pcnt", "air_mean", "prcp_mo_mean", "AreaSqKM", "elevation", "impervious")])
pdf(file = "Results/scatterplot_matrix.pdf")
pairs.panels(covs[ , c("forest", "slope_pcnt", "air_mean", "prcp_mo_mean", "AreaSqKM", "elevation", "impervious")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()

dfus_data <- list(y = dfus_3d, 
                  n_sites = dim(dfus_3d)[1], 
                  n_passes = dim(dfus_3d)[2],
                  n_years = dim(dfus_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$DFUS)) # range of the species 
                  
# Good starting values for occupancy = max over passes
dfus_init <- apply(dfus_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(dfus_init[dfus_init == -Inf]) # how many site-years with no obs
dfus_init[dfus_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = dfus_init)
  # p = runif(n_years, 0.4, 0.6))
}

params_autlog <- c(# "Z",
  "mean_psi",
  "mean_p",
  "a0_p",
  "a1",
  "a2",
  "mu_b0",
  "mu_b6",
  "sd_b0",
  "sd_b6",
  "b1",
  "b2",
  "b3",
  "b4",
  "b5",
  "b6",
  "mu_p",
  "sd_p",
  "Z_sum",
  "turnover")

autlog <- jags(data = dfus_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/dfus_mcmc.rds")

plot(autlog)

library(ggplot2)
library(tidyr)
library(bayesplot)
library(tidybayes)

posterior <- as.matrix(autlog$samples)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior, regex_pars = c("Z_sum"),
           prob = 0.8) + plot_title

posterior_Z <- select(as.data.frame(posterior), starts_with("Z_sum"))

# pivot_longer and make specific years then plot
Z_long <- as.data.frame(posterior) %>%
  select(starts_with("Z_sum")) 

colnames(Z_long) <- 2006:2018

Z_long <- Z_long %>%
  pivot_longer(cols = starts_with("2"),
               names_to = "year",
               values_to = "Z")

ggplot(data = Z_long, aes(year, Z)) + geom_violin()
ggplot(data = Z_long, aes(year, Z)) + geom_boxplot()

color_scheme_set("purple")

ppc_intervals(
  # y = mtcars$mpg,
  yrep = posterior_predict(posterior),
  x = mtcars$wt,
  prob = 0.5
) +
  labs(
    x = "Weight (1000 lbs)",
    y = "MPG",
    title = "50% posterior predictive intervals \nvs observed miles per gallon",
    subtitle = "by vehicle weight"
  ) +
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white")



#--------- DMON -------------
dmon_3d <- readRDS("Data/Derived/dmon_3d.rds")

dmon_data <- list(y = dmon_3d, 
                  n_sites = dim(dmon_3d)[1], 
                  n_passes = dim(dmon_3d)[2],
                  n_years = dim(dmon_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$DMON)) # range of the species 

# Good starting values for occupancy = max over passes
dmon_init <- apply(dmon_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(dmon_init[dmon_init == -Inf]) # how many site-years with no obs
dmon_init[dmon_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = dmon_init)
  # p = runif(n_years, 0.4, 0.6))
}

autlog <- jags(data = dmon_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/dmon_mcmc.rds")



#--------- DOCH -------------
doch_3d <- readRDS("Data/Derived/doch_3d.rds")

doch_data <- list(y = doch_3d, 
                  n_sites = dim(doch_3d)[1], 
                  n_passes = dim(doch_3d)[2],
                  n_years = dim(doch_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$DOCH)) # range of the species 

# Good starting values for occupancy = max over passes
doch_init <- apply(doch_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(doch_init[doch_init == -Inf]) # how many site-years with no obs
doch_init[doch_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = doch_init)
  # p = runif(n_years, 0.4, 0.6))
}

autlog <- jags(data = doch_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/doch_mcmc.rds")

#--------- EBIS -------------
ebis_3d <- readRDS("Data/Derived/ebis_3d.rds")

ebis_data <- list(y = ebis_3d, 
                  n_sites = dim(ebis_3d)[1], 
                  n_passes = dim(ebis_3d)[2],
                  n_years = dim(ebis_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$EBIS)) # range of the species 

# Good starting values for occupancy = max over passes
ebis_init <- apply(ebis_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(ebis_init[ebis_init == -Inf]) # how many site-years with no obs
ebis_init[ebis_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = ebis_init)
  # p = runif(n_years, 0.4, 0.6))
}

autlog <- jags(data = ebis_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/ebis_mcmc.rds")


#--------- EGUT -------------
egut_3d <- readRDS("Data/Derived/egut_3d.rds")

egut_data <- list(y = egut_3d, 
                  n_sites = dim(egut_3d)[1], 
                  n_passes = dim(egut_3d)[2],
                  n_years = dim(egut_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$EGUT)) # range of the species 

# Good starting values for occupancy = max over passes
egut_init <- apply(egut_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(egut_init[egut_init == -Inf]) # how many site-years with no obs
egut_init[egut_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = egut_init)
  # p = runif(n_years, 0.4, 0.6))
}

autlog <- jags(data = egut_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/egut_mcmc.rds")


#--------- ELON -------------
elon_3d <- readRDS("Data/Derived/elon_3d.rds")

elon_data <- list(y = elon_3d, 
                  n_sites = dim(elon_3d)[1], 
                  n_passes = dim(elon_3d)[2],
                  n_years = dim(elon_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$ELON)) # range of the species 

# Good starting values for occupancy = max over passes
elon_init <- apply(elon_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(elon_init[elon_init == -Inf]) # how many site-years with no obs
elon_init[elon_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = elon_init)
  # p = runif(n_years, 0.4, 0.6))
}

autlog <- jags(data = elon_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/elon_mcmc.rds")


#--------- GPOR -------------
gpor_3d <- readRDS("Data/Derived/gpor_3d.rds")

gpor_data <- list(y = gpor_3d, 
                  n_sites = dim(gpor_3d)[1], 
                  n_passes = dim(gpor_3d)[2],
                  n_years = dim(gpor_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$GPOR)) # range of the species 

# Good starting values for occupancy = max over passes
gpor_init <- apply(gpor_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(gpor_init[gpor_init == -Inf]) # how many site-years with no obs
gpor_init[gpor_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = gpor_init)
  # p = runif(n_years, 0.4, 0.6))
}

autlog <- jags(data = gpor_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/gpor_mcmc.rds")


#--------- PRUB -------------
prub_3d <- readRDS("Data/Derived/prub_3d.rds")

prub_data <- list(y = prub_3d, 
                  n_sites = dim(prub_3d)[1], 
                  n_passes = dim(prub_3d)[2],
                  n_years = dim(prub_3d)[3],
                  n_huc12 = length(unique(covs$huc12)),
                  huc = as.integer(as.factor(covs$huc12)),
                  forest = as.numeric(scale(covs$forest)),
                  slope = as.numeric(scale(covs$slope_pcnt)),
                  air_mean = as.numeric(scale(covs$air_mean)),
                  precip = as.numeric(scale(covs$prcp_mo_mean)),
                  area = as.numeric(scale(covs$AreaSqKM)),
                  prcp7 = as.matrix(prcp7_std),
                  zeta = as.numeric(covs$PRUB)) # range of the species 

# Good starting values for occupancy = max over passes
prub_init <- apply(prub_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(prub_init[prub_init == -Inf]) # how many site-years with no obs
prub_init[prub_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = prub_init)
  # p = runif(n_years, 0.4, 0.6))
}

autlog <- jags(data = prub_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ2.txt",
               n.chains = nc,
               n.adapt = na,
               n.iter = ni,
               n.burnin = nb,
               n.thin = nt, 
               parallel = TRUE,
               n.cores = nc,
               modules=c('glm'))

# Results
autlog

if(!dir.exists("Results")) dir.create("Results")
saveRDS(autlog, "Results/prub_mcmc.rds")



# from Royle and Dorazio page 314





######## Model is having lots of trouble using different values of psi by transect (site) for year one. There is very little data in that year 
######## Options:
# 1. Use a single value of psi because all data that year from a single site - problem is the estimates from other sites will not be realistic or based on their information
# 2. Consider some way to combine information about b0 and b1 and relate it to psi so it comes from a common distribution or with some covariance structure
# 3. Rather than using calendar year, have psi start with the first year at any site - maybe could add calendar year as some sort of covariate

#---- 3 seems like the best option but is certainly more problematic to implement - ugh :(
