library(jagsUI)
library(dplyr)
library(tidyr)


# from Royle and Dorazio page 314

testing <- TRUE
if(testing) {
  na = 100
  nb = 500
  ni = 500 + nb
  nt = 1
  nc = 1
} else {
  na = 1000
  nb = 10000
  ni = 40000
  nt = 4
  nc = 3
}


dfus_3d <- readRDS("Data/Derived/dfus_3d.rds")

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

sink("Code/JAGS/dynamic_autologistic_occ.txt")
cat("
    model {
      
      # Priors    
      
      mu_psi ~ dnorm(0, pow(2, -2))
      sd_psi ~ dt(0, pow(1.5,-2), 1)T(0, ) # half cauchy distribution with scale? = 1.5? (or 1.5^2 - look up defition of cauchy scale)
      
      mu_p ~ dnorm(0, pow(2, -2))
      sd_p ~ dt(0, pow(1.5,-2), 1)T(0, )
      
      # doesn't work with random effects by site and year on b0 and b1
      mu_b0 ~ dnorm(0, pow(2, -2))
      sd_b0 ~ dt(0, pow(1.5,-2), 1)T(0, )
      
      mu_b1 ~ dnorm(0, pow(2, -2))
      sd_b1 ~ dt(0, pow(1.5,-2), 1)T(0, )
      
      for(i in 1:n_huc12) {
        logit_psi1[i] ~ dnorm(mu_psi, sd_psi)
        logit(psi1[i]) <- logit_psi1[i] 
      }
      
      a0_p ~ dnorm(0, pow(2, -2)) # fixed intercept
      a1 ~ dnorm(-1, pow(2, -2))  # behavioral effect for reduced detect with removal
      for(i in 1:n_sites) {
        # a0_p[i] ~ dnorm(mu_p, sd_p)
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
          logit(p[i,j,t]) <- a0_p + a1 * j # add covariates
          }
        }
      } 
      
      # colonization and extinction not separable with data with both as random effects
      # for(i in 1:n_sites) {
      #   for(t in 1:(n_years - 1)) {
      #     b0[i, t] ~ dnorm(mu_b0, 1/ sd_b0 / sd_b0) # colonization prob
      #     b1[i,t] ~ dnorm(mu_b1, 1 / sd_b1 / sd_b1) # survival prob
      #   }
      # }
      
      # consider fixed effect by region
        for(t in 1:(n_years - 1)) {
          b0[t] ~ dnorm(mu_b0, pow(sd_b0, -2)) # colonization prob
          b1[t] ~ dnorm(mu_b1, pow(sd_b1, -2)) # survival prob
        }
      
      # Process model
      for(i in 1:n_sites) { # sites or transects or streams?
        Z[i, 1] ~ dbern(psi1[i])
        for(t in 2:n_years) {
          logit(mu_Z[i, t]) <- b0[t-1] + b1[t-1] * Z[i, t-1] 
          
          
          # b0[i, t-1] + b1[i, t-1] * Z[i, t-1] # + b2 * region[i] + b3 * forest[i] + b4 * temp[i] + b5 * region[i] * Z[i, t-1] + b6 * forest[i] * Z[i, t-1] + b7 * temp[i] * Z[i, t-1] # random stream or HUC
          Z[i, t] ~ dbern(mu_Z[i, t])
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
      mean_psi <- mean(psi1[ ])
      mean_p <- mean(p[ , 1, ])
      
      for(t in 1:n_years) {
      Z_sum[t] <- sum(Z[ , t])
      }
      
      # for(i in 1:n_sites) {
        for(t in 2:n_years) {
          turnover[t] <- sum(abs(Z[ , t] - Z[ , t-1])) / n_sites
        }
      # }
      
      } # end model

    ", fill=TRUE)
sink()



# make data list for JAGS
dfus_data <- list(y = dfus_3d, 
                  n_sites = dim(dfus_3d)[1], 
                  n_passes = dim(dfus_3d)[2],
                  n_years = dim(dfus_3d)[3])

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
  "mu_p",
  "sd_p",
  "mu_psi",
  "sd_psi",
  "Z_sum",
  "mu_b0",
  "mu_b1",
  "sd_b0",
  "sd_b1",
  "turnover")

autlog <- jags(data = dfus_data,
               inits = inits,
               parameters.to.save = params_autlog,
               model.file = "Code/JAGS/dynamic_autologistic_occ.txt",
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


# from Royle and Dorazio page 314





######## Model is having lots of trouble using different values of psi by transect (site) for year one. There is very little data in that year 
######## Options:
# 1. Use a single value of psi because all data that year from a single site - problem is the estimates from other sites will not be realistic or based on their information
# 2. Consider some way to combine information about b0 and b1 and relate it to psi so it comes from a common distribution or with some covariance structure
# 3. Rather than using calendar year, have psi start with the first year at any site - maybe could add calendar year as some sort of covariate

#---- 3 seems like the best option but is certainly more problematic to implement - ugh :(

# check the number of sites with observed salamanders by year
dfus_obs <- apply(dfus_3d, MARGIN = c(1, 3), max, na.rm = TRUE) 
dfus_obs[dfus_obs == -Inf] <- 0
cbind(sort(years), colSums(dfus_obs)) # no salamander observations in the first 2 years?!? Most years only a couple observations except 2012, 2015, and 2017 - something seems wrong with this but maybe only because DFUS. Other species better?

dfus_obs

# how many surveys
sites_yr <- apply(dfus_3d, MARGIN = c(1, 3), max, na.rm = TRUE) 
sites_yr[sites_yr != -Inf] <- 1
sites_yr[sites_yr == -Inf] <- 0

sites_yr <- colSums(sites_yr)
cbind(sort(years), sites_yr)

# for trial, could throw out first 5 years and start with 2006 where 52 transects were surveyed


dfus_3d_small <- dfus_3d[ , , 6:18]

# make data list for JAGS
dfus_data <- list(y = dfus_3d_small, 
                  n_sites = n_sites, 
                  n_passes = n_passes,
                  n_years = n_years - 5)

# Good starting values for occupancy = max over passes
dfus_init <- apply(dfus_3d_small, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(dfus_init[dfus_init == -Inf]) # how many site-years with no obs
dfus_init[dfus_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = dfus_init,
       p = runif(n_years-5, 0.4, 0.6))
}

params <- c(# "Z",
  "p",
  "mu_psi",
  "sd_psi",
  "gamma",
  "phi")

out <- jags(data = dfus_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/dynamic_occ.txt",
            n.chains = 3,
            # n.adapt = 100,
            n.iter = ni,
            n.burnin = nb,
            n.thin = nt, 
            parallel = TRUE,
            n.cores = 3)

# Results
out
plot(out, parameters = c("p"))




####### - static with correlation by site --------

sink("Code/JAGS/static_occ.txt")
cat("
    model {
      
      # Priors    
      
      mu_psi ~ dnorm(0, pow(2, -2))
      sd_psi ~ dt(0, pow(1.5,-2), 1)T(0, )
      
      mu_p ~ dnorm(0, pow(2, -2))
      sd_p ~ dt(0, pow(2.5,-2), 1)T(0, )
      
      # doesn't work with random effects by site and year on b0 and b1
      mu_b0 ~ dnorm(0, pow(3, -2))
      sd_b0 ~ dt(0, pow(1,-2), 1)T(0, )
      
      mu_b1 ~ dnorm(0, pow(3, -2))
      sd_b1 ~ dt(0, pow(1,-2), 1)T(0, )
      
      for(i in 1:n_sites) {
        logit_psi1[i] ~ dnorm(mu_psi, sd_psi)
        logit(psi1[i]) <- logit_psi1[i] 
      }
      
      for(i in 1:n_sites) {
        for(t in 1:n_years) {
          a0_p[i,t] ~ dnorm(mu_p, sd_p)
          logit(p[i,t]) <- a0_p[i,t]
        }
      }
      
      # colonization and extinction not separable with data with both as random effects
      for(i in 1:n_sites) {
        # for(t in 1:(n_years - 1)) {
          b0[i] ~ dnorm(mu_b0, 1/ sd_b0 / sd_b0) # colonization prob
          # b1[i,t] ~ dnorm(mu_b1, 1 / sd_b1 / sd_b1) # survival prob
        }
      # }
      
      # consider fixed effect by region
        for(t in 1:(n_years - 1)) {
          # b0[t] ~ dnorm(mu_b0, pow(sd_b0, -2)) # colonization prob
          b1[t] ~ dnorm(mu_b1, pow(sd_b1, -2)) # survival prob
        }
      
      # Process model
      for(i in 1:n_sites) { # sites or transects or streams?
        Z[i, 1] ~ dbern(psi1[i])
        for(t in 2:n_years) {
          logit(mu_Z[i, t]) <- b0[i] + b1[t-1] # * Z[i, t-1] # b0[i, t-1] + b1[i, t-1] * Z[i, t-1] # + b2 * region[i] + b3 * forest[i] + b4 * temp[i] + b5 * region[i] * Z[i, t-1] + b6 * forest[i] * Z[i, t-1] + b7 * temp[i] * Z[i, t-1] # random stream or HUC
          Z[i, t] ~ dbern(mu_Z[i, t])
        }
      }
      
      # Observation model - need to separate for visits vs. passes
      for(i in 1:n_sites) {
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
            mu_y[i,j,t] <- Z[i, t] * p[i,t]
            y[i,j,t] ~ dbern(mu_y[i,j,t])
          }
        }
      }
      
      # Derived parameters?
      mean_psi <- mean(psi1[ ])
      mean_p <- mean(p[ , ])
      
      # for(t in 1:n_years) {
        # Z_sum[t] <- sum(Z[ , t])
      # }
      
      } # end model

    ", fill=TRUE)
sink()

params <- c(# "Z",
  "p",
  "mu_psi",
  "sd_psi",
  "mu_b0",
  "sd_b0")

out <- jags(data = dfus_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/static_occ.txt",
            n.chains = 3,
            # n.adapt = 100,
            n.iter = ni,
            n.burnin = nb,
            n.thin = nt, 
            parallel = TRUE,
            n.cores = 3)

# Results
out
