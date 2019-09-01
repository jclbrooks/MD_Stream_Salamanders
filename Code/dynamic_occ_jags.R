# from Royle and Dorazio page 314

sink("Code/JAGS/dynamic_occ_simple.txt")
cat("
    model {
      
      # Priors    
      psi1 ~ dunif(0, 1) # change or make a logit function and vary by site?
      p[n_years] ~ dunif(0, 1) # change all uniform priors?
      
      for(t in 1:(n_years - 1)) {
        p[t] ~ dunif(0, 1) # make a logit linear model with neg trend by pass
        gamma[t] ~ dunif(0, 1) # colonization prob
        phi[t] ~ dunif(0, 1) # survival prob
      }
      
      # Process model
      for(i in 1:n_sites) {
        Z[i, 1] ~ dbern(psi1)
        for(t in 2:n_years) {
          mu_Z[i, t] <- Z[i, t-1] * phi[t-1] + (1 - Z[i, t-1]) * gamma[t-1]
          Z[i, t] ~ dbern(mu_Z[i, t])
        }
      }
      
      # Observation model - need to separate for visits vs. passes
      for(i in 1:n_sites) {
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
            mu_y[i,j,t] <- Z[i, t] * p[t]
            y[i,j,t] ~ dbern(mu_y[i,j,t])
          }
        }
      }
      
      
      } # end model

    ", fill=TRUE)
sink()

library(jagsUI)

testing <- TRUE
if(testing) {
  nb = 500
  ni = 500 + nb
  nt = 1
} else {
  nb = 10000
  ni = 40000
  nt = 10
}

# make data list for JAGS
dfus_data <- list(y = dfus_3d, 
                        n_sites = n_sites, 
                        n_passes = n_passes,
                        n_years = n_years)

# Good starting values for occupancy = max over passes
dfus_init <- apply(dfus_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(dfus_init[dfus_init == -Inf]) # how many site-years with no obs
dfus_init[dfus_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = dfus_init,
       p = runif(n_years, 0.4, 0.6))
}

params <- c(# "Z",
            "p",
            "psi1",
            "gamma",
            "phi")

out <- jags(data = dfus_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/dynamic_occ_simple.txt",
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

if(!dir.exists("Results/JAGS")) dir.create("Results/JAGS", recursive = TRUE)
# save(out, file = "Results/JAGS/doch_adult_mcmc.RData")
# saveRDS(pjor_od, file = "Results/JAGS/pjor_mcmc_out.rds")




sink("Code/JAGS/dynamic_autologistic_occ_simple.txt")
cat("
    model {
      
      # Priors    
      
      mu_psi ~ dnorm(0, 1 / 2 / 2)
      sd_psi ~ dunif(0, 2)
      
      for(i in 1:n_sites) {
      logit_psi1[i] ~ dnorm(mu_psi, sd_psi)
      logit(psi1[i]) <- logit_psi1[i] # change or make a logit function
      }
      
      p[n_years] ~ dunif(0, 1) # change all uniform priors?
      
      for(t in 1:(n_years - 1)) {
        p[t] ~ dunif(0, 1) # make a logit linear model with neg trend by pass
        b0[t] ~ dnorm(0, 1/ 2/ 2) # colonization prob
        b1[t] ~ dnorm(0, 1 / 2/ 2) # survival prob
      }
      
      # Process model
      for(i in 1:n_sites) { # sites or transects or streams?
        Z[i, 1] ~ dbern(psi1[i])
        for(t in 2:n_years) {
          logit(mu_Z[i, t]) <- b0[t-1] + b1[t-1] * Z[i, t-1] # + b2 * region[i] + b3 * forest[i] + b4 * temp[i] + b5 * region[i] * Z[i, t-1] + b6 * forest[i] * Z[i, t-1] + b7 * temp[i] * Z[i, t-1] # random stream or HUC
          Z[i, t] ~ dbern(mu_Z[i, t])
        }
      }
      
      # Observation model - need to separate for visits vs. passes
      for(i in 1:n_sites) {
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
            mu_y[i,j,t] <- Z[i, t] * p[t]
            y[i,j,t] ~ dbern(mu_y[i,j,t])
          }
        }
      }
      
      # Derived parameters?
      
      
      } # end model

    ", fill=TRUE)
sink()



# make data list for JAGS
dfus_data <- list(y = dfus_3d, 
                  n_sites = n_sites, 
                  n_passes = n_passes,
                  n_years = n_years)

# Good starting values for occupancy = max over passes
dfus_init <- apply(dfus_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(dfus_init[dfus_init == -Inf]) # how many site-years with no obs
dfus_init[dfus_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = dfus_init,
       p = runif(n_years, 0.4, 0.6))
}

params <- c(# "Z",
  "p",
  "mu_psi",
  "sd_psi",
  "b0",
  "b1")

out <- jags(data = dfus_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/dynamic_autologistic_occ_simple.txt",
            n.chains = 3,
            # n.adapt = 100,
            n.iter = ni,
            n.burnin = nb,
            n.thin = nt, 
            parallel = TRUE,
            n.cores = 3)

# Results
out

plot(out)


sink("Code/JAGS/dynamic_autologistic_occ.txt")
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
          logit(mu_Z[i, t]) <- b0[t-1] + b1[t-1] * Z[i, t-1] # b0[i, t-1] + b1[i, t-1] * Z[i, t-1] # + b2 * region[i] + b3 * forest[i] + b4 * temp[i] + b5 * region[i] * Z[i, t-1] + b6 * forest[i] * Z[i, t-1] + b7 * temp[i] * Z[i, t-1] # random stream or HUC
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



# make data list for JAGS
dfus_data <- list(y = dfus_3d, 
                  n_sites = n_sites, 
                  n_passes = n_passes,
                  n_years = n_years)

# Good starting values for occupancy = max over passes
dfus_init <- apply(dfus_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(dfus_init[dfus_init == -Inf]) # how many site-years with no obs
dfus_init[dfus_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = dfus_init)
       # p = runif(n_years, 0.4, 0.6))
}

params <- c(# "Z",
  "mean_psi",
  "mean_p",
  "mu_p",
  "sd_p",
  "mu_psi",
  "sd_psi",
  "Z_sum",
  "mu_b0",
  "mu_b1",
  "sd_b0",
  "sd_b1")

out <- jags(data = dfus_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/dynamic_autologistic_occ.txt",
            n.chains = nc,
            n.adapt = na,
            n.iter = ni,
            n.burnin = nb,
            n.thin = nt, 
            parallel = TRUE,
            n.cores = nc)

# Results
out

plot(out)






# from Royle and Dorazio page 314

sink("Code/JAGS/dynamic_occ.txt")
cat("
    model {
      
      # Priors  
      mu_psi ~ dnorm(0, 3)
      sd_psi ~ dunif(0, 5)
        
      for(i in 1:n_sites) {
        a0_psi[i] ~ dnorm(mu_psi, pow(sd_psi, -2))
        logit(psi1[i]) <- a0_psi[i]
      }
      
      p[n_years] ~ dunif(0, 1) # change all uniform priors?
      
      for(t in 1:(n_years - 1)) {
        p[t] ~ dunif(0, 1) # make a logit linear model with neg trend by pass
        gamma[t] ~ dunif(0, 1) # colonization prob
        phi[t] ~ dunif(0, 1) # survival prob
      }
      
      # Process model
      for(i in 1:n_sites) {
        Z[i, 1] ~ dbern(psi1[i])
        for(t in 2:n_years) {
          mu_Z[i, t] <- Z[i, t-1] * phi[t-1] + (1 - Z[i, t-1]) * gamma[t-1]
          Z[i, t] ~ dbern(mu_Z[i, t])
        }
      }
      
      # Observation model - need to separate for visits vs. passes
      for(i in 1:n_sites) {
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
            mu_y[i,j,t] <- Z[i, t] * p[t]
            y[i,j,t] ~ dbern(mu_y[i,j,t])
          }
        }
      }
      
      # Derived parameters
      psi_mean <- mean(psi1[ ])
      
      } # end model

    ", fill=TRUE)
sink()

library(jagsUI)

testing <- TRUE
if(testing) {
  nb = 500
  ni = 500 + nb
  nt = 1
} else {
  nb = 10000
  ni = 40000
  nt = 10
}

# make data list for JAGS
dfus_data <- list(y = dfus_3d, 
                  n_sites = n_sites, 
                  n_passes = n_passes,
                  n_years = n_years)

# Good starting values for occupancy = max over passes
dfus_init <- apply(dfus_3d, MARGIN = c(1, 3), max, na.rm = TRUE) # obs in any pass then Z = 1, warnings ok and addressed below
fill_len <- length(dfus_init[dfus_init == -Inf]) # how many site-years with no obs
dfus_init[dfus_init == -Inf] <- rbinom(fill_len, 1, 0.5) # fill unobs with 50% chance of being occupied for starting values

inits <- function(){
  list(Z = dfus_init,
       p = runif(n_years, 0.4, 0.6))
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

if(!dir.exists("Results/JAGS")) dir.create("Results/JAGS", recursive = TRUE)
# save(out, file = "Results/JAGS/doch_adult_mcmc.RData")
# saveRDS(pjor_od, file = "Results/JAGS/pjor_mcmc_out.rds")




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



