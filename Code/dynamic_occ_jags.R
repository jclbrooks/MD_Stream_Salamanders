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
      
      mu_psi ~ dnorm(0, 1 / 2 / 2)
      sd_psi ~ dunif(0, 2)
      
      mu_p ~ dnorm(0, 1 / 2 / 2)
      sd_p ~ dunif(0, 2)
      
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
      
      for(t in 1:(n_years - 1)) {
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
            mu_y[i,j,t] <- Z[i, t] * p[i,t]
            y[i,j,t] ~ dbern(mu_y[i,j,t])
          }
        }
      }
      
      # Derived parameters?
      mean_psi <- mean(psi1[ ])
      mean_p <- mean(p[ , ])
      
      for(t in 1:n_years) {
        Z_sum[t] <- sum(Z[ , t])
      }
      
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
  "mu_p",
  "sd_p",
  "mu_psi",
  "sd_psi",
  "Z_sum",
  "b0",
  "b1")

out <- jags(data = dfus_data,
            inits = inits,
            parameters.to.save = params,
            model.file = "Code/JAGS/dynamic_autologistic_occ.txt",
            n.chains = nc,
            # n.adapt = 100,
            n.iter = ni,
            n.burnin = nb,
            n.thin = nt, 
            parallel = TRUE,
            n.cores = nc)

# Results
out

plot(out)
