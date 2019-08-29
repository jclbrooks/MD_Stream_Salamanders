# from Royle and Dorazio page 314

sink("Code/JAGS/dynamic_occ.txt")
cat("
    model {
      
      # Priors    
      psi1 ~ dunif(0, 1) # change or make a logit function
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
            y[i,j,t] <- dbern(mu_y[i,j,t])
          }
        }
      }
      
      
      } # end model

    ", fill=TRUE)
sink()


sink("Code/JAGS/dynamic_autologistic_occ.txt")
# cat("
    model {
      
      # Priors    
      psi1 ~ dunif(0, 1) # change or make a logit function
      p[n_years] ~ dunif(0, 1) # change all uniform priors?
      
      for(t in 1:(n_years - 1)) {
        p[t] ~ dunif(0, 1) # make a logit linear model with neg trend by pass
        gamma[t] ~ dunif(0, 1) # colonization prob
        phi[t] ~ dunif(0, 1) # survival prob
      }
      
      # Process model
      for(i in 1:n_sites) { # sites or transects or streams?
        Z[i, 1] ~ dbern(psi1)
        for(t in 2:n_years) {
          logit(mu_Z[i, t]) <- b0[t-1] + b1[t-1] * Z[i, t-1] + b2 * region[i] + b3 * forest[i] + b4 * temp[i] + b5 * region[i] * Z[i, t-1] + b6 * forest[i] * Z[i, t-1] + b7 * temp[i] * Z[i, t-1] # random stream or HUC
          Z[i, t] ~ dbern(mu_Z[i, t])
        }
      }
      
      # Observation model - need to separate for visits vs. passes
      for(i in 1:n_sites) {
        for(t in 1:n_years) {
          for(j in 1:n_passes) {
            mu_y[i,j,t] <- Z[i, t] * p[t]
            y[i,j,t] <- dbern(mu_y[i,j,t])
          }
        }
      }
      
      # Derived parameters?
      
      } # end model

    ", fill=TRUE)
sink()
