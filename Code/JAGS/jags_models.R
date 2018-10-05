sink("Code/JAGS/min_od.txt")
cat("
model{
# Priors
for(s in 1:n_streams) {
  Z_N[s] ~ dnorm(0, 1)
}
sigma_stream ~ dt(0, 1/(5^2), 1)I(0, ) # half cauchy with scale = 5

a0 ~ dnorm(0, 0.25)
a1 ~ dnorm(0, 0.25)
a2 ~ dnorm(0, 0.25)

b0 ~ dnorm(0, 0.111111)

for(i in 1:n_transects) {
  for(j in 1:n_visits) {
    Z_p[i,j] ~ dnorm(0, 1)
  }
}
sigma_p ~ dt(0, 1/(5^2), 1)I(0, ) # half cauchy with scale = 5


# likelihood
for(i in 1:n_transects) {
  N[i] ~ dpois(lambda[i])
  log(lambda[i]) = a0 + Z_N[stream[i]] * sigma_stream + a1 * up[i] + a2 * down[i]
  for(j in 1:n_visits) {
    lp[i,j] <- b0 + Z_p[i,j] * sigma_p # b1 * temp[i,j] + b2 * precip[i,j] 
    Count[i,j] ~ dbin(p[i,j], N[i])
    p[i,j] <- 1 / (1 + exp(-lp.lim[i,j]))    
    lp.lim[i,j] <- min(999, max(-999, lp[i,j])) # Help stabilize the logit
  }
}
} # end model
    ", fill = TRUE)
sink()


sink("Code/JAGS/min.txt")
cat("
    model{
    # Priors
    for(s in 1:n_streams) {
    Z_N[s] ~ dnorm(0, 1)
    }
    sigma_stream ~ dt(0, 1/(5^2), 1)I(0, ) # half cauchy with scale = 5
    
    a0 ~ dnorm(0, 0.25)
    a1 ~ dnorm(0, 0.25)
    a2 ~ dnorm(0, 0.25)
    
    b0 ~ dnorm(0, 1/(3^2))
    
    # likelihood
    for(i in 1:n_transects) {
    N[i] ~ dpois(lambda[i])
    log(lambda[i]) = a0 + Z_N[stream[i]] * sigma_stream + a1 * up[i] + a2 * down[i]
    for(j in 1:n_visits) {
    lp[i,j] <- b0 # b1 * temp[i,j] + b2 * precip[i,j] 
    Count[i,j] ~ dbin(p[i,j], N[i])
    p[i,j] <- 1 / (1 + exp(-lp.lim[i,j]))    
    lp.lim[i,j] <- min(999, max(-999, lp[i,j])) # Help stabilize the logit
    }
    }
    } # end model
    ", fill = TRUE)
sink()


#### Multispecies Occupancy Models #####

# Not sure what to do about larvae/adult

# adapted from http://mbjoseph.github.io/2013/02/24/com_occ.html
cat("
  model{
    #### priors
    # beta hyperparameters
    p_beta ~ dbeta(1, 1)
    mubeta <- log(p_beta / (1 - p_beta))
    sigmabeta ~ dunif(0, 10)
    taubeta <- (1 / (sigmabeta * sigmabeta))
    
    # rho hyperparameters
    p_rho ~ dbeta(1, 1)
    murho <- log(p_rho / (1 - p_rho))
    sigmarho~dunif(0,10)
    taurho<-1/(sigmarho*sigmarho)
    
    # p hyperparameters
    p_p ~ dbeta(1, 1)
    mup <- log(p_p / (1 - p_p))
    sigmap ~ dunif(0,10)
    taup <- (1 / (sigmap * sigmap))
    
    #### occupancy model
    # species specific random effects
    for (i in 1:(nspec)) {
      rho0[i] ~ dbeta(1, 1)
      beta[i] ~ dnorm(mubeta, taubeta)
      rho[i] ~ dnorm(murho, taurho)
    }
    
    # occupancy states
    for (j in 1:nsite) {
      for (i in 1:nspec) {
        z0[j, i] ~ dbern(rho0[i])
        logit(psi[j, i, 1]) <- beta[i] + rho[i] * z0[j, i]
        z[j, i, 1] ~ dbern(psi[j, i, 1])
        for (t in 2:nyear) {
          logit(psi[j, i, t]) <- beta[i] + rho[i] * z[j, i, t-1]
          z[j, i, t] ~ dbern(psi[j, i, t])
        }
      }
    }
    
    #### detection model
    for(i in 1:nspec){
      lp[i] ~ dnorm(mup, taup)
      p[i] <- (exp(lp[i])) / (1 + exp(lp[i]))
    }
    
    #### observation model
    for (j in 1:nsite){
      for (i in 1:nspec){
        for (t in 1:nyear){
          mu[j, i, t] <- z[j, i, t] * p[i]
          for (k in 1:nrep){
            x[j, i, t, k] ~ dbern(mu[j, i, t])
          }
        }
      }
    }
    }
    ", fill=TRUE, file="com_occ.txt")