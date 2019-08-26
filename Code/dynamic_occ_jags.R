sink("Code/JAGS/single_spp_occ.txt")
cat("
    model {
    
    for(i in 1:n_sites) {
      for(j in 1:n_visits) {
    
    # first year (shoot, need first year at each site!?!)
    
    y[i,j,1] ~ dbern(mu_p[i,j,1]) # likelihood
    
    mu_p[i,j,1] <- p[i,j,1] * Z[i,1] # prob obs given occ
    
    Z[i,1] ~ dbern(psi[i,1]) # occ
    
    logit(psi[i,1]) <- b0 + b1 * pH[i,1] # prob occ
    
    logit(p[i,j,1]) <- a0 + a1 * time_min[j, 1] # prob obs 
    
    
    
for(year t in 2:n_years) {
    
    y[i,j,t] ~ dbern(mu_p[i,j,t]) # likelihood
    
    mu_p[i,j,t] <- p[i,j,t] * Z[i,t] # prob obs given occ
    
    Z[i,t] ~ dbern(psi[i,t]) # occ MAKE AUTOREGRESSIVE on Z or psi?
    
    logit(psi[i,t]) <- b0 + b1 * pH[i,t] # prob occ
    
    logit(p[i,j,t]) <- a0 + a1 * time_min[j, k] # prob obs 
}
      }
    }

    
    }
    ", fill=TRUE)
sink()
