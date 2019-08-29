#----- Format Landscape Characteristics Data ----

huc02_landscape <- read_csv("landscape_characteristics/HUC02_sites_landscape_characteristics.csv")
# National Capitals Region, Shenandoah, and W MD reference streams

huc05_landscape <- read_csv("landscape_characteristics/HUC05_sites_landscape_characteristics.csv")
# Canaan Valley NWR and W MD restored streams

landscape_characteristics <- bind_rows(huc02_landscape, huc05_landscape)

landscape_characteristics <- landscape_characteristics %>%
  mutate(region = ifelse(region == "C & O Canal National Historic Park", "Capital", region),
         region = ifelse(region == "Rock Creek National Park", "Capital", region),
         region = ifelse(region == "Prince William Forest National Park", "Capital", region),
         region = ifelse(region == "Shenandoah National Park", "Shenandoah", region),
         region = ifelse(region == "Canaan Valley National Wildlife Refuge", "Canaan", region),
         region = ifelse(region == "Savage River State Park", "Savage", region),
         transect = ifelse(region == "Canaan", gsub(pattern = "*_.*_", replacement = "", x = transect), transect)#,
         #transect = ifelse(region == "Canaan", substr(transect, 1, nchar(transect) - 7), transect)
  )

# changes colname "transect" to "stream" to match landscape_occ

#-----Pick variables to potentially use ----
vars <- c("agriculture", "elevation", "forest", "impervious", "AreaSqKM")

landscape_vars <- landscape_characteristics %>%
  filter(zone == "local",
         variable %in% vars)%>%
  # mutate(loc = paste0(.$region, "_", .$transect)) %>%
  select(region, transect, variable, value) %>%
  distinct() %>%
  # pivot_wider(names_from = variable, values_from = value, values_fill = NA)
  spread(key = variable, value = value)

# modeldata <- landscape_occ %>%
#   left_join(landscape_vars)

modeldata <- landscape_vars %>%
  right_join(landscape_occ)

str(modeldata)
summary(modeldata)

unique(landscape_occ$transect) %in% unique(landscape_vars$transect)

unique(landscape_occ$transect)[!(unique(landscape_occ$transect) %in% unique(landscape_vars$transect))] # names of transects in the occupancy that are not in landscape variables

unique(landscape_occ[,1:2])
unique(landscape_characteristics[,1:2])
# length(unique(paste(landscape_characteristics[,1], landscape_characteristics[,2])))
# unique_char <- unique(landscape_characteristics[,1:2])
# unique(landscape_occ$transect)
# unique(landscape_characteristics$transect)



########################################
########### CODE FOR JAGS ##############
########################################

n_trans <- length(unique(landscape_occ$transect))
n_species <- length(unique(landscape_occ$species)) # I
n_streams <- length(unique(landscape_occ$transect)) # J
n_visits <- length(unique(sal$visit)) # K
n_stages <- length(unique(occ$stage)) # L

########### temporary hack ##########3
n_visits <- 5
n_stages <- 2 
###########################

# y[j,k,i,l]

# Make 4-D observation array
# Order everything the same way!!!!!!!!!
modeldata <- modeldata %>%
  arrange(age, species, transect)
str(modeldata)

occ_array <- array(NA, dim = c(n_trans, n_visits, n_species, n_stages))

species_observed <- unique(modeldata$species)
transects <- unique(modeldata$transect)
stages <- unique(modeldata$age)

# foo <- occ[which(occ$species == species_observed[1] & occ$stage == stages[1]), c("v1", "v2", "v3", "v4")]

# for(j in 1:n_sites) {
# for (k in 1:n_visits) {
for (i in 1:n_species) {
  for (l in 1:n_stages) {
    occ_array[1:n_trans, 1:n_visits, i, l] <- as.matrix(modeldata[which(modeldata$species == species_observed[i] & modeldata$age == stages[l]), c("pass1", "pass2", "pass3", "pass4", "pass5")])
  }
}
#   }
# }
str(occ_array)

data_list = list(y = occ_array, n_trans = n_trans, n_visits = n_visits, n_species = n_species, n_stages = n_stages,
                 forest = (modeldata$forest - mean(modeldata$forest, na.rm = TRUE)) / sd(modeldata$forest, na.rm = TRUE), 
                 area = as.numeric(scale(modeldata$AreaSqKM)),
                 ec = as.numeric(df_trans$ec_s),
                 water = as.matrix(select(water, v1, v2, v3, v4)),
                 time_min = as.matrix(select(time_min, v1, v2, v3, v4)))


# MSOM - beta1: ec , alpha1: water - add covariance with occupancy and detection
sink("Code/JAGS/multi_spp_occ3.txt")
cat("
    model {
    
    #PRIORS
    
    # Set species loop
    for(i in 1:n_species) {
    
    b0[i] ~ dnorm(mu.b0, tau.b0) 
    b1[i] ~ dnorm(mu.b1, tau.b1)

    a0.exp[i] <- mu.a0 + (rho * sigma.p / sigma.psi) * (b0[i] - mu.b0)    
    a0[i] ~ dnorm(a0.exp[i], var.p) # expected mean detection for each species over mean covariate conditions
    a1[i] ~ dnorm(mu.a1, tau.a1)
    }
    
    #HYPERPRIORS
    mu.b0 ~ dnorm(0, 0.01)
    # sigma.psi ~ dt(0, pow(3, -2), 1)T(0, ) # half cauchy with sd = 3
    sigma.psi ~ dunif(0, 5)
    tau.b0 <- 1 / sigma.psi / sigma.psi
    # tau.b0 ~ dgamma(0.1, 0.1)
    # psi.mean ~ dunif(0, 1) # real prob scale mean occupancy among species
    # mu.b0 <- log(psi.mean / (1 - psi.mean)) # mean occupancy among species on logit scale
    
    mu.b1 ~ dnorm(0, 0.01)
    tau.b1 ~ dgamma(0.1, 0.1)
    
    mu.a0 ~ dnorm(0, 0.01)
    sigma.p ~ dunif(0, 10)
    # sigma.p ~ dt(0, pow(5, -2), 1)T(0, ) # half cauchy with sd = 1 # informative prior pulling things close to the center
    tau.a0 <- 1 / sigma.p / sigma.p
    # tau.a0 ~ dgamma(0.1, 0.1)
    var.p <- tau.b0 / (1.-pow(rho, 2))

    rho ~ dunif(-1, 1) # prior correlation between occupancy and detection
    
    mu.a1 ~ dnorm(0, 0.01)
    tau.a1 ~ dgamma(0.1, 0.1)
    
    # array transect (j) x visit (k) x species (i) x stage (l)
    # Estimation of Z matrix (true occurrence for species i at site j)	
    for(i in 1:n_species) {	
      for(j in 1:n_trans) { 
        for(l in 1:n_stages) {
          logit(psi[j,i,l]) <- b0[i] + b1[i]*ec[j]
    
          Z[j,i,l] ~ dbern(psi[j,i,l])
    
          # Estimate detection for species i and stage l at transect j during visit k
          for (k in 1:n_visits) {
            logit(p[j,k,i,l]) <- a0[i] + a1[i] * water[j, k]
            mu.p[j,k,i,l] <- p[j,k,i,l] * Z[j,i,l]
            y[j,k,i,l] ~ dbern(mu.p[j,k,i,l])
    
          }
        }
      }
    }
    
    ###### Derived quantities #######
    
    
    for(i in 1:n_species) {
    for(j in 1:n_trans) {
    occ_spp_stage[j, i] <- max(Z[ j, i, ])
    }
    occ_sp[i] <- sum(occ_spp_stage[ , i])        # Number of occupied transects by this species
    }
    
    for(j in 1:n_trans) {
    for(i in 1:n_species) {
    occ_trans_sp[j, i] <- max(Z[j, i, ])
    }
    occ_trans[j] <- sum(occ_trans_sp[j, ])      # Number of occurring species at each transect
    }
    
    # mean detection by species and stage
    for(i in 1:n_species) {
    for(l in 1:n_stages) {
    p_sp[i, l] <- sum(p[ , , i, l])
    }
    }
    
    # overall mean detection
    p_mean <- mean(p)
    
    
    }
    ", fill=TRUE)
sink()


#Parameters monitored
params <-c('p', 'Z', 'psi', 'a0','a1', 'b0','b1', "mu.b0", "tau.b0", "mu.a0", "tau.a0", "mu.b1", "tau.b1", "mu.a1", "tau.a1", "occ_sp", "occ_trans", "p_mean", "rho", "sigma.psi", "sigma.p")


# MCMC settings
testing <- TRUE
if(testing) {
  saved.per.chain <- 1000
  nc <- 3         #num. chains (this is a standard number of chains)
  nb <- 500    #burn-in; draws from Markov chain that are discarded
  nt <- 1        #thin chains to save diskspace / reduce autocorrelation among repeated draws
  ni <- saved.per.chain*nt + nb  #iterations (draws from posterior dist.)
} else {
  saved.per.chain <- 2500 # might have to make larger
  nc <- 4         #num. chains (this is a standard number of chains)
  nb <- 50000    #burn-in; draws from Markov chain that are discarded
  nt <- 4        #thin chains to save diskspace / reduce autocorrelation among repeated draws (might have to make 40)
  ni <- saved.per.chain*nt + nb  #iterations (draws from posterior dist.)
}



###################################################################
#Call JAGS from R using rjags and setup to run on multiple cores

library(jagsUI)

# beta1: pH, alpha1: time_min
out1 <- jags(data = data_list, inits = inits, params, model.file = "Code/JAGS/multi_spp_occ2.txt", 
             n.chains = nc,
             n.burnin = nb,
             n.iter = ni,
             parallel = TRUE,
             bugs.format = TRUE)



#out <- out1

summary(out1, parameters = c("mu.b0", "tau.b0", "mu.b1", "tau.b1", "mu.a0", "tau.a0", "mu.a1", "tau.a1"))

traceplot(out4, parameters = c("rho",
                               "mu.b0",
                               "mu.a0",
                               "sigma.psi",
                               "sigma.p",
                               "a0"
                               ,"b0"
))

traceplot(out1, parameters = c("mu.b0", "tau.b0", "mu.a0", "tau.a0", "a0", "b0"))
traceplot(out4, parameters = c("mu.b0", "tau.b0", "mu.a0", "tau.a0", "a0", "b0"))

