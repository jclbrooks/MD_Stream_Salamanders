#######################################
### Multi-species Occupancy Model ####
#####################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(jagsUI)

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

# Read in data set and look at data 
sal <- read.csv("Data/Date_Location_Transect_Visit_Data_Processed.csv", stringsAsFactors = FALSE)
str(sal)  # structure of the data
dim(sal)  # dimensions of the data
# head(sal) # first couple of lines of data with header

# Rearrange data into long format
df <- sal %>%
  mutate(trans = paste0(stream, "_", transect)) %>%
  group_by(trans, stream, transect, visit) %>%
  tidyr::gather(sp_stage, count, -date, -trans, - stream, -transect, -type, -up_down, -dist, -visit, -time_min, -air, -water, -pH, -DO, -EC, -TDS, -observers) %>%
  tidyr::separate(sp_stage, into = c("species", "stage"), sep = 4) %>%
  filter(species != "tota") %>%
  mutate(type = ifelse(type == "res", up_down, type))
str(df)

# Combine across life stages?

# Convert counts to binary (detection/nondetection)
df$obs <- df$count
df[df$obs > 1 & !is.na(df$obs), "obs"] <- 1
summary(df)

# Transect-level covariates
df_trans <- df %>%
  ungroup() %>%
  group_by(trans) %>%
  select(trans, visit, species, stage, obs, pH, air, water, EC) %>%
  summarise(pH = mean(pH, na.rm = TRUE),
            air = mean(air, na.rm = TRUE),
            water = mean(water, na.rm = TRUE),
            EC = mean(EC, na.rm = TRUE)) %>%
  arrange(trans) %>%
  ungroup()

df_trans <- left_join(df_trans, distinct(df[ , c("trans", "type")]))
df_trans <- df_trans %>%
  mutate(up = ifelse(type == "UP", 1, 0),
         down = ifelse(type == "DOWN", 1, 0))

# Reorganize so data in 4D array: transect (j) x visit (k) x species (i) x stage (l)
occ <- df %>%
  ungroup() %>%
  group_by(trans, species, stage) %>%
  select(trans, visit, species, stage, obs) %>%
  mutate(visit = paste0("v", visit)) %>%
  spread(visit, obs) 
str(occ)
head(occ)

n_trans <- length(unique(occ$trans))
n_species <- length(unique(df$species)) # I
n_streams <- length(unique(sal$transect)) # J
n_visits <- length(unique(sal$visit)) # K
n_stages <- length(unique(occ$stage)) # L

# Order everything the same way!!!!!!!!!
occ <- occ %>%
  arrange(stage, species, trans)
str(occ)

# need to figure out how to do this
occ_array <- array(NA, dim = c(n_trans, n_visits, n_species, n_stages))

species_observed <- unique(df$species)
transects <- unique(df$trans)
stages <- unique(df$stage)

# foo <- occ[which(occ$species == species_observed[1] & occ$stage == stages[1]), c("v1", "v2", "v3", "v4")]

# for(j in 1:n_sites) {
  # for (k in 1:n_visits) {
    for (i in 1:n_species) {
      for (l in 1:n_stages) {
        occ_array[1:n_trans, 1:n_visits, i, l] <- as.matrix(occ[which(occ$species == species_observed[i] & occ$stage == stages[l]), c("v1", "v2", "v3", "v4")])
      }
    }
#   }
# }
str(occ_array)

# Detection Covariates (vary by transect-visit)
df$time <- ifelse(df$time_min == 0, 60, df$time_min)

time_min <- df %>%
  ungroup() %>%
  group_by(trans, visit) %>%
  select(trans, visit, time) %>%
  summarise(time = mean(time, na.rm = TRUE)) %>%
  mutate(visit = paste0("v", visit),
         time = scale(time)) %>%
  spread(visit, time, fill = 0) %>%
  ungroup()
str(time_min)
summary(time_min)

water <- df %>%
  ungroup() %>%
  group_by(trans, visit) %>%
  select(trans, visit, water) %>%
  summarise(water = mean(water, na.rm = TRUE)) %>%
  mutate(visit = paste0("v", visit),
         water = scale(water)) %>%
  spread(visit, water, fill = 0) %>%
  ungroup()
str(water)
summary(water)

# Organize covariates and standardize them
df_trans$pH_s <- scale(df_trans$pH)
df_trans$air_s <- scale(df_trans$air)
df_trans$ec_s <- scale(df_trans$EC)

# Number of observed sites by each species among the transects
obs_occ <- df %>%
  ungroup() %>%
  select(trans, visit, species, stage, obs) %>%
  group_by(trans, species) %>%
  summarise(obs = max(obs))

obs_occ <- obs_occ %>% 
  ungroup() %>%
  group_by(species) %>%
  summarise(obs_sites = sum(obs, na.rm = T))
 n_trans 
 
 obs_occ$prop_sites <- obs_occ$obs_sites / n_trans
 obs_occ
 
 # do the same for streams?

#convert 4D array to 3D to look at a naive occupancy array transect (j) x visit (k) x species (i) x stage (l)
NaiveOcc <- array(NA, dim = c(n_trans, n_species, n_stages))

for (i in 1:n_species) {
  for (l in 1:n_stages) {
    NaiveOcc[,i,l] <- apply(occ_array[,,i,l], MARGIN=1, max, na.rm = TRUE) 
  }
}
NaiveOcc

# Get observed species richness; Naive number of occurring species at each site
# obs.rich <- apply(NaiveOcc, 1, sum, na.rm = TRUE)
# obs.rich
# names(obs.rich) <- dimnames(A)[[1]]
# (sort(obs.rich))

#BUILD IN TO CHECK THERE ARE 34 SPECIES AT EACH SITE

##################
## CODE FOR JAGS ##
#################

#Bundle data (Note you may need to change nsite, nrep, nspec)
data_list = list(y = occ_array, n_trans = n_trans, n_visits = n_visits, n_species = n_species, n_stages = n_stages,
                pH = as.numeric(df_trans$pH_s), 
                air = as.numeric(df_trans$air_s),
                ec = as.numeric(df_trans$ec_s),
                water = as.matrix(select(water, v1, v2, v3, v4)),
                time_min = as.matrix(select(time_min, v1, v2, v3, v4)))

# tmp to check problems 
ec_s2 <- df_trans$ec_s
ec_s2[is.na(ec_s2)] <- 0
data_list = list(y = occ_array, n_trans = n_trans, n_visits = n_visits, n_species = n_species, n_stages = n_stages,
                pH = as.numeric(df_trans$pH_s), 
                air = as.numeric(df_trans$air_s),
                ec = as.numeric(ec_s2),
                water = as.matrix(select(water, v1, v2, v3, v4)),
                time_min = as.matrix(select(time_min, v1, v2, v3, v4)))

########################################################
# MSOM pH and time_min
sink("Code/JAGS/multi_spp_occ1.txt")
cat("
    model {
    
    #PRIORS
    
    # Set species loop
    for(i in 1:n_species) {
    
    b0[i] ~ dnorm(mu.b0, tau.b0)
    b1[i] ~ dnorm(mu.b1, tau.b1)

    a0[i] ~ dnorm(mu.a0, tau.a0)
    a1[i] ~ dnorm(mu.a1, tau.a1)
    }
    
    #HYPERPRIORS
    mu.b0 ~ dnorm(0, 0.01)    
    tau.b0 ~ dgamma(0.1, 0.1)
    
    mu.b1 ~ dnorm(0, 0.01)
    tau.b1 ~ dgamma(0.1, 0.1)
    
    mu.a0 ~ dnorm(0, 0.01)
    tau.a0 ~ dgamma(0.1, 0.1)
    
    mu.a1 ~ dnorm(0, 0.01)
    tau.a1 ~ dgamma(0.1, 0.1)
    
# array transect (j) x visit (k) x species (i) x stage (l)
    # Estimation of Z matrix (true occurrence for species i at site j)	
    for(i in 1:n_species) {	
      for(j in 1:n_trans) { 
        for(l in 1:n_stages) {
    logit(psi[j,i,l]) <- b0[i] + b1[i]*pH[j]

    Z[j,i,l] ~ dbern(psi[j,i,l])
    
    # Estimate detection for species i and stage k at transect j during visit k
    for (k in 1:n_visits) {
    logit(p[j,k,i,l]) <- a0[i] + a1[i] * time_min[j, k]
    
    
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


########################################################
# MSOM - beta1: ec , alpha1: water
sink("Code/JAGS/multi_spp_occ2.txt")
cat("
    model {
    
    #PRIORS
    
    # Set species loop
    for(i in 1:n_species) {
    
    b0[i] ~ dnorm(mu.b0, tau.b0)
    b1[i] ~ dnorm(mu.b1, tau.b1)

    a0[i] ~ dnorm(mu.a0, tau.a0)
    a1[i] ~ dnorm(mu.a1, tau.a1)
    }
    
    #HYPERPRIORS
    mu.b0 ~ dnorm(0, 0.01)    
    tau.b0 ~ dgamma(0.1, 0.1)
    
    mu.b1 ~ dnorm(0, 0.01)
    tau.b1 ~ dgamma(0.1, 0.1)
    
    mu.a0 ~ dnorm(0, 0.01)
    tau.a0 ~ dgamma(0.1, 0.1)
    
    mu.a1 ~ dnorm(0, 0.01)
    tau.a1 ~ dgamma(0.1, 0.1)
    
# array transect (j) x visit (k) x species (i) x stage (l)
    # Estimation of Z matrix (true occurrence for species i at site j)	
    for(i in 1:n_species) {	
      for(j in 1:n_trans) { 
        for(l in 1:n_stages) {
    logit(psi[j,i,l]) <- b0[i] + b1[i]*ec[j]

    Z[j,i,l] ~ dbern(psi[j,i,l])
    
    # Estimate detection for species i and stage k at transect j during visit k
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

########################################################
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
     sigma.p ~ dunif(0, 5)
    # sigma.p ~ dt(0, pow(1, -2), 1)T(0, ) # half cauchy with sd = 1 # informative prior pulling things close to the center
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
    
          # Estimate detection for species i and stage k at transect j during visit k
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











##########################################################
#INITIAL VALUES
# zst<-apply(A, c(1,3), max) #observed occurrence as starting values for Z
# zst[is.na(zst)]<-1

zst <- NaiveOcc

inits<-function() 
  list(Z = zst)


#Parameters monitored
params <-c('p', 'Z', 'psi', 'a0','a1', 'b0','b1', "mu.b0", "tau.b0", "mu.a0", "tau.a0", "mu.b1", "tau.b1", "mu.a1", "tau.a1", "occ_sp", "occ_trans", "p_mean", "rho", "sigma.psi", "sigma.p")


# MCMC settings
saved.per.chain <- 1000
nc <- 3         #num. chains (this is a standard number of chains)
nb <- 500    #burn-in; draws from Markov chain that are discarded
nt <- 1        #thin chains to save diskspace / reduce autocorrelation among repeated draws
ni <- saved.per.chain*nt + nb  #iterations (draws from posterior dist.)

###################################################################
#Call JAGS from R using rjags and setup to run on multiple cores

library(jagsUI)

out1 <- jags(data = data_list, inits = inits, params, model.file = "Code/JAGS/multi_spp_occ3.txt", 
            n.chains = nc,
            n.burnin = nb,
            n.iter = ni,
            parallel = TRUE,
            bugs.format = TRUE)

out <- out1

summary(out, parameters = c("mu.b0", "tau.b0", "mu.b1", "tau.b1", "mu.a0", "tau.a0", "mu.a1", "tau.a1"))

traceplot(out, parameters = c("rho", "mu.b0", "mu.a0", "sigma.psi", "sigma.p", "a0", "b0"))

traceplot(out, parameters = c("mu.b0", "tau.b0", "mu.a0", "tau.a0"))
whiskerplot(out, parameters = c("rho", "mu.b0", "sigma.psi", "mu.b1", "tau.b1", "mu.a0", "sigma.p", "mu.a1", "tau.a1"))
whiskerplot(out, parameters = c("p_mean"))
whiskerplot(out, parameters = c("b1")) # effects of pH by species
whiskerplot(out, parameters = c("occ_sp")) # number of transects occupied by each species
whiskerplot(out, parameters = c("occ_trans")) # number of species per transect

post <- data.frame(group = "posterior", mu.b0 = out$sims.list$mu.b0, tau.b0 = out$sims.list$tau.b0, stringsAsFactors = FALSE)

prior <- data.frame(group = "prior", mu.b0 = rnorm(nrow(post), 0, sqrt(1/0.001)), tau.b0 = rgamma(nrow(post), 0.1, 0.1), stringsAsFactors = FALSE)

library(dplyr)
prior_post <- bind_rows(prior, post)

# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

scatterPlot <- ggplot(prior_post, aes(mu.b0, tau.b0, color = group)) + 
  geom_point(alpha = 0.3) + 
  # scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(prior_post, aes(mu.b0, fill=group)) + 
  geom_density(alpha=.5) + 
  # scale_fill_manual(values = c('#999999','#E69F00')) + 
  coord_cartesian(xlim = c(-25, 25)) +
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(prior_post, aes(tau.b0, fill=group)) + 
  geom_density(alpha=.5) + 
  # scale_fill_manual(values = c('#999999','#E69F00')) + 
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.5))
ydensity
ydensity <- ydensity + theme(legend.position = "none")

library("gridExtra")
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#------- plot effect of pH ------
post <- data.frame(group = "posterior", mu.b1 = out$sims.list$mu.b1, tau.b1 = out$sims.list$tau.b1, stringsAsFactors = FALSE)

prior <- data.frame(group = "prior", mu.b1 = rnorm(nrow(post), 0, sqrt(1/0.001)), tau.b1 = rgamma(nrow(post), 0.1, 0.1), stringsAsFactors = FALSE)

prior_post <- bind_rows(prior, post)

scatterPlot <- ggplot(prior_post, aes(mu.b1, tau.b1, color = group)) + 
  geom_point(alpha = 0.3) + 
  # scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(prior_post, aes(mu.b1, fill=group)) + 
  geom_density(alpha=.5) + 
  # scale_fill_manual(values = c('#999999','#E69F00')) + 
  coord_cartesian(xlim = c(-25, 25)) +
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(prior_post, aes(tau.b1, fill=group)) + 
  geom_density(alpha=.5) + 
  # scale_fill_manual(values = c('#999999','#E69F00')) + 
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.5))
ydensity
ydensity <- ydensity + theme(legend.position = "none")

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))


#?jags.model
#library(parallel)
#cl <- makeCluster(3)                       # Request 3 cores
#clusterExport(cl, c("win.data", "inits", "params")) # Make these available
#clusterSetRNGStream(cl = cl, 6572)


#system.time({ # no status bar (% complete) when run in parallel
#  out <- clusterEvalQ(cl, {
#   library(rjags)
#  jm <- jags.model("modelRubyToiyabeMultiSP.txt", win.data, inits, n.adapt="nb", n.chains="nc") # Compile model and run burnin; run burnin = adapt
# ps <- coda.samples(jm, params, n.iter="ni", thin="nt") # Sample from posterior distribution 
#  return(as.mcmc(ps))
#})
#}) 

#?jags.model
#?ClusterExport
#?coda.samples


# Compile model - not run in parallel
#jm.logit <- jags.model("modelRubyToiyabeMultiSP.txt", win.data, inits = inits, n.chains=nc, n.adapt=300000)


#################################################################
#Alternative option to Call JAGS from R using R2JAGS
# start.time<-Sys.time()
# outj<-jags(data=win.data, inits=inits, parameters.to.save=params, model.file="modelRubyToiyabeMultiSP.txt",
#            n.chains=nc, n.iter=ni, n.burnin=nb, n.thin=nt, DIC=FALSE)
# end.time=Sys.time()
# elapsed.time = difftime(end.time, start.time, units='mins')
# elapsed.time





########## LOOKING AT THE OUTPUT ###########
#Has it converged? Rhat should be near 1
#Shows MCMC plots; lines should converge to be fat fuzzy caterpillars
traceplot(outj)



# Results
#ps.list <- mcmc.list(out)
#plot(ps.list)
##summary(ps.list)

#stopCluster(cl)

# Posterior samples when not run in parallel
#ps.logit <- coda.samples(jm.logit, params, n.iter=3000, thin = 3)





#Lookint at results when used R2JAGS
#***Warning: namespace 'rjags' is not available and has been replaced by .GlobalEnv when processing object 'outj'
print(outj, dig=3)
str(outj)
print(outj$BUGSoutput$summary, dig=3)
which(outj$BUGSoutput$summary[,8]>1.1)  #pulls out any Rhat's >1.1 (Rhat should be near 1 for convergence)


# Mean of all the iterations Z=occurrence matrix
print(outj$BUGSoutput$mean$Z, dig=3)    

#Number of occupied sites by this species among the 71
print(outj$BUGSoutput$mean$occ.sp, dig=3)  

#Number of occurring species at each site
print(outj$BUGSoutput$mean$occ.site, dig=3) 



#Shortcuts for calling out data from outj
MeanZ <- outj$BUGSoutput$mean$Z
MeanPsi <- outj$BUGSoutput$mean$psi
P <- outj$BUGSoutput$mean$p
P
#Save output as .csv

Table3 <- t(MeanZ)
colnames(Table3) <- site.list
rownames(Table3) <- species.list
Table3

write.table(Table3, file="C:/R/GreatBasin/ruby_toiyabes/RubyToiyabeMeanZ_10jun14.csv", 
            row.names=species.list, col.names=site.list, sep=",")


#############################################
#Does our naive occupancy data from Matrix A make sense with
# "true" occupancy from Matrix Z?

NaiveOcc  #See how cacluated above

#Make a transposed table to compare NaiveOcc and MeanZ
#rows=species, column1=NaiveOcc Sites, column2=MeanZ Sites
Table1 <- (cbind(dimnames(A)[[3]], t(NaiveOcc), t(MeanZ)))
colnames(Table1) <- c("Species", site.list, site.list)
Table1

#Comparison table of NaiveOcc and Z Matrix
write.table(Table1, 'comparison_10jun14.csv', sep = ",")


## Differences between NaiveOcc and MeanZ ##
round(MeanZ - NaiveOcc, digits=1)  

#Histograms of MeanZ and NaiveOcc
par(mfrow = c(1, 2))
hist(MeanZ, freq = FALSE, probability=TRUE)
hist(NaiveOcc, freq = FALSE)

sum(NaiveOcc > 0)/(72*34) #Overall NaiveOcc (for all sp. all sites)
sum(MeanZ > 0.1)/(72*34)  #Overall Z (for all sp. all sites) #Where did the 0.1 come from???


############################################
#Table of Mean psi
#MeanPsi are an average of conditions specified in the model for a site for a species
#Make a transposed table with rows=species, columns=MeanPsi
Table2 <- t(MeanPsi)
colnames(Table2) <- site.list
rownames(Table2) <- species.list
Table2
range(Table2)  #Range of Mean psi


#Histogram of Mean psi
hist(MeanPsi)
quantile(MeanPsi, c(0.25, 0.5, 0.975))



############################################
##Now let's look at detection
## We can't simply sum across the reps because they are probabilities
## detection probability = 1-(1-p)^t prob. of detct >=1X in t surveys 
## (above eq. is assuming have same p for each t...which we don't)


Prob <- matrix(NA, 34, 71) #Creates a new matrix
for(j in 1:71){
  for(i in 1:34){
    Prob[i, j] <- 1-((1 - P[j,1,i])*(1 - P[j,2,i])*(1 - P[j,3,i])*(1 - P[j,4,i])*(1 - P[j,5,i]))
  }
}
Prob  #These are the detection probabilities at each site

range(Prob) #Range of detection


C:/R/GreatBasin/ruby_toiyabes


###########################################
## Effect of covariates on occupancy

plot(outj$BUGSoutput$mean$b1, 1:34, xlim=c(-8,8), xlab = "Effect size", ylab="Species number", main="Effect of Elevation on Occupancy")
abline(v=0, lwd=1, col="black")
segments(outj$BUGSoutput$sd$b1, 1:34)
outj$BUGSoutput$sd$b1,7, 1:34)

, col="grey", lwd=1)
sig4<-outj$BUGSoutput$sd$b1,3, <0) &
  outj$BUGSoutput$sd$b1,7, <0) + outj$BUGSoutput$sd$b1,3, >0) & 
  outj$BUGSoutput$sd$b1,7, >0)
segments(outj$BUGSoutput$sd$b1,3, [sig4==1]), (1:34)[sig4==1],
outj$BUGSoutput$sd$b1,7, [sig4==1]), (1:34)[sig4==1], col="blue", lwd=2)
































#####################################
#RANDOM CODE


plot(ps.logit, ask=TRUE)
par(mfrow = c(1,1))
plot(ps.logit[["K"]], ask=TRUE)
ps.logit["G[1,1]"]


ps.list[1:10, "K"]
plot(ps.list[,c("K", "p0", "p.can", "b", "n0")]) # traceplots of just certain variables
par(mfrow = c(1,1))

summary(ps.logit)

ps.logit.mat2 <- as.matrix(ps.logit)
logitN2 <- ps.logit.mat2[,grep("Ntot", colnames(ps.logit.mat2))]
plot(colMeans(logitN2))


# Plot posterior summaries of pop size in each year
ps.dep.mat <- as.matrix(ps.dep)
ps.Ntot <- ps.dep.mat[,grep("Ntot", colnames(ps.dep.mat))]
head(ps.Ntot)

plot(1:(nYears+extraYears), colMeans(ps.Ntot), type="b", ylim=c(0, 2000),
     xlab="Year", ylab="Population size", pch=16, col="blue")
points(1:nYears, colSums(N))
arrows(1:(nYears), apply(ps.Ntot, 2, quantile, prob=0.025),
       1:(nYears), apply(ps.Ntot, 2, quantile, prob=0.975),
       angle=90, code=3, length=0.03)
polygon(c(20.5, 35, 35, 20.5, 20.5), c(-100, -100, 3000, 3000, -100),
        col=rgb(0,0,0,0.2), border=FALSE)
legend(5, 2000, c("Actual", "Posterior mean and CI"), pch=c(1,16),
       col=c("black", "blue"))


# MCMC diagnostics
gelman.diag(ps.logit)    # Should be ~ <1.1 (run chains longer if need be) - not sure if this works for list from multipe cores
autocorr.plot(ps.logit)  # If samples are autocorrelated, you need more iters and/or more thinning
crosscorr(ps.logit)      # Correlation among parameters

