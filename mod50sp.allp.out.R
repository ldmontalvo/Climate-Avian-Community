
#  Set directory
setwd("C:/Users/Owner/Desktop/Dropbox/Thesis/Count_Analysis/data_carpentry")


load("model2_gradt.image.RData")

#clear memory and set limit
gc()
memory.limit()
memory.limit(size=640000)


###############################################################
### DATA MANAGEMENT

# BIRD COUNTS

# Reading data for bird count
count.50sp.allp <- read.csv("birdcount.sp50.csv", header=TRUE, sep=",")

# Ordering by species and point
count.50sp.allp.2 <- count.50sp.allp[order(count.50sp.allp$idspp, count.50sp.allp$idpoint),]
dim(count.50sp.allp.2)

#install.packages("reshape2")
library(reshape2)

# Getting the wide format with surveys as columns
count.wide50sp.allp <- dcast(count.50sp.allp.2,  idspp + idpoint ~ survey, mean, value.var = "count")

# Getting just the numbers of the count
count.wide50sp.allp.2 <- count.wide50sp.allp[,3:8]

# Changing the counts for presence absence data
spp.50sp.allp <- as.matrix(ifelse(count.wide50sp.allp.2 > 0, 1, 0))

# Getting the numbers of sites, replicates and species
# Now we will get the six samples separated
n.point50sp.allp <- length(unique(count.wide50sp.allp$idpoint)) # Number of site
n.rep50sp.allp <- (rowSums(!is.na(count.wide50sp.allp [,c(3:8)])))[1:n.point50sp.allp]  # Number of replicates (six days of survey per site)
n.spec50sp.allp <- length(unique(count.wide50sp.allp$idspp))   # Number of species
spec.list50sp.allp <- unique(count.wide50sp.allp $idspp)       # List of species
max.rep50sp.allp <- 6

# Creating the array with the two samples
spp.grad50sp.allp <- array(NA, dim = c(n.point50sp.allp, max.rep50sp.allp, n.spec50sp.allp))
dim(spp.grad50sp.allp)

for(i in 1:n.spec50sp.allp){
  spp.grad50sp.allp[,,i] <- spp.50sp.allp[((i-1)*n.point50sp.allp+1):(i*n.point50sp.allp),]
}



# COVARIATES

## Covariates for observation process

# Getting the dates per point and surveys
date.grad50sp.allp <- dcast(count.50sp.allp.2,  idpoint ~ survey, mean, value.var = "date")

# Getting the time per point and surveys
time.grad50sp.allp <- dcast(count.50sp.allp.2,  idpoint ~ survey, mean, value.var = "time")

## Scaling the variable for date, we use the sd and mean of all the data and not just per column
date.grad50sp.allp[,2] <- scale(date.grad50sp.allp[,2])
date.grad50sp.allp[,3] <- scale(date.grad50sp.allp[,3])
date.grad50sp.allp[,4] <- scale(date.grad50sp.allp[,4])
date.grad50sp.allp[,5] <- scale(date.grad50sp.allp[,5])
date.grad50sp.allp[,6] <- scale(date.grad50sp.allp[,6])
date.grad50sp.allp[,7] <- scale(date.grad50sp.allp[,7])
date50sp.allp <- date.grad50sp.allp[,-1]
date50sp.allp[is.na(date50sp.allp)] <- NA


## Scaling the variable for time
time.grad50sp.allp[,2] <- scale(time.grad50sp.allp[,2])
time.grad50sp.allp[,3] <- scale(time.grad50sp.allp[,3])
time.grad50sp.allp[,4] <- scale(time.grad50sp.allp[,4])
time.grad50sp.allp[,5] <- scale(time.grad50sp.allp[,5])
time.grad50sp.allp[,6] <- scale(time.grad50sp.allp[,6])
time.grad50sp.allp[,7] <- scale(time.grad50sp.allp[,7])
time50sp.allp <- time.grad50sp.allp[,-1]
time50sp.allp[is.na(time50sp.allp)] <- NA


## Covariates for Ecological process

# Reading data for covariates for ecological process
covar.50sp.allp <- read.csv("climveg.csv", header=TRUE, sep=",")
dim(covar.50sp.allp)

# Getting the idpoint
covar.50sp.allp$idpoint <- count.50sp.allp.2$idpoint[match(covar.50sp.allp$Point, count.50sp.allp.2$Point)]

# Getting the covariates order by point
covar.50sp.allp <- covar.50sp.allp[order(covar.50sp.allp$idpoint),]

# Getting the climate variables and scale them
clim.allp <- covar.50sp.allp$AMP
clim.allp <- (clim.allp - mean(clim.allp))/ sd(clim.allp)

# Getting the veg1 variables and scale them
veg.allp <- covar.50sp.allp$veg1
veg.allp <- (veg.allp - mean(veg.allp))/ sd(veg.allp)

###############################################################
### The Model
{
  

sink("model_grad2v2.txt")
cat("
model {

## PRIOR

# We estimate intercept for detection for every species
for (k in 1:nspec){
  alpha0[k] ~ dnorm(mu.alpha0, tau.alpha0)
  alpha.dat[k] ~ dnorm(mu.alpha.dat, tau.alpha.dat)
  alpha.time[k] ~ dnorm(mu.alpha.time, tau.alpha.time)
  beta0[k] ~ dnorm(mu.beta0, tau.beta0) # mean intercept for species
  beta.veg[k] ~ dnorm(mu.beta.veg, tau.beta.veg) #beta for veg1 doesn't have random effect
}
    
    
# Random effects for CLIMATE
for (k in 1:nspec){
  for (i in 1:nsite){
    beta.for[i,k] ~ dnorm(mu.beta.for[i,k], tau.beta.for)
  
    mu.beta.for[i,k] <- beta0.site[k] + beta.site[k]*clim[i]
  # intercept vary per species
  }
  # Hyperprior for climate lineal model
    beta0.site[k] ~ dnorm(0, tau0.site) 
    beta.site[k] ~ dnorm(0, tau.site)
}
    
tau.beta.for <- pow(sd.beta.for, -2)
sd.beta.for ~ dunif(0, 10)
    
    
## HYPERPRIORS
    
# alpha for detectability
    
mu.alpha0 ~ dnorm(0, 0.6) #intercept for psi
tau.alpha0 <- pow(sd.alpha0, -2) 
sd.alpha0 ~ dunif(0, 10)
    
mu.alpha.dat ~ dnorm(0, 0.6)
tau.alpha.dat <- pow(sd.alpha.dat, -2) 
sd.alpha.dat ~ dunif(0, 10)
    
mu.alpha.time ~ dnorm(0, 0.6)
tau.alpha.time <- pow(sd.alpha.time, -2) 
sd.alpha.time ~ dunif(0, 10)
    
    
# betas for occupancy
  
mu.beta0 ~ dnorm(0, 0.6)
tau.beta0 <- pow(sd.beta0, -2)
sd.beta0 ~ dunif(0, 10)
    
mu.beta.veg ~ dnorm(0, 0.6)
tau.beta.veg <- pow(sd.beta.veg, -2)
sd.beta.veg ~ dunif(0, 10)
    
    
# Hyperprior for multilevel efffect of climate 
    
tau0.site <- pow(sd0.site, -2)
sd0.site ~ dunif(0, 10)
    
tau.site <- pow(sd.site, -2)
sd.site ~ dunif(0, 10)
    
    
### The Likelihood:  
    
  # Logistic model for occupancy (process model)
    
for(k in 1:nspec){
  for (i in 1:npoint){
    z[i,k] ~ dbern(psi[i,k]) # True presence/absence
    logit(psi[i,k]) <- beta0[k] + beta.for[site[i],k] + beta.veg[k]*veg1[i]
  }#i
}#k
    
  # Observation model for detection/nondetection data
    
for(k in 1:nspec){
 for (i in 1:npoint){
  for (j in 1:nrep[i]){
   y[i,j,k] ~ dbern(mu.p[i,j,k]) # Observed detection/nondetection 
   mu.p[i,j,k] <- p[i,j,k]*z[i,k]
   logit(p[i,j,k]) <- alpha0[k] + alpha.dat[k]*date1[i,j] + alpha.time[k]*time1[i,j]
  }
 }
}
  
  # Derived parameters:
    
  # This matrix will be the richness estimated from the model
    
for (i in 1:npoint){
  richness[i] <- sum(z[i,]) #species richness at each site i
}#i
    
    
}

", fill=TRUE)
sink()


}


###############################################################
### RUNNING THE MODEL WITH SIMULATED DATA

# Getting the data in order
data.mod.grad50 <- list(
  npoint = n.point50sp.allp,
  nrep = n.rep50sp.allp,
  nspec = n.spec50sp.allp,
  site = as.numeric(factor(covar.50sp.allp$site)),
  nsite = length(unique(covar.50sp.allp$site)),
  clim = clim.allp,
  veg1 = veg.allp,
  date1 = date50sp.allp,
  time1 = time50sp.allp,
  y = spp.grad50sp.allp)

# Initial values for the model
# Use max of each row as starting value for z (0 / 1)
# Get the maximum values per row (every site) and per species
# In a matrix of dimensions nsites X nspec
z.init.grad50 <- apply(spp.grad50sp.allp, c(1,3), max, na.rm = T)

# Jags requires to have all the initial values in a list. In this case, we just have on element
mod50sp.allp.v2.inits <- function(){list(z=z.init.grad50)}

# Parameters to be estimated
mod50sp.allp.v2.par <- c(
  "richness",     # Estimated site richness
  "alpha0",       # Intercept of p for every species
  "alpha.dat",
  "alpha.time",
  "beta0",        # Intercept for psi for every species
  "beta.for",
  "beta0.site",
  "beta.site",
  "beta.veg",
  "p",           # Detection probability per species, site and replicate
  "psi"
  )

# Install the package
# install.packages("R2jags")
library(R2jags)

# Settign some parameters for the model
nchain <- 3 # Number of chains it will run simultaneously
nthin <- 5 # only a fraction of the MCMC iterations are used in inference because autocorrelation
nadapt <- 900
burniter <- 900 # Number of interactions that will be used to stabilized the chains. Won't be included in final estimations. The point of burn-in is to move away from the arbitrary starting values, and that also happens during adaptation.
niter <- 2500 # total number of interactions that will be used. If I use 2500, it means I will use 1500, subtracting the burning interactions (update.iter <- 1000).

# Finally, we run the model
mod50sp.allp.v2 <- jags("model_grad2v2.txt", data = data.mod.grad50, inits = mod50sp.allp.v2.inits, 
                        n.chains = nchain, parameters.to.save = mod50sp.allp.v2.par, 
                        n.burnin = burniter, n.iter = niter, n.thin = nthin)


###############################################################

## i ran this model with all the point in hipergator
## Load the results from hypergator
load("mod50sp.allp.v4.RData")


###############################################################


### Checking Results with Simulated Data
mod50sp.allp.v2
str(mod50sp.allp.v2)
plogis(mod50sp.allp.v2$BUGSoutput$mean$beta0) # mean psi per species (50 spp)

# Posterior distribution for parameters in 'sims.list'
dim(sim1.grad$BUGSoutput$sims.list$p)

# Getting the mean p of the 1000 simulations we got from the MCMC
p.sim50.grad <- t(apply(mod50sp.allp.v2$BUGSoutput$sims.list$p, c(3,4), mean, na.rm = TRUE)) #mean effect for each species
dim(p.sim50.grad)

# Getting the detections probabilities per species
p.spec.grad50 <- data.frame(species = spec.list50sp.allp, p = p.sim50.grad)
head(p.spec.grad50)


###############################################################
###  CHECKING SOME REAL DATA COVARIATES

# Original variables for date
date.ori50sp.allp <- dcast(count.50sp.allp.2,  idpoint ~ survey, mean, value.var = "date")
date.ori50sp.allp <- date.ori50sp.allp[,-1]

# Original variables for time
time.ori50sp.allp <- dcast(count.50sp.allp.2,  idpoint ~ survey, mean, value.var = "time")
time.ori50sp.allp <- time.ori50sp.allp[,-1]


# Visualize covariate mean relationships for the average species

# Get covariates values for the graphs
r.clim.allp <- seq(min(covar.50sp.allp[,12]), max(covar.50sp.allp[,12]),, length(unique(covar.50sp.allp$site)))
r.veg.allp <- seq(min(covar.50sp.allp[,7]), max(covar.50sp.allp[,7]),, length(covar.50sp.allp$Point))
r.date50sp.allp <- seq(min(date.ori50sp.allp, na.rm = TRUE), max(date.ori50sp.allp, na.rm = TRUE),, length(covar.50sp.allp$Point))
r.time50sp.allp <- seq(min(time.ori50sp.allp, na.rm = TRUE), max(time.ori50sp.allp, na.rm = TRUE),, length(covar.50sp.allp$Point))

# Scale the covariates values for getting the predicted values
clim.allp <- (r.clim.allp - mean(covar.50sp.allp[,12]))/sd(covar.50sp.allp[,12])
Veg1.allp <- (r.veg.allp - mean(covar.50sp.allp[,7]))/sd(covar.50sp.allp[,7])
date.pred50sp.allp <- (r.date50sp.allp - mean(as.matrix(date.ori50sp.allp), na.rm = TRUE))/sd(as.matrix(date.ori50sp.allp), na.rm = TRUE)
time.pred50sp.allp <- (r.time50sp.allp - mean(as.matrix(time.ori50sp.allp), na.rm = TRUE))/sd(as.matrix(time.ori50sp.allp), na.rm = TRUE)


# Getting the means estimates from the model
pm50sp.allp <- mod50sp.allp.v4$BUGSoutput$mean 

# Predictions for detections for time, date, and veg1 and occupancy for Clim and veg1
pred.50sp.allp <- array(NA, c(n.point50sp.allp, n.spec50sp.allp, 3)) # n.site X n.spec X num of covariates


# Loop for all the species
for(k in 1:n.spec50sp.allp){
  pred.50sp.allp[,k,1] <- plogis(pm50sp.allp$alpha0[k] + pm50sp.allp$alpha.dat[k]*date.pred50sp.allp)
  pred.50sp.allp[,k,2] <- plogis(pm50sp.allp$alpha0[k] + pm50sp.allp$alpha.time[k]*time.pred50sp.allp)
  pred.50sp.allp[,k,3] <- plogis(pm50sp.allp$beta0[k] + pm50sp.allp$beta.veg[k]*Veg1.allp)
  }



# Plotting the effects of the covariates
par(mfrow = c(2,2), cex.lab = 1.3, cex.axis = 1.3)


##### DETECTABILITY


# Effect for date
plot(r.date50sp.allp, pred.50sp.allp[,1,1], lwd = 3, type = 'l', lty = 1, frame = F, 
     ylim = c(0, 1), xlab = "Survey date (15857 = 1 June)", 
     ylab = "Detection probability")
for(i in 2:length(unique(count.wide50sp.allp$idspp))){
  lines(r.date50sp.allp, pred.50sp.allp[,i,1], col = i, lwd = 3)
}


# Effect for time
plot(r.time50sp.allp, pred.50sp.allp[,1,2], lwd = 3, type = 'l', lty = 1, frame = F, 
     ylim = c(0, 1), xlab = "Survey time (21600 = 6:00 am)", 
     ylab = "Detection probability")
for(i in 2:length(unique(count.wide50sp.allp$idspp))){
  lines(r.time50sp.allp, pred.50sp.allp[,i,2], col = i, lwd = 3)
}


##### OCCUPANCY


# Effect for veg1
plot(r.veg.allp, pred.50sp.allp[,1,3], lwd = 3, type = 'l', lty = 1, frame = F, 
     ylim = c(0, 1), xlab = "Vegetation Structure PCA1", 
     ylab = "Expected Occupancy")
for(i in 2:length(unique(count.wide50sp.allp $idspp))){
  lines(r.veg.allp, pred.50sp.allp[,i,3], col = i, lwd = 3)
}



# Effect for Climate

clim.pred50sp.allp <- matrix(NA, length(unique(covar.50sp.allp$site)), length(unique(count.wide50sp.allp$idspp)))


for(k in 1:n.spec50sp.allp){
  for(i in 1:length(unique(covar.50sp.allp$site))){
    clim.pred50sp.allp[i,k] <- plogis(pm50sp.allp$beta0[k] + pm50sp.allp$beta0.clim[i,k] + pm50sp.allp$beta.clim[k]*clim.allp[i])
  }
}


plot(r.clim.allp, clim.pred50sp.allp[,1], lwd = 3, type = 'l', lty = 1, frame = F, 
     ylim = c(0, 1), xlab = "AMP", 
     ylab = "Expected Occupancy")
for(i in 2:length(unique(count.wide50sp.allp$idspp))){
  lines(r.clim.allp, clim.pred50sp.allp[,i], col = i, lwd = 3)
}









### Looking the effect (betas) per species


### Climate

# Getting a data frame with names of species and idspp
beta.test <-dcast(count.50sp.allp.2, species + idspp ~ season)

# Getting the effect of climate per species
beta.site.spec <- data.frame(idspp = spec.list50sp.allp, beta.site = pm50sp.allp$beta.site)

# Adding the whole scientific names of the species
beta.site.spec$spp <- beta.test$species[match(beta.test$idspp, beta.site.spec$idspp)]


# Getting the effect of climate per species
beta.site.spec <- data.frame(idspp = spec.list50sp.allp, beta.site = pm50sp.allp$beta.site)

# Adding the whole scientific names of the species
beta.site.spec$spp <- beta.test$species[match(beta.test$idspp, beta.site.spec$idspp)]

View(beta.site.spec)

### Vegetation

# Getting the effect of climate per species
beta.veg.spec <- data.frame(idspp = spec.list50sp.allp, beta.veg = pm50sp.allp$beta.veg)

# Adding the whole scientific names of the species
beta.veg.spec$spp <- beta.test$species[match(beta.test$idspp, beta.veg.spec$idspp)]

View(beta.veg.spec)






# Getting the mean p of the 1000 simulations we got from the MCMC
p.species3 <- t(apply(mlmsom3$BUGSoutput$sims.list$p, c(3,4), mean, na.rm = TRUE)) #mean effect for each species
#dim(p.species)

# How the detectability probability looks like
hist(p.spec3$p.2)

# Species with the highest detectability probability
p.spec3[p.spec3$p.2 == max(p.spec3$p.2),]




save.image("C:/Users/Owner/Desktop/Dropbox/Thesis/Count_Analysis/data_carpentry/model2_gradt.image.RData")
