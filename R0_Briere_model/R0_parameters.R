# To get the posterior distribution of R0 for chikungunya
# Data is taken from Mordecai et al. (2017) except for 
# EIP and vector competence data.
# two data files: 1. mordecai_data.csv and 2. hills_eip_vc.csv 

#set seed
set.seed(123)

# set working directory
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/R0_models/R0_pdr/R0_briere")

# load packages
library('ggplot2')
library('R2jags')
library('mcmcplots')
library('gridExtra')
library('HDInterval')

# Add Tohama font
library(showtext)
font_add("Tahoma", "font/tahoma.ttf")  # Replace with the actual file path
showtext_auto()

# upload data files
albopictus <- read.csv("mordecai_data.csv")
chikungunya_vc <- read.csv("hills_eip_vc_hdi.csv")
chikungunya_pdr <- read.csv("hills_pdr.csv")

# define jags models: 1. Briere and 2. quadratic

# 1. Briere model
sink("briere_mordecai.txt")
cat("
    model{
    
    ## Priors
    c ~ dgamma(1, 10)
    T0 ~ dunif(0, 24)
    Tm ~ dunif(25, 45)
    sigma ~ dunif(0, 1000)
    tau <- 1 / (sigma * sigma)
    
    ## Likelihood
    for(i in 1:N.obs){
    trait.mu[i] <- c * temp[i] * (temp[i] - T0) * sqrt((Tm - temp[i]) * (Tm > temp[i])) * (T0 < temp[i])
    trait[i] ~ dnorm(trait.mu[i], tau)
    }
    
    ## Derived Quantities and Predictions
    for(i in 1:N.Temp.xs){
    trait.mu.pred[i] <- c * Temp.xs[i] * (Temp.xs[i] - T0) * sqrt((Tm - Temp.xs[i]) * (Tm > Temp.xs[i])) * (T0 < Temp.xs[i])
    }
    
    } # close model
    ",fill=T)
sink()

# 2. quadratic model
sink("quadratic_mordecai.txt")
cat("
    model{
    
    ## Priors
    c ~ dgamma(1, 1)
    T0 ~ dunif(0, 24)
    Tm ~ dunif(25, 45)
    sigma ~ dunif(0, 1000)
    tau <- 1 / (sigma * sigma)
    
    ## Likelihood
    for(i in 1:N.obs){
    trait.mu[i] <- (-c * (temp[i] - T0) * (temp[i] - Tm) * (T0 < temp[i])) * (temp[i] < Tm)
    trait[i] ~ dnorm(trait.mu[i], tau)
    }
    
    ## Derived Quantities and Predictions
    for(i in 1:N.Temp.xs){
    trait.mu.pred[i] <- (-c * (Temp.xs[i] - T0) * (Temp.xs[i] - Tm)) * (T0 < Temp.xs[i]) * (Temp.xs[i] < Tm)
    }
    
    } # close model
    ",fill=T)
sink()

## Fitting traits using either Briere or quadratic functions

# 1. Biting rate 'a'
# 'a' is defined as 1/GCD
# define 'a' from GCD
gcd <- albopictus[which(albopictus$trait.name=="GCD"),]

# transforming gcd into biting rate
gcd$trait <- 1/(gcd$trait)

# initial function
inits<-function(){list(
  c = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("c", "T0", "Tm","sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 50000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
#Temp.xs <- seq(5, 45, 0.5)
#N.Temp.xs <-length(Temp.xs)

# organize data for JAGS
trait <- gcd$trait
N.obs <- length(trait)
temp <- gcd$T

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
biting.out <- jags(data=jag.data,
                              inits=inits,
                              parameters.to.save=parameters,
                              model.file="briere_mordecai.txt",
                              n.thin=nt,
                              n.chains=nc,
                              n.burnin=nb,
                              n.iter=ni,
                              DIC=T,
                              working.directory=getwd())

##### Examine Output
biting.out$BUGSoutput$summary[1:5,]
#mcmcplot(biting.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = biting.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = biting.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = biting.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait
)

biting_rate <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 1) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', linewidth = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = "Briere function",
       x = "Temperature (\u00b0C)", y = expression( "Biting rate " * (day^-1))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )
ggsave("biting_rate.pdf", plot = biting_rate, width = 4, height = 3, dpi = 300)
# saving posterior biting rate
biting_sample <- biting.out$BUGSoutput$sims.list$trait.mu.pred

# 2. mosquito development rate (MDR) 'MDR'
# data has two type of entries MDR and 1/MDR
# joining MDR and (1/MDR)^-1 to define MDR data
mdr1 <- albopictus[which(albopictus$trait.name=="MDR"),]
mdr2 <- albopictus[which(albopictus$trait.name=="1/MDR"),]

# redefining 1/MDR in dataset mdr2 as MDR
mdr2$trait <- 1/(mdr2$trait)

# joining two datasets
mdr = rbind(mdr1, mdr2)

# initial function
inits<-function(){list(
  c = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("c", "T0", "Tm","sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 50000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
#Temp.xs <- seq(5, 45, 0.5)
#N.Temp.xs <-length(Temp.xs)

# organize data for JAGS
trait <- mdr$trait
N.obs <- length(trait)
temp <- mdr$T

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
mdr.out <- jags(data=jag.data,
                   inits=inits,
                   parameters.to.save=parameters,
                   model.file="briere_mordecai.txt",
                   n.thin=nt,
                   n.chains=nc,
                   n.burnin=nb,
                   n.iter=ni,
                   DIC=T,
                   working.directory=getwd())

##### Examine Output
mdr.out$BUGSoutput$summary[1:5,]
#mcmcplot(mdr.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = mdr.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = mdr.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = mdr.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait
)

mdr_rate <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 1) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = "Briere function",
       x = "Temperature (\u00b0C)", y = expression( "MDR " * (day^-1))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )
ggsave("mdr.pdf", plot = mdr_rate, width = 4, height = 3, dpi = 300)

# saving posterior mdr
mdr_sample <- mdr.out$BUGSoutput$sims.list$trait.mu.pred

# 3. total female fecundity TFD
# notice that, in R0 model, EFD = (EFD = TFD*(1/(length of gonotrophic cycle)) = a*TFD, where a is the biting rate, will be used.
tfd <- albopictus[which(albopictus$trait.name=="TFD"),]

# refine the data
# R indicates the oviposition cycle number
# omit R4, R5, R6, R7, R8
tfd <- tfd[which(tfd$trait2.name %in% c("R1", "R2", "R3", NA)),]
tfd = subset(tfd, ref != "Yee_et al_2016 JAE_in_review")

# initial function
inits<-function(){list(
  c = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("c", "T0", "Tm","sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 50000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
#Temp.xs <- seq(5, 45, 0.5)
#N.Temp.xs <-length(Temp.xs)

# organize data for JAGS
trait <- tfd$trait
N.obs <- length(trait)
temp <- tfd$T

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
tfd.out <- jags(data=jag.data,
                inits=inits,
                parameters.to.save=parameters,
                model.file="briere_mordecai.txt",
                n.thin=nt,
                n.chains=nc,
                n.burnin=nb,
                n.iter=ni,
                DIC=T,
                working.directory=getwd())

##### Examine Output
tfd.out$BUGSoutput$summary[1:5,]
#mcmcplot(tfd.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = tfd.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = tfd.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = tfd.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait
)

tfd_rate <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 1) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = "Briere function",
       x = "Temperature (\u00b0C)", y = expression( "TFD " * (days))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )
ggsave("tfd.pdf", plot = tfd_rate, width = 4, height = 3, dpi = 300)

# saving posterior TFD
tfd_sample <- tfd.out$BUGSoutput$sims.list$trait.mu.pred

# 4.survival probability pEA
# the probability a mosquito will survive from hatching to maturation 
pea <- albopictus[which(albopictus$trait.name=="pEA"),]

# initial function
inits<-function(){list(
  c = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("c", "T0", "Tm","sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 50000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
#Temp.xs <- seq(5, 45, 0.5)
#N.Temp.xs <-length(Temp.xs)

# organize data for JAGS
trait <- pea$trait
N.obs <- length(trait)
temp <- pea$T

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
pea.out <- jags(data=jag.data,
                inits=inits,
                parameters.to.save=parameters,
                model.file="quadratic_mordecai.txt",
                n.thin=nt,
                n.chains=nc,
                n.burnin=nb,
                n.iter=ni,
                DIC=T,
                working.directory=getwd())

##### Examine Output
pea.out$BUGSoutput$summary[1:5,]
#mcmcplot(pea.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = pea.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = pea.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = pea.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait
)

pea_rate <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 1) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = "Quadratic function",
       x = "Temperature (\u00b0C)", y = "Survival probability") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )
ggsave("pea.pdf", plot = pea_rate, width = 4, height = 3, dpi = 300)

# saving posterior survival probability
pea_sample <- pea.out$BUGSoutput$sims.list$trait.mu.pred

# 5. adult mortality rate mu
# it is fitted as adult lifespan, lf = 1/mu.
long1 <- albopictus[which(albopictus$trait.name=="prop.dead"),]
long2 <- albopictus[which(albopictus$trait.name=="1/mu"),]

# recovering mu from the data
long1$trait <- -log(1 - long1$trait)
long2$trait <- 1/long2$trait

# joining two datasets
long = rbind(long1, long2)
long$trait <- 1/(long$trait)

# The starved mosquitoes had much shorter survival than all other data, so remove them
long = subset(long, trait2 %in% c("sugar-fed", NA))

# initial function
inits<-function(){list(
  c = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("c", "T0", "Tm","sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 50000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
#Temp.xs <- seq(5, 45, 0.5)
#N.Temp.xs <-length(Temp.xs)

# organize data for JAGS
trait <- long$trait
N.obs <- length(trait)
temp <- long$T

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
long.out <- jags(data=jag.data,
                inits=inits,
                parameters.to.save=parameters,
                model.file="quadratic_mordecai.txt",
                n.thin=nt,
                n.chains=nc,
                n.burnin=nb,
                n.iter=ni,
                DIC=T,
                working.directory=getwd())

##### Examine Output
long.out$BUGSoutput$summary[1:5,]
#mcmcplot(long.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = long.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = long.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = long.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait
)

long_rate <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 1) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = "Quadratic function",
       x = "Temperature (\u00b0C)", y = expression( "Adult lifespan " * (days))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )
ggsave("adult_life.pdf", plot = long_rate, width = 4, height = 3, dpi = 300)

# saving posterior adult lifespan
long_sample <- long.out$BUGSoutput$sims.list$trait.mu.pred


# extrinsic incubation period (EIP50) and vector competence (average vector competence)

# 6. parasite development rate (pdr) = 1/eip
pdr <- chikungunya_pdr$pdr90_median
tempdat <- chikungunya_pdr$temperature

# JAGS model definition

sink("briere_pdr.txt")
cat("
    model{
    
    ## Priors
    c ~ dgamma(1, 10)
    T0 ~ dunif(0, 18)
    Tm ~ dunif(30, 45)
    sigma ~ dunif(0, 1000)
    tau <- 1 / (sigma * sigma)
    
    ## Likelihood
    for(i in 1:N.obs){
    trait.mu[i] <- c * temp[i] * (temp[i] - T0) * sqrt((Tm - temp[i]) * (Tm > temp[i])) * (T0 < temp[i])
    trait[i] ~ dnorm(trait.mu[i], tau)
    }
    
    ## Derived Quantities and Predictions
    for(i in 1:N.Temp.xs){
    trait.mu.pred[i] <- c * Temp.xs[i] * (Temp.xs[i] - T0) * sqrt((Tm - Temp.xs[i]) * (Tm > Temp.xs[i])) * (T0 < Temp.xs[i])
    }
    
    } # close model
    ",fill=T)
sink()

# initial function
inits<-function(){list(
  c = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("c", "T0", "Tm","sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 50000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
#Temp.xs <- seq(5, 45, 0.5)
#N.Temp.xs <-length(Temp.xs)

# organize data for JAGS
trait <- pdr
N.obs <- length(trait)
temp <- tempdat

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
pdr.out <- jags(data=jag.data,
                 inits=inits,
                 parameters.to.save=parameters,
                 model.file="briere_pdr.txt",
                 n.thin=nt,
                 n.chains=nc,
                 n.burnin=nb,
                 n.iter=ni,
                 DIC=T,
                 working.directory=getwd())

##### Examine Output
pdr.out$BUGSoutput$summary[1:5,]
#mcmcplot(pdr.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = pdr.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = pdr.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = pdr.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait
)

pdr_rate <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 2) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = "Briere function",
       x = "Temperature (\u00b0C)", y = expression( "Parasite development rate " * (day^-1))) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "darkgray"),
    axis.ticks = element_line(colour = "darkgray")
  )

# saving posterior parasite development rate
pdr_sample <- pdr.out$BUGSoutput$sims.list$trait.mu.pred

# 7. vector competence (average)
# JAGS model definition

vc <- chikungunya_vc$vcmax_median
tempdat <- chikungunya_vc$temperature

sink("quadratic_vc.txt")
cat("
    model{
    
    ## Priors
    c ~ dgamma(1, 1)
    T0 ~ dunif(0, 18)
    Tm ~ dunif(30, 45)
    sigma ~ dunif(0, 1000)
    tau <- 1 / (sigma * sigma)
    
    ## Likelihood
    for(i in 1:N.obs){
    trait.mu[i] <- (-c * (temp[i] - T0) * (temp[i] - Tm) * (T0 < temp[i])) * (temp[i] < Tm)
    trait[i] ~ dnorm(trait.mu[i], tau)
    }
    
    ## Derived Quantities and Predictions
    for(i in 1:N.Temp.xs){
    trait.mu.pred[i] <- (-c * (Temp.xs[i] - T0) * (Temp.xs[i] - Tm)) * (T0 < Temp.xs[i]) * (Temp.xs[i] < Tm)
    }
    
    } # close model
    ",fill=T)
sink()

# initial function
inits<-function(){list(
  c = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("c", "T0", "Tm","sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 50000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
#Temp.xs <- seq(5, 45, 0.5)
#N.Temp.xs <-length(Temp.xs)

# organize data for JAGS
trait <- vc
N.obs <- length(trait)
temp <- tempdat

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
vc.out <- jags(data=jag.data,
                inits=inits,
                parameters.to.save=parameters,
                model.file="quadratic_vc.txt",
                n.thin=nt,
                n.chains=nc,
                n.burnin=nb,
                n.iter=ni,
                DIC=T,
                working.directory=getwd())

##### Examine Output
vc.out$BUGSoutput$summary[1:5,]
#mcmcplot(vc.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = vc.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = vc.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = vc.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait
)

vc_rate <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 2) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = "Quadratic function",
       x = "Temperature (\u00b0C)", y = expression( "Vector competence " * (proportion))) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "darkgray"),
    axis.ticks = element_line(colour = "darkgray")
  )

# saving posterior vector competence
vc_sample <- vc.out$BUGSoutput$sims.list$trait.mu.pred

# Saving all the data in R data format
save(biting_sample,
     mdr_sample,
     tfd_sample,
     pea_sample,
     long_sample,
     pdr_sample,
     vc_sample,
     file = "R0_traits_posterior_90.RData")


# getting mean, median, and 95% HPD intervals for all the traits
# biting rate
biting_c <- biting.out$BUGSoutput$sims.list$c
biting_T0 <- biting.out$BUGSoutput$sims.list$T0
biting_Tm <- biting.out$BUGSoutput$sims.list$Tm

biting_c_mean <- mean(biting_c)
biting_c_median <- median(biting_c)
biting_c_hdi <- hdi(biting_c)

biting_T0_mean <- mean(biting_T0)
biting_T0_median <- median(biting_T0)
biting_T0_hdi <- hdi(biting_T0)

biting_Tm_mean <- mean(biting_Tm)
biting_Tm_median <- median(biting_Tm)
biting_Tm_hdi <- hdi(biting_Tm)

# mdr
mdr_c <- mdr.out$BUGSoutput$sims.list$c
mdr_T0 <- mdr.out$BUGSoutput$sims.list$T0
mdr_Tm <- mdr.out$BUGSoutput$sims.list$Tm

mdr_c_mean <- mean(mdr_c)
mdr_c_median <- median(mdr_c)
mdr_c_hdi <- hdi(mdr_c)

mdr_T0_mean <- mean(mdr_T0)
mdr_T0_median <- median(mdr_T0)
mdr_T0_hdi <- hdi(mdr_T0)

mdr_Tm_mean <- mean(mdr_Tm)
mdr_Tm_median <- median(mdr_Tm)
mdr_Tm_hdi <- hdi(mdr_Tm)

# tfd
tfd_c <- tfd.out$BUGSoutput$sims.list$c
tfd_T0 <- tfd.out$BUGSoutput$sims.list$T0
tfd_Tm <- tfd.out$BUGSoutput$sims.list$Tm

tfd_c_mean <- mean(tfd_c)
tfd_c_median <- median(tfd_c)
tfd_c_hdi <- hdi(tfd_c)

tfd_T0_mean <- mean(tfd_T0)
tfd_T0_median <- median(tfd_T0)
tfd_T0_hdi <- hdi(tfd_T0)

tfd_Tm_mean <- mean(tfd_Tm)
tfd_Tm_median <- median(tfd_Tm)
tfd_Tm_hdi <- hdi(tfd_Tm)

# pea
pea_c <- pea.out$BUGSoutput$sims.list$c
pea_T0 <- pea.out$BUGSoutput$sims.list$T0
pea_Tm <- pea.out$BUGSoutput$sims.list$Tm

pea_c_mean <- mean(pea_c)
pea_c_median <- median(pea_c)
pea_c_hdi <- hdi(pea_c)

pea_T0_mean <- mean(pea_T0)
pea_T0_median <- median(pea_T0)
pea_T0_hdi <- hdi(pea_T0)

pea_Tm_mean <- mean(pea_Tm)
pea_Tm_median <- median(pea_Tm)
pea_Tm_hdi <- hdi(pea_Tm)

# long
long_c <- long.out$BUGSoutput$sims.list$c
long_T0 <- long.out$BUGSoutput$sims.list$T0
long_Tm <- long.out$BUGSoutput$sims.list$Tm

long_c_mean <- mean(long_c)
long_c_median <- median(long_c)
long_c_hdi <- hdi(long_c)

long_T0_mean <- mean(long_T0)
long_T0_median <- median(long_T0)
long_T0_hdi <- hdi(long_T0)

long_Tm_mean <- mean(long_Tm)
long_Tm_median <- median(long_Tm)
long_Tm_hdi <- hdi(long_Tm)

# pdr
pdr_c <- pdr.out$BUGSoutput$sims.list$c
pdr_T0 <- pdr.out$BUGSoutput$sims.list$T0
pdr_Tm <- pdr.out$BUGSoutput$sims.list$Tm

pdr_c_mean <- mean(pdr_c)
pdr_c_median <- median(pdr_c)
pdr_c_hdi <- hdi(pdr_c)

pdr_T0_mean <- mean(pdr_T0)
pdr_T0_median <- median(pdr_T0)
pdr_T0_hdi <- hdi(pdr_T0)

pdr_Tm_mean <- mean(pdr_Tm)
pdr_Tm_median <- median(pdr_Tm)
pdr_Tm_hdi <- hdi(pdr_Tm)

# vc
vc_c <- vc.out$BUGSoutput$sims.list$c
vc_T0 <- vc.out$BUGSoutput$sims.list$T0
vc_Tm <- vc.out$BUGSoutput$sims.list$Tm

vc_c_mean <- mean(vc_c)
vc_c_median <- median(vc_c)
vc_c_hdi <- hdi(vc_c)

vc_T0_mean <- mean(vc_T0)
vc_T0_median <- median(vc_T0)
vc_T0_hdi <- hdi(vc_T0)

vc_Tm_mean <- mean(vc_Tm)
vc_Tm_median <- median(vc_Tm)
vc_Tm_hdi <- hdi(vc_Tm)

# Create a data frame
data_all <- data.frame(
  Functions = c("Biting", "MDR", "TFD", "pEA", "Lifespan", "PDR", "VC"),
  T0_mean = c(biting_T0_mean, mdr_T0_mean, tfd_T0_mean, pea_T0_mean, long_T0_mean, pdr_T0_mean, vc_T0_mean),
  T0_median = c(biting_T0_median, mdr_T0_median, tfd_T0_median, pea_T0_median, long_T0_median, pdr_T0_median, vc_T0_median),
  T0_hdi_lower = c(biting_T0_hdi[1], mdr_T0_hdi[1], tfd_T0_hdi[1], pea_T0_hdi[1], long_T0_hdi[1],  pdr_T0_hdi[1],  vc_T0_hdi[1]),
  T0_hdi_upper = c(biting_T0_hdi[2], mdr_T0_hdi[2], tfd_T0_hdi[2], pea_T0_hdi[2], long_T0_hdi[2],  pdr_T0_hdi[2],  vc_T0_hdi[2]),
  
  Tm_mean = c(biting_Tm_mean, mdr_Tm_mean, tfd_Tm_mean, pea_Tm_mean, long_Tm_mean, pdr_Tm_mean, vc_Tm_mean),
  Tm_median = c(biting_Tm_median, mdr_Tm_median, tfd_Tm_median, pea_Tm_median, long_Tm_median, pdr_Tm_median, vc_Tm_median),
  Tm_hdi_lower = c(biting_Tm_hdi[1], mdr_Tm_hdi[1], tfd_Tm_hdi[1], pea_Tm_hdi[1], long_Tm_hdi[1],  pdr_Tm_hdi[1],  vc_Tm_hdi[1]),
  Tm_hdi_upper = c(biting_Tm_hdi[2], mdr_Tm_hdi[2], tfd_Tm_hdi[2], pea_Tm_hdi[2], long_Tm_hdi[2],  pdr_Tm_hdi[2],  vc_Tm_hdi[2]),
  
  c_mean = c(biting_c_mean, mdr_c_mean, tfd_c_mean, pea_c_mean, long_c_mean, pdr_c_mean, vc_c_mean),
  c_median = c(biting_c_median, mdr_c_median, tfd_c_median, pea_c_median, long_c_median, pdr_c_median, vc_c_median),
  c_hdi_lower = c(biting_c_hdi[1], mdr_c_hdi[1], tfd_c_hdi[1], pea_c_hdi[1], long_c_hdi[1],  pdr_c_hdi[1],  vc_c_hdi[1]),
  c_hdi_upper = c(biting_c_hdi[2], mdr_c_hdi[2], tfd_c_hdi[2], pea_c_hdi[2], long_c_hdi[2],  pdr_c_hdi[2],  vc_c_hdi[2])
  
)

# Write the data frame to a CSV file
#write.csv(data_all, "trait_summary.csv", row.names = FALSE)

# printing Mordecai trait fitted curves
# Create empty plots for the grid
empty_plot <- ggplot() + theme_void()

# Arrange plots in a grid
all_fig_mordecai <- grid.arrange(
  biting_rate, mdr_rate, tfd_rate,
  pea_rate, long_rate, empty_plot,
  nrow = 2,
  ncol = 3
)

# printing pdr and vc fitted curves
# Create empty plots for the grid
empty_plot <- ggplot() + theme_void()

# Arrange plots in a grid
all_fig_pdr_vc <- grid.arrange(
  pdr_rate, vc_rate,
  nrow = 1,
  ncol = 2
)

# saving plots
#ggsave("traits_mordecai.jpg", plot = all_fig_mordecai, dpi = 300, width = 10, height = 8, units = "in", limitsize = FALSE)
#ggsave("traits_pdr_vc.jpg", plot = all_fig_pdr_vc, dpi = 300, width = 10, height = 8, units = "in", limitsize = FALSE)
