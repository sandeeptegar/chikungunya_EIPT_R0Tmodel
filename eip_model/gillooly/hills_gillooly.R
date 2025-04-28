# load necessary packages
library('ggplot2')
library('R2jags')
library('mcmcplots')

# Add Tohama font
library(showtext)
font_add("Tahoma", "font/tahoma.ttf")  # Replace with the actual file path
showtext_auto()

# seed
set.seed(123)

# set working directory and read the data
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/eip_vc_models/gillooly")
data <- read.csv("hills_eip_vc_hdi.csv")

# define the data
x <- data$temperature
y <- data$eip90_median

yl <- data$eip90_l
yu <- data$eip90_u

# standard deviation

stdev <- (yu - yl)

# JAGS model definition

sink("gillooly_eip.txt")
cat("
    model{
    
    ## Priors
    A ~ dunif(0, 100)
    k ~ dgamma(1, 10)
    sigma ~ dunif(0, 1000)
    tau <- 1 / (sigma * sigma)
    
    ## Likelihood
    for(i in 1:N.obs){
    trait.mu[i] <- A*exp(-k * (temp[i]/(1 + (temp[i]/273))))
    trait[i] ~ dnorm(trait.mu[i], tau)
    }
    
    ## Derived Quantities and Predictions
    for(i in 1:N.Temp.xs){
    trait.mu.pred[i] <- A*exp(-k * (Temp.xs[i]/(1 + (Temp.xs[i]/273))))
    }
    
    } # close model
    ",fill=T)
sink()

# initial function
inits<-function(){list(
  A = 3,
  k = 1,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("A", "k", "sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 25000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(5, 40, 0.5)
N.Temp.xs <-length(Temp.xs)

# fit PDR for CHIKV in Aedes albopictus - Briere

# organize data for JAGS
trait <- y
N.obs <- length(trait)
temp <- x

# bundle data
jag.data<-list(trait = trait,
               N.obs = N.obs,
               stdev = stdev,
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
PDR.Ae.albo.CHIKV.out <- jags(data=jag.data,
                        inits=inits,
                        parameters.to.save=parameters,
                        model.file="gillooly_eip.txt",
                        n.thin=nt,
                        n.chains=nc,
                        n.burnin=nb,
                        n.iter=ni,
                        DIC=T,
                        working.directory=getwd())

##### Examine Output
PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[1:5,]
#mcmcplot(PDR.Ae.albo.CHIKV.out)

# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[5:(5 + N.Temp.xs - 1), "50%"],
  lower = PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[5:(5 + N.Temp.xs - 1), "2.5%"],
  upper = PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[5:(5 + N.Temp.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = temp,
  y = trait,
  yl = yl,
  yu = yu
)

predicted_curve <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = 'black', size = 2) +
  geom_errorbar(data = plot_data_points, aes(x = x, ymin = yl, ymax = yu), 
                width = 0.1, color = 'gray', alpha = 0.6) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = 'black', size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = expression("Gilloly function: " * EIP[90]),
       x = "Temperature (\u00b0C)", y = expression( "EIP " * (days))) +
  #xlim(0, 25) + ylim(0, 1) +
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
    legend.position = "none"
  ) +
  
  # Ensure fill identity for rain background colors
  scale_fill_identity()

# Posterior and prior plots

# Define prior samples for A and k
prior_A <- runif(100000, 0, 100)
prior_k <- rgamma(100000, 1, 10)

# Extracting posterior samples for each parameter
posterior_A <- PDR.Ae.albo.CHIKV.out$BUGSoutput$sims.list$A
posterior_k <- PDR.Ae.albo.CHIKV.out$BUGSoutput$sims.list$k

# Create data frames for plotting
prior_data_A <- data.frame(A = prior_A, type = "Prior")
posterior_data_A <- data.frame(A = posterior_A, type = "Posterior")

prior_data_k <- data.frame(k = prior_k, type = "Prior")
posterior_data_k <- data.frame(k = posterior_k, type = "Posterior")

# Plot using ggplot2
# Plot for beta_0
p1 <- ggplot() +
  geom_histogram(data = posterior_data_A,
                 aes(x = A, y = after_stat(density), fill = "Posterior", colour = "Posterior"),
                 alpha = 0.5, bins = 50) +
  
  geom_histogram(data = prior_data_A,
                 aes(x = A, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 50, position = "identity") +
  
  geom_vline(xintercept = median(posterior_A), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  
  labs(x = expression(A), y = "density") + ylim(0, 0.02) +
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "snow4"),
    axis.ticks = element_line(colour = "snow4"),
    legend.position = c(0.75, 0.8),
    legend.title = element_text(colour = "black", family = "Tahoma", size = 14),
    legend.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3))
  )

# Plot for beta_T
p2 <- ggplot() +
  geom_histogram(data = posterior_data_k,
                 aes(x = k, y = after_stat(density), fill = "Posterior", colour = "Posterior"),
                 alpha = 0.5, bins = 50) +
  
  geom_histogram(data = prior_data_k,
                 aes(x = k, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 50, position = "identity") +
  
  geom_vline(xintercept = median(posterior_k), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  
  labs(x = expression(k), y = "density") +
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "snow4"),
    axis.ticks = element_line(colour = "snow4"),
    legend.position = c(0.8, 0.8),
    legend.title = element_text(colour = "black", family = "Tahoma", size = 14),
    legend.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3))
  )


# compute WAIC for model with intercept only
samples.waic <- jags.samples(PDR.Ae.albo.CHIKV.out$model, 
                             c("WAIC","deviance"), 
                             type = "mean", 
                             n.iter = 5000,
                             n.burnin = 1000,
                             n.thin = 1)

samples.waic$p_waic <- samples.waic$WAIC
samples.waic$waic <- samples.waic$deviance + samples.waic$p_waic
tmp <- sapply(samples.waic, sum)
waic.eip <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
print(waic.eip)

# saving plots
ggsave("gillooly_eip_90_A.pdf", plot = p1, width = 4, height = 3, dpi = 300)
ggsave("gillooly_eip_90_k.pdf", plot = p2, width = 4, height = 3, dpi = 300)
ggsave("gillooly_eip_90.pdf", plot = predicted_curve, width = 4, height = 3, dpi = 300)

