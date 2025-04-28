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
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/eip_vc_models/pdrs/briere")
data <- read.csv("hills_pdr.csv")

# define the data
x <- data$temperature
y <- data$pdr90_median

yl <- data$pdr90_l
yu <- data$pdr90_u

# standard deviance
stdev = (yu - yl)

# JAGS model definition

sink("briere_pdr.txt")
cat("
    model{
    
    ## Priors
    q ~ dgamma(1, 10)
    T0 ~ dunif(0, 18)
    Tm ~ dunif(30, 45)
    sigma ~ dunif(0, 1000)
    tau <- 1 / (sigma * sigma)
    
    ## Likelihood
    for(i in 1:N.obs){
    trait.mu[i] <- q * temp[i] * (temp[i] - T0) * sqrt((Tm - temp[i]) * (Tm > temp[i])) * (T0 < temp[i])
    trait[i] ~ dnorm(trait.mu[i], tau)
    }
    
    ## Derived Quantities and Predictions
    for(i in 1:N.Temp.xs){
    trait.mu.pred[i] <- q * Temp.xs[i] * (Temp.xs[i] - T0) * sqrt((Tm - Temp.xs[i]) * (Tm > Temp.xs[i])) * (T0 < Temp.xs[i])
    }
    
    } # close model
    ",fill=T)
sink()

# initial function
inits<-function(){list(
  q = 0.01,
  Tm = 35,
  T0 = 5,
  sigma = rlnorm(1))}

# parameters to estimate
parameters <- c("q", "T0", "Tm", "sigma", "trait.mu.pred")

# MCMC settings
# number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 25000  # number of iterations in each chain
nb <- 5000   # number of 'burn in' iterations to discard
nt <- 8      # thinning rate - jags saves every nt iterations in each chain
nc <- 4      # number of chains

# temp sequence for derived quantity calculations
# for actual fits
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)
# for priors - fewer temps for derived calculations makes it go faster
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
               temp = temp,
               Temp.xs = Temp.xs,
               N.Temp.xs = N.Temp.xs)

# run JAGS
PDR.Ae.albo.CHIKV.out <- jags(data=jag.data,
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
PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[1:5,]
#mcmcplot(PDR.Ae.albo.CHIKV.out)


# Plotting
# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = Temp.xs,
  fitted = PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "50%"],
  lower = PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "2.5%"],
  upper = PDR.Ae.albo.CHIKV.out$BUGSoutput$summary[6:(6 + N.Temp.xs - 1), "97.5%"]
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
  labs(title = expression("Briere function: " * PDR[90]),
       x = "Temperature (\u00b0C)", y = expression( "PDR " * (day^-1))) +
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

# Define prior samples for k, n, and d50
prior_q <- rgamma(100000, 1, 10)
prior_T0 <- runif(100000, 0, 18)
prior_Tm <- runif(100000, 30, 45)

# Extracting posterior samples for each parameter
posterior_q <- PDR.Ae.albo.CHIKV.out$BUGSoutput$sims.list$q
posterior_T0 <- PDR.Ae.albo.CHIKV.out$BUGSoutput$sims.list$T0
posterior_Tm <- PDR.Ae.albo.CHIKV.out$BUGSoutput$sims.list$Tm

# Create data frames for plotting
prior_data_q <- data.frame(q = prior_q, type = "Prior")
posterior_data_q <- data.frame(q = posterior_q, type = "Posterior")

prior_data_T0 <- data.frame(T0 = prior_T0, type = "Prior")
posterior_data_T0 <- data.frame(T0 = posterior_T0, type = "Posterior")

prior_data_Tm <- data.frame(Tm = prior_Tm, type = "Prior")
posterior_data_Tm <- data.frame(Tm = posterior_Tm, type = "Posterior")

# Plot using ggplot2
# Plot for q
p1 <- ggplot() +
  geom_histogram(data = posterior_data_q,
                 aes(x = q, y = after_stat(density), fill = "Posterior", colour = "Posterior"),
                 alpha = 0.5, bins = 50) +
  
  geom_histogram(data = prior_data_q,
                 aes(x = q, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 50, position = "identity") +
  
  geom_vline(xintercept = median(posterior_q), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  
  labs(x = expression(q), y = "density")  +
  
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

# Plot for T0
p2 <- ggplot() +
  geom_histogram(data = posterior_data_T0,
                 aes(x = T0, y = after_stat(density), fill = "Posterior", colour = "Posterior"),
                 alpha = 0.5, bins = 50) +
  
  geom_histogram(data = prior_data_T0,
                 aes(x = T0, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 50, position = "identity") +
  
  geom_vline(xintercept = median(posterior_T0), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  
  labs(x = expression(T[0]), y = "density") + ylim(0, 0.12) +
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "snow4"),
    axis.ticks = element_line(colour = "snow4"),
    legend.position = c(0.8, 0.85),
    legend.title = element_text(colour = "black", family = "Tahoma", size = 14),
    legend.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 14),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3))
  )

# Plot for Tm
p3 <- ggplot() +
  geom_histogram(data = posterior_data_Tm,
                 aes(x = Tm, y = after_stat(density), fill = "Posterior", colour = "Posterior"),
                 alpha = 0.5, bins = 50) +
  
  geom_histogram(data = prior_data_Tm,
                 aes(x = Tm, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 50, position = "identity") +
  
  geom_vline(xintercept = median(posterior_Tm), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  
  labs(x = expression(T[m]), y = "density") +
  
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
#ggsave("briere_pdr_90.pdf", plot = predicted_curve, width = 4, height = 3, dpi = 300)
#ggsave("briere_pdr_90_q.pdf", plot = p1, width = 4, height = 3, dpi = 300)
#ggsave("briere_pdr_90_T0.pdf", plot = p2, width = 4, height = 3, dpi = 300)
#ggsave("briere_pdr_90_Tm.pdf", plot = p3, width = 4, height = 3, dpi = 300)
