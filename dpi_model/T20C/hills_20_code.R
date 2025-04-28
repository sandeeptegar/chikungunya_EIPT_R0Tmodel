set.seed(52)
# Load necessary packages
library('ggplot2')
library('R2jags')
library('mcmcplots')

# Add Tohama font
library(showtext)
font_add("Tahoma", "font/tahoma.ttf")  # Replace with the actual file path
showtext_auto()

# data
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/dpi_models/hills/T20C")

data <- read.csv("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/chikv_data/T20.csv")

# Define the data
dpi <- data$dpi
pi <- data$trans
dpimax <- max(dpi)

# adding dummy entry transmission 0 at dpi 0
dpi <- c(0, dpi[!is.na(pi)])
pi <- c(0, pi[!is.na(pi)])

# JAGS model definition
#1 Hills function 

sink("hills_20_model.txt")
cat("
    model{
    
    ## Priors
    vc ~ dbeta(1, 1)
    k ~ dlnorm(2.61, 5)
    d50 ~ dlnorm(2, 0.7)
    sigma ~ dunif(0, 1000)
    tau <- 1 / (sigma * sigma)
    
    ## Likelihood
    for(i in 1:N.obs){
    pi.mu[i] <- (vc * dpi[i]^k)/(d50^k + dpi[i]^k)
    pi[i] ~ dnorm(pi.mu[i], tau)
    }
    
    ## Derived Quantities and Predictions
    for(i in 1:N.dpi.xs){
    pi.mu.pred[i] <- (vc * dpi.xs[i]^k)/(d50^k + dpi.xs[i]^k)
    }
    
    } # close model
    ",fill=T)
sink()

##### initial function
inits<-function(){list(
  vc = 0.01,
  k = 1,
  d50 = dpimax/2,
  sigma = rlnorm(1))}

##### Parameters to Estimate
parameters <- c("vc", "k", "d50","sigma", "pi.mu.pred")

##### MCMC Settings
# Number of posterior dist elements = [(ni - nb) / nt ] * nc = [ (25000 - 5000) / 8 ] * 3 = 7500
ni <- 25000 # number of iterations in each chain
nb <- 5000 # number of 'burn in' iterations to discard
nt <- 8 # thinning rate - jags saves every nt iterations in each chain
nc <- 4 # number of chains

##### dpi sequence for derived quantity calculations
# For actual fits
dpi.xs <- seq(0, 30, 0.1)
N.dpi.xs <-length(dpi.xs)

# number of observations
N.obs <- length(pi)

##### Bundle Data
jag.data<-list(pi = pi,
               N.obs = N.obs,
               dpi = dpi,
               dpi.xs = dpi.xs,
               N.dpi.xs = N.dpi.xs)

##### Run JAGS
hills.pi.20.out <- jags(data=jag.data,
                           inits=inits,
                           parameters.to.save=parameters,
                           model.file="hills_20_model.txt",
                           n.thin=nt,
                           n.chains=nc,
                           n.burnin=nb,
                           n.iter=ni,
                           DIC=T,
                           working.directory=getwd())


##### Examine Output
hills.pi.20.out$BUGSoutput$summary[c("vc", "k", "d50"), ]
#mcmc_samples <- hills.pi.18.out$BUGSoutput$sims.matrix[, c("vc", "k", "d50", "sigma")]
#mcmcplot(hills.pi.20.out)

# Create a data frame for the fitted curve
plot_fitted <- data.frame(
  x_seq = dpi.xs,
  fitted = hills.pi.20.out$BUGSoutput$summary[6:(6 + N.dpi.xs - 1), "50%"],
  lower = hills.pi.20.out$BUGSoutput$summary[6:(6 + N.dpi.xs - 1), "2.5%"],
  upper = hills.pi.20.out$BUGSoutput$summary[6:(6 + N.dpi.xs - 1), "97.5%"]
)

# Create a data frame for the data points
plot_data_points <- data.frame(
  x = dpi,
  y = pi
)

predicted_curve <- ggplot() +
  geom_point(data = plot_data_points, aes(x = x, y = y), color = "#D55E00", size = 1) +
  geom_line(data = plot_fitted, aes(x = x_seq, y = fitted), color = "#0072B2", size = 0.5) +
  geom_ribbon(data = plot_fitted, aes(x = x_seq, ymin = lower, ymax = upper), fill = "#0072B2", alpha = 0.1) +
  labs(title = " ",
       x = " ", y = " ") + #Days post infection (dpi); Infectious proportion
  geom_text(aes(x = 20, y = 0.97, label = "T = 20 \u00b0C"), family = "Tahoma", size=7)+
  xlim(0, 25) + ylim(0, 1) +
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


# Define prior samples for k, n, and d50
prior_vc <- rbeta(10000, 1, 1)
prior_k <- rlnorm(10000, 2.61, 5)
prior_d50 <- rlnorm(10000, 2, 0.7)

# Extracting posterior samples for each parameter
posterior_vc <- hills.pi.20.out$BUGSoutput$sims.list$vc
posterior_k <- hills.pi.20.out$BUGSoutput$sims.list$k
posterior_d50 <- hills.pi.20.out$BUGSoutput$sims.list$d50



# Create data frames for plotting
prior_data_vc <- data.frame(vc = prior_vc, type = "Prior")
posterior_data_vc <- data.frame(vc = posterior_vc, type = "Posterior")

prior_data_k <- data.frame(k = prior_k, type = "Prior")
posterior_data_k <- data.frame(k = posterior_k, type = "Posterior")

prior_data_d50 <- data.frame(d50 = prior_d50, type = "Prior")
posterior_data_d50 <- data.frame(d50 = posterior_d50, type = "Posterior")


# Plot using ggplot2
# Plot for k
p1 <- ggplot() +
  geom_histogram(data = posterior_data_k,
               aes(x = k, y = after_stat(density), fill = "Posterior", colour = "Posterior"),
               alpha = 0.5, bins = 30) +
  
  geom_histogram(data = prior_data_k,
                 aes(x = k, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 30, position = "identity") +
  
  geom_vline(xintercept = median(posterior_k), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  
  labs(x = expression(k), y = "Density") + 
  
  xlim(0, 100) + ylim(0, 0.1) + 
  
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

# Plot for vc
p2 <- ggplot() +
  geom_histogram(data = posterior_data_vc,
               aes(x = vc, y = after_stat(density), fill = "Posterior", 
                   colour = "Posterior"), alpha = 0.5, bins = 30) +
  
  geom_histogram(data = prior_data_vc,
                 aes(x = vc, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 30, position = "identity") +
  
  geom_vline(xintercept = median(posterior_vc), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  labs(x = expression(vc), y = "Density") +
  
  #xlim(0, 100) + ylim(0, 0.1) + 
  
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

# Plot for d50
p3 <- ggplot() +
  geom_histogram(data = posterior_data_d50,
               aes(x = d50, y = after_stat(density), fill = "Posterior", colour = "Posterior"),
               alpha = 0.5, bins = 30) +
  
  geom_histogram(data = prior_data_d50,
                 aes(x = d50, y = after_stat(density), fill = "Prior", colour = "Prior"),
                 alpha = 0.5, bins = 30, position = "identity") +
  
  geom_vline(xintercept = median(posterior_d50), linetype = "dashed", color = "black") +
  
  scale_fill_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  scale_color_manual(name = "Distributions", values = c("Posterior" = "royalblue", "Prior" = "snow4")) +
  labs(x = expression(d[50]), y = "Density") +
  
  #xlim(0, 100) + ylim(0, 0.1) + 
  
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

# Save plots
ggsave("hills_20_out.pdf", plot = predicted_curve, width = 4, height = 3, dpi = 300)
ggsave("hills_20_k.pdf", plot = p1, width = 4, height = 3, dpi = 300)
ggsave("hills_20_vc.pdf", plot = p2, width = 4, height = 3, dpi = 300)
ggsave("hills_20_d50.pdf", plot = p3, width = 4, height = 3, dpi = 300)


# compute WAIC for model with intercept only
samples.waic <- jags.samples(hills.pi.20.out$model, 
                            c("WAIC","deviance"), 
                            type = "mean", 
                            n.iter = 5000,
                            n.burnin = 1000,
                            n.thin = 1)

samples.waic$p_waic <- samples.waic$WAIC
samples.waic$waic <- samples.waic$deviance + samples.waic$p_waic
tmp <- sapply(samples.waic, sum)
waic.hills20 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
print(waic.hills20)

