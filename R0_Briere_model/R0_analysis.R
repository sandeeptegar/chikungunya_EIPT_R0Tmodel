# generates R0 posterior distribution
#set seed
set.seed(123)

# set working directory
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/R0_models/R0_pdr/R0_briere")

# load the trait posterior data
load("R0_traits_posterior_90.RData")

# read each trait posterior 
trait.a <- biting_sample
trait.lf <- long_sample
trait.mdr <- mdr_sample
trait.pea <- pea_sample
trait.tfd <- tfd_sample
trait.pdr <- pdr_sample
trait.vc <- vc_sample

# Temperature vector
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)

# function to calculate mean and quantiles
R0.poststat <- function(input, grad.xs) {
  
  # Get the number of gradient points (columns)
  N.grad.xs <- length(grad.xs)
  
  # Pre-allocate output dataframe
  output.df <- data.frame(
    mean = numeric(N.grad.xs), 
    median = numeric(N.grad.xs), 
    lowerCI = numeric(N.grad.xs), 
    upperCI = numeric(N.grad.xs), 
    lowerQuartile = numeric(N.grad.xs), 
    upperQuartile = numeric(N.grad.xs), 
    temp = grad.xs
  )
  
  # Use apply to vectorize over columns of 'input'
  summary.stats <- apply(input, 2, function(col) {
    c(mean = mean(col, na.rm = TRUE),
      median = quantile(col, 0.5, na.rm = TRUE),
      lowerCI = quantile(col, 0.025, na.rm = TRUE),
      upperCI = quantile(col, 0.975, na.rm = TRUE),
      lowerQuartile = quantile(col, 0.25, na.rm = TRUE),
      upperQuartile = quantile(col, 0.75, na.rm = TRUE))
  })
  
  # Transpose the result to match the dataframe structure
  output.df[, 1:6] <- t(summary.stats)
  
  return(output.df)
}


# define R0*sqrt(rN)
R0.rN <- function(a, vc, lf, pdr, tfd, pea, mdr) {
  # cut-off
  cutoff <- 0.00001
  # Calculate mu
  mu <- 1 / (lf + cutoff)
  
  # Calculate D
  D <- (a^3 * vc * exp(-mu / pdr) * tfd * pea * mdr) / (mu^3)
  
  return(sqrt(D))
}

# calculate posterior distribution for R0*sqrt(rN)
R0.chikv.post <- R0.rN(trait.a, trait.vc, trait.lf, trait.pdr, trait.tfd, trait.pea, trait.mdr)

R0.chikv.out <- R0.poststat(R0.chikv.post, Temp.xs)

# Specify function to calculate distitubtions of T0, Tm, and peak R0
R0.crit.stats <- function(input, temp.list) {
  
  # Pre-allocate output dataframe
  output.df <- data.frame(
    peak = numeric(nrow(input)), 
    T0 = numeric(nrow(input)), 
    Tmax = numeric(nrow(input))
  )
  
  # Pre-compute the length of temp.list
  temp.max <- max(temp.list)
  
  for (i in 1:nrow(input)) { 
    # Calculate peak R0 and store corresponding temperature
    output.df$peak[i] <- temp.list[which.max(input[i, ])]
    
    # Identify the indices where R0 > 0
    index.list <- which(input[i, ] > 0)
    length.index.list <- length(index.list)  # Cache the length of index.list
    
    # Determine T0: temperature before the first R0 > 0
    if (index.list[1] == 1) {
      output.df$T0[i] <- temp.list[1]
    } else {
      output.df$T0[i] <- temp.list[index.list[1] - 1]
    }
    
    # Determine Tmax: temperature after the last R0 > 0
    if (temp.list[index.list[length.index.list]] == temp.max) {
      output.df$Tmax[i] <- temp.max
    } else {
      output.df$Tmax[i] <- temp.list[index.list[length.index.list] + 1]
    }
  }
  
  return(output.df)
}

# calculate the distributions of T0, Tpeak and Tmax
R0.chikv.crits <- R0.crit.stats(R0.chikv.post, Temp.xs)

# save the R0_posterior, R0_statistics and R0-critical
save(R0.chikv.post,
     R0.chikv.out,
     R0.chikv.crits,
     file = "R0_statistics_90.RData")
