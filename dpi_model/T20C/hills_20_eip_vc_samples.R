# set working directory
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/dpi_models/hills/T20C")

# load ggplot2 and HD interval library
library('ggplot2')
library('HDInterval')

# set seed
set.seed(123)

# computing EIP posteiors
eip_10_post = posterior_d50 + (1/posterior_k)*log(10/(100-10) )
eip_50_post = posterior_d50 + (1/posterior_k)*log(50/(100-50) )
eip_90_post = posterior_d50 + (1/posterior_k)*log(90/(100-90) )

# computing vc posteriors
vc_max_post = posterior_vc
vc_50_post = 0.5*posterior_vc

# Bootstrap resampling
n_samples = length(eip_10_post)
n_bootstrap <- 1000  # Number of bootstrap samples
bootstrap_samples_10 <- matrix(0, nrow = n_bootstrap, ncol = n_samples)
bootstrap_samples_50 <- matrix(0, nrow = n_bootstrap, ncol = n_samples)
bootstrap_samples_90 <- matrix(0, nrow = n_bootstrap, ncol = n_samples)

bootstrap_samples_vc50 <- matrix(0, nrow = n_bootstrap, ncol = n_samples)
bootstrap_samples_vcmax <- matrix(0, nrow = n_bootstrap, ncol = n_samples)

for (i in 1:n_bootstrap) {
  # Resample with replacement
  bootstrap_sample_10 <- sample(eip_10_post, size = n_samples, replace = TRUE)
  bootstrap_sample_50 <- sample(eip_50_post, size = n_samples, replace = TRUE)
  bootstrap_sample_90 <- sample(eip_90_post, size = n_samples, replace = TRUE)
  
  bootstrap_sample_vc50 <- sample(vc_50_post, size = n_samples, replace = TRUE)
  bootstrap_sample_vcmax <- sample(vc_max_post, size = n_samples, replace = TRUE)
  
  # Store the bootstrap sample
  bootstrap_samples_10[i, ] <- bootstrap_sample_10
  bootstrap_samples_50[i, ] <- bootstrap_sample_50
  bootstrap_samples_90[i, ] <- bootstrap_sample_90
  
  bootstrap_samples_vc50[i, ] <- bootstrap_sample_vc50
  bootstrap_samples_vcmax[i, ] <- bootstrap_sample_vcmax
}

# Calculate 95% HDI
hdi_limits_10 <- hdi(eip_10_post, credMass = 0.95)
hdi_limits_50 <- hdi(eip_50_post, credMass = 0.95)
hdi_limits_90 <- hdi(eip_90_post, credMass = 0.95)

hdi_limits_vc50 <- hdi(vc_50_post, credMass = 0.95)
hdi_limits_vcmax <- hdi(vc_max_post, credMass = 0.95)

# Generate 50 points from the bootstrap samples
final_bootstrap_points_10 <- numeric(50)
final_bootstrap_points_50 <- numeric(50)
final_bootstrap_points_90 <- numeric(50)

final_bootstrap_points_vc50 <- numeric(50)
final_bootstrap_points_vcmax <- numeric(50)

#Ensure that at least one point is within the HDI limits
final_bootstrap_points_10[1:2] <- seq(hdi_limits_10[1], hdi_limits_10[2], length.out = 2) 
final_bootstrap_points_50[1:2] <- seq(hdi_limits_50[1], hdi_limits_50[2], length.out = 2)  
final_bootstrap_points_90[1:2] <- seq(hdi_limits_90[1], hdi_limits_90[2], length.out = 2)

final_bootstrap_points_vc50[1:2] <- seq(hdi_limits_vc50[1], hdi_limits_vc50[2], length.out = 2)
final_bootstrap_points_vcmax[1:2] <- seq(hdi_limits_vcmax[1], hdi_limits_vcmax[2], length.out = 2)


# Fill the remaining points by sampling from bootstrap samples
for (i in 3:50) {
  # Randomly select a bootstrap sample and then randomly select a point from that sample
  selected_bootstrap_row <- sample(1:n_bootstrap, 1)
  final_bootstrap_points_10[i] <- sample(bootstrap_samples_10[selected_bootstrap_row, ], 1)
  final_bootstrap_points_50[i] <- sample(bootstrap_samples_50[selected_bootstrap_row, ], 1)
  final_bootstrap_points_90[i] <- sample(bootstrap_samples_90[selected_bootstrap_row, ], 1)
  
  final_bootstrap_points_vc50[i] <- sample(bootstrap_samples_vc50[selected_bootstrap_row, ], 1)
  final_bootstrap_points_vcmax[i] <- sample(bootstrap_samples_vcmax[selected_bootstrap_row, ], 1)
}

# Step 5: Visualize the bootstrap samples and selected points along with HDI
# Create a data frame for plotting
plot_data_10 <- data.frame(eip_10_post)
plot_data_50 <- data.frame(eip_50_post)
plot_data_90 <- data.frame(eip_90_post)

plot_data_vc50 <- data.frame(vc_50_post)
plot_data_vcmax <- data.frame(vc_max_post)

# Create histogram and add selected points
ggplot(plot_data_10, aes(x = eip_10_post)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'blue', alpha = 0.7) +
  geom_density(color = 'red', size = 1) +
  geom_point(data = data.frame(selected_points = final_bootstrap_points_10), 
             aes(x = selected_points, y = 0), color = 'green', size = 2, 
             position = position_jitter(height = 0.01)) +
  geom_vline(xintercept = hdi_limits_10, linetype = "dashed", color = "purple", size = 1) +  # HDI limits
  labs(title = "Posterior Distribution with Bootstrapped Selected Points and HDI Limits",
       x = "Posterior Samples", y = "Density") +
  theme_minimal()

# Create histogram and add selected points
ggplot(plot_data_50, aes(x = eip_50_post)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'blue', alpha = 0.7) +
  geom_density(color = 'red', size = 1) +
  geom_point(data = data.frame(selected_points = final_bootstrap_points_50), 
             aes(x = selected_points, y = 0), color = 'green', size = 2, 
             position = position_jitter(height = 0.01)) +
  geom_vline(xintercept = hdi_limits_50, linetype = "dashed", color = "purple", size = 1) +  # HDI limits
  labs(title = "Posterior Distribution with Bootstrapped Selected Points and HDI Limits",
       x = "Posterior Samples", y = "Density") +
  theme_minimal()

# Create histogram and add selected points
ggplot(plot_data_90, aes(x = eip_90_post)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'blue', alpha = 0.7) +
  geom_density(color = 'red', size = 1) +
  geom_point(data = data.frame(selected_points = final_bootstrap_points_90), 
             aes(x = selected_points, y = 0), color = 'green', size = 2, 
             position = position_jitter(height = 0.01)) +
  geom_vline(xintercept = hdi_limits_90, linetype = "dashed", color = "purple", size = 1) +  # HDI limits
  labs(title = "Posterior Distribution with Bootstrapped Selected Points and HDI Limits",
       x = "Posterior Samples", y = "Density") +
  theme_minimal()

# Create histogram and add selected points
ggplot(plot_data_vc50, aes(x = vc_50_post)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'blue', alpha = 0.7) +
  geom_density(color = 'red', size = 1) +
  geom_point(data = data.frame(selected_points = final_bootstrap_points_vc50), 
             aes(x = selected_points, y = 0), color = 'green', size = 2, 
             position = position_jitter(height = 0.01)) +
  geom_vline(xintercept = hdi_limits_vc50, linetype = "dashed", color = "purple", size = 1) +  # HDI limits
  labs(title = "Posterior Distribution with Bootstrapped Selected Points and HDI Limits",
       x = "Posterior Samples", y = "Density") +
  theme_minimal()

# Create histogram and add selected points
ggplot(plot_data_vcmax, aes(x = vc_max_post)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = 'blue', alpha = 0.7) +
  geom_density(color = 'red', size = 1) +
  geom_point(data = data.frame(selected_points = final_bootstrap_points_vcmax), 
             aes(x = selected_points, y = 0), color = 'green', size = 2, 
             position = position_jitter(height = 0.01)) +
  geom_vline(xintercept = hdi_limits_vcmax, linetype = "dashed", color = "purple", size = 1) +  # HDI limits
  labs(title = "Posterior Distribution with Bootstrapped Selected Points and HDI Limits",
       x = "Posterior Samples", y = "Density") +
  theme_minimal()


# prepare posterior eip and vc data

# trim eip posteriors using 95% HDI and save the posteriors

temperature <- 20

# Create a repeated temperature vector
temp_repeated <- rep(temperature, length(final_bootstrap_points_10))
temp_repeated_full <- rep(temperature, length(eip_10_post)) 

# Combine trimmed datasets into one data frame
eip_vc_data <- data.frame(
  temp = temp_repeated[1:length(final_bootstrap_points_10)],  # Use the length of trimmed_data
  eip10 = final_bootstrap_points_10,
  eip50 = final_bootstrap_points_50,
  eip90 = final_bootstrap_points_90,
  
  vc50 = final_bootstrap_points_vc50,
  vcmax = final_bootstrap_points_vcmax
)

# Save the combined dataset
save(eip_vc_data, file = "eip_vc_samples_20.RData")

# full posteriors
# Combine eip and vc datasets into one data frame
eip_vc_full_data <- data.frame(
  temp = temp_repeated_full[1:length(eip_10_post)],  # Use the length of trimmed_data
  eip10 = eip_10_post,
  eip50 = eip_50_post,
  eip90 = eip_90_post,
  
  vc50 = vc_50_post,
  vcmax = vc_max_post
)

# Save the combined dataset
save(eip_vc_full_data, file = "eip_vc_full_samples_20.RData")
