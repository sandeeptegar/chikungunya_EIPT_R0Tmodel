# R0 post analysis
#set seed
set.seed(123)

# set working directory
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/R0_models/R0_pdr/R0_briere")

# packages
library(ggplot2)

# Add Tohama font
library(showtext)
font_add("Tahoma", "font/tahoma.ttf")  # Replace with the actual file path
showtext_auto()

# load the R0_analysis outputs
load("R0_statistics_50.RData")

# R0 normalisation
R0_median_max <- max(R0.chikv.out$median)
R0_low_max <- max(R0.chikv.out$lowerCI)
R0_up_max <- max(R0.chikv.out$upperCI)

R0_norm_median <- R0.chikv.out$median/R0_median_max
R0_norm_low <- R0.chikv.out$lowerCI/R0_low_max
R0_norm_up <- R0.chikv.out$upperCI/R0_up_max

# Temperature vector
Temp.xs <- seq(1, 45, 0.1)
N.Temp.xs <-length(Temp.xs)

# Create a data frame for the relative R0 or the normalised R0
plot_R0 <- data.frame(
  x_seq = Temp.xs,
  fitted = R0_norm_median,
  lower = R0_norm_low,
  upper = R0_norm_up
)


R0_relative <- ggplot() +
  geom_line(data = plot_R0, aes(x = x_seq, y = fitted), color = "#DB6D00", linewidth = 0.5) +
  geom_ribbon(data = plot_R0, aes(x = x_seq, ymin = lower, ymax = upper), fill = "#DB6D00", alpha = 0.2) +
  labs(title = "",
       x = "Temperature (\u00b0C)", y = expression( bold("Relative ") * bold(R[0](T)))) +
  coord_cartesian(xlim = c(10, 40), ylim = c(0, 1)) +
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

# Plotting the distributions of T0, peak and Tmax
peak_crit_data <- data.frame(peak = R0.chikv.crits$peak)
T0_crit_data <- data.frame(T0 = R0.chikv.crits$T0)
Tmax_crit_data <- data.frame(Tmax = R0.chikv.crits$Tmax)

peak <- ggplot() +
  geom_histogram(data = peak_crit_data,
                 aes(x = peak, y = after_stat(density)), 
                 fill = "#DB6D00", colour = "#DB6D00",
                 alpha = 0.5, bins = 20) +
  
  geom_vline(xintercept = mean(R0.chikv.crits$peak), linetype = "dashed", color = 'black', linewidth = 0.7) +
  
  labs(x = expression(T[opt]), y = "Density") + #xlim(0, 0.003) +
  
  scale_x_continuous(breaks = c(23, 29)) +
  scale_y_continuous(breaks = c(0, 0.75)) +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 18),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )

tmin <- ggplot() +
  geom_histogram(data = T0_crit_data,
                 aes(x = T0, y = after_stat(density)), 
                 fill = "#DB6D00", colour = "#DB6D00",
                 alpha = 0.5, bins = 20) +
  
  geom_vline(xintercept = mean(R0.chikv.crits$T0), linetype = "dashed", color = 'black', linewidth = 0.7) +
  
  labs(x = expression(T[min]), y = "Density") + #xlim(0, 0.003) +
  
  scale_x_continuous(breaks = c(7, 20)) +
  scale_y_continuous(breaks = c(0, 0.25)) +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 18),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )

tmax <- ggplot() +
  geom_histogram(data = Tmax_crit_data,
                 aes(x = Tmax, y = after_stat(density)), 
                 fill = "#DB6D00", colour = "#DB6D00",
                 alpha = 0.5, bins = 20) +
  
  geom_vline(xintercept = mean(R0.chikv.crits$Tmax), linetype = "dashed", color = 'black', linewidth = 0.7) +
  
  labs(x = expression(T[max]), y = "Density") + #xlim(0, 0.003) +
  
  scale_x_continuous(breaks = c(25, 38)) +
  scale_y_continuous(breaks = c(0, 0.45)) +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(family = "Tahoma", size = 14, face = "bold"),
    axis.title = element_text(family = "Tahoma", size = 18),
    axis.text = element_text(colour = "black", family = "Tahoma", size = 14),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    legend.position = c(0.8, 0.7)
  )

ggsave("relative_R0.pdf", plot = R0_relative, width = 4, height = 3, dpi = 300)
ggsave("peak_R0_b.pdf", plot = peak, width = 2, height = 2, dpi = 600)
ggsave("tmin_R0.pdf", plot = tmin, width = 2, height = 2, dpi = 600)
ggsave("tmax_R0_b.pdf", plot = tmax, width = 2, height = 2, dpi = 600)


#  save R0 critical values

library('HDInterval')

T0_hdi = hdi(R0.chikv.crits$T0)
T0_mean = mean(R0.chikv.crits$T0)

Topt_hdi = hdi(R0.chikv.crits$peak)
Topt_mean = mean(R0.chikv.crits$peak)

Tmax_hdi = hdi(R0.chikv.crits$Tmax)
Tmax_mean = mean(R0.chikv.crits$Tmax)

# Create a data frame
data_all <- data.frame(
  Functions = c("T0", "Topt", "Tmax"),
  mean = c(T0_mean, Topt_mean, Tmax_mean),
  hpd = c(paste0("(", T0_hdi[1], ", ", T0_hdi[2], ")"), 
          paste0("(", Topt_hdi[1], ", ", Topt_hdi[2], ")"),
          paste0("(", Tmax_hdi[1], ", ", Tmax_hdi[2], ")"))
)

# Write the data frame to a CSV file
write.csv(data_all, "R0_summary.csv", row.names = FALSE)


# save R0 mean (without normalization) and temperature data
# Create a data frame with the two variables
R0_data <- data.frame(T = Temp.xs, R0 = R0.chikv.out$median)
R0_data_norm <- data.frame(T = Temp.xs, R0 = R0.chikv.out$median/R0_median_max)

# Save the data frame to a CSV file
write.csv(R0_data, file = "R0_out.csv", row.names = FALSE)
write.csv(R0_data_norm, file = "R0_out_norm.csv", row.names = FALSE)
