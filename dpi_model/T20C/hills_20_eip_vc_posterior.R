# first run the main code hills_20_code.R and keep outputs in the memory

# set working directory
setwd("C:/Users/santeg/OneDrive - UKCEH/SandeepTegar/Projects/Project EIP/eip_final/dpi_models/hills/T20C")

# load ggplot2 and HD interval library
library('ggplot2')
library('HDInterval')

# Add Tohama font
library(showtext)
font_add("Tahoma", "font/tahoma.ttf")  # Replace with the actual file path
showtext_auto()

posterior_dk <- (posterior_d50)^(posterior_k)

# computing EIP posteiors
eip_10_post = (posterior_dk*10/(100 - 10))^(1/posterior_k)
eip_50_post = (posterior_dk*50/(100 - 50))^(1/posterior_k)
eip_90_post = (posterior_dk*90/(100 - 90))^(1/posterior_k)

# plotting eip posteriors with mean
# create dataframes
posterior_eip_10 <- data.frame(eip10 = eip_10_post, type = "eip10_post")
posterior_eip_50 <- data.frame(eip50 = eip_50_post, type = "eip50_post")
posterior_eip_90 <- data.frame(eip90 = eip_90_post, type = "eip90_post")

# plot eip distributions
eip10_plot <- ggplot() +
  geom_histogram(data = posterior_eip_10,
                 aes(x = eip10, y = after_stat(density)),
                 fill = "#6DB6FF", colour = "#6DB6FF",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(eip_10_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(EIP[10]), y = "Density") + 
  
  #xlim(0, 100) + ylim(0, 0.15) + 
  
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

eip50_plot <- ggplot() +
  geom_histogram(data = posterior_eip_50,
                 aes(x = eip50, y = after_stat(density)),
                 fill = "#6DB6FF", colour = "#6DB6FF",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(eip_50_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(EIP[50]), y = "Density") + 
  
  #xlim(0, 100) + ylim(0, 0.15) + 
  
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

eip90_plot <- ggplot() +
  geom_histogram(data = posterior_eip_90,
                 aes(x = eip90, y = after_stat(density)),
                 fill = "#6DB6FF", colour = "#6DB6FF",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(eip_90_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(EIP[90]), y = "Density") + 
  
  #xlim(0, 100) + ylim(0, 0.15) + 
  
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

# saving plots
ggsave("hills_20_eip10.pdf", plot = eip10_plot, width = 4, height = 3, dpi = 300)
ggsave("hills_20_eip50.pdf", plot = eip50_plot, width = 4, height = 3, dpi = 300)
ggsave("hills_20_eip90.pdf", plot = eip90_plot, width = 4, height = 3, dpi = 300)

# computing vc posteriors
vc_max_post = posterior_vc
vc_50_post = 0.5*posterior_vc

# plotting vc posteriors with mean
# create dataframes
posterior_vc_max <- data.frame(vcmax = vc_max_post, type = "vcmax_post")
posterior_vc_50 <- data.frame(vc50 = vc_50_post, type = "vc50_post")

# plot vc distributions
vcmax_plot <- ggplot() +
  geom_histogram(data = posterior_vc_max,
                 aes(x = vcmax, y = after_stat(density)),
                 fill = "#009292", colour = "#009292",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(vc_max_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(VC[max]), y = "Density") + 
  
  #xlim(0, 100) + ylim(0, 0.15) + 
  
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

vc50_plot <- ggplot() +
  geom_histogram(data = posterior_vc_50,
                 aes(x = vc50, y = after_stat(density)),
                 fill = "#009292", colour = "#009292",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(vc_50_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(VC[50]), y = "Density") + 
  
  #xlim(0, 100) + ylim(0, 0.15) + 
  
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

# saving plots
ggsave("hills_20_vcmax.pdf", plot = vcmax_plot, width = 4, height = 3, dpi = 300)
ggsave("hills_20_vc50.pdf", plot = vc50_plot, width = 4, height = 3, dpi = 300)


# computing mean and 95% HD intervals
mean_eip_10 = mean(eip_10_post)
median_eip_10 = median(eip_10_post)
hdi_eip_10 = hdi(eip_10_post)

mean_eip_50 = mean(eip_50_post)
median_eip_50 = median(eip_50_post)
hdi_eip_50 = hdi(eip_50_post)

mean_eip_90 = mean(eip_90_post)
median_eip_90 = median(eip_90_post)
hdi_eip_90 = hdi(eip_90_post)

mean_vc_max = mean(vc_max_post)
median_vc_max = median(vc_max_post)
hdi_vc_max = hdi(vc_max_post)

mean_vc_50 = mean(vc_50_post)
median_vc_50 = median(vc_50_post)
hdi_vc_50 = hdi(vc_50_post)


# saving mean and HD intervals to csv file
# Create a data frame
results <- data.frame(
  traits = c("EIP10", "EIP50", "EIP90", "VC50", "VCmax"),
  mean = c(mean_eip_10, mean_eip_50, mean_eip_90, mean_vc_50, mean_vc_max),
  median = c(median_eip_10, median_eip_50, median_eip_90, median_vc_50, median_vc_max),
  HDI_95_lower = c(hdi_eip_10[1], hdi_eip_50[1], hdi_eip_90[1], hdi_vc_50[1], hdi_vc_max[1]),
  HDI_95_upper = c(hdi_eip_10[2], hdi_eip_50[2], hdi_eip_90[2], hdi_vc_50[2], hdi_vc_max[2])
)

# Save the data frame to CSV
write.csv(results, file = "hills_20_eip_vc.csv", row.names = FALSE)