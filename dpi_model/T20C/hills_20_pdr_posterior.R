# first run the main code hills_18_code.R and keep outputs in the memory

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

# computing PDR posteiors
pdr_10_post = 1/eip_10_post
pdr_50_post = 1/eip_50_post
pdr_90_post = 1/eip_90_post

# plotting pdr posteriors with mean
# create dataframes
posterior_pdr_10 <- data.frame(pdr10 = pdr_10_post, type = "pdr10_post")
posterior_pdr_50 <- data.frame(pdr50 = pdr_50_post, type = "pdr50_post")
posterior_pdr_90 <- data.frame(pdr90 = pdr_90_post, type = "pdr90_post")

# plot pdr distributions
pdr10_plot <- ggplot() +
  geom_histogram(data = posterior_pdr_10,
                 aes(x = pdr10, y = after_stat(density)),
                 fill ="#DB6D00", colour = "#DB6D00",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(pdr_10_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(PDR[10]), y = "Density") + 
  
  #xlim(0, 2) + ylim(0, 5) + 
  
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

pdr50_plot <- ggplot() +
  geom_histogram(data = posterior_pdr_50,
                 aes(x = pdr50, y = after_stat(density)),
                 fill ="#DB6D00", colour = "#DB6D00",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(pdr_50_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(PDR[50]), y = "Density") + 
  
  #xlim(0, 2) + ylim(0, 5) + 
  
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

pdr90_plot <- ggplot() +
  geom_histogram(data = posterior_pdr_90,
                 aes(x = pdr90, y = after_stat(density)),
                 fill ="#DB6D00", colour = "#DB6D00",
                 alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = median(pdr_90_post), linetype = "dashed", color = "black") +
  
  labs(x = expression(PDR[90]), y = "Density") + 
  
  #xlim(0, 2) + ylim(0, 5) + 
  
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
ggsave("hills_20_pdr10.pdf", plot = pdr10_plot, width = 4, height = 3, dpi = 300)
ggsave("hills_20_pdr50.pdf", plot = pdr50_plot, width = 4, height = 3, dpi = 300)
ggsave("hills_20_pdr90.pdf", plot = pdr90_plot, width = 4, height = 3, dpi = 300)


# computing mean and 95% HD intervals
mean_pdr_10 = mean(pdr_10_post)
median_pdr_10 = median(pdr_10_post)
hdi_pdr_10 = hdi(pdr_10_post)

mean_pdr_50 = mean(pdr_50_post)
median_pdr_50 = median(pdr_50_post)
hdi_pdr_50 = hdi(pdr_50_post)

mean_pdr_90 = mean(pdr_90_post)
median_pdr_90 = median(pdr_90_post)
hdi_pdr_90 = hdi(pdr_90_post)


# saving mean and HD intervals to csv file
# Create a data frame
results <- data.frame(
  traits = c("PDR10", "PDR50", "PDR90"),
  mean = c(mean_pdr_10, mean_pdr_50, mean_pdr_90),
  median = c(median_pdr_10, median_pdr_50, median_pdr_90),
  HDI_95_lower = c(hdi_pdr_10[1], hdi_pdr_50[1], hdi_pdr_90[1]),
  HDI_95_upper = c(hdi_pdr_10[2], hdi_pdr_50[2], hdi_pdr_90[2])
)

# Save the data frame to CSV
write.csv(results, file = "hills_20_pdr.csv", row.names = FALSE)