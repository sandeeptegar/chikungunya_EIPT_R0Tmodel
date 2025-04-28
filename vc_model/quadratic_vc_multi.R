# Add Tohama font
library(showtext)
font_add("Tahoma", "font/tahoma.ttf")  # Replace with the actual file path
showtext_auto()

# run logis_vc.R file for vc_50 and save the plot output as
plot_fitted_50 <- plot_fitted
plot_data_points_50 <- plot_data_points
plot_fitted_50$type = "VC50"

# re-run logis_vc.R file for vc_max and save the plot output as (don't run above two line)
plot_fitted_max <- plot_fitted
plot_data_points_max <- plot_data_points
plot_fitted_max$type = "VCmax"


# rbind the plot_fitted dataframes and add data points to it

predicted <- rbind(plot_fitted_50, plot_fitted_max)

data_points <- rbind(plot_data_points_50, plot_data_points_max)

fitted_vcs <- ggplot() +
  geom_point(data = plot_data_points_50, aes(x = x, y = y, color = "VC50"), size = 1) +
  geom_errorbar(data = plot_data_points_50, aes(x = x, ymin = yl, ymax = yu, color = "VC50"), 
                width = 0.1, alpha = 0.6) +
  
  geom_point(data = plot_data_points_max, aes(x = x, y = y, color = "VCmax"), size = 1) +
  geom_errorbar(data = plot_data_points_max, aes(x = x, ymin = yl, ymax = yu, color = "VCmax"), 
                width = 0.1, alpha = 0.6) +
  
  geom_line(data = plot_fitted_50, aes(x = x_seq, y = fitted, color = "VC50"), size = 0.5) +
  geom_ribbon(data = plot_fitted_50, aes(x = x_seq, ymin = lower, ymax = upper, fill = "VC50"), alpha = 0.2) +
  
  geom_line(data = plot_fitted_max, aes(x = x_seq, y = fitted, color = "VCmax"), size = 0.5) +
  geom_ribbon(data = plot_fitted_max, aes(x = x_seq, ymin = lower, ymax = upper, fill = "VCmax"), alpha = 0.2) +
  
  scale_fill_manual(name = "Quadratic", values = c("VC50" = "#0072B2", "VCmax" = "#E69F00")) +
  scale_color_manual(name = "Quadratic", values = c("VC50" = "#0072B2", "VCmax" = "#E69F00")) +
  
  labs(title = " ",
       x = "Temperature (\u00b0C)", y = "Proportion") + ylim(0, 1.5) +
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
    legend.position = c(0.85, 0.75)
  )

fitted_vcs

ggsave("quadratic_vcs.pdf", plot = fitted_vcs, width = 4, height = 3, dpi = 300)