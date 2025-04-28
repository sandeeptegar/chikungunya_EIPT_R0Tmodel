# Add Tohama font
library(showtext)
font_add("Tahoma", "font/tahoma.ttf")  # Replace with the actual file path
showtext_auto()

# run hills_briere.R file for eip10 and save the plot output as
plot_fitted_10 <- plot_fitted
plot_data_points_10 <- plot_data_points
plot_fitted_10$type = "PDR10"

# re-run hills_briere.R file for eip50 and save the plot output as (don't run above lines)
plot_fitted_50 <- plot_fitted
plot_data_points_50 <- plot_data_points
plot_fitted_50$type = "PDR50"

# re-run hills_briere.R file for eip90 and save the plot output as (don't run above lines)
plot_fitted_90 <- plot_fitted
plot_data_points_90 <- plot_data_points
plot_fitted_90$type = "PDR90"


# rbind the plot_fitted dataframes and add data points to it

predicted <- rbind(plot_fitted_10, plot_fitted_50, plot_fitted_90)

data_points <- rbind(plot_data_points_10, plot_data_points_50, plot_data_points_90)

fitted_eips <- ggplot() +
  geom_point(data = plot_data_points_10, aes(x = x, y = y, color = "PDR10"), size = 1) +
  geom_errorbar(data = plot_data_points_10, aes(x = x, ymin = yl, ymax = yu, color = "PDR10"), 
                width = 0.1, alpha = 0.6) +
  
  geom_point(data = plot_data_points_50, aes(x = x, y = y, color = "PDR50"), size = 1) +
  geom_errorbar(data = plot_data_points_50, aes(x = x, ymin = yl, ymax = yu, color = "PDR50"), 
                width = 0.1, alpha = 0.6) +
  
  geom_point(data = plot_data_points_90, aes(x = x, y = y, color = "PDR90"), size = 1) +
  geom_errorbar(data = plot_data_points_90, aes(x = x, ymin = yl, ymax = yu, color = "PDR90"), 
                width = 0.1, alpha = 0.6) +
  
  geom_line(data = plot_fitted_10, aes(x = x_seq, y = fitted, color = "PDR10"), size = 0.5) +
  geom_ribbon(data = plot_fitted_10, aes(x = x_seq, ymin = lower, ymax = upper, fill = "PDR10"), alpha = 0.2) +
  
  geom_line(data = plot_fitted_50, aes(x = x_seq, y = fitted, color = "PDR50"), size = 0.5) +
  geom_ribbon(data = plot_fitted_50, aes(x = x_seq, ymin = lower, ymax = upper, fill = "PDR50"), alpha = 0.2) +
  
  geom_line(data = plot_fitted_90, aes(x = x_seq, y = fitted, color = "PDR90"), size = 0.5) +
  geom_ribbon(data = plot_fitted_90, aes(x = x_seq, ymin = lower, ymax = upper, fill = "PDR90"), alpha = 0.2) +
  
  scale_fill_manual(name = "Briere",
                    values = c("PDR10" = "#0072B2", "PDR50" = "#E69F00", "PDR90" = "#D55E00")) +
  scale_color_manual(name = "Briere",
                     values = c("PDR10" = "#0072B2", "PDR50" = "#E69F00", "PDR90" = "#D55E00")) +
  
  labs(title = " ",
       x = "Temperature (\u00b0C)", y = expression(bold("PDR") * bold((day^-1)))) +
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
    legend.position = c(0.2, 0.7)
  )

fitted_eips
ggsave("briere_pdrs.pdf", plot = fitted_eips, width = 4, height = 3, dpi = 300)