# summary: DIC, WAIC, MD, mean, median and HDI

library('HDInterval')

# 1. Extract DIC from JAGS output
dic_value <- hills.pi.20.out$BUGSoutput$DIC

# 2. Extract WAIC
waic_value <- waic.hills20[[1]]

# 3. Extract mean deviance
mean_deviance <- hills.pi.20.out$BUGSoutput$summary["deviance",][[1]]

# 4. Extract mean and median
mean_vc = mean(posterior_vc)
median_vc = median(posterior_vc)

mean_k = mean(posterior_k)
median_k = median(posterior_k)

mean_d50 = mean(posterior_d50)
median_d50 = median(posterior_d50)

# 5. Extraxt 95% hdi interval
hdi_vc = hdi(posterior_vc)
hdi_k = hdi(posterior_k)
hdi_d50 = hdi(posterior_d50)

# saving mean and HD intervals to csv file
# Create a data frame
results <- data.frame(
  pars = c("DIC", "WAIC", "MD", "k", "vc", "d50"),
  score = c(dic_value, waic_value, mean_deviance, NA, NA, NA),
  mean = c(NA, NA, NA, mean_k, mean_vc, mean_d50),
  median = c(NA, NA, NA, median_k, median_vc, median_d50),
  HDI_95_lower = c(NA, NA, NA, hdi_k[1], hdi_vc[1], hdi_d50[1]),
  HDI_95_upper = c(NA, NA, NA, hdi_k[2], hdi_vc[2], hdi_d50[2])
)

# Save the result_table as a CSV file
write.csv(results, file = "hills_20_summary.csv", row.names = FALSE)
