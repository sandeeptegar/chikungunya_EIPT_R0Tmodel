# Assuming these objects are already in environment:
# PDR.Ae.albo.CHIKV.out, waic.eip
library('HDInterval')

# 1. Extract DIC from JAGS output
dic_value <-  PDR.Ae.albo.CHIKV.out$BUGSoutput$DIC

# 2. Extract WAIC
waic_value <- waic.eip[[1]]

# 3. Extract mean deviance
mean_deviance <- PDR.Ae.albo.CHIKV.out$BUGSoutput$summary["deviance",][[1]]

# 4. Extract mean and median
mean_A= mean(posterior_A)
median_A = median(posterior_A)

mean_k = mean(posterior_k)
median_k = median(posterior_k)

# 5. Extract 95% hdi interval
hdi_A = hdi(posterior_A)
hdi_k = hdi(posterior_k)

# saving mean and HD intervals to csv file
# Create a data frame
results <- data.frame(
  pars = c("DIC", "WAIC", "MD", "A", "k"),
  score = c(dic_value, waic_value, mean_deviance, NA, NA),
  mean = c(NA, NA, NA, mean_A, mean_k),
  median = c(NA, NA, NA, median_A, median_k),
  HDI_95_lower = c(NA, NA, NA, hdi_A[1], hdi_k[1]),
  HDI_95_upper = c(NA, NA, NA, hdi_A[2], hdi_k[2])
)

# Save the result_table as a CSV file
write.csv(results, file = "gillooly_eip90_summary.csv", row.names = FALSE)
