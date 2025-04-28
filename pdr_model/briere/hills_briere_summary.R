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
mean_q = mean(posterior_q)
median_q = median(posterior_q)

mean_T0 = mean(posterior_T0)
median_T0 = median(posterior_T0)

mean_Tm = mean(posterior_Tm)
median_Tm = median(posterior_Tm)

# 5. Extract 95% hdi interval
hdi_q = hdi(posterior_q)
hdi_T0 = hdi(posterior_T0)
hdi_Tm = hdi(posterior_Tm)

# saving mean and HD intervals to csv file
# Create a data frame
results <- data.frame(
  pars = c("DIC", "WAIC", "MD", "q", "T0", "Tm"),
  score = c(dic_value, waic_value, mean_deviance, NA, NA, NA),
  mean = c(NA, NA, NA, mean_q, mean_T0, mean_Tm),
  median = c(NA, NA, NA, median_q, median_T0, median_Tm),
  HDI_95_lower = c(NA, NA, NA, hdi_q[1], hdi_T0[1], hdi_Tm[1]),
  HDI_95_upper = c(NA, NA, NA, hdi_q[2], hdi_T0[2], hdi_Tm[2])
)

# Save the result_table as a CSV file
write.csv(results, file = "briere_pdr10_summary.csv", row.names = FALSE)