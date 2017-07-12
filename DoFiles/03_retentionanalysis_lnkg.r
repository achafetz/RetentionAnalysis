# Retention Analysis
# A.Chafetz, USAID
# Purpose: analyze community linkage
# Updated: 7/6/17 
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code


#load global file created
load(paste0(data, "df_global.RData"))

## LINKAGE MODELS ##

#review DV distribution
summary(df_global$tx_new)

#review linakge spending distribution
summary(df_global$cbcts_lnkg_exp)

#remove districts with no TX_NEW
  print(nrow(filter(df_global,tx_new==0))) #main dataset
  df_global_h2 <- df_global

#graph
# retention histogram
  ggplot(df_global_h2, aes(tx_new)) + 
    geom_histogram() + 
    labs(x = "Patients new treatment", y ="frequency")
  p1 <- ggplot(df_global_h2, aes(cbcts_lnkg_exp)) + 
    geom_histogram() + 
    labs(x= "Community linkage expenditures", y="frequency") +
    scale_x_continuous(labels = comma)
  p2 <- ggplot(df_global_h2, aes(ln_lnkg_exp)) + 
    geom_histogram() + 
    labs(x= "Log community linkage expenditures", y="frequency")

  grid.arrange(p1, p2, ncol=2) #two plotted together
  
#models
h2a <- lm(proxy_lnkg~ ln_lnkg_exp, data=df_global_h2)
h2b <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat, data=df_global_h2, na.action = na.exclude)
h2c <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup, data=df_global_h2, na.action = na.exclude)
h2d <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h2, na.action = na.exclude)
h2e <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup + factor(operatingunit), data=df_global_h2, na.action = na.exclude)

#output
stargazer(h2a, h2b, h2c, h2d, h2e, type = "text")
stargazer(h2a, h2b, h2c, h2d, h2e, type = "html", out = paste0(output, "linkage_output.htm"))


#plot model

#add residuals and predicted values to df
df_global_h2 <- df_global_h2 %>%
  mutate(h2e_resid = resid(h2e)) %>%
  mutate(h2e_predict = predict(h2e))

#H2e - predicted v actual
ggplot(df_global_h2, aes(h2e_predict, tx_new)) +
  labs(title= "TX_New - Predicted v Actual", x="predicted", y="actual") +
  geom_point(shape=1) + 
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(labels = comma)

#H2e - predicted v residuals
ggplot(df_global_h2, aes(h2e_predict, h2e_resid)) +
  labs(title= "TX_NEW - Predicted v Residuals", x="predicted", y="residuals") +
  geom_point(shape=1) + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(labels = comma)

#H2e - residual plot
ggplot(df_global_h2, aes(tx_new, h2e_resid)) +
  labs(title= "TX_New Residuals", x="TX_NEW", y="Residuals (H2e)") +
  geom_point(shape=1) + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(labels = comma)


rm(df_global_h2, h2a, h2b, h2c, h2d, h2e)

#review countries with large community spending

#load global file created
load(paste0(data, "df_global.RData"))

#select countries
ctry <- c("Tanzania", "Uganda", "South Africa")

for (k in ctry) {
  
  print(k)
  #filter
  df_ctry <- df_global %>%
    filter(operatingunit== k, !is.nan(proxy_lnkg) )  
  
  #models
  h2a <- lm(proxy_lnkg ~ ln_lnkg_exp, data=df_ctry, na.action = na.exclude)
  h2b <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat, data=df_ctry, na.action = na.exclude)
  h2c <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup, data=df_ctry, na.action = na.exclude)
  
  #output
  stargazer(h2a, h2b, h2c, type = "text")
  stargazer(h2a, h2b, h2c, type = "html", out = paste0(output, k, "_linkage_output.htm"))
  
  rm(df_ctry_h2, h2a, h2b, h2c)
}
