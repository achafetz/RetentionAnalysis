# Retention Analysis
# A.Chafetz, USAID
# Purpose: analyze community linkage
# Updated: 7/24/17 
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code


#load global file created
load(file.path(data, "df_global.RData"))

## LINKAGE MODELS ##

#review DV distribution
summary(df_global$tx_new)
summary(df_global$proxy_lnkg)

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
h2a <- lm(proxy_lnkg ~ ln_lnkg_exp, data=df_global_h2)
h2b <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat, data=df_global_h2, na.action = na.exclude)
h2c <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup, data=df_global_h2, na.action = na.exclude)
h2d <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h2, na.action = na.exclude)
h2e <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup + factor(operatingunit), data=df_global_h2, na.action = na.exclude)

#output
stargazer(h2a, h2b, h2c, h2d, h2e, type = "text")
stargazer(h2a, h2b, h2c, h2d, h2e, type = "html", 
          covariate.labels=c("Log comm. spending", "PLHIV","Patients on Tx (nat'l)", "Non-scale up district"),
          out = file.path(output, "linkage_output.htm"))

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
  
  df_global %>%
    select(operatingunit, cbcts_lnkg_exp) %>%
    group_by(operatingunit) %>%
    summarise_at(vars(cbcts_lnkg_exp), funs(sum(., na.rm = TRUE))) %>%
    arrange(desc(cbcts_lnkg_exp)) 
    #kable(format.args = list(big.mark = ","))

  #filter
  df_zmb <- df_global %>% filter(operatingunit== "Zambia", !is.nan(proxy_lnkg))
  df_tnz <- df_global %>% filter(operatingunit== "Tanzania", !is.nan(proxy_lnkg))
  df_zaf <- df_global %>% filter(operatingunit== "South Africa", !is.nan(proxy_lnkg))
  
  #models
  h2_zmb <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup, data=df_zmb, na.action = na.exclude)
  h2_tnz <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup, data=df_tnz, na.action = na.exclude)
  h2_zaf <- lm(proxy_lnkg ~ ln_lnkg_exp + plhiv + tx_curr_subnat + nonscaleup, data=df_zaf, na.action = na.exclude)
  
  #output
  stargazer(h2_zmb, h2_tnz, h2_zaf, type = "text")
  stargazer(h2_zmb, h2_tnz, h2_zaf, type = "html",
            column.labels = c("ZMB", "TNZ", "ZAF"),
            covariate.labels = c("Log comm. spending", "PLHIV","Patients on Tx (nat'l)", "Non-scale up district"),
            out = file.path(output, "ctry_linkage_output.htm"))
  
  rm(df_ctry_h2, h2a, h2b, h2c)

