# Retention Analysis
# A.Chafetz, USAID
# Purpose: analyze community linkage
# Updated: 6/28/17 
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code


## DEPENDENT LIBRARIES ##
library("plyr")
library("tidyverse")
library("readxl")
library("readr")
library("ggplot2")
library("broom")
library("knitr")
library("stargazer")
library("scales")

#load global file created
setwd("C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data")
load("df_global.RData")

## LINKAGE MODELS ##

#review IV distribution
summary(df_global$tx_new)

#remove districts with no TX_NEW
  print(nrow(filter(df_global,tx_new==0))) #main dataset
  df_global_h2 <- filter(df_global, tx_new!=0)

#graph
# retention histogram
  ggplot(df_global_h2, aes(tx_new)) + 
    geom_histogram() + 
    labs(title = "New on treatment histogram (PEPFAR FY16)", x = "Patients new treatment", y ="frequency")

#models
h2a <- lm(tx_new ~ ln_lnkg_exp, data=df_global_h2)
h2b <- lm(tx_new ~ ln_lnkg_exp + plhiv + tx_curr_subnat , data=df_global_h2)
h2c <- lm(tx_new ~ ln_lnkg_exp + plhiv + tx_curr_subnat + scaleup, data=df_global_h2)
h2d <- lm(tx_new ~ ln_lnkg_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h2)
h2e <- lm(tx_new ~ ln_lnkg_exp + plhiv + tx_curr_subnat + scaleup + factor(operatingunit), data=df_global_h2)

#output
stargazer(h2a, h2b, h2c, h2d, h2e, type = "text")
stargazer(h2a, h2b, h2c, h2d, h2e, type = "html", out = "linkage_output.htm")


#plot model

#add residuals and predicted values to df
df_global <- df_global_h2 %>%
  mutate(h2e_resid = resid(h2e)) %>%
  mutate(h2e_predict = predict(h2e))

#H2e - predicted v actual
ggplot(df_global_h2, aes(h2e_predict, tx_new)) +
  labs(title= "TX_New - Predicted v Actual", x="predicted", y="actual") +
  geom_point(shape=1) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)

#H2e - predicted v residuals
ggplot(df_global_h2, aes(h2e_predict, h2e_resid)) +
  labs(title= "TX_NEW - Predicted v Residuals", x="predicted", y="residuals") +
  geom_point(shape=1) + 
  geom_hline(yintercept = 0, linetype="dashed")
scale_y_continuous(labels = scales::percent)

#H2e - residual plot
ggplot(df_global_h2, aes(tx_new, h2e_resid)) +
  labs(title= "TX_New Residuals", x="TX_NEW", y="Residuals (H2e)") +
  geom_point(shape=1) + 
  geom_hline(yintercept = 0, linetype="dashed")


rm(df_global_h2, h2a, h2b, h2c, h2d, h2e)