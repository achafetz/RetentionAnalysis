# Retention Analysis
# A.Chafetz, USAID
# Purpose: analyze community linkage
# Updated: 6/22/17 
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

#correlation
x <- select(df_global, tx_new,  cbcts_lnkg_exp)
cor(x)
#models
h2a <- lm(tx_new ~ cbcts_lnkg_exp, data=df_global)
h2b <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat , data=df_global)
h2c <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization), data=df_global)
h2d <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global)
h2e <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization) + factor(operatingunit), data=df_global)

#output
stargazer(h2a, h2b, h2c, h2d, h2e, type = "text")
stargazer(h2a, h2b, h2c, h2d, h2e, type = "html", out = "linkage_output.htm")

rm(df_global_h2, h2a, h2b, h2c, h2d, h2e)