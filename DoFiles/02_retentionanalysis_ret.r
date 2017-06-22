# Retention Analysis
# A.Chafetz, USAID
# Purpose: analyze community retention
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

## RETENTION ##

#dataset (for logitistice model -> must be bound between 0<x<=1)
df_global_h1 <- filter(df_global, tx_ret_denom!=0, tx_ret_pct<=1)

#how many rows with zeros or 1s
  print(nrow(filter(df_global,tx_ret_denom==0 | tx_ret_pct==1))) #main dataset
  print(nrow(filter(df_global_h1,tx_ret_denom==0 | tx_ret_pct==1))) #filtered dataset
#review IV distribution
  summary(df_global$tx_ret_pct)
  ggplot(df_global, aes(tx_ret_pct)) + 
    geom_histogram() + 
    labs(title = "Treatment retention histogram (PEPFAR FY16)", x = "treatment retention", y ="frequency") +
    scale_x_continuous(labels = scales::percent)

## RETENTION MODELS ##

#model comparison -> transformations and linear v logit

h1a <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global, na.action = na.exclude)
  summary(fitted(h1a)) #are predicted values between 0 and 1?
h1a2 <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global, na.action = na.exclude)
  summary(fitted(h1a2)) #are predicted values between 0 and 1?
h1a3 <- lm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global, na.action = na.exclude)
  summary(fitted(h1a3)) #are predicted values between 0 and 1?
h1f <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1, family=binomial(link="logit"), na.action=na.exclude)
h1f2 <- glm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_h1, family=binomial (link="logit"), na.action=na.exclude)
h1f3 <- glm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global_h1, family=binomial (link="logit"), na.action=na.exclude)

stargazer(h1a, h1a2, h1a3, h1f, h1f2, h1f3, type = "text")




df_global_h1 <- filter(df_global, tx_ret_denom!=0, tx_ret_pct<=1, designation=="Standard")
df_global <- filter(df_global, designation=="Standard")


stargazer(h1a, h1a2, h1a3, h1f, h1f2, h1f3, type = "html", 
          dep.var.labels=c("treatment retention (%)"),
          covariate.labels=c("Community retention spending", "Comm. spending per PLHIV", "Log comm. spending"),
          out="ret_out_comp.htm")

#correlation
x <- select(df_global_h1, tx_ret_pct, cbcts_rtnadhr_exp)
cor(x)
#linear models
#lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1) %>% summary %>% tidy %>% kable(digits = 3, col.names = c("Param", "B", "SE", "t", "p"))
h1a <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1)
h1b <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat, data=df_global_h1)
h1c <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization), data=df_global_h1)
h1d <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h1)
h1e <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization) + factor(operatingunit), data=df_global_h1)

#linear output
stargazer(h1a, h1b, h1c, h1d, h1e, type = "text")
stargazer(h1a, h1b, h1c, h1d, h1e, type = "html", out="ret_out.htm")
rm(df_global_ou, h1a, h1b, h1c, h1d, h1e, x)


#logisitc models
h1f <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1, family=binomial(link="logit"))
h1f2 <- glm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_h1, family=quasibinomial (link="logit"), na.action=na.omit) %>% summary()
h1f3 <- glm(tx_ret_pct ~ log(rtnadhr_exp_per_plhiv), data=df_global_h1, family=binomial(link="logit")) %>% summary()
h1g <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat, data=df_global_h1, family=binomial(link="logit"))
h1h <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization), data=df_global_h1, family=binomial(link="logit"))
h1i <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h1, family=binomial(link="logit"))
h1j <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization) + factor(operatingunit), data=df_global_h1, family=binomial(link="logit"))

#logistic output
stargazer(h1f, h1f2, h1f3, type = "text")
stargazer(h1f, h1g, h1h, h1i, h1j, type = "text")
stargazer(h1f, h1g, h1h, h1i, h1j, type = "html", out="ret_out.htm")
rm(df_global_h1, h1f, h1g, h1h, h1i, h1j)


