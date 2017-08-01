# Retention Analysis
# A.Chafetz, USAID
# Purpose: analyze community retention
# Updated: 7/24/17 
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code


#load global file created
load(file.path(data, "df_global.RData"))

## RETENTION ##

#dataset (for logitistice model -> must be bound between 0<x<=1)
  print(nrow(filter(df_global,tx_ret_pct>1)))
  df_global_h1 <- filter(df_global, tx_ret_denom!=0, tx_ret_pct<=1)

#review IV distribution
  summary(df_global$tx_ret_pct)
#graph
  # retention spending
    ggplot(df_global, aes(cbcts_rtnadhr_exp)) + 
      geom_histogram() + 
      labs(x = "retention expenditures ($)", y ="frequency") +
      scale_x_continuous(labels = comma) + 
      scale_y_continuous(labels = comma)
  # retention histogram
    ggplot(df_global, aes(tx_ret_pct)) + 
      geom_histogram() + 
      labs(title = "Treatment retention histogram (PEPFAR FY16)", x = "treatment retention", y ="frequency") +
      scale_x_continuous(labels = scales::percent)
    #qqplot
      ggplot(df_global_h1, aes(sample=tx_ret_pct)) + stat_qq()
  #Ret Spending v RET scatter
    ggplot(df_global_h1, aes(cbcts_rtnadhr_exp, tx_ret_pct)) +
      labs(x="Comm. Ret/Adh. Spending", y="Tx Retention") +
      scale_y_continuous(labels = scales::percent) +
      geom_point(shape=1)
  #Log Ret Spending v RET scatter
    ggplot(df_global_h1, aes(ln_rtnadhr_exp, tx_ret_pct)) +
      labs(x="Log Comm. Ret/Adh. Spending", y="Tx Retention") +
      geom_point(shape=1)
  #Log Ret Spending v RET scatter w linear fit
    ggplot(df_global_h1, aes(ln_rtnadhr_exp, tx_ret_pct)) +
      labs(x="Log Comm. Ret/Adh. Spending", y="Tx Retention") +
      scale_y_continuous(labels = scales::percent) +
      geom_point(shape=1) +
      geom_smooth(method=lm, se=FALSE)
  #Ret spending per PLHIV histogram
    p1 <- ggplot(df_global_h1, aes(rtnadhr_exp_per_plhiv)) + 
      geom_histogram() + 
      labs(x = "retention expenditures per PLHIV ($)", y ="frequency") +
      scale_x_continuous(labels = comma) + 
      scale_y_continuous(labels = comma)
  #Log Ret Spending histogram
    p2 <- ggplot(df_global_h1, aes(ln_rtnadhr_exp)) + 
      geom_histogram() + 
      labs(x = "log comm. ret/adh. spending", y ="frequency")
    
    grid.arrange(p1, p2, ncol=2)
    
## RETENTION MODELS ##

#model comparison -> transformations and linear v logit

  #linear
    #raw exp
    h1a <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1, na.action = na.exclude)
    #exp per plhiv
    h1a2 <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_h1, na.action = na.exclude)
    #log exp
    h1a3 <- lm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global_h1, na.action = na.exclude)
  #logistic
    #raw exp
    h1f <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1, family=binomial(link="logit"), na.action=na.exclude)
    #exp per plhiv
    h1f2 <- glm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_h1, family=binomial (link="logit"), na.action=na.exclude)
    #log exp
    h1f3 <- glm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global_h1, family=binomial (link="logit"), na.action=na.exclude)
    #make sure linear predicted values are bound between 0 and 1
      summary(fitted(h1f))
      summary(fitted(h1f2))
      summary(fitted(h1f3))
  #print regression output
  stargazer(h1a, h1a2, h1a3, h1f, h1f2, h1f3, type = "text")
  #export regression output
  stargazer(h1a, h1a2, h1a3, h1f, h1f2, h1f3, type = "html", 
            dep.var.labels=c("treatment retention (%)"),
            covariate.labels=c("Community retention spending", "Comm. spending per PLHIV", "Log comm. spending"),
            out= file.path(output,"ret_out_comp.htm"))
  
  #remove stored values
    rm(h1a, h1a2, h1a3, h1f, h1f2, h1f3)
    
#does it matter if we only look at Standard coutnries?
df_global_st_h1 <- filter(df_global, tx_ret_denom!=0, tx_ret_pct<=1, designation=="Standard")
df_global_st <- filter(df_global, designation=="Standard")

#model comparison -> transformations and linear v logit

  #linear
    #raw exp
    h1sa <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_st, na.action = na.exclude)
    #exp per plhiv
    h1sa2 <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_st, na.action = na.exclude)
    #log exp
    h1sa3 <- lm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global_st, na.action = na.exclude)
  #logistic
    #raw exp
    h1sf <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_st_h1, family=binomial(link="logit"), na.action=na.exclude)
    #exp per plhiv
    h1sf2 <- glm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_st_h1, family=binomial (link="logit"), na.action=na.exclude)
    #log exp
    h1sf3 <- glm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global_st_h1, family=binomial (link="logit"), na.action=na.exclude)

  #print regression output
  stargazer(h1sa, h1sa2, h1sa3, h1sf, h1sf2, h1sf3, type = "text")

  #remove stored values and dfs
  rm(df_global_st, df_global_st_h1, h1sa, h1sa2, h1sa3, h1sf, h1sf2, h1sf3)


  
## LINEAR MODEL including control variables ##
  #lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1) %>% summary %>% tidy %>% kable(digits = 3, col.names = c("Param", "B", "SE", "t", "p"))
  h1a <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_h1, na.action = na.exclude)
  h1b <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv + plhiv + tx_curr_subnat, data=df_global_h1, na.action = na.exclude)
  h1c <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv + plhiv + tx_curr_subnat + nonscaleup, data=df_global_h1, na.action = na.exclude)
  h1d <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h1, na.action = na.exclude)
  h1e <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv + plhiv + tx_curr_subnat + nonscaleup + factor(operatingunit), data=df_global_h1, na.action = na.exclude)

#linear output
stargazer(h1a, h1b, h1c, h1d, h1e, type = "text")
stargazer(h1a, h1b, h1c, h1d, h1e, type = "html", 
  dep.var.labels=c("treatment retention (%)"),
  covariate.labels=c("Comm. spending per PLHIV", "PLHIV","Patients on Tx (nat'l)", "Non-scale up district"),
  out=file.path(output, "ret_out.htm"))


#plot model

  #add residuals and predicted values to df
  df_global_h1 <- df_global_h1 %>%
    mutate(h1e_resid = resid(h1e)) %>%
    mutate(h1e_predict = predict(h1e))
  
  #H1e - predicted v actual
  ggplot(df_global_h1, aes(h1e_predict, tx_ret_pct)) +
    labs(title= "TX_RET - Predicted v Actual", x="predicted", y="actual") +
    geom_point(shape=1) + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)
  
  #H1e - predicted v residuals
  ggplot(df_global_h1, aes(h1e_predict, h1e_resid)) +
    labs(title= "TX_RET - Predicted v Residuals", x="predicted", y="residuals") +
    geom_point(shape=1) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_y_continuous(labels = scales::percent)

  #H1e - residual plot
  ggplot(df_global_h1, aes(rtnadhr_exp_per_plhiv, h1e_resid)) +
    labs(title= "TX_RET Residuals", x="Ret/Adh. Spending per PLHIV", y="Residuals (H1e)") +
    geom_point(shape=1) + 
    geom_hline(yintercept = 0, linetype="dashed")
  
  ggplot(df_global_h1, aes(sample = residuals(h1e))) + stat_qq()
        
#remove values
  rm(df_global_h1, h1a, h1b, h1c, h1d, h1e)
