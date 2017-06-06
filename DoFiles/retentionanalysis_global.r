# Retention Analysis R Script
# DRAFT
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code

## DEPENDENT LIBRARIES ##
library("tidyverse")
library("readxl")
library("readr")
library("ggplot2")
library("broom")
library("knitr")
library("stargazer")
library("scales")

# FACT VIEW DATA ##

#import data and call the dataframe factviewdata
  setwd("C:/Users/achafetz/Documents/ICPI/Data/")
  fvdata <- read_delim("ICPI_FactView_PSNU_20170515_v1_1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# change all header names to lower case to make it easier to use
  names(fvdata) <- tolower(names(fvdata))

# subset fv to only & key indicators
  df_mer <- fvdata %>%
      filter(indicator %in% c("TX_RET","TX_NEW"), disaggregate %in% c("Total Denominator", "Total Numerator")) %>%
      select(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid, fy16snuprioritization, indicator,numeratordenom, fy2016apr)

#aggregate for reshape
  df_mer <- df_mer %>%
      group_by(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid, fy16snuprioritization, indicator, numeratordenom) %>%
      summarize_each(funs(sum(., na.rm=TRUE)), fy2016apr) %>%
      ungroup %>%
      
      #reshape by N/D
      spread(numeratordenom, fy2016apr) %>%
      #reshape by indicator
      spread(indicator, N) %>%
      #rename variables
      rename(tx_ret_denom = D) %>%
      rename(tx_ret_num = TX_RET) %>%
      rename(tx_new = TX_NEW) %>%
      
      #aggregate so just one line per psnu
      group_by(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid, fy16snuprioritization) %>%
      summarize_each(funs(sum(., na.rm=TRUE)), tx_ret_denom, tx_ret_num, tx_new) %>%
      ungroup %>%
      
      #create a retention variable
      mutate(tx_ret_pct = round(tx_ret_num/tx_ret_denom, 3)) %>%
    
      #reorder
      select(operatingunit:tx_ret_num, tx_ret_pct, tx_new) %>%
      
      #adjust psnu/psnuuid to snu1/snu1uid if EA was collected at higher level of the hierarchy
      mutate(psnu = 
                 ifelse(operatingunit %in% 
                            c("Nigeria", "Democratic Republic of the Congo", 
                              "Ethiopia", "Burma", "India", "South Sudan"), snu1, psnu))  %>%
      
      mutate(psnuuid = 
                 ifelse(operatingunit %in% 
                            c("Nigeria", "Democratic Republic of the Congo", 
                              "Ethiopia", "Burma", "India", "South Sudan"), snu1uid, psnuuid)) %>%
      
      #remove Burundi column with missing data and no psnuuid
      subset(!is.na(psnuuid))

#drop fv dataset
  rm(fvdata)

## IMPATT DATA ##

#import Nat/SubNat data
  impattdata <- read_delim("ICPI_FactView_NAT_SUBNAT_20170515_v1_1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# change all header names to lower case to make it easier to use
  names(impattdata) <- tolower(names(impattdata))

# subset fv to Bots only & key indicators
  df_impattdata <- impattdata %>%
      mutate(psnuuid = 
                 ifelse(operatingunit %in% 
                            c("Nigeria", "Democratic Republic of the Congo", 
                              "Ethiopia", "Burma", "India", "South Sudan"), snu1uid, psnuuid)) %>%
      filter(indicator %in% c("PLHIV (SUBNAT)", "TX_CURR_SUBNAT"), disaggregate == "Total Numerator", psnuuid!="") %>%
      select(psnuuid, indicator, fy2016) %>%
      
      #aggregate so just one line per psnu
      group_by(psnuuid, indicator) %>%
      summarize_each(funs(sum(., na.rm=TRUE)), fy2016) %>%
      ungroup

#remove original dataset
  rm(impattdata)

#reshape wide
  df_impattdata <- spread(df_impattdata, indicator, fy2016)
  
#rename column headers
  names(df_impattdata)[names(df_impattdata) == 'PLHIV (SUBNAT)'] <- 'plhiv'
  names(df_impattdata)[names(df_impattdata) == 'TX_CURR_SUBNAT'] <- 'tx_curr_subnat'


## EA DATA NAV DATA ##

#import EA data from Botswana data nav tool
  setwd("C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data")
  df_ea <- read_csv("2014-2016 allcntry SAS Output 24JAN17.csv")

# change all header names to lower case to make it easier to use
  names(df_ea) <- tolower(names(df_ea))

#subset
  df_ea <- df_ea %>%
      filter(rptgcycle==2016 & data_type=="De-Dup") %>%
      select(ou, national_sub_unit, national_sub_sub_unit,datim_snu_id, cbcts_lnkg_exp, cbcts_rtnadhr_exp, fbcts_loaded_tot)

#rename for merge
  df_ea <- rename(df_ea, ea_districts = national_sub_unit)
  df_ea <- rename(df_ea, psnuuid = datim_snu_id)
  
  df_global <- full_join(df_mer, df_ea, by="psnuuid")
  df_global <- full_join(df_global, df_impattdata, by="psnuuid")

#remove intermediate dfs
  rm(df_ea, df_impattdata, df_mer)

  
## RETENTION GRAPHS ##

#dataset  
  df_global_h1 <- filter(df_global, is.finite(tx_ret_pct), tx_ret_denom!=0, tx_ret_pct<=1, is.finite(cbcts_rtnadhr_exp))

#Ret by OU
  df_global_ou <- df_global_h1 %>%
    select(operatingunit, tx_ret_denom, tx_ret_num, plhiv) %>%
    group_by(operatingunit) %>%
    summarize_each(funs(sum(., na.rm=TRUE))) %>%
    ungroup %>%
    mutate(tx_ret_pct = round(tx_ret_num/tx_ret_denom,3))
  
  ggplot(df_global_ou, aes(reorder(operatingunit, tx_ret_pct), tx_ret_pct)) +
    labs(x = "Operating Unit", y ="Tx Retention") +
    scale_y_continuous(labels = scales::percent) +
    geom_bar(stat="identity") + 
    coord_flip()
  
  ggplot(df_global_ou, aes(log(plhiv), tx_ret_pct)) +
    labs(x="Log PLHIV (in PEPFAR supported areas)", y="Tx Retention") +
    scale_y_continuous(labels = scales::percent) +
    geom_point(shape=1)

v
  
#Ret Spending v RET scatter
  ggplot(df_global_h1, aes(cbcts_rtnadhr_exp, tx_ret_pct)) +
    labs(x="Comm. Ret/Adh. Spending", y="Tx Retention") +
    scale_y_continuous(labels = scales::percent) +
    geom_point(shape=1)
#Ret Spending v RET scatter by OU
  ggplot(df_global_h1, aes(log(cbcts_rtnadhr_exp), tx_ret_pct)) +
    labs(x="Log Comm. Ret/Adh. Spending", y="Tx Retention") +
    scale_y_continuous(labels = scales::percent) +
    geom_point(shape=1) + 
    facet_wrap(~operatingunit)
#Log Ret Spending v RET scatter
  ggplot(df_global_h1, aes(log(cbcts_rtnadhr_exp), tx_ret_pct)) +
    labs(x="Log Comm. Ret/Adh. Spending", y="Tx Retention") +
    geom_point(shape=1)
#Log Ret Spending v RET scatter w linear fit
  ggplot(df_global_h1, aes(log(cbcts_rtnadhr_exp), tx_ret_pct)) +
    labs(x="Log Comm. Ret/Adh. Spending", y="Tx Retention") +
    scale_y_continuous(labels = scales::percent) +
    geom_point(shape=1) +
    geom_smooth(method=lm, se=FALSE)


## RETENTION MODELS ##
#correlation
  x <- select(df_global_h1, tx_ret_pct, cbcts_rtnadhr_exp)
  cor(x)
#models
  #lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1) %>% summary %>% tidy %>% kable(digits = 3, col.names = c("Param", "B", "SE", "t", "p"))
  h1a <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1)
    summary(h1a)
  h1b <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat, data=df_global_h1)
    summary(h1b)
  h1c <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization), data=df_global_h1)
    summary (h1c)
  h1d <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h1)
    summary (h1d)
  h1e <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization) + factor(operatingunit), data=df_global_h1)
    summary (h1e)
    
#output
stargazer(h1a, h1b, h1c, h1d, h1e, type = "text")
rm(df_global_h1, df_global_ou, h1a, h1b, h1c, h1d, h1e, x)


## LINKAGE GRAPHS ##

#dataset
  df_global_h2 <- filter(df_global, is.finite(tx_new), is.finite(cbcts_lnkg_exp))

#Linkage by OU
  df_global_ou <- df_global_h2 %>%
    select(operatingunit, tx_new, cbcts_lnkg_exp, plhiv) %>%
    group_by(operatingunit) %>%
    summarize_each(funs(sum(., na.rm=TRUE))) %>%
    ungroup
  
  ggplot(df_global_ou, aes(reorder(operatingunit, tx_new), tx_new)) +
    labs(x = "Operating Unit", y ="New on Tx") +
    scale_y_continuous(labels = comma) +
    geom_bar(stat="identity") + 
    coord_flip()
  
  ggplot(df_global_ou, aes(log(plhiv), tx_new)) +
    labs(x="Log PLHIV (in PEPFAR supported areas)", y="New on Tx") +
    scale_y_continuous(labels = comma) +
    geom_point(shape=1)
  
#Linkage spending histogram
  ggplot(df_global_h2, aes(cbcts_lnkg_exp)) + 
    labs(x="Comm. Linkage to Tx Spending", y="Frequency") +
    geom_histogram()
  
#Linkage v TX New scatter
  ggplot(df_global_h2, aes(cbcts_lnkg_exp, tx_new)) +
    labs(x="Comm. Linkage to Tx Spending", y="New on Tx Retention") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    geom_point(shape=1) +
    geom_smooth(method=lm, se=FALSE)
  
## LINKAGE MODELS ##

#correlation
  x <- select(df_global_h2, tx_new,  cbcts_lnkg_exp)
  cor(x)
#models
  #lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1) %>% summary %>% tidy %>% kable(digits = 3, col.names = c("Param", "B", "SE", "t", "p"))
  h2a <- lm(tx_new ~ cbcts_lnkg_exp, data=df_global_h2)
    summary(h2a)
  h2b <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat , data=df_global_h2)
    summary(h2b)
  h2c <-  lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization), data=df_global_h2)
    summary(h2c)
  h2d <-  lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global_h2)
    summary(h2d)
  h2e <-  lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization) + factor(operatingunit), data=df_global_h2)
    summary(h2e)

#output
    stargazer(h2a, h2b, h2c, h2d, h2e, type = "text")

    
    
## EXPORT DATA ###
  write.csv(df_global, "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data/ret_global.csv", na="")
    
  write.csv(df_global_h1, "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data/ret_global.csv", na="")


