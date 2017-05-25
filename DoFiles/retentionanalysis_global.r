# Retention Analysis R Script
# DRAFT
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code

## DEPENDENT LIBRARIES ##
library("tidyverse")
library("readxl")
library("readr")

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
    select(operatingunit:tx_ret_num, tx_ret_pct, tx_new)
    
    #adjust psnu/psnuuid to snu1/snu1uid if EA was collected at higher level of the hierarchy
    mutate(psnu = 
           ifelse(operatingunit %in% 
                    c("Nigeria", "Democratic Republic of the Congo", 
                      "Ethiopia", "Burma", "India"), snu1, psnu))  
  
    mutate(psnuuid = 
             ifelse(operatingunit %in% 
                      c("Nigeria", "Democratic Republic of the Congo", 
                        "Ethiopia", "Burma", "India"), snu1uid, psnuuid))
  #drop fv dataset
  rm(fvdata)
  
  

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
  
  
  #compare districts
  df_districts <- select(df_global, psnuuid, countryname, snu1, psnu, ea_districts, national_sub_sub_unit, ou)
 
  write.table(df_districts, "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Documents/districts.txt", sep = "\t")
  
  
  df_test <- filter(df_ea, operatingunit == "Ghana")
  