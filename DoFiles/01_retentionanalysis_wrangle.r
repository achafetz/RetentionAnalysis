# Retention Analysis
# A.Chafetz, USAID
# Purpose: import and wrangle input datasets
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

# FACT VIEW DATA ##

#import data and call the dataframe factviewdata
  setwd("C:/Users/achafetz/Documents/ICPI/Data/")
  fvdata <- read_delim("ICPI_FactView_PSNU_20170515_v1_1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# change all header names to lower case to make it easier to use
  names(fvdata) <- tolower(names(fvdata))

# subset fv to only & key indicators
  df_mer <- fvdata %>%
    filter(indicator %in% c("TX_RET","TX_NEW"), disaggregate %in% c("Total Denominator", "Total Numerator")) %>%
    select(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid, fy16snuprioritization, indicator, numeratordenom, fy2016apr)

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
    
    #adjust psnu/psnuuid to snu1/snu1uid if EA was collected at higher level of the hierarchy & remove prioritization
    mutate(psnu = 
             ifelse(operatingunit %in% 
                      c("Nigeria", "Democratic Republic of the Congo", 
                        "Ethiopia", "Burma", "India", "South Sudan"), snu1, psnu))  %>%
    mutate(psnuuid = 
             ifelse(operatingunit %in% 
                      c("Nigeria", "Democratic Republic of the Congo", 
                        "Ethiopia", "Burma", "India", "South Sudan"), snu1uid, psnuuid)) %>%
    mutate(fy16snuprioritization = 
             ifelse(operatingunit %in% 
                      c("Nigeria", "Democratic Republic of the Congo", 
                        "Ethiopia", "Burma", "India", "South Sudan"), NA, fy16snuprioritization)) %>%
    
    #aggregate so just one line per psnu
    group_by(operatingunit, countryname, snu1, snu1uid, psnu, psnuuid, fy16snuprioritization) %>%
    summarize_each(funs(sum(., na.rm=TRUE)), tx_ret_denom, tx_ret_num, tx_new) %>%
    ungroup %>%
    
    #create a retention variable
    mutate(tx_ret_pct = round(tx_ret_num/tx_ret_denom, 3)) %>%
    
    # add STAR v Standard OU designation
    mutate(designation = 
            ifelse(operatingunit %in%
                 c("Angola", "Asia Regional Program", "Burma", "Cambodia", 
                   "Caribbean Region", "Central America Region", "Central Asia Region", 
                   "Dominican Republic", "Ghana", "India", "Indonesia", "Papua New Guinea"),
                      0,1)) %>%
  
    # add scaleup or other psnu designation
    mutate(scaleup = 
            ifelse(is.na(fy16snuprioritization), NA,
            ifelse(fy16snuprioritization %in% c("1 - Scale-Up: Saturation", "2 - Scale-Up: Aggressive"),
                    1,0))) %>%
    
    #reorder
    select(operatingunit:countryname, designation, snu1:fy16snuprioritization, scaleup, tx_ret_denom:tx_ret_num, tx_ret_pct, tx_new) %>%
    
    #remove Burundi column with missing data and no psnuuid
    subset(!is.na(psnuuid))
    
    #assign value labels
    df_mer$designation <- factor(df_mer$designation,
                                levels = c(0, 1), 
                                labels = c("STAR", "Standard"))
    df_mer$scaleup<- factor(df_mer$scaleup,
                                 levels = c(1,0 ), 
                                 labels = c("Scale up", "Other"))


    
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
    filter(indicator %in% c("PLHIV (SUBNAT)", "TX_CURR_SUBNAT", "POP_EST (SUBNAT)"), disaggregate == "Total Numerator", psnuuid!="") %>%
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
  names(df_impattdata)[names(df_impattdata) == 'POP_EST (SUBNAT)'] <- 'pop'
  

## EA DATA NAV DATA ##

#import EA data from Botswana data nav tool
  setwd("C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data")
  df_ea <- read_csv("2014-2016 allcntry SAS Output 24JAN17.csv")

# change all header names to lower case to make it easier to use
  names(df_ea) <- tolower(names(df_ea))

#subset
  df_ea <- df_ea %>%
    filter(rptgcycle==2016 & data_type=="De-Dup") %>%
    select(ou, national_sub_unit, national_sub_sub_unit,datim_snu_id, cbcts_nonfbt_exp, cbcts_lnkg_exp, cbcts_rtnadhr_exp, cbcts_othcare_exp, cbcts_loaded_tot, fbcts_loaded_tot)

#rename for merge
  df_ea <- rename(df_ea, ea_districts = national_sub_unit)
  df_ea <- rename(df_ea, psnuuid = datim_snu_id)

#merge mer, ea, and impatt
  df_global <- left_join(df_mer, df_ea, by="psnuuid")
  df_global <- left_join(df_global, df_impattdata, by="psnuuid")

#remove unnecessary variables
  df_global <- subset(df_global, select = -c(ou, ea_districts, national_sub_sub_unit))

#remove intermediate dfs
  rm(df_ea, df_impattdata, df_mer)

#create adjusted expenses
  df_global <- df_global %>% 
    mutate(rtnadhr_exp_per_plhiv = round(cbcts_rtnadhr_exp/plhiv, 3)) %>%
    mutate(lnkg_exp_per_plhiv = round(cbcts_lnkg_exp/plhiv, 3)) %>%
    mutate(ln_rtnadhr_exp = log(cbcts_rtnadhr_exp)) %>%
    mutate(ln_lnkg_exp = log(cbcts_lnkg_exp))

#remove Inf
  is.na(df_global) <- sapply(df_global, is.infinite)

#save output
  save(df_global, file = "df_global.RData")

## EXPORT DATA ###
#write.csv(df_global, "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data/ret_global.csv", na="")


