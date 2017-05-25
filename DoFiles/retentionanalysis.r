# Retention Analysis R Script
# DRAFT
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code


## DEPENDENT LIBRARIES ##
library("tidyverse")
library("readxl")


## DISTRICT CROSSWALK ACROSS DATASETS ##

#import
  setwd("C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data")
  crosswalk <- read.csv("District Crosswalk.csv", header = TRUE, sep = ",")


## AIS DATA ##

#import
  aisdata <- read.csv("BWA AIS relevant data.csv", header=TRUE, sep=",")

#rename for merge
  aisdata <- rename(aisdata, ais_districts = district)

  aisdata <- aisdata %>%
    left_join(crosswalk, by = "ais_districts") %>% #merge
    subset(select = -c(fv_districts, ea_districts, ais_districts)) %>% #remove unnecessary district names
    select(district, everything()) #reorder
    
#convert percentages to raw numbers before aggregating up
  bots_aisdata = aisdata %>% 
    # convert everything to percents
    mutate_each(funs(./100), drugs_male, drugs_female, drugs_bothsex, tested_pct,
                contains('prevention'), 
                contains('attitude'), 
                contains('access'),
                contains("media_"),
                contains("condom_")) %>%
    
    # convert percents to raw numbers
    mutate_each(funs(.*total_heardofhiv), contains('prevention'), contains('attitude')) %>% 
    mutate_each(funs(.*pop_total), contains('tested_pct'), contains('access')) %>% 
    mutate_each(funs(.*total_media), contains("media_")) %>% 
    mutate_each(funs(drugs_male*total_drugs_male), contains("drugs_male")) %>% 
    mutate_each(funs(drugs_female*total_drugs_female),  contains("drugs_female")) %>% 
    mutate_each(funs(drugs_bothsex*total_drugs_bothsex), contains("drugs_bothsex")) %>% 
    mutate_each(funs(.*total_condom), contains("condom_"))  %>% 
    
    # Group by your district/grouping variable
    group_by(district) %>% 
    summarise_all(funs(sum(.))) %>%
    
    # convert raw number to percentages
    mutate_each(funs(./total_heardofhiv), contains('prevention'), contains('attitude')) %>% 
    mutate_each(funs(./pop_total), contains('tested_pct'), contains('access')) %>% 
    mutate_each(funs(./total_media), contains("media_")) %>% 
    mutate_each(funs(drugs_male/total_drugs_male), contains("drugs_male")) %>% 
    mutate_each(funs(drugs_female/total_drugs_female),  contains("drugs_female")) %>% 
    mutate_each(funs(drugs_bothsex/total_drugs_bothsex), contains("drugs_bothsex")) %>% 
    mutate_each(funs(./total_condom), contains("condom_")) %>%
    
    # convert from .753 to 75.3
    mutate_each(funs(round(.*100,1)), drugs_male, drugs_female, drugs_bothsex, tested_pct,
                contains('prevention'), 
                contains('attitude'), 
                contains('access'),
                contains("media_"),
                contains("condom_"))
  
    #remove original AIS dataset
    rm(aisdata)
  
## FACT VIEW DATA ##
  
#import data and call the dataframe factviewdata
  setwd("C:/Users/achafetz/Documents/ICPI/Data/")
  fvdata <- read.table("ICPI_FactView_PSNU_IM_20170515_v1_1.txt", header = TRUE, sep = "\t", fill = TRUE)

# change all header names to lower case to make it easier to use
  names(fvdata) <- tolower(names(fvdata))

# subset fv to Bots only & key indicators
  bots_fvdata <- fvdata %>%
    filter(operatingunit == "Botswana", indicator == "TX_RET", disaggregate == "Total Denominator" | disaggregate == "Total Numerator") %>%
    select(psnu, psnuuid, fy16snuprioritization, numeratordenom, fy2016apr)

#destring APR data
  bots_fvdata$fy2016apr <-as.numeric(bots_fvdata$fy2016apr)

#aggregate
  bots_fvdata <- bots_fvdata %>%
    group_by(psnu, psnuuid, fy16snuprioritization, numeratordenom) %>%
    summarize_each(funs(sum), fy2016apr) %>%
    ungroup

#reshape wide
  bots_fvdata <- spread(bots_fvdata, numeratordenom, fy2016apr)

#gen retention percentages by district
  bots_fvdata <-mutate(bots_fvdata, ret_pct = N/D)
  bots_fvdata$ret_pct <- round(bots_fvdata$ret_pct, 3)

#drop fv dataset
  rm(fvdata)

#rename districst for merge
  bots_fvdata <- rename(bots_fvdata, fv_districts = psnu)

#merge with district crosswalk
  bots_fvdata <- bots_fvdata %>%
    left_join(crosswalk, by = "fv_districts") %>% #merge
    subset(select = -c(fv_districts, ea_districts, ais_districts, psnuuid)) %>% #remove unnecessary district names
    select(district, everything()) #reorder

  
## IMPATT DATA ##

#import Nat/SubNat data
  impattdata <- read.table("ICPI_FactView_NAT_SUBNAT_20170515_v1_1.txt", header = TRUE, sep = "\t", fill = TRUE)

# change all header names to lower case to make it easier to use
  names(impattdata) <- tolower(names(impattdata))
  
  # subset fv to Bots only & key indicators
  bots_impattdata <- impattdata %>%
    filter(operatingunit == "Botswana", indicator == "PLHIV (SUBNAT)" | indicator == "TX_CURR_SUBNAT", disaggregate == "Total Numerator") %>%
    select(psnu, psnuuid, fy16snuprioritization, indicator, fy2016)

  rm(bots_impattdata)

#rename indicators for reshape 
  levels(bots_impattdata$indicator)[match("PLHIV (SUBNAT)",levels(bots_impattdata$indicator))] <- "plhiv"
  levels(bots_impattdata$indicator)[match("TX_CURR_SUBNAT",levels(bots_impattdata$indicator))] <- "tx_curr_subnat"
  
#reshape wide
  bots_impattdata <- spread(bots_impattdata, indicator, fy2016)

#rename districst for merge
  bots_impattdata <- rename(bots_impattdata, fv_districts = psnu)
  
#merge with district crosswalk
  bots_impattdata <- bots_impattdata %>%
    left_join(crosswalk, by = "fv_districts") %>% #merge
    subset(select = -c(fv_districts, ea_districts, ais_districts, psnuuid)) %>% #remove unnecessary district names
    select(district, everything()) #reorder    
  
## EA DATA NAV DATA ##

#import EA data from Botswana data nav tool
  setwd("C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data")
  bots_eadata <- read_excel("Botswana 2016 EA Data Nav Tool v01.17.17.xlsm", sheet = "Totals_MCCNav")

# change all header names to lower case to make it easier to use
  names(bots_eadata) <- tolower(names(bots_eadata))
  
#subset
bots_eadata <- bots_eadata %>%
  filter(rptgcycle==2016 & data_type=="De-Dup") %>%
  select(national_sub_unit, datim_snu_id, cbcts_lnkg_exp, cbcts_rtnadhr_exp, fbcts_loaded_tot)

#rename for merge
  bots_eadata <- rename(bots_eadata, ea_districts = national_sub_unit)
  bots_eadata <- rename(bots_eadata, psnuuid = datim_snu_id)

#merge with district crosswalk
  bots_eadata <- bots_eadata %>%
    left_join(crosswalk, by = "ea_districts") %>%
    subset(select = -c(fv_districts, ea_districts, ais_districts)) %>%
    select(district, everything())

#merge all
  bots <- full_join(bots_eadata, bots_fvdata, by="district")
  bots <- full_join(bots, aisdata, by="district")
