
# Retention Analysis R Script
# DRAFT
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
    mutate(ln_rtnadhr_exp = log(cbcts_rtnadhr_exp))

#remove Inf
  is.na(df_global) <- sapply(df_global, is.infinite)

#save output
  save(df_global, file = "df_global.RData")

#load global file created
  #setwd("C:/Users/achafetz/Documents/ICPI/Data/")
  #load("df_global.RData")

## RETENTION ##

#dataset  
  df_global_h1 <- filter(df_global, tx_ret_denom!=0, tx_ret_pct<=1)

#how many zeros or 1s
  nrow(filter(df_global_h1,tx_ret_denom==0 | tx_ret_pct==1)) 
  summary(df_global_h1$tx_ret_pct)
  ggplot(df_global_h1, aes(tx_ret_pct)) + 
    geom_histogram() + 
    labs(title = "Treatment retention histogram (PEPFAR FY16)", x = "treatment retention", y ="frequency") +
    scale_x_continuous(labels = scales::percent)


# GRAPHS

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

#model comparison -> transformations and linear v logit

h1a <- lm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global, na.action = na.exclude)
summary(fitted(h1a))
h1a2 <- lm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global, na.action = na.exclude)
summary(fitted(h1a2))
h1a3 <- lm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global, na.action = na.exclude)
summary(fitted(h1a3))
h1f <- glm(tx_ret_pct ~ cbcts_rtnadhr_exp, data=df_global_h1, family=binomial(link="logit"))
h1f2 <- glm(tx_ret_pct ~ rtnadhr_exp_per_plhiv, data=df_global_h1, family=binomial (link="logit"), na.action=na.omit)
h1f3 <- glm(tx_ret_pct ~ ln_rtnadhr_exp, data=df_global_h1, family=binomial (link="logit"), na.action=na.omit)

stargazer(h1a, h1a2, h1a3, h1f, h1f2, h1f3, type = "text")
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

## LINKAGE GRAPHS ##

#Linkage by OU
df_global_ou <- df_global %>%
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
h2a <- lm(tx_new ~ cbcts_lnkg_exp, data=df_global)
h2b <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat , data=df_global)
h2c <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization), data=df_global)
h2d <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(operatingunit), data=df_global)
h2e <- lm(tx_new ~ cbcts_lnkg_exp + plhiv + tx_curr_subnat + factor(fy16snuprioritization) + factor(operatingunit), data=df_global)

#output
stargazer(h2a, h2b, h2c, h2d, h2e, type = "text")
stargazer(h2a, h2b, h2c, h2d, h2e, type = "html", out = "linkage_output.htm")

rm(df_global_h2, h2a, h2b, h2c, h2d, h2e)
## EXPORT DATA ###
write.csv(df_global_h1, "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data/ret_global_h1.csv", na="")

write.csv(df_global_h1, "C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data/ret_global.csv", na="")


