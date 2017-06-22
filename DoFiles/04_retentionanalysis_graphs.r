# Retention Analysis
# A.Chafetz, USAID
# Purpose: graphs for retention analysis
# Updated: 6/22/17 
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code


#load global file created
setwd("C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data")
load("df_global.RData")

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
