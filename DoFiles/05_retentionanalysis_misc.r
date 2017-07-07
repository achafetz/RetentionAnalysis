# Retention Analysis
# A.Chafetz, USAID
# Purpose: miscellaneous
# Updated: 7/7/17 
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code

# how many districts are represented
nrow(filter(df_global, cbcts_loaded_tot>0))
# out of how many districts?
nrow(filter(df_global, !is.na(cbcts_loaded_tot)))


df_global %>% 
  select(operatingunit, psnu, cbcts_loaded_tot, fbcts_loaded_tot) %>%
  group_by(operatingunit) %>%
  summarise_at(vars(cbcts_loaded_tot, fbcts_loaded_tot), funs(sum(., na.rm = TRUE))) %>%
  mutate(com_ratio = round(cbcts_loaded_tot/fbcts_loaded_tot, 3)) %>%
  mutate(cbcts_loaded_tot = round(cbcts_loaded_tot, 0)) %>%
  mutate(fbcts_loaded_tot = round(fbcts_loaded_tot, 0)) %>%
  arrange(desc(com_ratio)) %>%
  kable(col.names = c("OU", "CBCTS $", "FBCTS $", "CBCTS Prop."), format.args = list(big.mark = ","))

df_global %>%
  select(operatingunit, cbcts_lnkg_exp, cbcts_rtnadhr_exp, cbcts_nonfbt_exp, cbcts_othcare_exp, cbcts_loaded_tot) %>%
  group_by(operatingunit) %>%
  summarise_at(vars(cbcts_lnkg_exp, cbcts_rtnadhr_exp, cbcts_nonfbt_exp, cbcts_othcare_exp, cbcts_loaded_tot), funs(sum(., na.rm = TRUE))) %>%
  arrange(desc(cbcts_lnkg_exp)) %>%
  kable(format.args = list(big.mark = ","))
  
  
nrow(df_global)
 
