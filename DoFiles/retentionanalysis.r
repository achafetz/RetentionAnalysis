# Retention Analysis R Script
# DRAFT
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code

library("tidyverse")

setwd("C:/Users/achafetz/Documents/GitHub/RetentionAnalysis/Data")
aisdata <- read.csv("BWA AIS relevant data.csv", header=TRUE, sep=",")
crosswalk <- read.csv("District Crosswalk.csv", header = TRUE, sep = ",")

#rename for merge
names(aisdata)[1] <- "ais_districts"


aisdata <- dplyr::full_join(aisdata, crosswalk, by = "ais_districts")

new <- dplyr::summarise_each(aisdata, funs(sum))

#TODO
aisdata %>%
    group_by(district) %>%
    summarise_each(funs(toString))


# IMPORT DATA
# set the working directory(Wd), eg the file path where your data is stored --> change to folder path on your machine
setwd("C:/Users/achafetz/Documents/ICPI/Data/")

#import data and call the dataframe factviewdata
fvdata <- read.table("ICPI_FactView_PSNU_IM_20170324_v2_2.txt", header = TRUE, sep = "\t", fill = TRUE)
# change all header names to lower case to make it easier to use
names(fvdata) <- tolower(names(fvdata))
# subset fv to Bots
bots_fvdata <- fvdata %>%
  filter(operatingunit == "Botswana", indicator == "TX_RET", disaggregate == "Total Denominator" | disaggregate == "Total Numerator") %>%
  select(psnu, psnuuid, fy16snuprioritization, numeratordenom, fy2016apr)

#destring
bots_fvdata$fy2016apr <-as.numeric(bots_fvdata$fy2016apr)

#collapse
bots_fvdata <- bots_fvdata %>%
  group_by(psnu, psnuuid, fy16snuprioritization, numeratordenom) %>%
  summarize_each(funs(sum), fy2016apr) %>%
  ungroup

#reshape wide
bots_fvdata <- spread(bots_fvdata, numeratordenom, fy2016apr)

#gen
bots_fvdata <-mutate(bots_fvdata, ret_pct = N/D)





