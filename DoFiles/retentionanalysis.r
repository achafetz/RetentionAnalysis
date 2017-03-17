# Retention Analysis R Script
# DRAFT
# https://github.com/achafetz/RetentionAnalysis/wiki/Draft-R-Code

setwd("/Users/Aaron/Desktop/RetentionAnalysis/Data/")
aisdata <- read.csv("BWA AIS relevant data.csv", header=TRUE, sep=",")
crosswalk <- read.csv("District Crosswalk.csv", header = TRUE, sep = ",")

#rename for merge
names(aisdata)[1] <- "ais_districts"

library("tidyverse")

aisdata <- dplyr::full_join(aisdata, crosswalk, by = "ais_districts")

new <- dplyr::summarise_each(aisdata, funs(sum))

#TODO
aisdata %>%
    group_by(district) %>%
    summarise_each(funs(toString))
