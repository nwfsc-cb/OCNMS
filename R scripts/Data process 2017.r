options(max.print=99999)
library(dplyr)
#### Script for importing historical Kviteck data and our OCNMS data
base.dir <- getwd()
setwd(paste(base.dir,"/Data/csv files",sep=""))

dat.2016 <- read.csv("NWFSC_SWATH_2016_data_entry.csv")
dat.2015 <- read.csv("2015_OCNMSDataComplete_standardized_122116.csv")
dat.kvitek <- read.csv("Kvitek_Data_SummaryByYr_MASTER-invert-quad.csv")

## Cull 2015 data to only include swath data
dat.2015 <- filter(dat.2015,data.type == "swath")


dat.kvitek