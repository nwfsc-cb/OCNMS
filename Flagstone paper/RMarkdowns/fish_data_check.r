ntfish <- readRDS("C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/Data/Data_Fish_Kelp_area_wide.rds")
jsfish <- readRDS("C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/Data/yoy_kelp_2015_2021_year_site_area_zone.rds")
allfish <- readRDS("C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/Data/Fish_2015-2021.rds")



nt <- ntfish[, c('year','site','area','zone','TOTyoy') ]
js <- jsfish[, c('year','site','area','zone','mean_TOTyoy') ]

library(tidyverse)
fish = nt %>% full_join(.,js)

fish$TOTyoy == fish$mean_TOTyoy


di = allfish %>% filter(site == "Destruction Island" &
                        year == 2019 & 
                        zone == 5 & 
                        area == 'S' &
                        species %in% c("RYOY", "SEME","SEBYT",
                                      "SEMY", "SENE", "SEPI",
                                      "SEMA") &
                        size_class != "large")
di


di = fish_transect %>% filter(site == "Destruction Island" &
                             year == 2019 & 
                             zone == 5 & 
                             area == 'S' &
                             species %in% c("RYOY", "SEME","SEBYT",
                                            "SEMY", "SENE", "SEPI",
                                            "SEMA") &
                             size_class != "large")
di