### Process Length Structure information and summarize.

#### Analysis of the fish and cover data for the OCNMS survey region.

# Libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)
library(tidyverse)

# select species and max year ##################################################
# spp = 'SEME' # black rockfish
spp = 'SEFL' # yellowtail rockfish
survey_year = 2024
################################################################################

home_dir<- "~/GitHub/OCNMS/"
setwd(home_dir)
data_out = paste0(home_dir,"/_01_Assessment-indicies-BRF-YTRF/")

dat.2015 <- read.csv("~/Github/OCNMS/Data/CSV_2015_on/2015_OCNMSDataComplete_standardized_122116.csv")

dat.2016.on.fish <- read.csv(
    paste0(home_dir,"Data/", survey_year, "/NWFSC_FISH_ALLYEARS_data_",survey_year,".csv"))

species_names <- read.csv("~/Github/OCNMS/_00_Annual-Update/spp_codes_fish.csv")

# trim data to include only swath dat
dat.fish <- dat.2015 %>% filter(data.type=="swath",PISCO.datatype=="fish")
# Trim further to only include any rockfish.
dat.fish.seb.2015 <- dat.fish %>% filter(grepl("SE|RYOY",PISCO.Classcode))
# check to see we didn't get any extra species in that filter
# dat.fish.seb.2015 %>% distinct(PISCO.Classcode)
# Pull out only black rockfish and unid YOY, add labels to match later years
dat.fish.spp.2015 <- dat.fish.seb.2015 %>% 
                          filter(PISCO.Classcode %in% c("SEME","RYOY")) %>%
                          mutate(YEAR=2015,SIDE=NA,ZONE=5,VIS_M=NA) %>%
                          dplyr::select(YEAR,
                                        SITE = Site,
                                        SIDE ,
                                        ZONE ,# all 2015 transects were at 5m
                                        VIS_M , # all vis 2015 > 2.5 m except Destruction
                                        TRANSECT=Transect,
                                        SPECIES=PISCO.Classcode,
                                        QUANTITY=Count,
                                        SIZE.MIN= Size.cm,
                                        SIZE.MAX= Size.cm)

# Pause with 2015 to clean up 2016
dat.fish.seb.2016 <- dat.2016.on.fish %>% filter(grepl("SE|RYOY",SPECIES))
# CHECK: dat.fish.seb.2016 %>% distinct(SPECIES)
dat.fish.spp.2016 <- dat.fish.seb.2016 %>% 
                        dplyr::select(YEAR,
                                      SITE,
                                      SIDE,
                                      ZONE,
                                      VIS_M,
                                      TRANSECT,
                                      SPECIES,
                                      QUANTITY,
                                      SIZE.MIN= SIZE..MIN.,
                                      SIZE.MAX= SIZE..MAX.) %>%
                        filter(SPECIES %in% c(spp,"SEBYT","RYOY")) 
dat.fish.spp.2015$QUANTITY = as.numeric(dat.fish.spp.2015$QUANTITY)
dat.fish.spp.2016$QUANTITY = as.numeric(dat.fish.spp.2016$QUANTITY)

# Keep only the 5 core sites
nom <- c("Destruction Island","Cape Johnson","Cape Alava",
         "Tatoosh Island","Neah Bay")

dat.spp <- bind_rows(dat.fish.spp.2015,dat.fish.spp.2016) %>%
                filter(SITE %in% nom)

# Check which ones min size != max size
dat.spp %>% filter(SIZE.MIN!=SIZE.MAX)
# WHAT TO DO WITH SIZE RANGES FOR ADULTS

# ONLY USE TRANSECTS WITH A MINIMUM AMOUNT OF VISIBILITY (or from 2015)
TRIM <- 2
dat.spp.ad <- dat.spp %>% filter(VIS_M > TRIM | YEAR==2015) %>%
                      filter(SIZE.MIN >= 10) %>%
                      mutate(SIZE = (SIZE.MIN+SIZE.MAX)*0.5) %>%
                      group_by(YEAR,SITE,SPECIES,SIZE) %>% # Sum across transects, depth zones
                      summarise(COUNT = sum(QUANTITY))

dat.spp.all <- dat.spp %>% filter(VIS_M > TRIM | YEAR==2015) %>%
  mutate(SIZE = (SIZE.MIN+SIZE.MAX)*0.5) %>%
  group_by(YEAR,SITE,SPECIES,SIZE) %>% # Sum across transects, depth zones
  summarise(COUNT = sum(QUANTITY))


BIN <- seq(0,70,by=5)
BIN.MIN <- BIN -2.5
BIN.MAX <- BIN +2.5

# There has to be a better way to do this, but looping works fine.
dat.spp.ad$bin <- 0
dat.spp.all$bin <- 0
for(i in 1: length(BIN)){
  dat.spp.ad <- dat.spp.ad %>% 
                    mutate(bin =ifelse(SIZE < BIN.MAX[i] & SIZE >= BIN.MIN[i],BIN[i],bin))
  dat.spp.all <- dat.spp.all %>% 
    mutate(bin =ifelse(SIZE < BIN.MAX[i] & SIZE >= BIN.MIN[i],BIN[i],bin))
}

dat.spp.ad.binned <- dat.spp.ad %>% group_by(YEAR,SITE,SPECIES,bin) %>% # Sum across transects, depth zones
                        summarise(COUNT = sum(COUNT))
dat.spp.all.binned <- dat.spp.all %>% group_by(YEAR,SITE,SPECIES,bin) %>% # Sum across transects, depth zones
  summarise(COUNT = sum(COUNT))

####################################
# Add a second binning group. (2cm)
BIN <- seq(2,70,by=2)
BIN.MIN <- BIN - 2
BIN.MAX <- BIN 

# There has to be a better way to do this, but looping works fine.
dat.spp.ad$bin_2 <- 0
dat.spp.all$bin_2 <- 0
for(i in 1: length(BIN)){
  dat.spp.ad <- dat.spp.ad %>% 
    mutate(bin_2 =ifelse(SIZE < BIN.MAX[i] & SIZE >= BIN.MIN[i],BIN[i],bin_2))
  dat.spp.all <- dat.spp.all %>% 
    mutate(bin_2 =ifelse(SIZE < BIN.MAX[i] & SIZE >= BIN.MIN[i],BIN[i],bin_2))
}

dat.spp.ad <- dat.spp.ad %>% mutate(bin_min= bin_2 -2, bin_max = bin_2,
                                      bin_range = paste0(bin_2-2,"-",bin_2))
dat.spp.all <- dat.spp.all %>% mutate(bin_min= bin_2 -2, bin_max = bin_2,
                                        bin_range = paste0(bin_2-2,"-",bin_2))
####################################


dat.spp.ad$SITE <- factor(dat.spp.ad$SITE,
                           levels=c("Neah Bay","Tatoosh Island","Cape Alava",
                                    "Cape Johnson","Destruction Island"))

dat.spp.ad.binned$SITE <- factor(dat.spp.ad.binned$SITE,
                           levels=c("Neah Bay","Tatoosh Island","Cape Alava",
                                    "Cape Johnson","Destruction Island"))

dat.spp.all$SITE <- factor(dat.spp.all$SITE,
                           levels=c("Neah Bay","Tatoosh Island","Cape Alava",
                                    "Cape Johnson","Destruction Island"))

dat.spp.all.binned$SITE <- factor(dat.spp.all.binned$SITE,
                                  levels=c("Neah Bay","Tatoosh Island","Cape Alava",
                                           "Cape Johnson","Destruction Island"))

################3 PLOTS
    
p.spp.size1 <- ggplot(dat.spp.ad) +
                    geom_col(aes(x=SIZE,y=COUNT)) +
                    facet_grid(SITE~YEAR) +
                    labs(x="Length(cm)",y="Count") +
                    theme_bw()
p.spp.size1

p.spp.size2 <- ggplot(dat.spp.ad) +
  geom_col(aes(x=SIZE,y=COUNT,color=SITE,fill=SITE)) +
  facet_wrap(~YEAR,ncol=1) +
  labs(x="Length(cm)",y="Count") +
  theme_bw()

p.spp.size2

p.spp.size3 <- ggplot(dat.spp.ad) +
  geom_col(aes(x=SIZE,y=COUNT)) +
  facet_wrap(~YEAR,ncol=1) +
  labs(x="Length(cm)",y="Count") +
  theme_bw()

p.spp.size3

### BINNED (5cm bins)

p.spp.size.bin1 <- ggplot(dat.spp.ad.binned) +
  geom_col(aes(x=bin,y=COUNT)) +
  facet_grid(SITE~YEAR) +
  labs(x="Length (5cm bins)",y="Count") +
  theme_bw()
p.spp.size.bin1

p.spp.size.bin2 <- ggplot(dat.spp.ad.binned) +
  geom_col(aes(x=bin,y=COUNT,color=SITE,fill=SITE),width=4) +
  facet_wrap(~YEAR,ncol=1) +
  labs(x="Length (5 cm bins)",y="Count") +
  theme_bw()
p.spp.size.bin2

###

p.spp.size.bin3 <- ggplot(dat.spp.ad.binned) +
  geom_col(aes(x=bin,y=COUNT,color=SITE,fill=SITE),width=4) +
  facet_wrap(~YEAR,ncol=1) +
  labs(x="Length (5 cm bins)",y="Count") +
  theme_bw()
p.spp.size.bin3

## 3 INCLUDE YOY sizes

p.spp.size.all.bin1 <- ggplot(dat.spp.all.binned) +
  geom_col(aes(x=bin,y=COUNT)) +
  facet_grid(SITE~YEAR) +
  labs(x="Length (5cm bins)",y="Count") +
  scale_y_continuous(trans="sqrt") +
  theme_bw()
p.spp.size.all.bin1

# write data and plots to file

length.dat <- list(
        # data frames that include all sizes.
        dat.spp.all.binned = dat.spp.all.binned,
        dat.spp.all = dat.spp.all,
        # data frames that include only individuals > 10cm
        dat.spp.ad.binned = dat.spp.ad.binned,
        dat.spp.ad = dat.spp.ad)

# Write to file.        

if(spp=='SEME'){SPP = "Black rockfish"}
if(spp=='SEFL'){SPP = "Yellowtail rockfish"}

save(length.dat,file=paste0(data_out,SPP, "_lengths_2015-", survey_year,".Rdata"))

dat.spp.all.2cm = dat.spp.all %>% 
                    dplyr::select(YEAR,SITE,SPECIES,COUNT,bin_min,bin_max,bin_range) %>%
                    filter(bin_min >=10)                    

write.csv(dat.spp.all.2cm,
          file=paste0(data_out, SPP, "_2015-", survey_year,"_lengths_2cm_bin_OCNMS.csv"),row.names = FALSE)





