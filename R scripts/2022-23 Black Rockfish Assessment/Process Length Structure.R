### Process Length Structure information and summarize.

#### Analysis of the fish and cover data for the OCNMS survey region.

# Libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)

data.dir <- "/Users/ole.shelton/GitHub/OCNMS/Data/CSV_2015_on"
setwd(data.dir)

dat.2015 <- read.csv("2015_OCNMSDataComplete_standardized_122116.csv")
dat.2016.on.fish <- read.csv("NWFSC_FISH_ALLYEARS_data_2021.csv")
species_names <- read.csv("species_code_list.csv")

# trim data to include only swath dat
dat.fish <- dat.2015 %>% filter(data.type=="swath",PISCO.datatype=="fish")
# Trim further to only include any rockfish.
dat.fish.seb.2015 <- dat.fish %>% filter(grepl("SE|RYOY",PISCO.Classcode))
# check to see we didn't get any extra species in that filter
# dat.fish.seb.2015 %>% distinct(PISCO.Classcode)
# Pull out only black rockfish and unid YOY, add labels to match later years
dat.fish.seme.2015 <- dat.fish.seb.2015 %>% 
                          filter(PISCO.Classcode %in% c("SEME","RYOY")) %>%
                          mutate(YEAR=2015,SIDE=NA,ZONE=5,VIS_M=NA) %>%
                          dplyr::select(YEAR ,
                                        SITE =Site,
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
dat.fish.seme.2016 <- dat.fish.seb.2016 %>% 
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
                        filter(SPECIES %in% c("SEME","SEBYT","RYOY"))

# Keep only the 5 core sites
nom <- c("Destruction Island","Cape Johnson","Cape Alava",
         "Tatoosh Island","Neah Bay")

dat.seme <- bind_rows(dat.fish.seme.2015,dat.fish.seme.2016) %>%
                filter(SITE %in% nom)

# Check which ones min size != max size
dat.seme %>% filter(SIZE.MIN!=SIZE.MAX)
# WHAT TO DO WITH SIZE RANGES FOR ADULTS

# ONLY USE TRANSECTS WITH A MINIMUM AMOUNT OF VISIBILITY (or from 2015)
TRIM <- 2
dat.seme.ad <- dat.seme %>% filter(VIS_M > TRIM | YEAR==2015) %>%
                      filter(SIZE.MIN >= 10) %>%
                      mutate(SIZE = (SIZE.MIN+SIZE.MAX)*0.5) %>%
                      group_by(YEAR,SITE,SPECIES,SIZE) %>% # Sum across transects, depth zones
                      summarise(COUNT = sum(QUANTITY))

BIN <- seq(0,70,by=5)
BIN.MIN <- BIN -2.5
BIN.MAX <- BIN +2.5

# There has to be a better way to do this, but looping works fine.
dat.seme.ad$bin <- 0
for(i in 1: length(BIN)){
  dat.seme.ad <- dat.seme.ad %>% 
                    mutate(bin =ifelse(SIZE < BIN.MAX[i] & SIZE >= BIN.MIN[i],BIN[i],bin))
}

dat.seme.ad.binned <- dat.seme.ad %>% group_by(YEAR,SITE,SPECIES,bin) %>% # Sum across transects, depth zones
                        summarise(COUNT = sum(COUNT))

dat.seme.ad$SITE <- factor(dat.seme.ad$SITE,
                           levels=c("Neah Bay","Tatoosh Island","Cape Alava",
                                    "Cape Johnson","Destruction Island"))

dat.seme.ad.binned$SITE <- factor(dat.seme.ad.binned$SITE,
                           levels=c("Neah Bay","Tatoosh Island","Cape Alava",
                                    "Cape Johnson","Destruction Island"))

p.seme.size1 <- ggplot(dat.seme.ad) +
                    geom_col(aes(x=SIZE,y=COUNT)) +
                    facet_grid(SITE~YEAR) +
                    labs(x="Length(cm)",y="Count") +
                    theme_bw()
p.seme.size1

p.seme.size2 <- ggplot(dat.seme.ad) +
  geom_col(aes(x=SIZE,y=COUNT,color=SITE,fill=SITE)) +
  facet_wrap(~YEAR,ncol=1) +
  labs(x="Length(cm)",y="Count") +
  theme_bw()

p.seme.size2

p.seme.size3 <- ggplot(dat.seme.ad) +
  geom_col(aes(x=SIZE,y=COUNT)) +
  facet_wrap(~YEAR,ncol=1) +
  labs(x="Length(cm)",y="Count") +
  theme_bw()

p.seme.size3

### BINNED (5cm bins)

p.seme.size.bin1 <- ggplot(dat.seme.ad.binned) +
  geom_col(aes(x=bin,y=COUNT)) +
  facet_grid(SITE~YEAR) +
  labs(x="Length (5cm bins)",y="Count") +
  theme_bw()
p.seme.size.bin1

p.seme.size.bin2 <- ggplot(dat.seme.ad.binned) +
  geom_col(aes(x=bin,y=COUNT),width=4) +
  facet_wrap(~YEAR,ncol=1) +
  labs(x="Length (5 cm bins)",y="Count") +
  theme_bw()
p.seme.size.bin2


###

# Define codes for rockfish 
ROCKFISH <- c(as.character(species_names$species[grep("SE",species_names$species)]),"RYOY")

###############################
###############################
# Repeat for 2016 and later data.
###############################
###############################

