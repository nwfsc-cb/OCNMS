### New Script for making the 2015 data look like later years.

#### Analysis of the fish and cover data for the OCNMS survey region.

# Libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)
library(lubridate)
rm(list=ls())

base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
data.dir <- paste0(base.dir,"/Data/CSV_2015_on")

setwd(data.dir)
dat.2015 <- read.csv("2015_OCNMSDataComplete_standardized_122116.csv")
species_names <- read.csv("species_code_list_swath.csv")

######### THIS IS FOR PROCESSING 2015 DATA.
nom <- c("Destruction Island","Teahwhit Head","Cape Johnson","Rock 305","Cape Alava","Point of the Arches",
         "Anderson Point","Tatoosh Island","Chibadehl Rocks","Neah Bay")
nom.ord <- 1:length(nom)
nom.merge <- data.frame(nom=nom,id=nom.ord)

# Make list of transects by site, and species group. # extract swath data only.
dat.2015 <- left_join( dat.2015, species_names %>% dplyr::select(species,group) %>% rename(PISCO.Classcode=species))
base.dat <- dat.2015 %>% filter(PISCO.datatype=="swath",data.type=="swath") %>% group_by(Date,Site,Transect,Observer,Transect.area..inverts.,group) %>% 
  summarise(x=length(Transect)) %>% filter(is.na(Transect)==F) %>% dplyr::select(Site,Date,Transect,Observer,Transect.area=Transect.area..inverts.,group) %>% as.data.frame()

# Replace missing transect area with standard 60 m^2 searched.
base.dat$Transect.area <- as.numeric(as.character(base.dat$Transect.area))
base.dat <- base.dat %>% mutate(Transect.area=ifelse(is.na(Transect.area) ==T,60,Transect.area))

#MAKE CATCH FOR if there are more than 4 total observations for each site in 2015
# A <- base.dat %>% group_by(Site) %>% summarise(N= length(Transect))
# if(max(A$N) > 4 | min(A$N) < 4 ){ print(rep("STOP, SOMETHING IS WRONG",100))}
base.dat$Transect <- as.integer(base.dat$Transect )

#### OK. Pull out all of the swath data. Do some simple data fixing in the process
swath.dat <- dat.2015 %>% 
            mutate(PISCO.datatype = as.character(PISCO.datatype),
                  PISCO.datatype=ifelse(PISCO.datatype=="" & data.type=="swath","swath",PISCO.datatype)) %>%
            filter(PISCO.datatype=="swath" | PISCO.datatype =="fish") %>% 
            filter(data.type=="swath") 

# Get rid of a few screwed up entries in the "Transect.area..inverts." column, convert NA to 0

swath.dat <- swath.dat %>% 
              mutate(Transect.area..inverts. = as.character(Transect.area..inverts.),
                Transect.area..inverts.=ifelse(Transect.area..inverts.=="",0,Transect.area..inverts.)) %>%
              mutate(Transect.area..fish.=ifelse(is.na(Transect.area..fish.),0,Transect.area..fish.)) %>%            
              mutate(Transect.area..stipes.=ifelse(is.na(Transect.area..stipes.),0,Transect.area..stipes.)) %>%            
              mutate(Transect.area..inverts.= as.numeric(as.character(Transect.area..inverts.)))

# check to make sure that only one of the transect area columns is > 0.
check.1 <- swath.dat %>% 
              filter(Transect.area..inverts. > 0) %>% filter(Transect.area..fish. > 0 | Transect.area..stipes. > 0)
check.2 <- swath.dat %>% 
              filter(Transect.area..fish. > 0) %>% filter( Transect.area..stipes. > 0)

bind_rows(check.1,check.2)

# There is 1 row. Destruction Island. In has area for Inverts when it shouldn't (It's an algae count). Cull that entry.
swath.dat <- swath.dat %>% 
            mutate(Transect.area..inverts.=ifelse(Transect.area..inverts.> 0 &
                                                    Transect.area..stipes. > 0 & 
                                                    group == "Algae",0,Transect.area..inverts.))

# check again to make sure that only one of the transect area columns is > 0.
check.1 <- swath.dat %>% 
  filter(Transect.area..inverts. > 0) %>% filter(Transect.area..fish. > 0 | Transect.area..stipes. > 0)
check.2 <- swath.dat %>% 
  filter(Transect.area..fish. > 0) %>% filter( Transect.area..stipes. > 0)

bind_rows(check.1,check.2)

################################################################################  
#  Collapse Transect.area to one column by addition.
swath.dat <- swath.dat %>% 
            mutate(Transect.area = Transect.area..stipes. + Transect.area..inverts. + Transect.area..fish.)

# Separate out Dates
swath.dat <- swath.dat %>% mutate(Date2 = as.Date(Date,"%m/%d/%y")) %>% 
  mutate(YEAR =year(Date2),MONTH=month(Date2),DAY=day(Date2))

# Make rules for defining Meters Sampled and Swath Width.
swath.dat <- swath.dat %>%
              mutate(Swath.width = NA_real_,
                     Swath.width = case_when(Transect.area == 60 ~2,
                                            Transect.area == 30~1,
                                            TRUE~Swath.width),
                     METERS.sampled =ifelse(is.na(METERS.sampled)==T,Transect.area / Swath.width,METERS.sampled))

# Add in a Fish group id.                                             
swath.dat <- swath.dat %>% mutate(group = as.character(group),
                                  group = ifelse(PISCO.datatype == "fish","Fish",group))

# Fix Destruction Island Naming conventions
swath.dat <- swath.dat %>% mutate(Site = as.character(Site),
                            Site=ifelse(Site == "Destruction Island SW","Destruction Island",Site))

# Make a table that looks like later data.frames.
dat.2015.swath.invert.algae <- swath.dat %>% 
  filter(group %in% c("Algae","Invert")) %>%
  mutate(BUDDY = NA,
         SIDE = NA,
         ZONE = 5,
         HEADING = NA,
         "DEPTH(m)" = NA,
         "DEPTH(ft)" = NA,
         SEGMENT = NA,
         CLASSCODE = PISCO.Classcode,
         cntrl_a = NA,
         "DATA ENTRY PROBLEMS" = NA) %>%
  dplyr::select(YEAR,MONTH,DAY,
                SITE = Site,
                OBSERVER = Observer,
                BUDDY,
                SIDE ,
                ZONE ,
                TRANSECT = Transect,
                SWATH_WIDTH = Swath.width,
                HEADING ,
                "DEPTH(m)" ,
                "DEPTH(ft)" ,
                SEGMENT,
                CLASSCODE,
                COUNT = Count,
                "METERS sampled" = METERS.sampled,
                "TRANSECT AREA" = Transect.area,
                SIZE_CODE = Echinoderm.size..TD..TR..BD..BR..T.Total..B.Basal..D.Diameter..R.Radius..L.length.,
                SIZE =Size.cm,
                cntrl_a ,
                notes	= Notes ,
                "DATA ENTRY PROBLEMS" ,
                ORG_TYPE = group)

### Last Checks
# Cull observations without CLASSCODE
#dat.2015.swath.invert.algae <- dat.2015.swath.invert.algae %>% filter(!CLASSCODE == "" )

# Write to File:
# write.csv(dat.2015.swath.invert.algae,file="Invert_Algae_swath_2015.csv",row.names = F)

# SIZE_CODE is new and has three levels BD = Basal Diameter , TD = Total Diameter, TR = Total Radius

##### Make an equivalent Fish data frame.



dat.2015.swath.fish <- swath.dat %>% 
  filter(group %in% c("Fish")) %>%
  mutate(BUDDY = NA,
         SIDE = NA,
         ZONE = 5,
         LEVEL = "BOT",
         HEADING = NA,
         DEPTH_M = NA,
         DEPTH_FT = NA,
         VIS_M = NA,
         SURGE = NA,
         HEADING = NA,
         TEMP_C = NA,
         TEMP_F = NA,
         SEGMENT = NA,
         SPECIES = PISCO.Classcode,
         cntrl_a = NA,
         "DATA ENTRY PROBLEMS" = NA) %>%
  dplyr::select(YEAR,MONTH,DAY,
                SITE = Site,
                OBSERVER = Observer,
                BUDDY,
                SIDE ,
                ZONE ,
                LEVEL,
                TRANSECT = Transect,
                DEPTH_M ,
                DEPTH_FT ,
                VIS_M,
                SURGE,
                HEADING,
                TEMP_C,
                TEMP_F,
                SPECIES,
                SEGMENT,
                QUANTITY = Count,
                "SIZE (MIN)" = Size.cm,
                "SIZE (MAX"  = Size.cm,
                "METERS sampled" = Transect.area,
                Cntrl_a ,
                Notes	= Notes ,
                ORG_TYPE = group)

# Write to FILE
#write.csv(dat.2015.swath.fish,file="Fish_swath_2015.csv",row.names = F)
###

