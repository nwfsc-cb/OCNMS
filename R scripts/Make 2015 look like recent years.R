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
              mutate(Transect.area..inverts.=ifelse(Transect.area..inverts.=="",0,Transect.area..inverts.)) %>%
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
                                                    Transect.area..stipes. > 0 ,0,Transect.area..inverts.))

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
              mutate(Swath.width = case_when(Transect.area > 30 ~ 2,
                                             Transect.area <= 30 ~ 1))

# Add in a Fish group id.                                             
swath.dat <- swath.dat %>% mutate(group = as.character(group),
                                  group = ifelse(PISCO.datatype == "fish","Fish",group))


dat.2015.swath <- swath.dat %>% 
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
                "METERS sampled" = Transect.area,
                SIZE_CODE = Echinoderm.size..TD..TR..BD..BR..T.Total..B.Basal..D.Diameter..R.Radius..L.length.,
                SIZE =Size.cm,
                cntrl_a ,
                notes	= Notes ,
                "DATA ENTRY PROBLEMS" ,
                ORG_TYPE = group)

### Last Checks
# Cull observations without CLASSCODE
dat.2015.swath <- dat.2015.swath %>% filter(!CLASSCODE == "" )

# SIZE_CODE is new and has three levels BD = Basal Diameter , TD = Total Diameter, TR = Total Radius



write.csv(dat.2015.swath,file="TEMP.2015.csv",row.names = F)



















  
  
  
  
  
  
  

# observed species in all years
SP.all <- data.frame(species=unique(c(as.character(swath.dat$PISCO.Classcode))))
SP.all.common.names <- left_join(SP.all,species_names)

#####
dat.long <- NULL
SP <- SP.all[SP.all != "NO_ORG" & SP.all != "NOT_DONE"]
for( i in 1: length(SP)){
  if(nrow(swath.dat %>% filter(PISCO.Classcode == SP[i])) >0){
    temp  <-  left_join(base.dat %>% filter(group == species_names$group[species_names$species==SP[i]]) , swath.dat %>% filter(PISCO.Classcode == SP[i]) ) 
    temp$PISCO.Classcode <- SP[i]; temp$Species <- unique(temp$Species)[is.na(unique(temp$Species))==F][1]
  }
  if(nrow(swath.dat %>% filter(PISCO.Classcode == SP[i])) ==0){
    temp  <-  data.frame(base.dat %>% filter(group == species_names$group[species_names$species==SP[i]]), 
                         Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], PISCO.Classcode = SP[i],
                         Count= 0,Size.cm=NA) #,size_class=NA) 
  }
  dat.long <- rbind(dat.long, temp)
}

dat.long$Count[is.na(dat.long$Count)==T] <- 0
dat.long$year <- 2015

dat.long <- dat.long %>% 
  rename(site=Site,transect=Transect,observer=Observer,
         common.name=Species,species=PISCO.Classcode)

dat.2015.swath  <-  dat.long
dat.2015.swath.count <- dat.long %>% group_by(site,transect,observer,Transect.area,common.name,species,year,group) %>%
  summarise(count=sum(Count)) %>% rename(Count=count) 
base.dat.2015 <- base.dat
######################################

                                  

                                  



class(dat.2015$Date
      )



