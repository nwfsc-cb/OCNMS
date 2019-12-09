#### Analysis of the fish and cover data for the OCNMS survey region.

# Libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)

rm(list=ls())

# Pull in pre-2015 data (Kvitek surveys)
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Fish, Invert, Kelp Analysis/Data process pre-2017 data.R",sep=""))
dat.pre.2015 <- dat.trim

data.dir <- "/Users/ole.shelton/GitHub/OCNMS/Data/CSV_2015_on"
setwd(data.dir)

dat.2015 <- read.csv("2015_OCNMSDataComplete_standardized_122116.csv")
dat.2016.on.swath <- read.csv("NWFSC_SWATH_ALLYEARS_data_entry_2018.csv")

species_names <- read.csv("species_code_list_swath.csv")

#########

nom <- c("Destruction Island","Teahwhit Head","Cape Johnson","Rock 305","Cape Alava","Point of the Arches",
         "Anderson Point","Tatoosh Island","Chibadehl Rocks","Neah Bay")
nom.ord <- 1:length(nom)
nom.merge <- data.frame(nom=nom,id=nom.ord)

# Make list of transects by site, 
base.dat <- dat.2015 %>% filter(PISCO.datatype=="swath",data.type=="swath") %>% group_by(Site,Transect,Observer,Transect.area..inverts.) %>% 
  summarise(x=length(Transect)) %>% filter(is.na(Transect)==F) %>% dplyr::select(Site,Transect,Observer,Transect.area=Transect.area..inverts.) %>% as.data.frame()

# Replace missing transect area with standard 60 m^2 searched.
base.dat$Transect.area <- as.numeric(as.character(base.dat$Transect.area))
base.dat <- base.dat %>% mutate(Transect.area=ifelse(is.na(Transect.area) ==T,60,Transect.area))

#MAKE CATCH FOR if there are more than 4 total observations for each site in 2015
# A <- base.dat %>% group_by(Site) %>% summarise(N= length(Transect))
# if(max(A$N) > 4 | min(A$N) < 4 ){ print(rep("STOP, SOMETHING IS WRONG",100))}
base.dat$Transect <- as.integer(base.dat$Transect )

#### OK. Merge in the fish data to the base data for each species observed.  Make one giant data frame
swath.dat <- dat.2015 %>% filter(PISCO.datatype=="swath", data.type=="swath") %>% 
  group_by(Site,Transect,Observer,Species,PISCO.Classcode,Size.cm) %>%
  summarise(Count = sum(Count))  

# observed species in all years
SP.all <- data.frame(species=unique(c(as.character(swath.dat$PISCO.Classcode),as.character(dat.2016.on.swath$CLASSCODE))))
SP.all.common.names <- left_join(SP.all,species_names)

#####
dat.long <- NULL
SP <- SP.all[SP.all != "NO_ORG"]
  for( i in 1: length(SP)){
    if(nrow(swath.dat %>% filter(PISCO.Classcode == SP[i])) >0){
      temp  <-  left_join(base.dat, swath.dat %>% filter(PISCO.Classcode == SP[i]) %>% select(-Size.cm)) 
      temp$PISCO.Classcode <- SP[i]; temp$Species <- unique(temp$Species)[is.na(unique(temp$Species))==F][1]
    }
    if(nrow(swath.dat %>% filter(PISCO.Classcode == SP[i])) ==0){
      temp  <-  data.frame(base.dat, 
                           Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], PISCO.Classcode = SP[i],
                           Count= 0) #,size_class=NA) 
    }
  dat.long <- rbind(dat.long, temp)
  }
  
dat.long$Count[is.na(dat.long$Count)==T] <- 0
dat.long$year <- 2015

dat.long <- dat.long %>% 
  rename(site=Site,transect=Transect,observer=Observer,
         common.name=Species,species=PISCO.Classcode)

dat.2015.swath <- dat.long
base.dat.2015 <- base.dat
############################################################
############################################################
############################################################
# Repeat for 2016 and later data.

colnames(dat.2016.on.swath)[2] <- "YEAR"
colnames(dat.2016.on.swath)[which(colnames(dat.2016.on.swath)=="SIDE")] <- "AREA"
base.dat <- dat.2016.on.swath %>% group_by(YEAR,SITE,AREA,TRANSECT,OBSERVER,ZONE,SEGMENT) %>% 
  summarise(x=length(TRANSECT))%>% dplyr::select(-x) %>%as.data.frame()

swath.dat <- dat.2016.on.swath %>% group_by(YEAR,SITE,AREA,TRANSECT,OBSERVER,ZONE,CLASSCODE,SIZE,SEGMENT,METERS.sampled) %>% 
  summarise(Count=sum(COUNT)) %>% as.data.frame() %>% rename(SPECIES=CLASSCODE)

dat.long <- NULL
  for( i in 1: length(SP)){
  #Loop for non-rockfish
    if(nrow(swath.dat %>% filter(SPECIES == SP[i])) >0){
      temp  <-  left_join(base.dat, swath.dat %>% filter(SPECIES == SP[i]) ) 
      temp$SPECIES <- SP[i]; temp$SPECIES <- unique(temp$SPECIES)[is.na(unique(temp$SPECIES))==F]
    }
    if(nrow(swath.dat %>% filter(SPECIES == SP[i])) ==0){
      temp  <-  data.frame(base.dat, 
                           #Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], 
                           SPECIES = SP[i],
                           SIZE=NA, METERS.sampled=10, Count= 0) 
    }
    dat.long <- rbind(dat.long, temp)
  }
  
# Housekeeping
dat.long$Count[is.na(dat.long$Count)==T] <- 0
dat.long <- dat.long %>% 
  rename(year=YEAR,site=SITE,area=AREA,transect=TRANSECT,observer=OBSERVER,
         zone=ZONE,species=SPECIES)
dat.2016.plus.swath <- dat.long
dat.2016.plus.swath <- dat.2016.plus.swath %>% mutate(METERS.sampled=ifelse(is.na(METERS.sampled)==T,10,METERS.sampled))
dat.2016.plus.swath = dat.2016.plus.swath %>% mutate(expand = 10 / METERS.sampled,
                                                     expand=ifelse(SEGMENT=="ALL",1,expand),
                                                     total.count = Count * expand )

# Aggregate by transect, site, etc. using the expanded counts 
dat.2016.plus.swath <- dat.2016.plus.swath %>% group_by(year,site,area,transect,observer,zone,species,SEGMENT) %>%
                          summarize(Count.seg=sum(total.count)) %>% group_by(year,site,area,transect,observer,zone,species) %>%
                          summarize(Count=sum(Count.seg),N.segments = length(Count.seg)) %>% mutate(Transect.area = 20*N.segments)

#############################################################################
# Combine all years of data into one data frame
dat.swath <- full_join(dat.2015.swath,dat.2016.plus.swath)
# Assign all 2015 transets to the 5m depth zone
dat.swath <- dat.swath %>% mutate(zone=ifelse(year==2015 & is.na(zone)==T,5,zone))

# Convert to m2 for comparability across all years.
dat.swath <- dat.swath %>% mutate(density = Count / Transect.area) %>%
                group_by(year,site,area,zone,common.name,species) %>% 
                summarize(MEAN=mean(density),N.obs=length(Count),SD=sd(density),SE=SD/sqrt(N.obs))

#  Pull out urchins and seastar species
urchin = c("MESFRA","STRDRO", "STRPUR")
seastar = c("ASTSPP","CROPAP","DERIMB","EVATRO","HENLEV","MEDAEQ","ORTKOE",
              "PATMIN","PISBRE","PISGIG","PISOCH","PISSPP","PYCHEL","SOLSTI")

urchin.seastar.post.2015  <- dat.swath %>% filter(species %in% c(seastar,urchin)) %>% 
                              mutate(group="seastar",group=ifelse(species %in% urchin,"urchin",group))

both.temp <- urchin.seastar.post.2015 %>% group_by(year,site,group) %>% 
                summarize(Mean=sum(MEAN), SE.tot = sqrt(sum(SE^2))) %>% as.data.frame()
                  
# Combine with Kviteck surveys (pre-2015)
all.urchin.seastar <- dat.pre.2015 %>% select(year=Year,site=Site,group,Mean=MEAN,SE.tot=SE) %>% 
                        filter(group %in% c("urchin","seastar"))
all.urchin.seastar <- rbind(all.urchin.seastar,both.temp)                    


A <- ggplot(all.urchin.seastar %>% filter(site %in% c("Neah Bay","Tatoosh Island"))) +
    geom_point(aes(x=year,y=Mean,color=site)) +
    geom_line(aes(x=year,y=Mean,color=site)) +
    geom_errorbar(aes(x=year,ymin=Mean-SE.tot,ymax=Mean+SE.tot,color=site))+
    facet_grid(group~.,scales = "free") +
    ylab(expression("Mean density (m"^-2*")"))+
    xlab("Year") +
    scale_color_discrete("Site") +
    theme_bw()


quartz(file=paste0(base.dir,"/Plots/__Chicago 12-2018/Seastars and Urchins, Neah + Tatoosh.pdf"),type="pdf",dpi=600,width=5,height=5)
print(A)
dev.off()




