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
dat.pre.2015 <- dat.trim %>% rename(group.name=group)

data.dir <- paste0(base.dir,"/Data/CSV_2015_on")
setwd(data.dir)

dat.2015 <- read.csv("2015_OCNMSDataComplete_standardized_122116.csv")
dat.2016.on.swath <- read.csv("NWFSC_SWATH_ALLYEARS_data_2021.csv")

dat.2016.on.swath$CLASSCODE[dat.2016.on.swath$CLASSCODE=="no_alg"] <- "NO_ALG"

species_names <- read.csv("species_code_list_swath.csv")

######### THIS IS FOR PROCESSING 2015 DATA.

nom <- c("Destruction Island","Teahwhit Head","Cape Johnson","Rock 305","Cape Alava","Point of the Arches",
         "Anderson Point","Tatoosh Island","Chibadehl Rocks","Neah Bay")
nom.ord <- 1:length(nom)
nom.merge <- data.frame(nom=nom,id=nom.ord)


# Make list of transects by site, and species group.
dat.2015 <- left_join( dat.2015, species_names %>% dplyr::select(species,group) %>% rename(PISCO.Classcode=species))
base.dat <- dat.2015 %>% filter(PISCO.datatype=="swath",data.type=="swath") %>% group_by(Site,Transect,Observer,Transect.area..inverts.,group) %>% 
  summarise(x=length(Transect)) %>% filter(is.na(Transect)==F) %>% dplyr::select(Site,Transect,Observer,Transect.area=Transect.area..inverts.,group) %>% as.data.frame()

# Replace missing transect area with standard 60 m^2 searched.
base.dat$Transect.area <- as.numeric(as.character(base.dat$Transect.area))
base.dat <- base.dat %>% mutate(Transect.area=ifelse(is.na(Transect.area) ==T,60,Transect.area))

#MAKE CATCH FOR if there are more than 4 total observations for each site in 2015
# A <- base.dat %>% group_by(Site) %>% summarise(N= length(Transect))
# if(max(A$N) > 4 | min(A$N) < 4 ){ print(rep("STOP, SOMETHING IS WRONG",100))}
base.dat$Transect <- as.integer(base.dat$Transect )

#### OK. Merge in the invert data to the base data for each species observed.  Make one giant data frame
swath.dat <- dat.2015 %>% filter(PISCO.datatype=="swath", data.type=="swath") %>% 
  group_by(Site,Transect,Observer,Species,PISCO.Classcode,Size.cm,group) %>%
  summarise(Count = sum(Count))  

### Fix one mis-spelling in the 2015 data 
swath.dat$PISCO.Classcode <- as.character(swath.dat$PISCO.Classcode)
swath.dat <- swath.dat %>% mutate(PISCO.Classcode = ifelse(PISCO.Classcode=="DOROHD","DORODH",PISCO.Classcode))
## Replace ASTSPP with STARREC (term used in 2016 and later)
swath.dat <- swath.dat %>% mutate(PISCO.Classcode = ifelse(PISCO.Classcode=="ASTSPP","STARREC",PISCO.Classcode))


# observed species in all years
SP.all <- data.frame(species=unique(c(as.character(swath.dat$PISCO.Classcode),as.character(dat.2016.on.swath$CLASSCODE))))
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
      temp  <-  data.frame(base.dat %>% filter(group == species_names$group[species_names$species==SP[i]]) %>%
                           mutate(Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], 
                                  PISCO.Classcode = SP[i], Count= 0, Size.cm=NA)) #,size_class=NA) 
    }
  dat.long <- rbind(dat.long, temp)
  }
  
dat.long$Count[is.na(dat.long$Count)==T] <- 0
dat.long$year <- 2015

dat.long <- dat.long %>% 
  rename(site=Site,transect=Transect,observer=Observer,
         common.name=Species,species=PISCO.Classcode)

# manually remove an oddball observation from Jameal, 2015, destruction Is., for the three main canopy species.
dat.long <- dat.long %>% 
                    mutate(Transect.area = 
                             ifelse(observer=="JS"& transect==2 & group=="Algae" &
                                      site=="Destruction Island SW" & species %in% c("PTECAL","NERLUE","MACPYR") &
                                      Transect.area ==60, -99,Transect.area)) %>%
                    mutate(Transect.area = 
                            ifelse(observer=="JS"& transect==2 & group=="Algae" &
                                      site=="Destruction Island SW" & !species %in% c("PTECAL","NERLUE","MACPYR") &
                                      Transect.area ==10, -99,Transect.area)) %>%
                    filter(!Transect.area==-99)
                                 

dat.2015.swath  <-  dat.long
dat.2015.swath.count <- dat.long %>% group_by(site,transect,observer,Transect.area,common.name,species,year,group) %>%
                          summarise(count=sum(Count)) %>% rename(Count=count) 
base.dat.2015 <- base.dat
############################################################
############################################################
############################################################
# Repeat for 2016 and later data.

colnames(dat.2016.on.swath)[2] <- "YEAR"
colnames(dat.2016.on.swath)[which(colnames(dat.2016.on.swath)=="SIDE")] <- "AREA"
## Replace a couple of CLASSCODE entry errors from 2016:
#dat.2016.on.swath$CLASSCODE[dat.2016.on.swath$CLASSCODE=="no_alg"] <- "NO_ALG"

# Add in an indicator for Algae or Invert transects
dat.2016.on.swath <- dat.2016.on.swath %>% left_join(., species_names %>% rename(CLASSCODE=species))

base.dat <- dat.2016.on.swath %>%
              group_by(YEAR,SITE,AREA,TRANSECT,OBSERVER,ZONE,SEGMENT,group) %>% 
              summarise(x=length(TRANSECT))%>% dplyr::select(-x) %>%as.data.frame()

swath.dat <- dat.2016.on.swath %>% group_by(YEAR,SITE,AREA,TRANSECT,OBSERVER,ZONE,CLASSCODE,SIZE,SEGMENT,group,METERS.sampled) %>% 
              summarise(Count=sum(COUNT))  %>% rename(SPECIES=CLASSCODE) %>% as.data.frame()

dat.long <- NULL
  for( i in 1: length(SP)){
  #Loop for species (both inverts and algae)
    if(nrow(swath.dat %>% filter(SPECIES == SP[i])) >0){
      GROUP <- species_names %>% filter(species ==SP[i])%>% dplyr::select(group) %>% unlist(.) %>% as.character()
      
      temp  <-  left_join(base.dat %>% filter(group == GROUP), swath.dat %>% filter(SPECIES == SP[i], group == GROUP) ) 
      temp$SPECIES <- SP[i]; temp$SPECIES <- unique(temp$SPECIES)[is.na(unique(temp$SPECIES))==F]
    }
    if(nrow(swath.dat %>% filter(SPECIES == SP[i])) ==0){
      GROUP <- species_names %>% filter(species ==SP[i])%>% dplyr::select(group) %>% pull() %>% as.character()
      temp  <-  data.frame(base.dat %>% filter(group==GROUP), 
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
dat.2016.plus.swath <- dat.long %>% filter(!group == "MISSING")

dat.2016.plus.swath <- dat.2016.plus.swath %>% mutate(METERS.sampled=ifelse(is.na(METERS.sampled)==T,10,METERS.sampled))

# Tiny data fix for one dat point
dat.2016.plus.swath <- dat.2016.plus.swath %>% 
                mutate(METERS.sampled = ifelse(METERS.sampled==0 & species =="PISOCH",10,METERS.sampled)) 
                

dat.2016.plus.swath = dat.2016.plus.swath %>% mutate(expand = 10 / METERS.sampled,
                                                     expand=ifelse(SEGMENT=="ALL",1,expand),
                                                     total.count = Count * expand )

# Aggregate by transect, site, etc. using the expanded counts 
dat.2016.plus.swath <- dat.2016.plus.swath %>% group_by(year,site,area,transect,observer,zone,species,group,SEGMENT) %>%
                          summarize(Count.seg=sum(total.count)) %>% group_by(year,site,area,transect,observer,zone,species,group,) %>%
                          summarize(Count=sum(Count.seg),N.segments = length(Count.seg)) %>% mutate(Transect.area = 20*N.segments)

#############################################################################
# Combine years of data later than 2015 into one data frame
dat.swath <- full_join(dat.2015.swath.count,dat.2016.plus.swath)
# Assign all 2015 transets to the 5m depth zone
dat.swath <- dat.swath %>% mutate(zone=ifelse(year==2015 & is.na(zone)==T,5,zone))
dat.swath <-dat.swath %>% filter(site != "")
dat.swath <- dat.swath %>% filter(!group =="MISSING")

dat.swath.base <- dat.swath %>% as.data.frame()
dat.swath.base$site <- as.character(dat.swath.base$site)

dat.swath.base <- dat.swath.base %>% mutate(site=ifelse(site=="Anderson Pt.","Anderson Point",site)) %>%
  mutate(site=ifelse(site=="Chibahdel","Chibadehl Rocks",site)) %>%
  mutate(site=ifelse(site=="Destruction Island SW","Destruction Island",site)) %>%
  mutate(site=ifelse(site=="Pt. of the Arches","Point of the Arches",site)) %>%
  mutate(site=ifelse(site=="Teawhit Head","Teahwhit Head",site))

dat.swath.base$area <- as.character(dat.swath.base$area)

dat.swath.base <- dat.swath.base %>% ungroup() %>%
  mutate(
    area = case_when(
      site == "Cape Alava" & area %in% c("1","W") ~ "W",
      site == "Cape Alava" & area %in% c("2","E") ~ "E",
      site == "Cape Johnson"  & area %in% c(1,"S") ~ "S",
      site == "Cape Johnson"  & area %in% c(2,"N") ~ "N",
      site == "Destruction Island" & area %in% c(1,"S") ~ "S",
      site == "Destruction Island" & area %in% c(2,"N") ~ "N",
      site == "Tatoosh Island" & area %in% c(1,"N") ~ "N",
      site == "Tatoosh Island" & area %in% c(2,"S") ~ "S",
      site == "Neah Bay" & area %in% c(1,"N") ~ "N",
      site == "Neah Bay" & area %in% c(2,"S") ~ "S",
      TRUE ~ NA_character_
    )
  )

# This is a long set of transects conducted North to South by Jameal and Ole. We divide them up by placing half in 
# The northern sub area and half in the southern sub area
dat.swath.base$area <- as.character(dat.swath.base$area)
dat.swath.base      <- dat.swath.base %>% ungroup() %>%
                        mutate(area = ifelse(site=="Cape Johnson" & year ==2017 & is.na(area) & transect <=3, "N",area)) %>%
                        mutate(area = ifelse(site=="Cape Johnson" & year ==2017 & is.na(area) & transect >3, "S",area))

# Check for area labeling
# dat.swath.base %>% ungroup() %>% dplyr::select(year, site,area,transect) %>% distinct(year,site,area) %>%
#       as.data.frame()
# 
 dat.swath.base %>% ungroup() %>% dplyr::select(year, site,area,transect) %>% distinct(year,site,area) %>%
   as.data.frame()

saveRDS(dat.swath.base,"Swath_2015-2021.rds")

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

###################################################################
# Separate out invertebrates and algae.
dat.invert <- dat.swath.base %>% filter(group == "Invert")
dat.algae  <- dat.swath.base %>% filter(group == "Algae")

#### INVERT ANALYSIS and plotting.
# Convert to m2 for comparability across all years.
dat.invert.density <- dat.invert %>% mutate(density = Count / Transect.area) %>%
                group_by(year,site,area,zone,common.name,species) %>% 
                summarize(MEAN=mean(density),N.obs=length(Count),SD=sd(density),SE.tot=SD/sqrt(N.obs))

dat.invert.density <- dat.pre.2015 %>% dplyr::select(year=Year,site=Site,group.name,MEAN=MEAN,SE.tot=SE) %>% full_join(.,dat.invert.density) %>% 
                        mutate(zone=ifelse(year<2015,5,zone))

dat.invert.density$group.name <- as.character(dat.invert.density$group.name)
dat.invert.density$site <- as.character(dat.invert.density$site)

# combine the names of the sites sensibly.
dat.invert.density <- dat.invert.density %>% mutate(site=ifelse(site=="Anderson Pt.","Anderson Point",site)) %>%
                                   mutate(site=ifelse(site=="Chibahdel","Chibadehl Rocks",site)) %>%
                                   mutate(site=ifelse(site=="Destruction Island SW","Destruction Island",site)) %>%
                                   mutate(site=ifelse(site=="Pt. of the Arches","Point of the Arches",site)) %>%
                                   mutate(site=ifelse(site=="Teawhit Head","Teahwhit Head",site))

#  Pull out urchins and seastar species
urchin = c("MESFRA","STRDRO", "STRPUR")
seastar = c("CROPAP","DERIMB","EVATRO","HENLEV","MEDAEQ","ORTKOE",
            "PATMIN","PISBRE","PISGIG","PISOCH","PISSPP","PYCHEL","SOLSTI")
seastar.2 <- c("LEPSPP","STARREC")
crab  = c("CANORE","CANSPP","CRYDEC","CRYSIT","LOPMAN","MIMFOL","PUGGRA","PUGPRO","SCYDEC")
crab.hermit = c("PAGSPP")
bivalve = c("CRAGIG", "MYTCAL", "PODSPP","CLAMSP","SCALLOP")
chiton = c("CRYSTE")
chiton.unid = c("CHITON")
nudibranch = c("ACAHUD","ACANAN","DIASAN","DIRALB","DORODH","DOROHD","HERCRA","JANFUS","LIMCOC","NUDIBR","PELNOB","TRICAT")
octopus = c("ENTDOF")
sponge = c("CRAARB")
cucumber = c("CUCMIN", "EUPQUI","PARCAL")
gastropod = c("CERFOL", "DIOASP", "FUSORE", "HALKAM", "LIRDIR", "NUCLAM", "ACMMIT","TEGSPP") 
tunicate = c("STYMON")
anenome = c("URTCRA","URTLOF","URTPIS","URTSPP", "ANTSPP","ANTELE","ANTXAN","EPIPRO","METGIG","METSPP")

dat.invert.density <- dat.invert.density %>%
        mutate(
           group.name=ifelse(species %in% urchin,"urchin",group.name),
           group.name=ifelse(species %in% seastar,"seastar",group.name),
           group.name=ifelse(species %in% seastar.2,"seastar.small",group.name),
           group.name=ifelse(species %in% crab,"crab",group.name),                                                 
           group.name=ifelse(species %in% bivalve,"bivalve",group.name),
           group.name=ifelse(species %in% chiton,"chiton",group.name),
           group.name=ifelse(species %in% cucumber,"cucumber",group.name),
           group.name=ifelse(species %in% nudibranch,"nudibranch",group.name),
           group.name=ifelse(species %in% octopus,"octopus",group.name),
           group.name=ifelse(species %in% gastropod,"gastropod",group.name),                                                  
           group.name=ifelse(species %in% tunicate,"tunicate",group.name),
           group.name=ifelse(species %in% sponge,"sponge",group.name),
           group.name=ifelse(species %in% anenome,"anenome",group.name))

write.csv(dat.invert,file="Invert counts.csv",row.names = F)
write.csv(dat.invert.density,file="Invert densities.csv",row.names = F)

# Summarize densities by species groups 
dat.invert.species.zone <- dat.invert.density %>% group_by(year,site,species,zone) %>%
  summarize(n.area=length(unique(area)),
            Mean=sum(MEAN)/n.area, 
            SE = sqrt(sum(SE.tot^2) / n.area^2)) %>% as.data.frame()


dat.invert.group.zone <- dat.invert.density %>% group_by(year,site,group.name,zone) %>%
                            summarize(n.area=length(unique(area)),
                                Mean=sum(MEAN)/n.area, 
                                SE = sqrt(sum(SE.tot^2) / n.area^2)) %>% as.data.frame()

dat.invert.group <- dat.invert.group.zone %>% group_by(year,site,group.name) %>%
                      #summarize(MeaN=sum(MEAN), Se = sqrt(sum(SE^2,na.rm=T))) %>% group_by( year,site,group.name) %>%
                      summarise(n.zone=length(unique(zone)),Mean=sum(Mean)/n.zone, SE = sqrt(sum(SE^2,na.rm=T) / n.zone^(2)) )

#loop over groups for plots
GROUPS <- c("urchin","seastar","crab","bivalve","chiton","cucumber","gastropod","tunicate","anenome")
SITES  <- c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay")

dat.invert.group$site <- factor(dat.invert.group$site,levels=SITES) 
dat.invert.species.zone$site <- factor(dat.invert.species.zone$site,levels=SITES)
dat.invert.group.zone$site <- factor(dat.invert.group.zone$site,levels=SITES)
# A <- list()
# for(i in 1:length(GROUPS)){
# A[[i]] <- ggplot(dat.swath.group %>% filter(group.name == GROUPS[i],site %in% SITES )) +
#     geom_point(aes(x=year,y=Mean,color=site)) +
#     geom_line(aes(x=year,y=Mean,color=site)) +
#     geom_errorbar(aes(x=year,ymin=Mean-SE,ymax=Mean+SE,color=site))+
#     #facet_grid(group~.,scales = "free") +
#     ylab(expression("Mean density (m"^-2*")"))+
#     xlab("Year") +
#     scale_color_discrete("Site") +
#     ggtitle(GROUPS[i]) +
#     theme_bw()
# }












# pdf(file=paste0(base.dir,"/Plots/__Chicago 12-2018/All Inverts.pdf"),onefile=T,width=7,height=5)
# for(i in 1:length(GROUPS)){
#   print(A[[i]])
# }
# dev.off()
# 
# 
# ### FOR UCHICAGO PROPOSAL ON DIVERSITY.
# 
# dat.chicago <- dat.swath.group %>% filter(group %in% c("urchin","seastar"),site %in% c("Tatoosh Island","Neah Bay")) %>%
#                           mutate(site=ifelse(site=="Neah Bay", "Koitlah",site),site=ifelse(site=="Tatoosh Island","Tatoosh",site))
# 
# B <- ggplot(dat.chicago ) +
#   geom_point(aes(x=year,y=Mean,color=site)) +
#   geom_line(aes(x=year,y=Mean,color=site)) +
#   geom_errorbar(aes(x=year,ymin=Mean-SE,ymax=Mean+SE,color=site))+
#   facet_grid(group~.,scales = "free") +
#   ylab(expression("Mean density (m"^-2*")"))+
#   xlab("Year") +
#   scale_color_discrete("Site") +
#   #theme(legend.position = c(0.9, 0.2)) +
#   theme_bw() +
#   theme(legend.position = c(0.7, 0.9)
#       ,legend.background = element_rect(fill = "white", colour = NA))
# 
# quartz(file=paste0(base.dir,"/Plots/__Chicago 12-2018/Seastars and Urchins, Neah + Tatoosh.pdf"),type="pdf",dpi=600,width=4,height=5)
# print(B)
# dev.off()
