#### Analysis of the fish and cover data for the OCNMS survey region.

# Libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)
library(readxl)
# home_dir = getwd()
# data_dir <- paste0(home_dir,"/data/")
# setwd(data_dir)

# load 2015 data ###############################################################
dat.2015 <- data.frame(read.csv("~/GitHub/OCNMS/Data/2015/2015_OCNMSDataComplete_standardized_122116.csv"))
dat.2015$PISCO.Classcode[dat.2015$PISCO.Classcode=="MYOPOL"] <- "MYOP"
# update this file each year ###################################################
# done automatically in the 01_annual-update-and-univariate-plots.rmd file
# data_year = 2024
# fishx = data.frame(read.csv("~/GitHub/OCNMS/Data/2024/NWFSC_FISH_ALLYEARS_data_2024.csv"))

dat.2016.on.fish <- fishx # or current year file....

# species_names <- fish_codes # read.csv("species_code_list.csv")
x = grep("y", fish_codes$species)
species_names = fish_codes[-x,]
# species_names = species_names %>% filter(species != 'RYOY')

# species_names <- species_names %>% rename("species"="PISCO.CLASSCODE")

# Define codes for rockfish 
ROCKFISH <- c("SEAU", "SEBSPP", "SECA", "SEFL",
                "SEMA","SEME", "SEMI", "SEMY", 
                "SENE", "SEPA", "SEPI")

## size break in CM for rockfish
size.break = 10

nom <- c("Destruction Island","Teahwhit Head","Cape Johnson","Rock 305",
         "Cape Alava","Point of the Arches", "Anderson Point","Tatoosh Island",
         "Chibadehl Rocks","Neah Bay")
nom.ord <- 1:length(nom)

nom.merge <- data.frame(nom=nom,id=nom.ord)

# Make list of transects by site, 
base.dat <- dat.2015 %>% filter(PISCO.datatype=="fish", data.type=="swath") %>% 
  group_by(Site,Transect,Observer) %>% 
  summarise(x=length(Transect)) %>% 
  filter(is.na(Transect)==F) %>% 
  dplyr::select(Site,Transect,Observer) %>% 
  as.data.frame()

#ADD IN ONE TRANSECT THAT HAD ZERO OBSERVATIONS (KELLY ANDREWS, TATOOSH, TRANSECT=2)
base.dat <- rbind(base.dat,c("Tatoosh Island",2,"KA")) %>% arrange(Site,Observer,Transect)
#MAKE CATCH FOR if there are more than 4 total observations for each site in 2015
  A <- base.dat %>% group_by(Site) %>% summarise(N= length(Transect))
  if(max(A$N) > 4 | min(A$N) < 4 ){ print(rep("STOP, SOMETHING IS WRONG",100))}
base.dat$Transect <- as.integer(base.dat$Transect )

#### OK. Merge in the fish data to the base data for each species observed.  Make one giant data frame
fish.dat <- dat.2015 %>% filter(PISCO.datatype=="fish", data.type=="swath") %>% 
  group_by(Site,Transect,Observer,Species,PISCO.Classcode,Size.cm) %>%
  summarise(Count = sum(Count))  

# observed species in all years
SP.all <- data.frame(species=unique(c(as.character(fish.dat$PISCO.Classcode),
                                      as.character(dat.2016.on.fish$SPECIES))))
SP.all.common.names <- left_join(SP.all, species_names)
###############################
### Parse Rockfish into large and small size categories.
non.SEB.dat <- fish.dat %>% filter(!PISCO.Classcode %in% ROCKFISH) %>% mutate(size_class=NA)
SEB.dat     <- fish.dat %>% filter(PISCO.Classcode %in% ROCKFISH) %>%
                      mutate(size_class=case_when(
                             Size.cm <= size.break~"small",
                             Size.cm > size.break ~"large",
                             TRUE ~as.character(Size.cm)))

fish.dat <- data.frame(rbind(non.SEB.dat,SEB.dat))

#####
# Make a padded data frame with zeros for each species.
#####
dat.long <- NULL
SP <- SP.all[SP.all != "NO_ORG"]
SP
for( i in 1: length(SP)){
  #Loop for non-rockfish
  if( (SP[i] %in% ROCKFISH) ==F){
    if(nrow(fish.dat %>% filter(PISCO.Classcode == SP[i])) >0){
      temp  <-  left_join(base.dat, fish.dat %>% filter(PISCO.Classcode == SP[i]) 
                          %>% select(-Size.cm)) 
      temp$PISCO.Classcode <- SP[i]; temp$Species <- unique(temp$Species)[is.na(unique(temp$Species))==F]
    }
    if(nrow(fish.dat %>% filter(PISCO.Classcode == SP[i])) ==0){
      temp  <-  data.frame(base.dat, 
                         Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], PISCO.Classcode = SP[i],
                         Count= 0,size_class=NA) 
    }
  }
  #Loop for ROCKFISH split into large and small size categories.
  if( (SP[i] %in% ROCKFISH) ==T){
    if(nrow(fish.dat %>% filter(PISCO.Classcode == SP[i],size_class=="large")) >0){
      temp1  <-  left_join(base.dat, fish.dat %>% filter(PISCO.Classcode == SP[i],size_class=="large") 
                           %>% select(-Size.cm)) 
      temp1$PISCO.Classcode <- SP[i]; temp1$Species <- unique(temp1$Species)[is.na(unique(temp1$Species))==F]; temp1$size_class <- "large"
    }
    if(nrow(fish.dat %>% filter(PISCO.Classcode == SP[i],size_class=="small")) >0){
      temp2  <-  left_join(base.dat, fish.dat %>% filter(PISCO.Classcode == SP[i],size_class=="small") 
                           %>% select(-Size.cm)) 
      temp2$PISCO.Classcode <- SP[i]; temp2$Species <- unique(temp2$Species)[is.na(unique(temp2$Species))==F]; temp2$size_class <- "small"
    }
    if(nrow(fish.dat %>% filter(PISCO.Classcode == SP[i],size_class=="large")) ==0){
      temp1  <-  data.frame(base.dat, 
                           Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], PISCO.Classcode = SP[i],
                           Count= 0,size_class="large") 
    }
    if(nrow(fish.dat %>% filter(PISCO.Classcode == SP[i],size_class=="small")) ==0){
      temp2  <-  data.frame(base.dat, 
                            Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], PISCO.Classcode = SP[i],
                            Count= 0,size_class="small")
    }
    temp <- rbind(temp1,temp2)
  }
  # combine the files into one
    dat.long <- rbind(dat.long, temp )
}              
dat.long$Count[is.na(dat.long$Count)==T] <- 0
dat.long$size_class[dat.long$PISCO.Classcode =="RYOY"] <- "small"
dat.long$size_class[dat.long$PISCO.Classcode =="SEBYT"] <- "small"

dat.long$year <- 2015

dat.long <- dat.long %>% 
            rename(site=Site,transect=Transect,observer=Observer,
                  common.name=Species,species=PISCO.Classcode)

### merge in small and large classes for all rockfish 
### to ensure the data frames are properly padded with zeros
dat.u <- dat.long %>% distinct(year,site,transect,observer,species)

sp.temp <- c( "SEAU", "SECA",
              "SEFL", "SEMA","SEME",
              "SEMI","SEMY",
              "SENE","SEPA","SEPI")
sp.size <- c("small","large")
temp <- expand.grid(species = sp.temp,size_class=sp.size)
temp <- bind_rows(temp,c(species="RYOY",size_class="small"))
temp <- bind_rows(temp,c(species="SEBYT",size_class="small"))

dat.u <- dat.u %>% filter(species %in% sp.temp) %>% 
     left_join(.,temp, relationship = "many-to-many")
dat.long.rock <- dat.u %>% left_join(.,dat.long) 
dat.long.rock$Count[is.na(dat.long.rock$Count)==T] <- 0

# replace rockfish in the data padded with small and large
dat.long <- dat.long %>% filter(!species %in% sp.temp) %>% bind_rows(.,dat.long.rock)


# Fix some duplicate entries in the data so each species only shows up once per
# transect
dat.long <- dat.long %>% 
                group_by(site, transect, observer, common.name, species, size_class, year) %>%
                summarise(Count_all = sum(Count)) %>% rename(Count=Count_all)

dat.2015.fish <- dat.long

base.dat.2015 <- base.dat
################################################################################
################ Repeat for 2016 and later #####################################
################################################################################
colnames(dat.2016.on.fish)[2] <- "YEAR"
colnames(dat.2016.on.fish)[which(colnames(dat.2016.on.fish)=="SIDE")] <- "AREA"
base.dat <- dat.2016.on.fish %>% group_by(YEAR,SITE,AREA,TRANSECT,OBSERVER,ZONE,VIS_M) %>% 
  summarise(x=length(TRANSECT))%>% dplyr::select(-x) %>%as.data.frame()

dat.2016.on.fish$QUANTITY = as.numeric(dat.2016.on.fish$QUANTITY)

fish.dat <- dat.2016.on.fish %>% 
  group_by(YEAR,SITE,AREA,TRANSECT,OBSERVER,ZONE,
           VIS_M,SPECIES,SIZE..MIN.,SIZE..MAX.) %>% 
  summarise(Count=sum(QUANTITY)) %>% as.data.frame()

##########
non.SEB.dat <- filter(fish.dat,!grepl("SE",SPECIES)) %>% mutate(size_class=NA)
SEB.dat     <- filter(fish.dat,grepl("SE",SPECIES)) %>%
  mutate(size_class=case_when(
    SIZE..MIN. <= size.break~"small",
    SIZE..MIN. > size.break ~"large",
    TRUE ~as.character(SIZE..MIN.)))

### get rid of small number of observations with NA sizes from 2017, Destruction Island
SEB.dat$size_class[is.na(SEB.dat$size_class) ==T & grepl("YTy",SEB.dat$SPECIES)] <- "small"
SEB.dat$size_class[is.na(SEB.dat$size_class) ==T ] <- "large"

fish.dat <- data.frame(rbind(non.SEB.dat,SEB.dat))
fish.dat$size_class[fish.dat$SPECIES =="RYOY"] <- "small"
# consolidate into large and small groups 
fish.dat <- fish.dat %>% group_by(YEAR,SITE,AREA,TRANSECT,OBSERVER,ZONE,VIS_M,SPECIES,size_class) %>%
              summarize(Count=sum(Count))

dat.long <- NULL
for( i in 1: length(SP)){
  #Loop for non-rockfish
  if( (SP[i] %in% ROCKFISH) ==F){
    if(nrow(fish.dat %>% filter(SPECIES == SP[i])) >0){
      temp  <-  left_join(base.dat, fish.dat %>% filter(SPECIES == SP[i]) ) 
      temp$SPECIES <- SP[i]; temp$SPECIES <- unique(temp$SPECIES)[is.na(unique(temp$SPECIES))==F]
    }
    if(nrow(fish.dat %>% filter(SPECIES == SP[i])) ==0){
      temp  <-  data.frame(base.dat, 
                           #Species= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])], 
                           SPECIES = SP[i],
                           Count= 0,size_class=NA) 
    }
  }
  #Loop for ROCKFISH split into large and small size categories.
  if( (SP[i] %in% ROCKFISH) ==T){
    if(nrow(fish.dat %>% filter(SPECIES== SP[i],size_class=="large")) >0){
      temp1  <-  left_join(base.dat, fish.dat %>% filter(SPECIES == SP[i],size_class=="large") ) 
      temp1$SPECIES <- SP[i]; temp1$SPECIES <- unique(temp1$SPECIES)[is.na(unique(temp1$SPECIES))==F]; temp1$size_class <- "large"
    }
    if(nrow(fish.dat %>% filter(SPECIES == SP[i],size_class=="small")) >0){
      temp2  <-  left_join(base.dat, fish.dat %>% filter(SPECIES == SP[i],size_class=="small") ) 
      temp2$SPECIES <- SP[i]; temp2$SPECIES <- unique(temp2$SPECIES)[is.na(unique(temp2$SPECIES))==F]; temp2$size_class <- "small"
    }
    if(nrow(fish.dat %>% filter(SPECIES == SP[i],size_class=="large")) ==0){
      temp1  <-  data.frame(base.dat, 
                            #SPECIES= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])],
                            SPECIES = SP[i],
                            Count= 0,size_class="large") 
    }
    if(nrow(fish.dat %>% filter(SPECIES == SP[i],size_class=="small")) ==0){
      temp2  <-  data.frame(base.dat, 
                            #SPECIES= SP.all.common.names$common.name[which(SP.all.common.names$species==SP[i])],
                            SPECIES = SP[i],
                            Count= 0,size_class="small")
    }
    temp <- rbind(temp1,temp2)
  }
  # combine the files into one
  dat.long <- rbind(dat.long, temp )
} 

# Housekeeping
dat.long$Count[is.na(dat.long$Count)==T] <- 0

dat.long <- dat.long %>% 
  rename(year=YEAR,site=SITE,area=AREA,transect=TRANSECT,observer=OBSERVER,
         zone=ZONE,vis_m=VIS_M,species=SPECIES)

### merge in small and large classes for all rockfish 
### to ensure the data frames are properly padded with zeros
dat.u <- dat.long %>% distinct(year,site,area,transect,observer,zone,vis_m,species)

sp.temp <- c( "SEAU", "SECA",
              "SEFL", "SEMA","SEME",
              "SEMI","SEMY",
              "SENE","SEPA","SEPI")
sp.size <- c("small","large")
temp <- expand.grid(species = sp.temp,size_class=sp.size)
temp <- bind_rows(temp,c(species="RYOY",size_class="small"))
temp <- bind_rows(temp,c(species="SEBYT",size_class="small"))

dat.u <- dat.u %>% filter(species %in% sp.temp) %>% 
     left_join(.,temp,relationship = "many-to-many")
dat.long.rock <- dat.u %>% left_join(.,dat.long) 
dat.long.rock$Count[is.na(dat.long.rock$Count)==T] <- 0

# replace rockfish in the data padded with small and large
dat.long <- dat.long %>% filter(!species %in% sp.temp) %>% bind_rows(.,dat.long.rock)

#
dat.2016.plus.fish <- dat.long

# Combine all years of data into one data frame
dat.fish <- full_join(dat.2015.fish,dat.2016.plus.fish)
#dat.fish <- dat.fish[-which(dat.fish$species=="RYOY" & dat.fish$size_class=="large"),]

################################################################################
##### Reconcile different area designations among years ########################
################################################################################
dat.fish$area <- as.character(dat.fish$area)

dat.fish <- dat.fish %>% ungroup() %>%
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

# All samples in 2015 were collected at 5m depth
dat.fish$zone[dat.fish$year == 2015] <- 5

# This is a check on labeling
dat.fish %>% ungroup() %>% dplyr::select(year, site,area,transect) %>% distinct(year,site,area) %>%
 as.data.frame()

dat.fish$site <- factor(dat.fish$site,
                        levels = c("Destruction Island",
                                    "Teahwhit Head",
                                    "Cape Johnson",
                                    "Rock 305",
                                    "Cape Alava",
                                    "Point of the Arches",
                                    "Anderson Point",
                                    "Tatoosh Island",
                                    "Chibadehl Rocks",
                                    "Neah Bay"))
# drop rows with no years and no data ####
dat.fish = dat.fish %>% filter(!is.na(year))
max_year = max(dat.fish$year)

# output data for future analysis ##############################################

saveRDS(dat.fish, paste0(data_dir,"Fish_2015-",max_year,".rds"))

################################################################################
################################################################################
################################################################################

dat.transect.summary <- dat.fish %>% filter(year>2015) %>% group_by(year,site,area,zone) %>%
  summarise(MIN.t = min(transect),MAX.t = max(transect),N.t = length(unique(transect))) %>%
  arrange(year,site,area,zone) %>%
  as.data.frame()

write.csv(dat.transect.summary,file=paste0(data_dir,"FISH ERROR CHECK.csv"),row.names=F)

##############################################################
##############################################################
##############################################################

# # GENERATE INDEXES BY SITE and AMONG SITES
# dat.large.fish.summary <- dat.fish %>% filter(size_class=="large" | is.na(size_class)==T) %>%
#                     group_by(year,site,zone,species,size_class) %>% 
#                     summarise(Mean=mean(Count),SD=sd(Count),N=length(Count),SE=SD/sqrt(N)) %>% as.data.frame()
# 
# dat.large.fish.by.year <- dat.large.fish.summary %>% filter(size_class=="large"| is.na(size_class)==T) %>%
#                     group_by(year,zone,species,size_class) %>% 
#                     summarise(MEAN=mean(Mean),SD=sd(Mean),N=length(Mean),SE=SD/sqrt(N)) %>% as.data.frame()
# 
# 
# dat.small.fish.summary <- dat.fish %>% filter(size_class=="small") %>%
#                     group_by(year,site,zone,species,size_class) %>% 
#                     summarise(Mean=mean(Count),SD=sd(Count),N=length(Count),SE=SD/sqrt(N)) %>% as.data.frame()
# 
# dat.small.fish.by.year <- dat.small.fish.summary %>% filter(size_class=="small") %>%
#                     group_by(year,zone,species,size_class) %>% 
#                     summarise(MEAN=mean(Mean),SD=sd(Mean),N=length(Mean),SE=SD/sqrt(N)) %>% as.data.frame()
# 
# dat.small.fish.complex.summary <- dat.fish %>% filter(size_class=="small") %>%
#                     group_by(year,site,transect,observer,area,zone,vis_m,size_class) %>% 
#                     summarise(Sum=sum(Count)) %>%
#                     group_by(year,site,zone,size_class) %>% 
#                     summarise(Mean=mean(Sum),SD=sd(Sum),N=length(Sum),SE=SD/sqrt(N)) %>% as.data.frame()
# 
# dat.small.fish.complex.by.year <- dat.small.fish.complex.summary %>% filter(size_class=="small") %>%
#                     group_by(year,zone,size_class) %>% 
#                     summarise(MEAN=mean(Mean),SD=sd(Mean),N=length(Mean),SE=SD/sqrt(N)) %>% as.data.frame()
# 
# dat.large.fish.summary$site <- factor(dat.large.fish.summary$site,levels=nom)
# dat.small.fish.summary$site <- factor(dat.small.fish.summary$site,levels=nom)
# 
# 
# ### Make some tables that are helpful
# SP.count <- dat.fish %>% group_by(species,size_class) %>% summarise(tot = sum(Count))  %>% arrange(desc(tot))
# SP.count.large <- SP.count %>% filter(size_class == "large" | is.na(size_class)==T) %>% arrange(desc(tot)) %>% as.data.frame()
# SP.count.large <- left_join(SP.count.large,species_names,by=c("species")) %>% dplyr::select(-size_class)
# 
# SP.count.small <- SP.count %>% filter(size_class == "small") %>% arrange(desc(tot)) %>% as.data.frame()
# 
# SP.count.small.cast <- dat.fish %>% filter(size_class == "small") %>% dcast(.,species~year,sum,value.var = c("Count"))
# SP.count.small.cast.year <- list()
# 
# YEAR <- unique(dat.fish$year)
# for(i in 1:length(YEAR)){
#   SP.count.small.cast.year[[i]] <- dat.fish %>% filter(size_class == "small",year==YEAR[i]) %>% 
#                                     dcast(.,species~site,sum,value.var = c("Count")) %>% mutate(year=YEAR[i])
# }
# ##############################################################################
# ##############################################################################
# ##############################################################################
# # Make some plots of visibility
# 
# # vis.hist <- ggplot(base.dat) +
# #     geom_histogram(aes(VIS_M),breaks=seq(-0.01,9.99,by=0.25)) +
# #     scale_x_continuous(limits = c(0,10)) +
# #     facet_wrap(~YEAR) +
# #     geom_vline(xintercept = 2,col="red")+
# #     theme_bw()
# # 
# # vis.sum <- base.dat %>% group_by(YEAR,SITE) %>% dplyr::summarise(MED = median(VIS_M,na.rm=T))
# # vis.sum$SITE <-  factor(vis.sum$SITE,levels=nom )
# # vis.sum$YEAR <- factor(vis.sum$YEAR)
# # 
# # vis.by.site <- ggplot(vis.sum) +
# #   geom_point(aes(y=MED,x=SITE,color=YEAR),alpha=0.5,size=4)+
# #   labs(y="Median Visibility (m)") +
# #   theme_bw()
# 
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# #### Make some fish plots
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# 
# # # Plot by site, year is color.
# # P.large <- list()
# # Q.large <- list()
# # 
# # for(i in 1:nrow(SP.count.large %>% filter(tot>=5))){
# #   #print(SP.count.large$species[i])
# #   P.large[[i]] <- ggplot(data=dat.large.fish.summary %>% filter(species == SP.count.large$species[i])) +
# #             geom_point(aes(y=Mean,x=site)) +
# #             geom_errorbar(aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=site),width=0.1) +          
# #             facet_grid(year~zone) +
# #             ggtitle(paste(SP.count.large$species[i],SP.count.large$common.name[i])) + 
# #             theme_bw() +
# #             theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=8))
# #   Q.large[[i]] <- ggplot() +
# #             geom_point(data=dat.large.fish.summary %>% filter(species == SP.count.large$species[i]),
# #                 aes(y=Mean,x=year,color=site)) +
# #             # geom_line(data=dat.large.fish.summary %>% filter(species == SP.count.large$species[i]),
# #             #     aes(y=Mean,x=year,color=site)) +
# #             geom_errorbar(data=dat.large.fish.summary %>% filter(species == SP.count.large$species[i]),
# #                 aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=year,color=site),width=0.1) +
# #     
# #     
# #             geom_point(data=dat.large.fish.by.year %>% filter(species == SP.count.large$species[i]),
#                aes(y=MEAN,x=year+0.1),color=1,size=4,shape=22) +
#             geom_line(data=dat.large.fish.by.year %>% filter(species == SP.count.large$species[i]),
#                aes(y=MEAN,x=year+0.1),color=1,size=1) +
#             geom_errorbar(data=dat.large.fish.by.year %>% filter(species == SP.count.large$species[i]),
#                   aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=year+0.1),color=1,width=0.1) +          
#             facet_wrap(~zone,nrow=2) +
#             ggtitle(paste(SP.count.large$species[i],SP.count.large$common.name[i])) + 
#             theme_bw() +
#             theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=8))
# }
#  
#  P.complex.small <- ggplot(data=dat.small.fish.complex.summary ) +
#             geom_point(aes(y=Mean,x=site)) +
#             geom_errorbar(aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=site),width=0.1) +          
#             facet_grid(year~zone) +
#             ggtitle(paste("Rockfish less than",size.break,"cm")) + 
#             theme_bw() +
#             theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=9))
#   Q.complex.small <- ggplot() +
#             geom_point(data=dat.small.fish.complex.summary,
#                aes(y=Mean,x=year,color=site)) +
#             geom_line(data=dat.small.fish.complex.summary,
#               aes(y=Mean,x=year,color=site)) +
#             geom_errorbar(data=dat.small.fish.complex.summary,
#                 aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=year,color=site),width=0.1) +
#             geom_point(data=dat.small.fish.complex.by.year,
#                aes(y=MEAN,x=year+0.1),color=1,size=4,shape=22) +
#             geom_line(data=dat.small.fish.complex.by.year,
#               aes(y=MEAN,x=year+0.1),color=1,size=1) +
#             geom_errorbar(data=dat.small.fish.complex.by.year,
#               aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=year+0.1),color=1,width=0.1) +          
#             facet_grid(~zone) +
#             ggtitle(paste("Rockfish less than",size.break,"cm")) + 
#             theme_bw() +
#             theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=9)) 
# 
# Q.complex.small.log <- ggplot() +
#             geom_point(data=dat.small.fish.complex.summary,
#                aes(y=Mean,x=year,color=site)) +
#             geom_line(data=dat.small.fish.complex.summary,
#               aes(y=Mean,x=year,color=site)) +
#             geom_errorbar(data=dat.small.fish.complex.summary,
#                   aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=year,color=site),width=0.1) +
#             geom_point(data=dat.small.fish.complex.by.year,
#                aes(y=MEAN,x=year+0.1),color=1,size=4,shape=22) +
#             geom_line(data=dat.small.fish.complex.by.year,
#               aes(y=MEAN,x=year+0.1),color=1,size=1) +
#             geom_errorbar(data=dat.small.fish.complex.by.year,
#                   aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=year+0.1),color=1,width=0.1) +          
#             facet_grid(~zone) +
#             ggtitle(paste("Rockfish less than",size.break,"cm")) + 
#             theme_bw() +
#             theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=9)) +
#             scale_y_continuous(trans="log10")
#   
#   
# # Repeat for sub group YOYS
# # yoy.names <- c("SEBYTy","SEPIy","RYOY")
# # for(i in 1:)
# # Q.sub[[i]] <- ggplot() +
# #   geom_point(data=dat.fish.complex.summary %>% filter(sp.complex == SP[i,1]),
# #              aes(y=Mean,x=year,color=site)) +
# #   geom_line(data=dat.fish.complex.summary %>% filter(sp.complex == SP[i,1]),
# #             aes(y=Mean,x=year,color=site)) +
# #   geom_errorbar(data=dat.fish.complex.summary %>% filter(sp.complex == SP[i,1]),
# #                 aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=year,color=site),width=0.1) +          
# #   geom_point(data=dat.fish.complex.sum.by.year %>% filter(sp.complex == SP[i,1]),
# #              aes(y=MEAN,x=year+0.1),color=1,size=4,shape=22) +
# #   geom_line(data=dat.fish.complex.sum.by.year %>% filter(sp.complex == SP[i,1]),
# #             aes(y=MEAN,x=year+0.1),color=1,size=1) +
# #   geom_errorbar(data=dat.fish.complex.sum.by.year %>% filter(sp.complex == SP[i,1]),
# #                 aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=year+0.1),color=1,width=0.1) +          
# #   facet_grid(~zone) +
# #   ggtitle(paste(SP[i,1],common.names[i,1])) + 
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# # 
# 
# #######  Make Tile plot of all species by site.
# MAX    <- max(dat.large.fish.summary %>% dplyr::select(Mean))
# BREAKS <- c(1,5,10,15,20,40,80,120,160)
# MIN    <- 0.05
# 
# dat.large.fish.summary$species <- factor(dat.large.fish.summary$species, levels=SP.count.large$species)
# THESE <- SP.count.large$species[SP.count.large$tot>0]
# 
# ## All 
# all.sp.count <- list()
# for(i in 1:length(YEAR)){
#   for(j in c(5,10)){
#   if(nrow(dat.large.fish.summary %>% filter(year==YEAR[i],zone==j,species%in%THESE))>0){
#     all.sp.count[[as.name(paste("X",YEAR[i],j,sep="."))]]  <-
#       ggplot(data=dat.large.fish.summary %>% filter(year==YEAR[i],zone==j,species%in%THESE),
#              aes(x=site,y=species)) +
#       geom_tile(aes_string(fill="Mean")) +
#       scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
#       labs(ylab="Species",xlab="Site") + 
#       ggtitle(paste0("All >10cm ", YEAR[i]," average count ",j,"m deep (per 30x2x2 m transect)"))+
#       theme_bw() +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
#     }
#   }
# }
# 
# ################
# ### Just Small (< size.break) ## SQRT TRANSFORMED
# ################
# MAX    <- max(dat.small.fish.summary %>% dplyr::select(Mean))
# BREAKS <- c(1,5,10,20,40,80,120,160)
# MIN    <- 0.05
# 
# dat.small.fish.summary$species <- factor(dat.small.fish.summary$species, levels=SP.count.small$species)
# THESE <- SP.count.small$species[SP.count.small$tot>0]
# 
# small.count <- list()
# for(i in 1:length(YEAR)){
#   for(j in c(5,10)){
#     if(nrow(dat.small.fish.summary %>% filter(year==YEAR[i],zone==j,species%in%THESE))>0){
#       small.count[[as.name(paste("X",YEAR[i],j,sep="."))]] <-
#         ggplot(data=dat.small.fish.summary %>% filter(year==YEAR[i],zone==j,species%in%THESE)) +
#         geom_tile(aes_string(fill="Mean",x="site",y="species")) +
#         scale_fill_gradientn(colors = viridis(32),trans="sqrt",limits=c(MIN,MAX),breaks=BREAKS) +
#         labs(ylab="Species",xlab="Site") + 
#         ggtitle(paste0("All <10cm, ",YEAR[i]," average count ",j,"m deep (per 30x2x2 m transect)"))+
#         theme_bw() +
#         theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
#     }
#   }
# }
# 
# 
# 
# # 
# # all.sp.count.2015 <-  ggplot(data=dat.large.fish.summary %>% filter(year==2015,zone==5,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All >10cm, 2015 average count 5m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # all.sp.count.2016.5m <-  ggplot(data=dat.large.fish.summary %>% filter(year==2016,zone==5,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All >10cm, 2016 average count 5m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # all.sp.count.2016.10m <-  ggplot(data=dat.large.fish.summary %>% filter(year==2016,zone==10,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All >10cm, 2016 average count 10m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # all.sp.count.2017.5m <-  ggplot(data=dat.large.fish.summary %>% filter(year==2017,zone==5,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All >10cm, 2017 average count 5m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # all.sp.count.2017.10m <-  ggplot(data=dat.large.fish.summary %>% filter(year==2017,zone==10,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All >10cm, 2017 average count 10m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # all.sp.count.2018.5m <-  ggplot(data=dat.large.fish.summary %>% filter(year==2018,zone==5,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All >10cm, 2018 average count 5m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # all.sp.count.2018.10m <-  ggplot(data=dat.large.fish.summary %>% filter(year==2018,zone==10,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All >10cm, 2018 average count 10m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# 
# 
# # small.count.2016.5m <-  ggplot(data=dat.small.fish.summary %>% filter(year==2016,zone==5,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),trans="sqrt",limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All <10cm, 2016 average count 5m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # small.count.2016.10m <-  ggplot(data=dat.small.fish.summary %>% filter(year==2016,zone==10,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),trans="sqrt",limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All <10cm, 2016 average count 10m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # small.count.2017.5m <-  ggplot(data=dat.small.fish.summary %>% filter(year==2017,zone==5,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),trans="sqrt",limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All <10cm, 2017 average count 5m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # small.count.2017.10m <-  ggplot(data=dat.small.fish.summary %>% filter(year==2017,zone==10,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),trans="sqrt",limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All <10cm, 2017 average count 10m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # small.count.2018.5m <-  ggplot(data=dat.small.fish.summary %>% filter(year==2018,zone==5,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),trans="sqrt",limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All <10cm, 2018 average count 5m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# # small.count.2018.10m <-  ggplot(data=dat.small.fish.summary %>% filter(year==2018,zone==10,species%in%THESE)) +
# #   geom_tile(aes(fill=Mean,x=site,y=species)) +
# #   scale_fill_gradientn(colors = viridis(32),trans="sqrt",limits=c(MIN,MAX),breaks=BREAKS) +
# #   labs(ylab="Species",xlab="Site") + 
# #   ggtitle("All <10cm, 2018 average count 10m deep (per 30x2x2 m transect)")+
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
# # 
# 
# 
# #pdf()
# 
# 
# 
# 
