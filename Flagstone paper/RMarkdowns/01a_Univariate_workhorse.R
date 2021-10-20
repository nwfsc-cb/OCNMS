library(knitr)
# library(tidyr)
library(dplyr)
library(tidyverse)
library(stringr)
# library(tinytex)
library(RColorBrewer)
# display.brewer.all(colorblindFriendly = TRUE)
library(readxl)

# stats packages etc
library(vegan)
library(BiodiversityR)
library(pracma)
library(factoextra)


# Paths for files
HomeFile = "/Users/ole.shelton/Github/OCNMS"
Fig_Loc = paste0(HomeFile,"/Flagstone paper/Plots/")
Data_Loc = paste0(HomeFile,"/Flagstone paper/Data/")
Results_Loc = paste0(HomeFile,"/Flagstone paper/Results/")
Other_Files = paste0(HomeFile,"/Flagstone paper/Other Files/")

setwd(Data_Loc)
spp_code = data.frame(read.csv("spp_codes.csv"))

########START WITH FISH.
# import fish data. wide format

# read in rds file with combined data
fish0 = readRDS( paste0(Data_Loc,'Fish_2015-2021.rds' ))

min.vis = 2.0
years = 2015:2021
pch = c(9, 4, 8 , 21, 22, 24, 25 )
col = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2,4,6,10,12,7,8)]
year.pch = data.frame(cbind(years, pch,col))

sites = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island")
col = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2,4,6, 10,12)]
site.col = data.frame(cbind(sites,col))
colnames(site.col) <- c('site', 'col')

#### species depths #####

kelp.depth = 5
fish.depth = c(5,10)
invert.depth = 5

###########################

# fix a spp name
#fish0$species[ fish0$species == 'SEBYT'] <- 'SEFL'

# add in taxa groupings
# add in yoy designator and fix some names
fish0 <- fish0 %>% left_join(.,spp_code %>% dplyr::select(code,group),by=c("species"="code")) %>%
            rename(taxa=group)
fish0 <- fish0 %>% mutate(taxa=case_when(is.na(size_class)==TRUE ~ as.character(taxa),
                                         size_class=="large" ~ as.character(taxa),
                                         size_class=="small" ~ paste0(taxa,"yoy"))) %>%
                  mutate(taxa= ifelse(taxa=="RYOYyoy","RYOY",taxa))

# combine YT and black or keep separate? # COMBINE!
fish0 %>% filter(taxa %in% c('SEFLyoy','SEMEyoy','SEBYTyoy')) %>% group_by(taxa) %>% 
        summarise(Tot=sum(Count))

fish0$taxa[ fish0$taxa %in% c('SEFLyoy','SEMEyoy')] <- 'SEBYTyoy'

# Aggregate the SEBYTyoy so that there are one, not three, entries for "SEBYTyoy"
fish_yt <- fish0 %>% filter(taxa=="SEBYTyoy") %>% 
              group_by(site, transect,observer,
                       size_class,year,area,zone,vis_m,taxa) %>%
              summarise(Count_all=sum(Count)) %>% rename(Count=Count_all) %>%
              mutate(species="SEBYT",common.name=NA)
                
fish0 <- fish0 %>% filter(!taxa == "SEBYTyoy") %>% full_join(.,fish_yt)

# subset visibility
# add fake vis for 2015
fish0$vis_m[ fish0$year == 2015 ] <- 3
# poor vis at destruction in year 1.
fish0$vis_m[ fish0$year == 2015 & fish0$site == "Destruction Island"] <- 1

# output for multivariate 
saveRDS(fish0, paste0(Data_Loc, "Fish_2015-2021-ntmods.rds"))

# get just five sites

fish1 = fish0 %>% filter(site %in% c("Destruction Island", "Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))

# drop low vis sites/transects and set depth
fish1a = fish1[fish1$vis_m >= min.vis, ]

# set depth
fish1b = fish1a[fish1a$zone %in% fish.depth,]

# calculate total yoy by site, transect, observer, and year
fish1b %>% filter(size_class == "small") %>% distinct(taxa)
# all of the small taxa are rockfish YOY.

fish1c <- fish1b %>% filter(size_class == "small") %>% 
                      group_by(site,year,transect,observer,zone,area) %>% 
                      summarise(Count_all=sum(Count)) %>%
                      mutate(species="TOTyoy",taxa="TOTyoy") %>%
                      rename(Count=Count_all)
fish1d <- fish1b %>% full_join(.,fish1c)                       
                    
# double check specific averaging process here 
# fish2 = aggregate( Count ~ site + zone + year + taxa, data = fish1c, FUN = mean )
# colnames(fish2)[ncol(fish2)] <- 'mean'
# fish = Sum_Stats('mean ~ year +  taxa', fish2)
# fish$se_lo = fish$mean - fish$se
# fish$se_up = fish$mean + fish$se

# Ole made this to check the aggregating in fish 2.
fish3 <-  fish1d %>% group_by(site,year,zone,species, taxa) %>%
              summarise(Mean=mean(Count),SD=sd(Count),N=length(Count),SE=SD/sqrt(N))

# Check to make sure all of the site-year-zone combinations have equivalent number of transects.
fish3 %>% group_by(site,year,zone) %>% distinct(N) %>% as.data.frame()

fish4 <-  fish1d %>% group_by(site,year,species, taxa) %>%
              summarise(Mean=mean(Count),SD=sd(Count),N=length(Count),
                        SE=SD/sqrt(N),SE_var=SE^2)

fish5 <- fish4 %>% group_by(year, species, taxa) %>%
                    summarise(grand_sum=sum(Mean),
                        N=length(Mean),
                        grand_mean= grand_sum / N,
                        grand_sum_var =sum(SE_var), 
                        grand_SE = sqrt(grand_sum_var / N^2))

SP.yoy <- c("SEBYTyoy", "Black & Yellowtail",
            "SECAyoy", "Copper", 
            #"SEMAyoy", "Quillback", 
            "SEMYyoy", "Blue" ,
            #"SENEyoy", "China", 
            "SEPIyoy", "Canary", 
            "RYOY", "Unidentified",
            "TOTyoy", "Total")

SP.yoy <- matrix(SP.yoy,ncol=2,byrow=T) %>% as.data.frame()
colnames(SP.yoy) <- c("taxa","name")

SP.yoy$name <- as.character(SP.yoy$name)
SP.yoy$name <- factor(SP.yoy$name,levels=SP.yoy$name)
######

ggplot(fish5 %>% filter(taxa %in% SP.yoy$taxa) %>% left_join(.,SP.yoy)) +
    geom_point(aes(x=year,y=grand_mean,color=name),alpha=0.5) + 
    geom_line(aes(x=year,y=grand_mean,color=name)) +
    geom_errorbar(aes(x=year,color=name,ymin=grand_mean-grand_SE,ymax=grand_mean+grand_SE),
                  width=0) +
    scale_y_continuous(trans="sqrt",breaks=c(0,1,5,10,20,40,60,80),limits=c(0,NA)) +
    scale_x_continuous(breaks=seq(2015,2021)) +
    scale_color_viridis_d(option="plasma", end=0.75) +  
    ylab("Rockfish Recruits") +
    xlab("Year") +
    theme_bw() +
    theme(legend.title=element_blank())


ggplot(fish5 %>% filter(species=="TOTyoy")) +
    geom_point(aes(x=year,y=grand_mean),color="blue") + 
    geom_line(aes(x=year,y=grand_mean),color="blue") +
    geom_errorbar(aes(x=year,ymin=grand_mean-grand_SE,ymax=grand_mean+grand_SE),
                color="blue",width=0) +
    scale_y_continuous(limits = c(0,80),breaks=c(0,5,10,20,30,40,50,60,70,80),expand=c(0,0.1)) +
    scale_x_continuous(breaks=seq(2015,2021)) +
    ylab("Rockfish Recruits (") +
    xlab("Year") +
    theme_bw() 




#######################################################################
# PULL IN INVERTEBRATES AND ALGAE FOR ANALYSIS.
#######################################################################

# read in rds file with combined data
swath0 <- readRDS( paste0(Data_Loc,'Swath_2015-2021.rds' ))
spp_swath <- read.csv("spp_codes_swath.csv")
#separate into algae and invertebrate data frames.

swath1 <- left_join(swath0,spp_swath)

SITES <- c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island", "Neah Bay")
swath1 <- swath1 %>% filter(site %in% SITES)
swath1$site <- factor(swath1$sites,levels=SITES)

dat.algae   <- swath1 %>% filter(group=="Algae")
dat.invert  <- swath1 %>% filter(group=="Invert")

#######################################################################
## WORK WITH ALGAE FIRST 
#######################################################################

# Aggregate up to the transect level (within year, site, area, zone)
algae1 <- dat.algae %>% group_by(year,site,transect,observer,species,zone,area,taxa) %>%
            summarise(total.count=sum(Count),total.area=sum(Transect.area)) %>% 
            mutate(density = total.count / total.area )
# Aggregate up to the area level (within year site, zone) -- treat transects as replicates
algae2 <- algae1 %>% 
  group_by(year,site,species,zone,area,taxa) %>%
  summarise(mean.density=mean(density), 
            SD = sd(density), 
            N =length(year),
            SE= SD/sqrt(N) ) 

# Aggregate up to the site level (within year, zone) -- treat transects as replicates
algae3 <- algae1 %>% 
  group_by(year,site,species,zone,taxa) %>%
  summarise(mean.density=mean(density), 
            SD = sd(density), 
            N =length(year),
            SE= SD/sqrt(N) ) 



#Trim to canopy species
 canopy <- algae3 %>% filter(taxa=="CANOPY",!species=="EGRMEN")
 canopy$site <- factor(canopy$site,levels=SITES)
 
 sm = 0.5
 bg = 1
 SIZE = cbind(site=SITES,size=c(sm,sm,sm,bg,sm)) %>% as.data.frame()
 
 canopy <- left_join(canopy,SIZE)
 canopy$size <- as.numeric(as.character(canopy$size))
  canopy$zone <- factor(canopy$zone)
 
 # Basic time-series by site and zone
 ggplot(canopy %>% filter(zone==5)) +
    geom_point(aes(x=year,y=mean.density,color=site),alpha=0.5) +
    geom_line(aes(x=year,y=mean.density,color=site),alpha=0.5) +
    geom_errorbar(aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=site),width=0) +
    facet_grid(rows="species",scales="free_y") +
    scale_x_continuous(breaks=seq(2015,2021)) +
    scale_y_continuous(expression("Stipe density (stipe m"^-2*")")) +
    scale_color_viridis_d(option="plasma",end=0.75)+
    theme_bw()

 # Basic time-series by site and zone
 ggplot(canopy %>% filter(zone==10)) +
   geom_point(aes(x=year,y=mean.density,color=site),alpha=0.5) +
   geom_line(aes(x=year,y=mean.density,color=site),alpha=0.5) +
   geom_errorbar(aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=site),width=0) +
   facet_grid(rows="species",scales="free_y") +
   scale_x_continuous(breaks=seq(2015,2021)) +
   scale_y_continuous(expression("Stipe density (stipe m"^-2*")")) +
   scale_color_viridis_d(option="plasma",end=0.75)+
   theme_bw()
 
 # Basic time-series by site and zone TATOOSH FOCUS
 ggplot(canopy %>% filter(site=="Tatoosh Island",species=="NERLUE")) +
   geom_point(aes(x=year,y=mean.density,color=zone),alpha=0.5) +
   geom_line(aes(x=year,y=mean.density,color=zone),alpha=0.5) +
   geom_errorbar(aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=zone),width=0) +
   #facet_grid(rows="species",scales="free_y") +
   scale_x_continuous(breaks=seq(2015,2021)) +
   scale_y_continuous(expression("Stipe density (stipe m"^-2*")"),limits=c(0,NA)) +
   scale_color_viridis_d("Depth (m)",option="plasma",end=0.75)+
   #scale_point_viridis_d("Depth (m)",option="plasma",end=0.75)+
   ggtitle("Tatoosh Island") +
   theme_bw()
 
### Bivariate and summed Plot of the two major canopy species.
 
 mac_ner <- algae1 %>% filter(species %in% c("NERLUE","MACPYR"))
 
 mac_ner1 <- mac_ner %>% group_by(year,site,zone,area) %>%
              summarise(mean.density=mean(density), 
                SD = sd(density), 
                N =length(year),
                SE= SD/sqrt(N) ) 

  mac_ner2 <- mac_ner %>% group_by(year,site,zone) %>%
    summarise(mean.density=mean(density), 
             SD = sd(density), 
             N =length(year),
             SE= SD/sqrt(N) ) 
 
  # Basic time-series by site and zone
  all_canopy_zone <- ggplot(mac_ner2 ) +
    geom_point(aes(x=year,y=mean.density,color=site),alpha=0.5) +
    geom_line(aes(x=year,y=mean.density,color=site),alpha=0.5) +
    geom_errorbar(aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=site),width=0) +
    facet_grid(rows="zone",scales="free_y") +
    scale_x_continuous(breaks=seq(2015,2021)) +
    scale_y_continuous(expression("Stipe density (stipe m"^-2*")")) +
    scale_color_viridis_d(option="plasma",end=0.75)+
    theme_bw()

  
  
  #######################################################################
  ## WORK WITH IVERTEBRATES
  #######################################################################
  
  # Aggregate up to the transect level (within year, site, area, zone)
  invert1 <- dat.invert %>% group_by(year,site,transect,observer,species,zone,area,taxa) %>%
    summarise(total.count=sum(Count),total.area=sum(Transect.area)) %>% 
    mutate(density = total.count / total.area )


  # Aggregate up to the area level (within year, site, zone) -- treat transects as replicates
  invert2 <- invert1 %>% 
    group_by(year,site,species,zone,area,taxa) %>%
    summarise(mean.density=mean(density), 
              SD = sd(density), 
              N =length(year),
              SE= SD/sqrt(N) ) 
  
  # Zoom in on urchins
  urchin1 <- invert1 %>% filter(taxa=="URCHIN") %>%
    group_by(year,site,transect,observer,zone,area,taxa) %>%
    summarise(tot.density = sum(density)) %>% 
    group_by(year,site,zone,area,taxa) %>%
    summarise(mean.density=mean(tot.density), 
              SD = sd(tot.density), 
              N =length(year),
              SE= SD/sqrt(N) ) 
  
  
  urchin2 <- invert1 %>% filter(taxa=="URCHIN") %>%
    group_by(year,site,transect,observer,area,zone,taxa) %>%
    summarise(tot.density = sum(density)) %>% 
    group_by(year,site,zone,taxa) %>%
    summarise(mean.density=mean(tot.density), 
              SD = sd(tot.density), 
              N =length(year),
              SE= SD/sqrt(N) ) 
  
  
    ggplot() +
      geom_point(data= urchin1,
                 aes(x=year,y=mean.density,color=site),alpha=0.2) +
      geom_point(data= urchin2,
                 aes(x=year,y=mean.density,color=site),alpha=0.5) +
      geom_line(data= urchin2,
                 aes(x=year,y=mean.density,color=site),alpha=0.5) +
      geom_errorbar(data=urchin2,aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=site),width=0) +
      facet_grid(rows="zone") + #,scales="free_y") +
      scale_x_continuous(breaks=seq(2015,2021)) +
      scale_y_continuous(expression("Urchin density (m"^-2*")")) +
      scale_color_viridis_d(option="plasma",end=0.75)+
      theme_bw()
        










# Make Plots of Fish
library(ggplot2)
library(gtable)
library(gridExtra)

fish.ylabel = "No. per transect"
all.colors =  RColorBrewer::brewer.pal(n = 12, name = "Paired")
fish.col = all.colors[ c(3,4,8,12,11,11,2) ]
fish.col[6] <- 'black'
yoy.col = all.colors[c(10,12,6,6,'black')]
yoy.col[4] <- 'black'  

uni.fish = fish %>% filter(taxa %in% c("OPEL", "HEXA", "SECA", "SCMA" ,"SENE", "SEME", "SEFL"))

fish.plot = species.plot(uni.fish, group.var = 'taxa', years = 2015:2021, 
                         Ylim = NA, Ylab = fish.ylabel , Colors = fish.col)

yoy.fish = fish[ fish$taxa %in% c("SECAyoy","SEPIyoy", "YTBLyoy", "RYOY", "totYOY"),]


yoy.plot = species.plot(yoy.fish, group.var = 'taxa', years = 2015:2021, 
                        Ylab = fish.ylabel ,Ylim = NA, Colors = yoy.col)

g2 <- ggplotGrob(fish.plot)
g3 <- ggplotGrob(yoy.plot)
g <- rbind(g2, g3, size = "first")

g$widths <- grid::unit.pmax(g2$widths, g3$widths)
grid::grid.newpage()
grid::grid.draw(g)

