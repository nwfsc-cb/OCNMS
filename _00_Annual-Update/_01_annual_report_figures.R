# libraries ####################################################################
rm(list=ls())
library(here)
library(tidyverse)
library(stringr)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(ggplot2)
library(stringr)
library(maps)
library(mapdata)
library(mapproj)

# set year #####################################################################
data_year = 2026

# set directories ##############################################################
repo_dir = here::here()
home_dir = paste0(repo_dir,"/_00_Annual-Update/")
fig_dir = paste0(home_dir,'figures/')
# data_dir = paste0(repo_dir,"/Data/")
data_dir = paste0(repo_dir,"/Data/",data_year,"/")

setwd(home_dir) # set home dir to annual update folder

# common plotting theme ########################################################
theme_nt   <-   theme(legend.title=element_blank(),
                      legend.background = element_blank(),
                      axis.title = element_text(size=8),
                      axis.text.x = element_text(size=8),
                      strip.background = element_blank(),
                      legend.text = element_text(size = 8),
                      legend.key.size = unit(0.25,'lines')
)

plot_colors = c(RColorBrewer::brewer.pal(n = 12, name ="Paired"),"black")

# more plotting stuff ##########################################################
pch = c(8, 21, 22, 23, 24, 4, 25, 26, 27,1,2 )
col = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2,4,6,10,12,7,8,3,5,1,9)]
year.pch = data.frame(cbind(years, pch, col))

sites = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island")
col = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2,4,6, 10,12)]
site.col = data.frame(cbind(sites,col))
colnames(site.col) <- c('site', 'col')

kelp.depth = 5
fish.depth = c(5,10)
invert.depth = 5
# set for labelling plots below
depth.labs = c('5-m','10-m')
names(depth.labs) = c(5,10)

# species codes ################################################################
fish_codes = data.frame(read.csv(paste0(repo_dir,"/Data/spp_codes_fish.csv"), header = TRUE))
kelp_codes = data.frame(read.csv(paste0(repo_dir,"/Data/spp_codes_kelp.csv"), header = TRUE))
swath_codes = data.frame(read.csv(paste0(repo_dir,"/Data/spp_codes_swath.csv"), header = TRUE))

################################################################################
# FISH FIGURES #################################################################
################################################################################

fish_transect = data.frame(read.csv(
  paste0(data_dir,'Summarized_Data_Fish_by_Transect.csv'), header = T))

## data prep ###################################################################
### adults
fish_taxa_Year_site <-  fish_transect %>% group_by(site,year,taxa) %>%
  summarise(Mean=mean(Count, na.rm = TRUE),
            SD=sd(Count),
            N=length(Count),
            SE=SD/sqrt(N),
            SE_var=SE^2)

fish_taxa_year <-fish_taxa_Year_site %>% group_by(year,taxa) %>%
  summarise(grand_sum=sum(Mean, na.rm = TRUE),
            N=length(Mean),
            grand_mean= grand_sum / N,
            grand_sum_var =sum(SE_var, na.rm = TRUE), 
            grand_SE = sqrt(grand_sum_var / N^2))
fish.col = c(RColorBrewer::brewer.pal(n = 12, name = "Paired"))

fish.taxa <- c("OPEL", "Lingcod",     fish.col[10],
               "HEXA", "Greenlings", fish.col[4],
               "SCMA" ,"Cabazon",     fish.col[8],
               "SECA", "Copper RF",   fish.col[12],
               "SENE", "China RF",     fish.col[2],
               "SEFL", "Yellowtail RF", fish.col[7],
               "SEME", 'Black RF',    'black')

fish.taxa <- matrix(fish.taxa,ncol=3,byrow=T) %>% as.data.frame()
colnames(fish.taxa) <- c("taxa","name","col")

fish.taxa$name <- as.character(fish.taxa$name)
fish.taxa$name <- factor(fish.taxa$name,levels=fish.taxa$name)

fish.col = c(RColorBrewer::brewer.pal(n = 12, name = "Paired"))

SP.yoy <- c("SEBYTyoy", "Black/Yellowtail RF",  'black',
            "SECAyoy", "Copper/Quillback/Brown RF",               fish.col[12],
            #"SEMAyoy", "Quillback",            fish.col[10],
            "SEMYyoy", "Blue RF" ,                fish.col[2],
            #"SENEyoy", "China",                fish.col[10],
            "SEPIyoy", "Canary RF",               fish.col[6],
            "RYOY", "Unidentified",            'darkgrey') # ,
#"TOTyoy", "Total",                 fish.col[10])

SP.yoy <- matrix(SP.yoy,ncol=3,byrow=T) %>% as.data.frame()
colnames(SP.yoy) <- c("taxa","name",'col')

SP.yoy$name <- as.character(SP.yoy$name)
SP.yoy$name <- factor(SP.yoy$name,levels=SP.yoy$name)

## Adults coastwiede ###########################################################
ggplot(fish_taxa_year %>% 
       filter(taxa %in% fish.taxa$taxa) %>% 
       left_join(.,fish.taxa)) +
  geom_point(aes(x=year,y=grand_mean,color=name)) + 
  geom_line(aes(x=year,y=grand_mean,color=name)) +
  geom_errorbar(aes(x=year,color=name,ymin=grand_mean-grand_SE,ymax=grand_mean+grand_SE),
                width=0) +
  scale_y_continuous(trans="sqrt",breaks=c(0,1,5,10),limits=c(0,10)) +
  scale_x_continuous(breaks=seq(2015,max(fish_taxa_year$year),2)) +
  #scale_color_viridis_d(option="plasma", end=0.75) + 
  scale_color_manual(values=fish.taxa$col)+
  ylab(expression("Number per 120 m"^3)) +
  xlab("") +
  theme_bw() + theme_nt 

ggsave(paste0(fig_dir,"fish-coastwide.png"), width=5, height = 2.5)

## Adults by site ##############################################################
fish_ys = fish_taxa_Year_site %>% filter(taxa %in% fish.taxa[,1])
fish_ys$site = factor(fish_ys$site , levels = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island"))

fish_ys <- fish_ys %>% left_join(.,fish.taxa)

ggplot(fish_ys , aes(x = year, y = Mean, color = site)) +
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(x=year,color=site,ymin=Mean-SE,ymax=Mean+SE),width=0) +
  scale_x_continuous(breaks=seq(2015,max(fish_ys$year),2), 
                     minor_breaks = seq(2015,max(fish_ys$year))) +
  scale_color_manual(values=site.col$col)+
  xlab("")+
  ylab(expression("Number per 120 m"^3))+
  # facet_grid(rows = 'taxa', scales="free_y") +
  facet_wrap( ~ name, scales="free",nrow=4) +
  theme_bw() + theme_nt +
  theme(legend.position = c(0.7, 0.1))

ggsave(paste0(fig_dir,"fish-by-site.png"), width=5, height = 6)

## Recruits data prep ##########################################################

fish_taxa_year <-  fish_transect %>% group_by(site,year,taxa) %>%
  summarise(Mean=mean(Count, na.rm = TRUE),
            SD=sd(Count),
            N=length(Count),
            SE=SD/sqrt(N),
            SE_var=SE^2)  %>%
  group_by(year,taxa) %>%
  summarise(grand_sum = sum(Mean, na.rm = TRUE),
            N=length(Mean),
            grand_mean = grand_sum / N,
            grand_sum_var = sum(SE_var, na.rm = TRUE), 
            grand_SE = sqrt(grand_sum_var / N^2))


## Recruits coastwide ##########################################################

ggplot(fish_taxa_year %>% filter(taxa %in% SP.yoy$taxa) %>% left_join(.,SP.yoy)) +
  geom_point(aes(x=year,y=grand_mean,color=name)) + 
  geom_line(aes(x=year,y=grand_mean,color=name)) +
  geom_errorbar(aes(x=year,color=name,ymin=grand_mean-grand_SE,ymax=grand_mean+grand_SE),
                width=0) +
  scale_y_continuous(trans="sqrt",breaks=c(0,1,5,10,20,40,60,80),limits=c(0,NA)) +
  scale_x_continuous(breaks=seq(2015,max(fish_taxa_year$year),2)) +
  #scale_color_viridis_d(option="plasma", end=0.75) + 
  scale_color_manual(values=SP.yoy$col)+
  ylab(expression("Rockfish juveniles per 120 m"^3)) +
  xlab("") +
  theme_bw() + theme_nt +
  theme(legend.position = 'inside', legend.position.inside =  c(0.7, 0.8) )

ggsave(paste0(fig_dir,'fish-recruits-coastwide.png'), width= 5, height=2.5)

## Recruits by site ############################################################

yoy_ys = fish_taxa_Year_site %>% filter(taxa %in% SP.yoy[,1]) %>% left_join(.,SP.yoy)
yoy_ys$site = factor(yoy_ys$site , levels = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island"))

ggplot(yoy_ys , aes(x = year, y = Mean, color = site)) +
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(x=year,color=site,ymin=Mean-SE,ymax=Mean+SE),width=0) +
  scale_x_continuous(breaks=seq(2015,max(yoy_ys$year),2)) +
  scale_color_manual(values=site.col$col)+
  xlab("")+
  ylab(expression("Rockfish juveniles per 120 m"^3))+
  # facet_grid(rows = 'taxa', scales="free_y") +
  facet_wrap( ~ name, scales="free",nrow=3) +
  theme_bw() + theme_nt + 
  theme(  legend.position = c(0.85, 0.2) )

ggsave(paste0(fig_dir,'fish-recruits-by-site.png'), width= 5, height=5)

################################################################################
# SWATH: ALGAE & INVERTS ######################################################
################################################################################
swath0 = readRDS( paste0(data_dir,'Swath_2015-',data_year,'.rds' ))
sites = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island")
swath <- swath0 %>% filter(site %in% sites)
swath$site <- factor(swath$site,levels=sites)
algae = swath[swath$group=='Algae',]

## ALGAE FIGURES ###############################################################
### algae data prep ####

kelp_transect <- algae %>% group_by(year,site,transect,zone,area,species) %>%
  summarise(total.count=sum(Count, na.rm = TRUE),
            total.area=sum(Transect.area, na.rm = TRUE)) %>% 
  mutate(density = total.count / total.area )

kelp_site_zone = kelp_transect %>% group_by(year,site,species,zone) %>%
  summarise(mean.density=mean(density, na.rm = TRUE), 
            SD = sd(density, na.rm = TRUE), 
            N = length(year),
            SE= SD/sqrt(N) ) 

kelp_year = kelp_site_zone %>% group_by(year, species) %>%
  summarise(mean.density=mean(mean.density, na.rm = TRUE), 
            SD = sd(mean.density, na.rm = TRUE), 
            N =length(year),
            SE= SD/sqrt(N) ) 

## Algae coastwide #############################################################

big3spp = c('MACPYR', 'NERLUE', 'PTECAL')
big3 <- kelp_year %>% filter(species %in% big3spp)

kelp.labs = c('Macrocystis','Nereocystis','Pterygophora')
names(kelp.labs) <- big3spp

ggplot(big3) +
  geom_point(aes(x=year,y=mean.density,color=species)) +
  geom_line(aes(x=year,y=mean.density,color=species)) +
  geom_errorbar(aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,
                    color=species), width=0) +
  scale_x_continuous(breaks=seq(2015,data_year,2)) +
  xlab("")+
  scale_y_continuous(expression("Stipes m"^-2)) +
  scale_color_manual(values = site.col$col)+
  theme_bw()+ theme_nt + 
  theme(strip.text.y = element_text(face='italic'))

ggsave(paste0(fig_dir,'kelp-coastwide.png'), width=5, height = 2.5)

## Algae by site and zone ######################################################

big3spp = c('MACPYR', 'NERLUE', 'PTECAL')
big3 <- kelp_site_zone %>% filter(species %in% big3spp)
big3$site <- factor(big3$site,levels=sites)

kelp.labs = c('Macrocystis','Nereocystis','Pterygophora')
names(kelp.labs) <- big3spp

big3$site = factor(big3$site, levels = sites)
ggplot(big3) +
  geom_point(aes(x=year,y=mean.density,color=site)) +
  geom_line(aes(x=year,y=mean.density,color=site)) +
  geom_errorbar(aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=site),
                width=0) +
  facet_grid(species ~ zone,scales="free_y", labeller = labeller(zone=depth.labs ,
                                                                 species=kelp.labs)) +
  scale_x_continuous(breaks=seq(2015,data_year,2)) +
  xlab("")+
  scale_y_continuous(expression("Stipes m"^-2)) +
  scale_color_manual(values = site.col$col)+
  theme_bw()+ theme_nt + 
  theme(strip.text.y = element_text(face='italic'))

ggsave(paste0(fig_dir,'kelp-by-site-zone.png'),width=5, height=5)


## INVERT FIGURES ##############################################################

## data prep ####

dat.invert = swath[swath$group=='Invert',]
# set proper site order
dat.invert$site = factor(dat.invert$site, levels = sites)
dat.invert$taxa = swath_codes$taxa[ match(dat.invert$species, swath_codes$species) ]

# add a column to sum urchins and seastars
u = grep('urchin', dat.invert$taxa)
dat.invert$grp2 = 'no'
dat.invert$grp2[u] <- 'URCHIN'
s = grep('star', dat.invert$taxa)
dat.invert$grp2[s] = 'STAR'
dat.invert$grp2[dat.invert$taxa %in% c('Pisaster','Pycnopodia')] <- 'STAR'

### Summarize by transect ####

# Aggregate up to the transect level (within year, site, area, zone)
invert_transect <- dat.invert %>% 
  group_by(year,site,transect,observer,species,zone,area,taxa,grp2) %>%
  summarise(total.count=sum(Count, na.rm = TRUE),
            total.area=sum(Transect.area, na.rm = TRUE)) %>% 
  mutate(density = total.count / total.area )

saveRDS(invert_transect, file=paste0(data_dir , "Summarized_Data_Inverts_by_Transect.rds") )
write.csv(invert_transect,paste0(data_dir , "Summarized_Data_Inverts_by_Transect.csv"), row.names = F )

### Pycnopodia #################################################################

#### total observed, all sites ####
pycnos = invert_transect %>% 
  group_by(year) %>% 
  filter(., taxa == 'Pycnopodia') %>%
  summarise(no = sum(total.count))

ggplot(pycnos, aes(year, no)) +
  ylab("Total observed coastwide")+
  scale_x_continuous(breaks=seq(2015,data_year,5),
                     minor_breaks = seq(2015,data_year,1)) +
  geom_line() + geom_point()+theme_bw()

ggsave(paste0(fig_dir, "pycno-total-coaswide.png"), width = 3.5, height=2.5)

p_out = pycnos %>% rename(index = no) %>%
  mutate(timeseries="Pycnopodia", metric = "Total observed coastwide")

write.csv(p_out, '~/GitHub/NMS-condition-reports/data/ocnms-dive-total-pycno.csv', row.names = F)

#### total observed, all sites ####
pycnos2 = invert_transect %>% 
  group_by(year, site) %>% 
  filter(., taxa == 'Pycnopodia') %>%
  summarise(no = sum(total.count))

ggplot(pycnos2, aes(year, no, color=site)) +
  ylab("Total observed")+
  scale_x_continuous(breaks=seq(2015,data_year,5),
                     minor_breaks = seq(2015,data_year,1)) +
  geom_line() + geom_point()+  theme_bw() + theme_nt + 
  theme(  legend.position = c(0.2, 0.8) ) + xlab("")

ggsave(paste0(fig_dir, "pycno-total-site.png"), width = 3.5, height=2.5)

p_out = pycnos %>% rename(index = no) %>%
  mutate(timeseries="Pycnopodia", metric = "Total observed coastwide")

### Urchins ####################################################################

#### additional data prep ####

urchin1 <- invert_transect %>% filter(grp2=="URCHIN") %>%
  group_by(year,site,transect,observer, area,grp2) %>%
  summarise(tot.density = sum(density, na.rm = TRUE)) %>% 
  group_by(year,site,area,grp2) %>%
  summarise(mean.density=mean(tot.density, na.rm = TRUE), 
            SD = sd(tot.density), 
            N =length(year),
            SE= SD/sqrt(N) ) 

urchin_mean <- invert_transect %>% filter(grp2=="URCHIN") %>%
  group_by(year,site,transect,observer,area,grp2) %>%
  summarise(tot.density = sum(density, na.rm = TRUE)) %>% 
  group_by(year,site,grp2) %>%
  summarise(mean.density=mean(tot.density, na.rm = TRUE), 
            SD = sd(tot.density), 
            N =length(year),
            SE= SD/sqrt(N) ) 

#### Total urchins, by site ####

ggplot() +
  geom_point(data=urchin_mean,aes(x=year,y=mean.density,color=site)) +
  geom_line(data= urchin_mean,
            aes(x=year,y=mean.density,color=site)) +
  geom_errorbar(data=urchin_mean,
                aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=site),
                width=0) +
  scale_x_continuous(breaks=seq(2015,data_year,2)) +
  scale_y_continuous(expression("Urchin density (m"^-2*")")) +
  scale_color_manual(values = site.col$col) +
  theme_bw()+ theme_nt + xlab("") +
  theme(legend.position='inside') + 
  theme(legend.position.inside = c(0.2, 0.8))

ggsave(paste0(fig_dir, "urchin-by-site.png"), width=3.5, height=2.5)

#### Urchins by species ####

urchin_year <-  invert_transect %>% filter(grp2=="URCHIN") %>% 
  group_by(site,year,taxa) %>%
  summarise(Mean=mean(density, na.rm = TRUE),
            SD=sd(density),
            N=length(density),
            SE=SD/sqrt(N),
            SE_var=SE^2)  %>%
  group_by(year,taxa) %>%
  summarise(grand_sum = sum(Mean, na.rm = TRUE),
            N=length(Mean),
            grand_mean = mean(Mean),
            grand_sum_var = sum(SE_var, na.rm = TRUE), 
            grand_SE = sqrt(grand_sum_var / N^2))
urchin_year$taxa[urchin_year$taxa == 'purple urchin'] <- 'Purple urchin'
urchin_year$taxa[urchin_year$taxa == 'red urchin'] <- 'Red urchin'
urchin_year$taxa[urchin_year$taxa == 'green urchin'] <- 'Green urchin'

urchin_year$taxa = factor(urchin_year$taxa , levels = c('Purple urchin','Red urchin','Green urchin'))

urchin.plot <- ggplot(urchin_year, aes(x = year, y=grand_mean, color=taxa)) +
  geom_point() + 
  geom_line() + 
  geom_errorbar(data=urchin_year, 
                aes(x=year, ymin=grand_mean-grand_SE ,
                    ymax=grand_mean+grand_SE , color=taxa),width=0) +
  scale_color_manual(values = site.col$col[c(4,3,2)]) +
  scale_x_continuous("", breaks=seq(2015,data_year,2)) +
  scale_y_continuous(expression("No m"^-2), limits=c(0,5)) +
  theme_bw()+ theme_nt +
  theme(legend.position='inside') + 
  theme(legend.position.inside = c(0.2, 0.8))

urchin.plot
ggsave(paste0(fig_dir, 'urchin-spp-by-year.png'), width=3.5, height=2.5)


#### Urchins by spp, site and depth zone ####

urchin_zone <- invert_transect %>% filter(grp2=="URCHIN") %>%
  group_by(year,site,transect,observer,area,zone,grp2) %>%
  summarise(tot.density = sum(density, na.rm = TRUE)) %>% 
  group_by(year,site,zone,grp2) %>%
  summarise(mean.density=mean(tot.density, na.rm = TRUE), 
            SD = sd(tot.density), 
            N =length(year),
            SE= SD/sqrt(N) ) 

ggplot() +
  # geom_point(data= urchin1,aes(x=year,y=mean.density,color=site)) +
  geom_point(data=urchin_zone,aes(x=year,y=mean.density,color=site)) +
  geom_line(data= urchin_zone,
            aes(x=year,y=mean.density,color=site)) +
  geom_errorbar(data=urchin_zone,
                aes(x=year,ymin=mean.density-SE,ymax=mean.density+SE,color=site),width=0) +
  facet_grid(rows="zone", labeller = labeller(zone=depth.labs )) +
  scale_x_continuous(breaks=seq(2015,data_year,2)) +
  scale_y_continuous(expression("Urchin density (m"^-2*")")) +
  # scale_color_viridis_d(option="plasma",end=0.75)+
  scale_color_manual(values = site.col$col)+
  theme_bw()+ theme_nt + xlab("") + 
  theme(legend.position='inside') + 
  theme(legend.position.inside = c(0.3, 0.85))

ggsave(paste0(fig_dir, "urchin-spp-site-zone.png"), width = 3.5, height=3.5)


### Seastars ###########################################################

#### by spp by year ####
star_spp <-  invert_transect %>% filter(grp2=="STAR") %>% filter(taxa != 'sea_star_YOY') %>%
  group_by(site,year,species) %>%
  summarise(Mean=mean(density, na.rm = TRUE),
            SD=sd(density),
            N=length(density),
            SE=SD/sqrt(N),
            SE_var=SE^2)  %>%
  group_by(year,species) %>%
  summarise(grand_sum = sum(Mean, na.rm = TRUE),
            N=length(Mean),
            grand_mean = mean(Mean),
            grand_sum_var = sum(SE_var, na.rm = TRUE), 
            grand_SE = sqrt(grand_sum_var / N^2))
star_spp$spp = swath_codes$spp[ match(star_spp$species, swath_codes$species)]

stars1 <- ggplot() +
  geom_point(data = star_spp, aes(x=year,y=grand_mean,color=spp)) +
  geom_line( data = star_spp, aes(x=year,y=grand_mean,color=spp)) +
  geom_errorbar(data=star_spp,aes(x=year,ymin=grand_mean-grand_SE,
                                   ymax=grand_mean+grand_SE,color=spp),width=0) +
  scale_x_continuous(breaks=seq(2015,data_year,2)) +
  scale_color_manual(values=plot_colors) +
  scale_y_continuous(expression("Sea stars m"^-2) ) + 
  theme_bw()+ theme_nt +   xlab("") +
  theme(legend.text = element_text(face='italic'))

stars1

ggsave(paste0(fig_dir, "stars-by-year.png"), width=6.5, height = 3)

################################################################################
# SITE MAP #####################################################################
################################################################################


#### READ IN WASHINGTON MARINE SHORELINES (FROM 
shore.df <- read.csv( paste0(repo_dir,"/Data/PG Shore shore_thematic_UTM10/shore df.csv"))

PROJ <- "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs+ellps=GRS80+towgs84=0,0,0"

temp <- vect(cbind(shore.df$long,shore.df$lat),crs=PROJ)
temp2 <- project(temp,"+proj=longlat +datum=WGS84")
shore.df[,c("lon2","lat2")] <- geom(temp2)[,c("x","y")]

loc <-  read.csv( paste0(repo_dir,"/Data/Survey locations.csv"),fileEncoding="latin1")
# update years
loc$type[loc$type == '2015-22'] <- paste0('2015-',data_year)
loc <- loc %>% filter(type %in% c("2015 only",paste0('2015-',data_year)))
loc$nudge_x = 0.09
loc$nudge_x[loc$Site =="Pt. of the Arches"] = 0.12
loc$nudge_x[loc$Site =="Tatoosh Is."] = -0.08
loc$nudge_x[loc$Site =="Chibahdehl"] = 0.08
loc$nudge_x[loc$Site =="Neah Bay"] = 0.08
loc$nudge_y = 0
loc$nudge_y[loc$Site =="Chibahdehl"] = 0.01
x.lim =c(-124.85,-124.3)
y.lim =c(47.65,48.4)

ggplot()+theme_bw() +
  theme(legend.key =  element_rect(fill = "white", colour = "white"),
        panel.border = element_rect(fill = NA, colour = "black",size=1.5)) +
  geom_polygon(data=shore.df[shore.df$POLYTYPE == "Land" | shore.df$POLYTYPE == "Island" ,],
               aes(lon2,lat2,group=group),fill=grey(0.7),color=NA)+
  geom_point(data= loc %>% filter(type %in% c(paste0('2015-',data_year),"2015 only")),aes(x=lon,y=lat,color=type)) +
  scale_color_discrete("Survey")+
  geom_text(data= loc %>% filter(type %in% c(paste0('2015-',data_year),"2015 only")),
            aes(label=Site,x=lon,y=lat),size=2.5,
            #check_overlap = TRUE,
            nudge_x=loc$nudge_x,
            nudge_y=loc$nudge_y
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_fixed(ratio=1.3,xlim =x.lim,ylim=y.lim) +
  theme(axis.text.x = element_text(angle=90))

ggsave(paste0(fig_dir,"site-map.png"), width=5, height=5)


