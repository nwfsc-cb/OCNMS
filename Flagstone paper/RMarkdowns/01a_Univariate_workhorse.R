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
                      mutate(species="totYOY",taxa="totYOY") %>%
                      rename(Count=Count_all)
fish1d <- fish1b %>% full_join(.,fish1c)                       
                    
# double check specific averaging process here 
# fish2 = aggregate( Count ~ site + zone + year + taxa, data = fish1c, FUN = mean )
# colnames(fish2)[ncol(fish2)] <- 'mean'
# fish = Sum_Stats('mean ~ year +  taxa', fish2)
# fish$se_lo = fish$mean - fish$se
# fish$se_up = fish$mean + fish$se

# Ole made this to check the aggregating in fish 2.
fish3 <- fish1d %>% group_by(site,year,zone,species, taxa) %>%
  summarise(Mean=mean(Count),SD=sd(Count),N=length(Count),SE=SD/sqrt(N))

# Check to make sure all of the site-year-zone combinations have equivalent number of transects.
fish3 %>% group_by(site,year,zone) %>% distinct(N) %>% as.data.frame()

#######################################################################
# PULL IN INVERTEBRATES AND ALGAE FOR ANALYSIS.
#######################################################################

# read in rds file with combined data
swath0 = readRDS( paste0(Data_Loc,'Swath_2015-2021.rds' ))
spp_swath <- read.csv("spp_codes_swath.csv")
#separate into algae and invertebrate data frames.

swath1 <- left_join(swath0,spp_swath)

dat.algae   <- swath1 %>% filter(group=="Algae")
dat.invert  <- swath1 %>% filter(group=="Invert")


























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

