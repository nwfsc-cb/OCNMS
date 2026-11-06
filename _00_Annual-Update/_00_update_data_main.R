library(here)

repo_dir = here::here()
home_dir = paste0(repo_dir,"/_00_Annual-Update/")
setwd(home_dir) # set home dir to annual update folder
source("settings.r")
source("R-functions-ocnms.r")
fish_codes = data.frame(read.csv("Data/spp_codes_fish.csv", header = TRUE))
kelp_codes = data.frame(read.csv("Data/spp_codes_kelp.csv", header = TRUE))
swath_codes = data.frame(read.csv("Data/spp_codes_swath.csv", header = TRUE))

# set year #####################################################################
data_year = 2026

################################################################################
# UDATE FISH ###################################################################
################################################################################

print("Loading fish")
fishx = data.frame(read.csv(
  paste0(data_dir,"NWFSC_FISH_ALLYEARS_data_",data_year,".csv"), header=T))
  
print('Running fish')
source(paste0(home_dir,"update-swath-fish.R"))
print('Finished fish')

## reload for more processing ##################################################
fish0 = readRDS( paste0(data_dir,'Fish_2015-',data_year,'.rds' ))
fish0 <- fish0 %>% 
  left_join(.,fish_codes %>% dplyr::select(species,group)) %>%
  rename(taxa=group)

# Identify rockfish YOY and add column; some summary of data ###################
fish0 <- fish0 %>% mutate(taxa=case_when(is.na(size_class)==TRUE ~ as.character(taxa), size_class=="large" ~ as.character(taxa), size_class=="small" ~ paste0(taxa,"yoy"))) %>%
  mutate(taxa= ifelse(taxa=="RYOYyoy","RYOY",taxa))

x = fish0 %>% filter(taxa %in% c('SEFLyoy','SEMEyoy','SEBYTyoy')) %>% group_by(taxa) %>% summarise(Tot=sum(Count))

# combine YT and black or keep separate? # COMBINE!
fish0$taxa[ fish0$taxa %in% c('SEFLyoy','SEMEyoy')] <- 'SEBYTyoy'

# Aggregate the SEBYTyoy so that there are one, not three, entries for "SEBYTyoy"
fish_yt <- fish0 %>% filter(taxa=="SEBYTyoy") %>% 
  group_by(site, transect,observer,
           size_class,year,area,zone,vis_m,taxa) %>%
  summarise(Count_all=sum(Count, na.rm = TRUE)) %>% 
  rename(Count=Count_all) %>%
  mutate(species="SEBYT",common.name=NA)

## re-add SEBYTyoy   
# could have just used rbind; don't know why I didn't
fish0 <- fish0 %>% filter(!taxa == "SEBYTyoy") %>% full_join(.,fish_yt)

sum(fish0$Count[fish0$taxa == 'SEBYTyoy'])

# subset visibility ####
# add fake vis for 2015
fish0$vis_m[ fish0$year == 2015 ] <- 3
# poor vis at destruction in year 1.
fish0$vis_m[ fish0$year == 2015 & fish0$site == "Destruction Island"] <- 1

# output for multivariate ####
fish1 = fish0 %>% filter(site %in%  sites)
sum(fish1$Count[fish1$taxa == 'SEBYTyoy'])
# drop low vis sites/transects and set depth
fish1a = fish1[fish1$vis_m >= min.vis, ]
sum(fish1a$Count[fish1a$taxa == 'SEBYTyoy'], na.rm = TRUE)
# select depth ####
fish1b = fish1a[fish1a$zone %in% fish.depth,]
sum(fish1b$Count[fish1b$taxa == 'SEBYTyoy'])

# calculate TOTyoy by site, transect, observer, and year
fish1b %>% filter(size_class == "small") %>% distinct(taxa)
# all of the small taxa are rockfish YOY.
fish1c <- fish1b %>% filter(size_class == "small") %>% 
  group_by(site,year,transect,observer,zone,area) %>% 
  summarise(Count_all=sum(Count, na.rm = TRUE)) %>%
  mutate(species="TOTyoy",taxa="TOTyoy") %>%
  rename(Count=Count_all)
# recombine
fish1d <- fish1b %>% full_join(.,fish1c) 
fish_transect <- fish1d

saveRDS(fish_transect, paste0(data_dir,'Summarized_Data_Fish_by_Transect.rds'))
write.csv(fish_transect, paste0(data_dir,'Summarized_Data_Fish_by_Transect.csv'))

################################################################################
# UPDATE SWATH #################################################################
################################################################################

# inverts and algae ####
print("Loading inverts & algae")
swathx = data.frame(read.csv(
  paste0(data_dir,"NWFSC_SWATH_ALLYEARS_data_",data_year,".csv")))
swathx = swathx %>% filter(!is.na(YEAR))
# remove some problematic Samhouri observations
swathx$COUNT = ifelse(swathx$OBSERVER=="Jameal Samhouri" &
                        swathx$SITE == "Cape Johnson" &
                        swathx$SIDE == "N" &
                        swathx$CLASSCODE =="MACPYR" & 
                        swathx$YEAR == 2024 &  
                        swathx$METERS.sampled < 10, NA, swathx$COUNT)
print("Running inverts & algae")
source(paste0(home_dir,"update-swath-inverts.R"))



       