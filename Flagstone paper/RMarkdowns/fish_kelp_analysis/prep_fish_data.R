# prep fish data

library(tidyverse)
library(here)
library(janitor)
# read in data and lookup keys
fish <- read_rds(here::here('Flagstone paper','Data','Fish_2015-2021.rds')) # these data are complete and include zeroes for transects in which a species is not seen

fish_codes = data.frame(read.csv( here::here('Flagstone paper','Data',"spp_codes_fish.csv") ))

sites = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island")

min.vis = 2

fish.depth = c(5,10)

fish %>%
  tabyl(site, area, year)

# add in yoy designator and fix some names
fish0 <- fish %>% 
  #remove_empty("rows") %>% 
  left_join(.,fish_codes %>% dplyr::select(code,group),by=c("species"="code")) %>%
  rename(taxa=group)

# dim(fish[!apply(fish == "", 1, all),])

# assign all small size class fish to taxa == taxayoy, so we can focus on RYOY
fish1 <- fish0 %>% mutate(taxa=case_when(is.na(size_class)==TRUE ~ as.character(taxa),
                                         size_class=="large" ~ as.character(taxa),
                                         size_class=="small" ~ paste0(taxa,"yoy"))) %>%
  mutate(taxa= ifelse(taxa=="RYOYyoy","RYOY",taxa))

# combine YT and black or keep separate? # COMBINE!
# fish1 %>% filter(taxa %in% c('SEFLyoy','SEMEyoy','SEBYTyoy')) %>% group_by(taxa) %>% 
#   summarise(Tot=sum(Count))

fish1$taxa[ fish1$taxa %in% c('SEFLyoy','SEMEyoy')] <- 'SEBYTyoy'

# Aggregate the SEBYTyoy so that there are one, not three, entries for "SEBYTyoy"
fish_yt <- fish1 %>% filter(taxa=="SEBYTyoy") %>% 
  group_by(site, transect, observer,
           size_class, year, area, zone, vis_m, taxa) %>%
  summarise(Count_all=sum(Count, na.rm = TRUE)) %>% rename(Count=Count_all) %>%
  mutate(species="SEBYT",common.name=NA) %>%
  ungroup()

# check on SEBYT. see if CJ 2015 zone 5 has any. it does not 
View(fish1 [ fish1$year == 2015 & fish1$site == "Cape Johnson" & fish1$zone == 5 & fish1$species == "SEBYT", ] )

fish1 %>%
  tabyl(site, area, year)

fish2 <- fish1 %>% filter(!taxa == "SEBYTyoy") %>% full_join(.,fish_yt)
length(which(is.na(fish2$Count)))
# check on SEBYT. see if CJ 2015 zone 5 is still there. it is
View(fish2 [ fish2$year == 2015 & fish2$site == "Cape Johnson" & fish2$zone == 5 & fish2$species == "SEBYT", ] )

# subset to 5 index sites ####
fish2a = fish2 %>% filter(site %in%  sites) %>%
  mutate(
    site = droplevels(site),
  )

# check for NAs in depth
unique(fish2a$zone) # none
length(which(is.na(fish2a$zone) == TRUE)) #0

# # drop empty rows. 
# fish2aa <- fish2a %>% remove_empty("rows")

# # select depth #### 
# fish1b = fish1a[fish1a$zone %in% fish.depth,]

# assign dummy area for 2015 data
fish2a$area[fish2a$year == 2015] <- "D"

# make sure everything looks ok. it does
fish2a %>%
  tabyl(site, area, year)

# assign dummy visibility ####
# add fake vis for 2015 at DI
fish2a$vis_m[ fish2a$year == 2015 ] <- 3
# poor vis at destruction in year 1. but no fish observed so do not need next line 
# fish2a$vis_m[ fish2a$year == 2015 & fish2a$site == "Destruction Island"] <- 1

# any NAs for vis_m? yes
length(which(is.na(fish2a$vis_m)))
View(fish2a[which(is.na(fish2a$vis_m)),]) # all DI 2017 10m depth zone. JS chatted with OS, agreed to assume that viz was bad on these transects
# View(fish[which(is.na(fish$vis_m)),])

# add poor vis for 2017 at DI deep
fish2a$vis_m[ fish2a$year == 2017 & fish2a$site == "Destruction Island" & fish2a$zone == 10] <- 1

# first how many transects have excludable viz?
fish2a %>% filter(vis_m < min.vis) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect)))
# 19 yr-site-area-zones, with varying number of transects

# convert low vis sites/transects to NAs
fish3 <- fish2a %>%
  mutate(
    Count = ifelse(
      vis_m >= min.vis, Count, -999999
    )
  ) %>%
  rename(count = Count)

# how many counts are NAs now
length(which(is.na(fish3$count)))
# how many counts are -999999
length(which(fish3$count == -999999))
# fish3 %>% filter(is.na(count)) %>% group_by(year, site, area, zone, transect) %>% distinct(transect)
# now how many transects have excludable viz?
fish3 %>% filter(vis_m < min.vis) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect)))
# 19 yr-site-area-zones, with varying number of transects

# drop low viz transects here
fish3a <- fish3 %>%
  filter(vis_m >= min.vis)
# how many counts are NAs now
length(which(is.na(fish3a$count))) # 0
# how many counts are -999999
length(which(fish3a$count == -999999))
# now how many transects have excludable viz?
fish3a %>% filter(vis_m < min.vis) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect)))
# 0 yr-site-area-zones

# calculate total yoy by site, transect, observer, and year
fish3a %>% filter(size_class == "small") %>% distinct(taxa)
# all of the small taxa are rockfish YOY.

fish3b <- fish3a %>% filter(size_class == "small") %>% 
  group_by(site,year,area,zone,transect,observer,vis_m) %>% 
  summarise(Count_all=sum(count)) %>%
  ungroup() %>%
  mutate(species="TOTyoy",taxa="TOTyoy") %>%
  rename(count=Count_all)
# how many counts are NAs now
length(which(is.na(fish3b$count)))
# now how many transects have excludable viz?
fish3b %>% filter(vis_m < min.vis) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect)))
# 0 yr-site-area-zones

# join total yoy df with the rest
fish4 <- fish3a %>% full_join(.,fish3b) %>%
  mutate(
    transect.area.fish = 60 # all fish transects are 30m x 2m
  ) 
# how many counts are NAs now
length(which(is.na(fish4$count)))
# now how many transects have excludable viz?
fish4 %>% filter(vis_m < min.vis) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect))) # none
fish4 %>% filter(vis_m < min.vis) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect))) %>% ungroup() %>% summarise(tot_transects = sum(num_NA_transects)) # none

# fish4a = fish4[,c('year', 'site', 'area', 'zone', 'transect', 'transect.area.fish', 'observer', 
#                    'common.name', 'species', 'taxa', 'size_class', 'count', 'vis_m')]
fish4a <- fish4 %>%
  dplyr::select(
    year, site, area, zone, transect, transect.area.fish, observer,
    common.name, species, taxa, size_class, count, vis_m
  )

glimpse(fish4a)

# make sure everything looks ok. it does
fish4a %>%
  tabyl(site, area, year)
# fish4a is a clean transect-level df 

View(fish4a [ fish4a$year == 2015 & fish4a$site == "Cape Johnson" & fish4a$zone == 5 & fish4a$species == "SEBYT", ] )
View(fish4a [ fish4a$year == 2015 & fish4a$site == "Cape Johnson" & fish4a$zone == 5 & fish4a$taxa == "SEBYT", ] ) # oh no, there is no SEBYT taxa


# unique(fish4a$taxa)

# how many unique yr-site-area-zones have NA values?
fish4a %>% filter(is.na(count)) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect))) # none
fish4a %>% filter(is.na(count)) %>% group_by(year, site, area, zone) %>% summarise(num_NA_transects = length(unique(transect))) %>% ungroup() %>% summarise(tot_transects = sum(num_NA_transects)) # none

write_rds(fish4a, here::here('Flagstone paper','Data','fish_2015_2021_year_site_area_zone_transect.rds'))


# quick data check ###
fish4b = fish4a[,c('year', 'site', 'area', 'zone', 'transect','taxa', 'count' )]
View(fish4b [ fish4b$year == 2015 & fish4b$site == "Cape Johnson" & fish4b$zone == 5 & fish4b$taxa == "SEBYT", ] )

fish5 = fish4b %>% group_by(year,site,area,zone,transect,taxa) %>%
  summarise(transect_mean = mean(count)) %>%
  ungroup() %>%
  group_by(year,site,area,zone, taxa)%>%
  summarise(Mean = mean(transect_mean)) %>%
  ungroup()
glimpse(fish5)

View(fish5 [ fish5$year == 2015 & fish5$site == "Cape Johnson" & fish5$zone == 5 & fish5$taxa == "SEBYT", ] )

fishwide =  pivot_wider(fish5, names_from = taxa, values_from = Mean, values_fill = NA)
dim(fishwide)
View(fishwide)
# there are SEBYT that are NA. return to this 11-30-2021

# nrows is 82 but low vis rows not excluded. does this match the df NT used? it has the same number of rows

df_nt <- read_rds(here::here('Flagstone paper','Data','Data_Fish_Kelp_area_wide.rds'))
glimpse(df_nt)
View(df_nt)

#dplyr::all_equal(fishwide, df_nt)

