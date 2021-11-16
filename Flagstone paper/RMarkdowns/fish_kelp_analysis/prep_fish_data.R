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

# add in yoy designator and fix some names
fish0 <- fish %>% 
  #remove_empty("rows") %>% 
  left_join(.,fish_codes %>% dplyr::select(code,group),by=c("species"="code")) %>%
  rename(taxa=group)

# dim(fish[!apply(fish == "", 1, all),])

fish0 <- fish0 %>% mutate(taxa=case_when(is.na(size_class)==TRUE ~ as.character(taxa),
                                         size_class=="large" ~ as.character(taxa),
                                         size_class=="small" ~ paste0(taxa,"yoy"))) %>%
  mutate(taxa= ifelse(taxa=="RYOYyoy","RYOY",taxa))

# combine YT and black or keep separate? # COMBINE!
# fish0 %>% filter(taxa %in% c('SEFLyoy','SEMEyoy','SEBYTyoy')) %>% group_by(taxa) %>% 
#   summarise(Tot=sum(Count))

fish0$taxa[ fish0$taxa %in% c('SEFLyoy','SEMEyoy')] <- 'SEBYTyoy'

# Aggregate the SEBYTyoy so that there are one, not three, entries for "SEBYTyoy"
fish_yt <- fish0 %>% filter(taxa=="SEBYTyoy") %>% 
  group_by(site, transect, observer,
           size_class, year, area, zone, vis_m, taxa) %>%
  summarise(Count_all=sum(Count, na.rm = TRUE)) %>% rename(Count=Count_all) %>%
  mutate(species="SEBYT",common.name=NA)

fish0 <- fish0 %>% filter(!taxa == "SEBYTyoy") %>% full_join(.,fish_yt)

# subset visibility ####
# add fake vis for 2015
fish0$vis_m[ fish0$year == 2015 ] <- 3
# poor vis at destruction in year 1.
fish0$vis_m[ fish0$year == 2015 & fish0$site == "Destruction Island"] <- 1

# subset to 5 index sites ####
fish1 = fish0 %>% filter(site %in%  sites) %>%
  mutate(
    site = droplevels(site),
  )

# convert low vis sites/transects to NAs
fish1a <- fish1 %>%
  mutate(
    Count = ifelse(
      vis_m >= min.vis, Count, NA
    )
  )

# check for NAs in depth
unique(fish1a$zone)
length(which(is.na(fish1a$zone) == TRUE)) #0

# # drop empty rows. RETURN TO CHECK
# fish1a <- fish1a %>% remove_empty("rows")

# # select depth #### 
# fish1b = fish1a[fish1a$zone %in% fish.depth,]

# calculate total yoy by site, transect, observer, and year
fish1a %>% filter(size_class == "small") %>% distinct(taxa)
# all of the small taxa are rockfish YOY.

# assign dummy area for 2015 data
fish1a$area[fish1a$year == 2015] <- "D"

# make sure everything looks ok. it does
fish1a %>%
  tabyl(site, area, year)

fish1c <- fish1a %>% filter(size_class == "small") %>% 
  group_by(site,year,transect,observer,zone,area) %>% 
  summarise(Count_all=sum(Count, na.rm = TRUE)) %>%
  mutate(species="TOTyoy",taxa="TOTyoy") %>%
  rename(Count=Count_all)
fish1d <- fish1a %>% full_join(.,fish1c) %>%
  rename(count = Count) %>%
  mutate(
    transect.area.fish = 60 # all fish transects are 30m x 2m
  ) %>%
  select(
    year, site, area, zone, transect, transect.area.fish, observer, 
    common.name, species, taxa, size_class, count, vis_m
  )
glimpse(fish1d)

# make sure everything looks ok. it does
fish1d %>%
  tabyl(site, area, year)
# fish1d is a clean transect-level df 

# unique(fish1d$taxa)

write_rds(fish1d, here::here('Flagstone paper','Data','fish_2015_2021_year_site_area_zone.rds'))


