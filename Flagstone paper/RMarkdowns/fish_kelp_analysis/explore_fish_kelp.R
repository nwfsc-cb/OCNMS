# Consider relationships between rockfish YOYs and kelp
# Each species/group of fish vs each kelp species
# Each species/group of fish vs sum of understory kelp species
# Each species/group of fish vs sum of canopy kelp species

library(tidyverse)
library(here)
library(viridis)
library(janitor)
library(lme4) # needed for glmer package


#### BRING IN DATA ##########

# run prep_fish_data.R, which uses df from ole Fish_2015-2021.rds 
fish <- read_rds(here::here('Flagstone paper','Data','fish_2015_2021_year_site_area_zone_transect.rds'))
glimpse(fish)

# run prep_algae_data.R, which uses df from ole Swath_2015-2021.rds
algae <- read_rds(here::here('Flagstone paper','Data','algae_2015_2021_year_site_area_zone.rds'))
glimpse(algae)

#### PREP DATA #########

# with(fish, table(year, observer, site, area))

## make a fish df summarised by year-site-area-zone, pivot_wide
fish_wide <- fish %>%
  # group_by(
  #   year, site, area, zone, taxa
  # ) %>%
  # summarise(
  #   count = mean(count, na.rm=TRUE)
  # )
  select(-c(common.name, species, size_class, vis_m)) %>%
  pivot_wider(
    names_from = taxa,
    values_from = count,
    values_fn = sum, # use sum because there are some taxa with counts listed under multiple size classes
    values_fill = -99999
              )
glimpse(fish_wide) # this is 318 rows, if we distill to yr-site-area-zone should only be 82 rows. need to check

# checked and yes it gives 82 rows with the example taxa TOTyoy
# glimpse(
#   fish_wide %>% select(year,site,area,zone,transect,TOTyoy) %>%
#     group_by(year,site,area,zone,transect) %>%
#     summarise(transect_mean_TOTyoy = mean(TOTyoy)) %>%
#     ungroup() %>%
#     group_by(year,site,area,zone)%>%
#     summarise(Mean = mean(transect_mean_TOTyoy)) %>%
#     ungroup()
# )

# are there values of -99999? NONE! Ole fixed by changing 15 SEBYT large/NA to small at DI S5 transect 4.
length(which(fish_wide == -99999)) 
length(which(is.na(fish_wide)))
View(fish_wide) 

# ok, now summarize num transects with YOY present vs absent by yr-site-area-zone

fish_wide2 <- fish_wide %>%
  mutate(
    SEBYTyoy_pres = ifelse(SEBYTyoy > 0, 1, 0),
    SECAyoy_pres = ifelse(SECAyoy > 0, 1, 0),
    SEPIyoy_pres = ifelse(SEPIyoy > 0, 1, 0),
    TOTyoy_pres = ifelse(TOTyoy > 0, 1, 0)
  ) 

yoy_wide <- fish_wide2 %>%
  group_by(year,site,area,zone) %>%
  summarise(
    mean_SEBYTyoy = mean(SEBYTyoy),
    num_trans_SEBYTyoy_Y = sum(SEBYTyoy_pres),
    num_trans_SEBYTyoy_N = length(transect) - num_trans_SEBYTyoy_Y,
    mean_SECAyoy = mean(SECAyoy),
    num_trans_SECAyoy_Y = sum(SECAyoy_pres),
    num_trans_SECAyoy_N = length(transect) - num_trans_SECAyoy_Y,
    mean_SEPIyoy = mean(SEPIyoy),
    num_trans_SEPIyoy_Y = sum(SEPIyoy_pres),
    num_trans_SEPIyoy_N = length(transect) - num_trans_SEPIyoy_Y,
    mean_TOTyoy = mean(TOTyoy),
    num_trans_TOTyoy_Y = sum(TOTyoy_pres),
    num_trans_TOTyoy_N = length(transect) - num_trans_TOTyoy_Y,
    n_fish_transects = n()
  ) %>%
  ungroup()
View(yoy_wide)
glimpse(yoy_wide)
#fish_wide %>% filter(year == 2015 & site == "Cape Johnson" & zone == 5) 

## make a kelp df summarised by year-site-area-zone, pivot_wide
algae_area_wide <- algae %>%
  group_by(
    year, site, area, zone, species
  ) %>%
  summarise(
    density = mean(density, na.rm=TRUE),
    #count = mean(count, na.rm=TRUE),
    transect.area.algae = sum(transect.area.algae, na.rm=TRUE)
  ) %>%
  mutate(
    transect.area.algae.weight = transect.area.algae/max(transect.area.algae)
  ) %>%
  pivot_wider(
    names_from = species,
    values_from = density,
    values_fill = -99999
  ) %>%
  ungroup()
glimpse(algae_area_wide)
# as of 2021, only 1 transect at DI in 2015 has -99999 values because we only counted macro, nereo, and ptery. these will get dropped anyway because there are no fish data for DI in 2015

length(which(is.na(algae_area_wide$NO_ALG)))
length(which(algae_area_wide == -99999))  #13

# join the resulting df's, at yr-site-area-zone level, make year_factor column
yoy_kelp <- yoy_wide %>% left_join(algae_area_wide, by=c('site'='site','year'='year','area'='area','zone'='zone')) %>%
  mutate(
    year_factor = factor(year)
  )

glimpse(yoy_kelp)

# a tabyl of site x area, split into a list by year
yoy_kelp %>%
  tabyl(site, area, year) 
# a tabyl of site x zone, split into a list by year
yoy_kelp %>%
  tabyl(site, zone, year) 

sort(unique(yoy_kelp$year))
sort(unique(yoy_kelp$site)) #do not need to drop levels
sort(unique(yoy_kelp$area)) 
sort(unique(yoy_kelp$zone)) 

# do a bit of cleanup, make summed kelps [ no longer making  fish and kelp occurrence and conditional abundance columns, 12-16-2021]

# YOYs: SEBYTyoy (black yellowtail), SECAyoy (copper), SEMAyoy (quill), SEMYyoy (blue), SENEyoy (china), SEPIyoy (canary)
# black rocks: SEME
# kelps: MACPYR, NERLUE, PTECAL, canopy_kelp, three_kelps, ner_pte, and diff_ner_pte

yoy_kelp2 <- yoy_kelp %>%
  mutate(
    site = droplevels(site),
    canopy_kelp = NERLUE + MACPYR,
    three_kelps = canopy_kelp + PTECAL,
    ner_pte = NERLUE + PTECAL,
    diff_ner_pte = NERLUE - PTECAL
   ) #%>%
  # dplyr::select(
  #   year, year_factor, site, area, zone, transect, 
  #   transect.area.algae, transect.area.algae.weight,
  #   SEBYTyoy, SECAyoy, SEPIyoy, TOTyoy, SEME,
  #   MACPYR, NERLUE, PTECAL, canopy_kelp, three_kelps, ner_pte, diff_ner_pte
  # ) %>%
  # mutate(
  #   SEBYTyoy_pres = ifelse(SEBYTyoy > 0, 1, 0),
  #   SECAyoy_pres = ifelse(SECAyoy > 0, 1, 0),
  #   SEPIyoy_pres = ifelse(SEPIyoy > 0, 1, 0),
  #   TOTyoy_pres = ifelse(TOTyoy > 0, 1, 0),
  #   SEME_pres = ifelse(SEME > 0, 1, 0),
  #   MACPYR_pres = ifelse(MACPYR > 0, 1, 0), 
  #   NERLUE_pres = ifelse(NERLUE > 0, 1, 0), 
  #   PTECAL_pres = ifelse(PTECAL > 0, 1, 0),
  #   three_kelps_pres = ifelse(three_kelps > 0, 1, 0),
  #   canopy_kelp_pres = ifelse(canopy_kelp > 0, 1, 0),
  #   ner_pte_pres = ifelse(ner_pte > 0, 1, 0),
  #   SEBYTyoy_cond_abund = ifelse(SEBYTyoy > 0, SEBYTyoy, NA),
  #   SECAyoy_cond_abund = ifelse(SECAyoy > 0, SECAyoy, NA),
  #   SEPIyoy_cond_abund = ifelse(SEPIyoy > 0, SEPIyoy, NA),
  #   TOTyoy_cond_abund = ifelse(TOTyoy > 0, TOTyoy, NA),
  #   SEME_cond_abund = ifelse(SEME > 0, SEME, NA)
  # )

write_rds(yoy_kelp2, here::here('Flagstone paper','Data','yoy_kelp_2015_2021_year_site_area_zone.rds'))

# does yoy_kelp2 match NTs df? close but not quite. there are 4 yr-site-area-zones that do NOT match. these are 2015-Cape Johnson-D-5, 2015-Tatoosh Island-D-5, 2015-Neah Bay-D-5, and 2019-Destruction Island-S-5
df_nt <- read_rds(here::here('Flagstone paper','Data','Data_Fish_Kelp_area_wide.rds'))
glimpse(df_nt)
View(df_nt)

yoy_kelp2$site <- as.factor(yoy_kelp2$site)
df_nt$site <- as.factor(df_nt$site)

df_nt_compare <- df_nt %>% dplyr::select(year, site, area, zone, SEBYTyoy, SECAyoy, SEPIyoy, TOTyoy, NERLUE, MACPYR, PTECAL)
yoy_kelp2_compare <- yoy_kelp2 %>% dplyr::select(year, site, area, zone, mean_SEBYTyoy, mean_SECAyoy, mean_SEPIyoy, mean_TOTyoy, NERLUE, MACPYR, PTECAL) %>% rename(
  SEBYTyoy = mean_SEBYTyoy, SECAyoy = mean_SECAyoy, SEPIyoy = mean_SEPIyoy, TOTyoy=mean_TOTyoy
  )

dplyr::all_equal(
  yoy_kelp2_compare, 
  df_nt_compare, convert = TRUE)

View(yoy_kelp2_compare[c(1, 3, 4, 46),])
View(df_nt_compare[c(6, 11, 45, 64),])


#### 
#### 
#### 
#### 
#### 
#### 
#### JAMEAL DIDN'T REALLY GO MUCH FURTHER THAN THIS QUICK CHECK ON ONE MODEL, WHICH IS HIGHLY NS FOR KELPS
# try occurrence

TOTyoy.3kelp.m1 <- glm(num_trans_TOTyoy_Y/(num_trans_TOTyoy_Y+num_trans_TOTyoy_N) ~ (three_kelps * zone) + year_factor, data = yoy_kelp2, family = quasibinomial(link = 'logit'), weights = (num_trans_TOTyoy_Y+num_trans_TOTyoy_N))
summary(TOTyoy.3kelp.m1)

TOTyoy.3kelp.m2 <- glm(num_trans_TOTyoy_Y/(num_trans_TOTyoy_Y+num_trans_TOTyoy_N) ~ three_kelps + zone + year_factor, data = yoy_kelp2, family = quasibinomial(link = 'logit'), weights = (num_trans_TOTyoy_Y+num_trans_TOTyoy_N))
summary(TOTyoy.3kelp.m2)


TOTyoy.nereo.m1 <- glm(num_trans_TOTyoy_Y/(num_trans_TOTyoy_Y+num_trans_TOTyoy_N) ~ (NERLUE * zone) + year_factor, data = yoy_kelp2, family = quasibinomial(link = 'logit'), weights = (num_trans_TOTyoy_Y+num_trans_TOTyoy_N))
summary(TOTyoy.nereo.m1)

TOTyoy.nereo.m2 <- glm(num_trans_TOTyoy_Y/(num_trans_TOTyoy_Y+num_trans_TOTyoy_N) ~ NERLUE + zone + year_factor, data = yoy_kelp2, family = quasibinomial(link = 'logit'), weights = (num_trans_TOTyoy_Y+num_trans_TOTyoy_N))
summary(TOTyoy.nereo.m2)


#### 
#### 
#### 
#### 
#### 
#### 
# 12-16-2021. the code below was intended to work with an earlier version of the df's. DO NOT USE FOR NOW UNTIL FIXED

#### MAKE BIVARIATE PLOTS #########

source(here::here('Flagstone paper','RMarkdowns','fish_kelp_analysis','fish_kelp_plotting_functions.R'))

# 1)
# test function for plotting abundance and conditional abundance
bivariate_plot_abund(kelp_sp = "PTECAL",fish_sp = "SEBYTyoy", trans="none")

# it works!
# loop over all desired fish and kelp species
abund_combos <- crossing(
  kelps = c('MACPYR', 'NERLUE', 'PTECAL', 'canopy_kelp', 'three_kelps', 'ner_pte'),#names(fish_kelp[,c(10:16)]),
  fishes = c('SEBYTyoy', 'SECAyoy', 'SEPIyoy', 'TOTyoy', 'SEME')
)

purrr::walk2(.x = abund_combos$kelps, .y = abund_combos$fishes, .f=bivariate_plot_abund, trans="none")

# 2)
# test function for plotting occurrence
bivariate_plot_occur(kelp_sp = "PTECAL",fish_sp = "SEBYTyoy_pres", trans="none")

# it works!
# loop over all desired fish and kelp species
occur_combos <- crossing(kelps = c('MACPYR', 'NERLUE', 'PTECAL', 'canopy_kelp', 'three_kelps', 'ner_pte'),#names(fish_kelp[,c(10:16)]),
                         fishes = c('SEBYTyoy_pres', 'SECAyoy_pres', 'SEPIyoy_pres', 'TOTyoy_pres', 'SEME_pres')) #names(fish_kelp[,c(17:21)])) # 22:27 are kelp presence, but boring
# expand.grid(kelps = names(fish_kelp[,c(10:16, 22:27)]),
#                             fishes = names(fish_kelp[,c(17:21)]))

purrr::walk2(.x = occur_combos$kelps, .y = occur_combos$fishes, .f=bivariate_plot_occur, trans="none")

#### INSPECT PLOTS, THEN MOVE TO analyze_fish_kelp.R #########


######################################################

######### GRAVEYARD #############

# # df from Nick, email sent 10-21-2021 2301
# fish_kelp <- read_rds(here::here('Flagstone paper','Data','Data_Fish_Kelp_area_wide.rds'))
# glimpse(fish_kelp)

# # look at distributions of kelps
# ggplot(data=fish_kelp) +
#   geom_density(aes(NERLUE)) +
#   theme_bw()
# ggplot(data=fish_kelp) +
#   geom_density(aes(MACPYR)) +
#   theme_bw()
# ggplot(data=fish_kelp) +
#   geom_density(aes(PTECAL)) +
#   theme_bw()
# ggplot(data=fish_kelp) +
#   geom_density(aes(three_kelps)) +
#   facet_grid(rows = vars(zone)) +
#   theme_bw()

# # Abundance
# fish_kelp %>%
#   ggplot() +
#   geom_point(aes(x=canopy_kelp, y = SEBYTyoy, color = as.factor(zone), shape = as.factor(site))) +
#   scale_color_viridis(discrete = T)  +
#   xlab("Canopy Kelp Density") +
#   ylab("Black and Yellowtail YOY") +
#   theme_classic()
# 
# # Occurrence
# # https://www.ericrscott.com/post/plot-logistic-regressions/
# fish_kelp %>%
#   filter(is.na(zone) == FALSE) %>%
#   ggplot(aes(x=canopy_kelp,y = SEBYTyoy_pres, color = as.factor(site))) +
#   #geom_jitter(aes(y = YTBLyoy_occur, color = as.factor(site.x))) +
#   stat_summary_bin(geom = "point", fun = mean, aes(y = SEBYTyoy_pres)) + # , bins = 60
#   facet_grid(rows = vars(zone)) +
#   scale_color_viridis(discrete = T, name = "")  +
#   xlab("Canopy Kelp Density") +
#   ylab("Black and Yellowtail YOY") +
#   theme_classic()  
# 
# fish_kelp %>%
#   filter(is.na(zone) == FALSE) %>%
#   ggplot(aes(x=PTECAL,y = SEPIyoy_pres, color = as.factor(site))) +
#   #geom_jitter(aes(y = YTBLyoy_occur, color = as.factor(site.x))) +
#   stat_summary_bin(geom = "point", fun = mean, aes(y = SEPIyoy_pres)) + # , bins = 60
#   facet_grid(rows = vars(zone)) +
#   scale_color_viridis(discrete = T, name = "")  +
#   xlab("Pterygophora Density") +
#   ylab("Canary YOY") +
#   theme_classic()  
# 
# # canary YOY abundance if kelp present
# 
# # Conditional abundance
# # try faceting a bit with positive data only
# fish_kelp %>%
#   filter(is.na(zone) == FALSE) %>%
#   ggplot(aes(x=canopy_kelp, y = SEBYTyoy_cond_abund, color = as.factor(site))) +
#   geom_point() +
#   #geom_smooth(method = lm) + 
#   facet_grid(rows = vars(zone)) +
#   #scale_x_log10() +
#   #scale_y_log10() +
#   scale_color_viridis(discrete = T, name = "")  +
#   xlab("Canopy Kelp Density") +
#   ylab("Black and Yellowtail YOY") +
#   theme_classic()


######################################################
######################################################
######################################################

### analysis prior to 10-26-2021 below here

######################################################

# df from Nick, email sent 10-13-2021 0909. represent raw means for year x site x depth x area by taxa/species.
# I believe the fish data have been filtered to include only transects that meet min viz criteria (2.0m)
fish_kelp <- read_rds(here::here('Flagstone paper','Data','Data_Fish_x_Kelp.RDS'))


unique(fish_kelp$year.x)
# 2015 data are MIA, drop NA

unique(fish_kelp$site.x)
# drop extra sites from 2015, drop NA

unique(fish_kelp$zone.x)
# drop NA

# check out that areas and depths have consistency across sites and years

with(fish_kelp,table(area.x, year.x, site.x, zone.x))

# depths are good. areas are not good. clean it up based on the key below.
#key <- read_csv(here::here('Flagstone paper','Data','site_area_lookup_key.csv'))

fish_kelp <- fish_kelp %>%
  # droplevels for Teahwhit Head, Rock 305, Point of the Arches, Anderson Point, Chibadehl Rocks
  mutate(
    site.x = droplevels(site.x),
    area = case_when(
      site.x == "Cape Alava" & area.x == 1 ~ "W",
      site.x == "Cape Alava" & area.x == 2 ~ "E",
      site.x == "Cape Johnson" | site.x == "Destruction Island" & area.x == 1 ~ "S",
      site.x == "Cape Johnson" | site.x == "Destruction Island" & area.x == 2 ~ "N",
      site.x == "Neah Bay" | site.x == "Tatoosh Island" & area.x == 1 ~ "N",
      site.x == "Neah Bay" | site.x == "Tatoosh Island" & area.x == 2 ~ "N",
      TRUE ~ NA_character_
    )
  )

# Kelp are either total kelp (NERLUE+ MACPYR+PTECAL), canopy kelp (NERLUE+ MACPYR), or understory kelp (PTECAL)
fish_kelp$canopy_kelp = fish_kelp$NERLUE + fish_kelp$MACPYR
fish_kelp$kelp = fish_kelp$NERLUE + fish_kelp$MACPYR + fish_kelp$PTECAL
fish_kelp$ner_pte =  fish_kelp$NERLUE + fish_kelp$PTECAL

###### fish ####
########fish1 includes all spp and five sites but no deletion of low vis.
# drop low vis sites/transects
# fish1a = fish1[fish1$vis_m >= min.vis, ]

#look at YTBLyoy v NERLUE

fish_kelp %>%
  ggplot() +
  geom_point(aes(x=NERLUE, y = YTBLyoy, color = as.factor(zone.x), shape = as.factor(site.x))) +
  scale_color_viridis(discrete = T)  +
  xlab("Nereo Density") +
  ylab("Black and Yellowtail YOY") +
  theme_classic()

# fish_kelp %>%
#   ggplot() +
#   geom_point(aes(x=log10(NERLUE+1), y = log10(YTBLyoy+1), color = as.factor(zone.x), shape = as.factor(site.x))) +
#   scale_color_viridis(discrete = T)  +
#   xlab("Nereo Density") +
#   ylab("Black and Yellowtail YOY") +
#   theme_classic()

#look at YTBLyoy v canopy
fish_kelp %>%
  ggplot() +
  geom_point(aes(x=canopy_kelp, y = YTBLyoy, color = as.factor(site.x), shape = as.factor(site.x))) +
  scale_color_viridis(discrete = T)  +
  xlab("Canopy Kelp Density") +
  ylab("Black and Yellowtail YOY") +
  theme_classic()

#look at YTBLyoy v all kelp
fish_kelp %>%
  ggplot() +
  geom_point(aes(x=kelp, y = YTBLyoy, color = as.factor(zone.x), shape = as.factor(site.x))) +
  scale_color_viridis(discrete = T)  +
  xlab("All Kelp Density") +
  ylab("Black and Yellowtail YOY") +
  theme_classic()

# try faceting a bit with positive data only
fish_kelp %>%
  filter(is.na(zone.x) == FALSE) %>%
  filter(YTBLyoy > 0) %>%
  filter(canopy_kelp > 0) %>%
  ggplot(aes(x=canopy_kelp, y = YTBLyoy, color = as.factor(site.x))) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_grid(rows = vars(zone.x)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis(discrete = T, name = "")  +
  xlab("Canopy Kelp Density") +
  ylab("Black and Yellowtail YOY") +
  theme_classic()

# try occurrence
# https://www.ericrscott.com/post/plot-logistic-regressions/
fish_kelp %>%
  mutate(
    YTBLyoy_occur = ifelse(YTBLyoy > 0,1,0),
    canopy_kelp_occur = ifelse(canopy_kelp > 0,1,0)
  ) %>%
  filter(is.na(zone.x) == FALSE) %>%
  ggplot(aes(x=canopy_kelp,y = YTBLyoy_occur, color = as.factor(site.x))) +
  #geom_jitter(aes(y = YTBLyoy_occur, color = as.factor(site.x))) +
  stat_summary_bin(geom = "point", fun = mean, aes(y = YTBLyoy_occur)) +
  facet_grid(rows = vars(zone.x)) +
  scale_color_viridis(discrete = T, name = "")  +
  xlab("Canopy Kelp Density") +
  ylab("Black and Yellowtail YOY") +
  theme_classic()

# this code generally works, so now write a for loop to do it for all combinations of the things we're interested in

# YOYs: SECAyoy, SEMAyoy, SEMYyoy, SENEyoy, SEPIyoy, YTBLyoy
# black rocks: SEME
# kelps: MACPYR, NERLUE, PTECAL, canopy_kelp, kelp, ner_pte
# consider whether we want to plot all of the fish abundances, occurrence only, positive data only
