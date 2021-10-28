# Consider relationships between rockfish YOYs and kelp
# Each species/group of fish vs each kelp species
# Each species/group of fish vs sum of understory kelp species
# Each species/group of fish vs sum of canopy kelp species

library(tidyverse)
library(here)
library(viridis)
library(janitor)

# df from Nick, email sent 10-21-2021 2301
fish_kelp <- read_rds(here::here('Flagstone paper','Data','Data_Fish_Kelp_area_wide.rds'))
glimpse(fish_kelp)

# a tabyl of site x area, split into a list by year
fish_kelp %>%
  tabyl(site, area, year) #hmm, there are some year-sites with no area designations (eg, 2015 DI)
fish_kelp %>%
  tabyl(site, zone, year) #hmm, there are some year-sites with no zone designations (eg, 2015 DI)

# jameal thinks these issues are a result of filters nick used in upstream code (eg, excluding site-years with bad viz)

sort(unique(fish_kelp$year))
sort(unique(fish_kelp$site)) #need to drop levels
sort(unique(fish_kelp$area)) 
sort(unique(fish_kelp$zone)) 

# do a bit of cleanup, make summed kelps and fish and kelp occurrence and conditional abundance columns

# YOYs: SEBYTyoy (black yellowtail), SECAyoy (copper), SEMAyoy (quill), SEMYyoy (blue), SENEyoy (china), SEPIyoy (canary)
# black rocks: SEME
# kelps: MACPYR, NERLUE, PTECAL, canopy_kelp, three_kelps, ner_pte, and diff_ner_pte

fish_kelp <- fish_kelp %>%
  mutate(
    site = droplevels(site),
    canopy_kelp = NERLUE + MACPYR,
    three_kelps = canopy_kelp + PTECAL,
    ner_pte = NERLUE + PTECAL,
    diff_ner_pte = NERLUE - PTECAL
  ) %>%
  select(
    year, site, area, zone,
    SEBYTyoy, SECAyoy, SEPIyoy, TOTyoy, SEME,
    MACPYR, NERLUE, PTECAL, canopy_kelp, three_kelps, ner_pte, diff_ner_pte
  ) %>%
  mutate(
    SEBYTyoy_pres = ifelse(SEBYTyoy > 0, 1, 0),
    SECAyoy_pres = ifelse(SECAyoy > 0, 1, 0),
    SEPIyoy_pres = ifelse(SEPIyoy > 0, 1, 0),
    TOTyoy_pres = ifelse(TOTyoy > 0, 1, 0),
    SEME_pres = ifelse(SEME > 0, 1, 0),
    MACPYR_pres = ifelse(MACPYR > 0, 1, 0), 
    NERLUE_pres = ifelse(NERLUE > 0, 1, 0), 
    PTECAL_pres = ifelse(PTECAL > 0, 1, 0),
    three_kelps_pres = ifelse(three_kelps > 0, 1, 0),
    canopy_kelp_pres = ifelse(canopy_kelp > 0, 1, 0),
    ner_pte_pres = ifelse(ner_pte > 0, 1, 0),
    SEBYTyoy_cond_abund = ifelse(SEBYTyoy > 0, SEBYTyoy, NA),
    SECAyoy_cond_abund = ifelse(SECAyoy > 0, SECAyoy, NA),
    SEPIyoy_cond_abund = ifelse(SEPIyoy > 0, SEPIyoy, NA),
    TOTyoy_cond_abund = ifelse(TOTyoy > 0, TOTyoy, NA),
    SEME_cond_abund = ifelse(SEME > 0, SEME, NA)
  )

# look at distributions of kelps
ggplot(data=fish_kelp) +
  geom_density(aes(NERLUE)) +
  theme_bw()
ggplot(data=fish_kelp) +
  geom_density(aes(MACPYR)) +
  theme_bw()
ggplot(data=fish_kelp) +
  geom_density(aes(PTECAL)) +
  theme_bw()
ggplot(data=fish_kelp) +
  geom_density(aes(three_kelps)) +
  facet_grid(rows = vars(zone)) +
  theme_bw()

### 1
# function for abundance and conditional abundance
bivariate_plot_abund <- function(kelp_sp, fish_sp, trans="none"){
  df <- fish_kelp %>% 
    filter(is.na(zone) == FALSE) %>% 
    select(year, site, area, zone, kelp_sp, fish_sp)
  
  # if(trans=="log"){
  #   df %<>%
  #     mutate(across(all_of(c("dens","upper","lower")),~pmax(.x,0))) %>% 
  #     mutate(across(all_of(c("dens","upper","lower")),~log(.x+1)))
  # }
  
  xl <- ifelse(trans=="log","Log Density (count/sq. m + 1)","Kelp Density (count/sq. m)")
  yl <- ifelse(trans=="log","Log Density (count/sq. m + 1)","YOY Density (count/sq. m)")
  
  pl <- df %>% 
    ggplot(aes(x=.data[[kelp_sp]], y = .data[[fish_sp]], color = as.factor(site)))+
    geom_point() +
    geom_smooth(aes(x=.data[[kelp_sp]],y = .data[[fish_sp]], group=1)) +
    facet_grid(rows = vars(zone)) + #nrow=1
    #scale_x_log10() +
    #scale_y_log10() +
    scale_color_viridis(discrete = T, name = "")  +
    labs(x=xl,y=yl,col="Site",
         title=paste0(fish_sp," as a function of\n",kelp_sp)) +
    theme_classic()
  print(pl)
  ggsave(here::here('Flagstone paper', 
                    'Plots', 
                    'explore_fish_kelp', 
                    paste0(fish_sp," as a function of ",kelp_sp,".pdf")))
  

}
# test function
bivariate_plot_abund(kelp_sp = "PTECAL",fish_sp = "SEBYTyoy", trans="none")

# it works!
# loop over all desired fish and kelp species
abund_combos <- crossing(
  kelps = names(fish_kelp[,c(10:16)]),
  fishes = names(fish_kelp[,c(5:9, 28:32)])
  ) # kelps 22:27 are kelp occurrence --> not useful

purrr::walk2(.x = abund_combos$kelps, .y = abund_combos$fishes, .f=bivariate_plot_abund, trans="none")

### 2
# function for occurrence
# # https://www.ericrscott.com/post/plot-logistic-regressions/
bivariate_plot_occur <- function(kelp_sp, fish_sp, trans="none"){
  df <- fish_kelp %>% 
    filter(is.na(zone) == FALSE) %>% 
    select(year, site, area, zone, kelp_sp, fish_sp)
  
  # if(trans=="log"){
  #   df %<>%
  #     mutate(across(all_of(c("dens","upper","lower")),~pmax(.x,0))) %>% 
  #     mutate(across(all_of(c("dens","upper","lower")),~log(.x+1)))
  # }
  
  # m <- glm(fish_sp ~ kelp_sp, family = binomial, data = df)
  # plot_df <- broom::augment(m, type.predict = "response")
  # 
  # base <-
  #   ggplot(plot_df, aes(x = .data[[kelp_sp]])) +
  #   geom_line(aes(y = .fitted), color = "blue") +
  #   theme_classic()
  
  xl <- ifelse(trans=="log","Log Density (count/sq. m + 1)","Kelp Density (count/sq. m)")
  yl <- ifelse(trans=="log","Log Density (count/sq. m + 1)","YOY Probability of Occurrence")
  
  pl <- df %>% 
    filter(is.na(zone) == FALSE) %>%
    ggplot(aes(x=.data[[kelp_sp]],y = .data[[fish_sp]], color = as.factor(site))) +
    stat_summary_bin(geom = "point", fun = mean, aes(y = .data[[fish_sp]])) + # , bins = 60
    geom_smooth(aes(x=.data[[kelp_sp]],y = .data[[fish_sp]], group=1),
                method = "glm", 
                method.args = list(family = "binomial")) +
    facet_grid(rows = vars(zone)) +
    scale_color_viridis(discrete = T, name = "")  +
    labs(x=xl,y=yl,col="Site",
         title=paste0(fish_sp," occurrence as a function of\n",kelp_sp, " with fitted logistic regression")) +
    theme_classic() #+ 
    #base
  
  print(pl)
  
  ggsave(here::here('Flagstone paper', 
                    'Plots', 
                    'explore_fish_kelp', 
                    paste0(fish_sp," occurrence as a function of ",kelp_sp,".pdf")))
  
  
}
# test function
bivariate_plot_occur(kelp_sp = "PTECAL",fish_sp = "SEBYTyoy_pres", trans="none")

# it works!
# loop over all desired fish and kelp species
occur_combos <- crossing(kelps = names(fish_kelp[,c(10:16)]),
                         fishes = names(fish_kelp[,c(17:21)])) # 22:27 are kelp presence, but boring
# expand.grid(kelps = names(fish_kelp[,c(10:16, 22:27)]),
#                             fishes = names(fish_kelp[,c(17:21)]))

purrr::walk2(.x = occur_combos$kelps, .y = occur_combos$fishes, .f=bivariate_plot_occur, trans="none")

### 3
# Analysis and plot of Total YOY occurrence vs 3 kelps

m_occur <- glm( TOTyoy_pres ~ three_kelps + zone, 
          family = binomial, 
          data = fish_kelp %>% 
            filter(is.na(zone) == FALSE)
          )
summary(m_occur)
plot_df <- broom::augment(m_occur, type.predict = "response")

base <-
  ggplot(plot_df, aes(x = three_kelps, color = factor(zone))) +
  geom_line(aes(y = .fitted), color = "blue") +
  theme_classic()

base + geom_point(aes(y = TOTyoy_pres), alpha = 0.2)
base + stat_summary_bin(geom = "point", fun = mean, aes(y = TOTyoy_pres), bins=60) 

p1 <- fish_kelp %>% 
  filter(is.na(zone) == FALSE) %>%
  ggplot(aes(x=three_kelps,y = TOTyoy_pres, color = as.factor(site))) +
  stat_summary_bin(geom = "point", fun = mean, aes(y = TOTyoy_pres)) + # , bins = 60
  geom_smooth(aes(x=three_kelps,y = TOTyoy_pres, group=1),
              method = "glm", 
              method.args = list(family = "binomial")) +
  scale_color_viridis(discrete = T, name = "")  +
  labs(x="Density of 3 kelps",y="Probability of occurrence",col="Site",
       title= "Total YOY rockfish occurrence as a function of 3 kelps,\nwith fitted logistic regression") +
  theme_classic() 

print(p1)

ggsave(here::here('Flagstone paper', 
                  'Plots', 
                  'Total YOY rockfish occurrence as a function of 3 kelps, with fitted logistic regression.pdf'))

### 4
# Analysis and plot of Total YOY conditional abundance vs 3 kelps

m_condabund <- glm( TOTyoy_cond_abund ~ three_kelps + zone, 
                    family=gaussian(link=log), 
          data = fish_kelp %>% 
            filter(is.na(zone) == FALSE)
)
summary(m_condabund)
plot_df2 <- broom::augment(m_condabund, type.predict = "response")

base <-
  ggplot(plot_df2, aes(x = three_kelps, color = factor(zone))) +
  geom_line(aes(y = .fitted), color = "blue") +
  theme_classic()

base + geom_point(aes(y = TOTyoy_cond_abund), alpha = 0.2)

# this doesn't plot correctly te
p2 <- fish_kelp %>% 
  filter(is.na(zone) == FALSE) %>%
  ggplot(aes(x = three_kelps, y = TOTyoy_cond_abund, color = as.factor(site))) +
  #geom_smooth(aes(x=three_kelps,y = TOTyoy_cond_abund, group=1)) +
  scale_color_viridis(discrete = T, name = "")  +
  labs(x="Density of 3 kelps",y="Conditional density",col="Site",
       title= "Total YOY rockfish conditional density as a function of 3 kelps") +
  theme_classic() 

print(p2)

ggsave(here::here('Flagstone paper', 
                  'Plots', 
                  'Total YOY rockfish conditional density as a function of 3 kelps.pdf'))

######################################################

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
