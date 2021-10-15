# Consider relationships between rockfish YOYs and kelp
# Each species/group of fish vs each kelp species
# Each species/group of fish vs sum of understory kelp species
# Each species/group of fish vs sum of canopy kelp species

library(tidyverse)
library(here)
library(viridis)

# df from Nick, email sent 10-13-2021 0909. represent raw means for year x site x depth x area by taxa/species.
# I believe the fish data have been filtered to include only transects that meet min viz criteria (2.0m)
fish_kelp <- read_rds(here::here('Flagstone paper','Data','Data_Fish_x_Kelp.RDS'))
glimpse(fish_kelp)

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
  geom_point(aes(x=canopy_kelp, y = YTBLyoy, color = as.factor(zone.x), shape = as.factor(site.x))) +
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
