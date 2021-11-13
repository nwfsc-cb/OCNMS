# prep algae data

library(tidyverse)
library(here)
library(janitor)

# read in data and lookup keys
swath <- read_rds(here::here('Flagstone paper','Data','Swath_2015-2021.rds'))
glimpse(swath)

kelp_codes = data.frame(read.csv( here::here('Flagstone paper','Data',"spp_codes_kelp.csv") ))

# Nick made kelp_codes. Jameal is changing them so that only Macro and Nereo are canopy
kelp_codes <- kelp_codes %>%
  rename(functional_group = functinal_group) %>%
  mutate(
    functional_group = ifelse(species == "EGRMEN" | species == "SACLAT", "understory", functional_group)
  )

sites = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island")

swath1 <- swath %>% filter(site %in% sites)

swath1$site <- factor(swath1$site, levels=sites)

dat.algae   <- swath1 %>% filter(group=="Algae")
# functional groupings for algae
dat.algae$fun_gr = kelp_codes$functional_group[ match(dat.algae$species, kelp_codes$species) ]

# assign dummy area for 2015 data
dat.algae$area[dat.algae$year == 2015] <- "D"

dat.algae1 <- dat.algae %>%
  rename(transect.area = Transect.area, count = Count) %>%
  # mutate(
  #   transect.area.weight = transect.area/max(transect.area)
  # ) %>% # we do not want weights here at the transect level, we want them at the area level
  select(
    year, site, area, zone, transect, transect.area, observer, # transect.area.weight, 
    group, fun_gr, common.name, species, count 
    ) %>%
  mutate(
    density = count/transect.area
  )
glimpse(dat.algae1)

# make sure everything looks ok. it does
dat.algae1 %>%
  tabyl(site, area, year)


write_rds(dat.algae1, here::here('Flagstone paper','Data','algae_2015_2021_year_site_area_zone.rds'))
