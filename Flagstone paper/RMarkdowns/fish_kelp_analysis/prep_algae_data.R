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
  rename(transect.area.algae = Transect.area, count = Count) %>%
  # mutate(
  #   transect.area.weight = transect.area/max(transect.area)
  # ) %>% # we do not want weights here at the transect level, we want them at the area level
  select(
    year, site, area, zone, transect, transect.area.algae, observer, # transect.area.weight, 
    group, fun_gr, common.name, species, count 
    ) %>%
  mutate(
    density = count/transect.area.algae
  )
glimpse(dat.algae1)

# complete data frame with NAs or zeroes as needed. 
# as of 2021, this only adds algae to 1 transect at DI in 2015 when we only counted macro, nereo, and ptery.
dat.algae2 <- dat.algae1 %>%
  complete(species,
           nesting(year, site, area, zone, transect, transect.area.algae),
           fill=list(fun_gr = "COMPLETED",
                     count = NA,
                     density = NA)
  )
glimpse(dat.algae2)

# make sure everything looks ok. it does
dat.algae2 %>%
  tabyl(site, area, year)


write_rds(dat.algae2, here::here('Flagstone paper','Data','algae_2015_2021_year_site_area_zone.rds'))
