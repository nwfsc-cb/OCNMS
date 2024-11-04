# Urchin timeseries
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(magrittr)
library(here)
library(viridis)

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())
theme_set(plot_theme)

dat <- read_rds(here('Flagstone paper','Data','invert_swath_zonelevel.rds'))
glimpse(dat)

urch <- dat %>% filter(lump %in% c('purple_urchin','red_urchin')) %>% 
  filter(!(site %in%c("Anderson Point","Chibadehl Rocks","Point of the Arches","Rock 305","Teahwhit Head"))) %>% 
  mutate(site=factor(site,levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay")))

urch_ts <- urch %>%
  mutate(upper=dens_weighted+se_w,lower=dens_weighted-se_w) %>% 
  mutate(across(all_of(c("dens_weighted","upper","lower")),~pmax(.x,0))) %>% 
  mutate(across(all_of(c("dens_weighted","upper","lower")),~log(.x+1))) %>% 
  ggplot(aes(year,dens_weighted,
             ymin=lower,ymax=upper,
             col=lump))+
  geom_pointrange(position=position_dodge(width=0.5))+geom_line(position=position_dodge(width=0.5))+
  geom_hline(yintercept=0)+
  facet_grid(zone~site)+
  scale_color_manual(values=viridis_pal()(5))+
  labs(x="Year",y="Log Density (count/sq. m + 1)",col="Species",
       title="")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5),
        panel.border = element_rect(fill=NA))

ggsave(here('Flagstone Paper','Plots',"log_density_urchin_ts.png"),urch_ts,w=8,h=6)
