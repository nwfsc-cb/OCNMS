# Examine overflight data from WADNR

library(tidyverse)
library(ggplot2)
# data.dir <- "/Users/ole.shelton/Github/OCNMS/Flagstone paper/Data"
# setwd(data.dir)

setwd('..')
HomeFile = getwd()
Data_Loc = paste0(HomeFile,"/Data/")
Fig_Loc = paste0(HomeFile,"/Plots/")
kelp.dat <- read.csv(paste0(Data_Loc,"WADNR_summary_kelp_stats_1989_2020.csv") )

sites = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island")
col = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2,4,6, 10,12)]
site.col = data.frame(cbind(sites,col))
colnames(site.col) <- c('site', 'col')

# These are the relevant columns of interest
# tot_ne_can	Total Nereo canopy area ha 
# tot_ma_can  Total Macro canopy area ha
# tot_can     Total canopy area (all species) ha 

# tot_ne_pl   Total Nereo extent ha (planimeter)
# tot_ma_pl   Total Macro extent ha  (planimeter)
# tot_pl      Total Canopy extent ha  (planimeter)

# Planimeter area maps were created by establishing a computer 
#  derived perimeter polygon
# around the kelp within each species class corresponsing 
# to the qualitative planimeter area occupied by each

# Focal areas for Outer coast are (map_index):
# 15.1 (just east of Neah Bay ) to 25.1 (just south of Destruction Is.)
# our sites are: 
# Neah Bay = 16.2
# Tatoosh Island = 16.3
# Cape Alava = 19.1
# Cape Johnson = 21.1
# Destruction Island = 25.2

loc <- c("Neah Bay", 16.2,
  "Tatoosh Island" , 16.3,
  "Cape Alava" , 19.1,
  "Cape Johnson", 21.1,
  "Destruction Island", 25.2)

sites <- matrix(loc,ncol=2,byrow=T) %>% as.data.frame()
colnames(sites) = c("site","map_index")

## Filter the data to only include the focal sites

kelp.coast <- kelp.dat %>% filter(map_index >= 15) %>% group_by(year=year_) %>%
                  summarise(tot_canopy = sum(tot_can),
                            tot_ne = sum(tot_ne_can),
                            tot_ma = sum(tot_ma_can),
                            tot_pl_canopy = sum(tot_pl),
                            tot_pl_ne = sum(tot_ne_pl),
                            tot_pl_ma = sum(tot_ma_pl)) %>% 
                  rename(tot_ne_can = tot_ne,
                          tot_ma_can = tot_ma,
                          tot_can = tot_canopy,   
                          tot_ne_pl = tot_pl_ne, 
                          tot_ma_pl = tot_pl_ma, 
                          tot_pl = tot_pl_canopy) %>% 
                  mutate(map_index = NA,site="Coastwide")
                  

kelp.focal <- kelp.dat %>% filter(map_index %in% sites$map_index) %>%
    dplyr::select(year=year_,
                  map_index,
                  tot_ne_can,
                  tot_ma_can,
                  tot_can,   
                  tot_ne_pl, 
                  tot_ma_pl, 
                  tot_pl  ) %>% 
   left_join(.,sites %>% mutate(map_index=as.numeric(as.character(map_index))))

kelp.all <- full_join(kelp.coast,kelp.focal)
kelp.all[kelp.all<0] <- NA


kelp.all$site <- factor(kelp.all$site, levels=
                          c("Coastwide",
                          "Neah Bay" ,
                          "Tatoosh Island" , 
                          "Cape Alava" , 
                          "Cape Johnson",
                          "Destruction Island"))

                          
kelp.can.long <- kelp.all %>% pivot_longer(.,cols=contains("can"),
                                     names_to="can_type",values_to="canopy_ha") %>%
                  dplyr::select(-tot_pl, -tot_ne_pl, -tot_ma_pl) %>%
                  mutate(species=NA,
                         species=ifelse(grepl("ne",can_type),"Nereocystis",species),
                         species=ifelse(grepl("ma",can_type),"Macrocystis",species),
                         species=ifelse(is.na(species),"Total",species)
                         )

kelp.can.long$species <- factor(kelp.can.long$species, levels=
                                  c("Total","Macrocystis", "Nereocystis"))        

kelp.pl.long <-  kelp.all %>% pivot_longer(.,cols=contains("pl"),
                                     names_to="pl_type",values_to="pl_ha") %>%
                  dplyr::select(-tot_can, -tot_ne_can, -tot_ma_can) %>%
                  mutate(species=NA,
                      species=ifelse(grepl("ne",pl_type),"Nereocystis",species),
                      species=ifelse(grepl("ma",pl_type),"Macrocystis",species),
                      species=ifelse(is.na(species),"Total",species)
                      )

kelp.pl.long$species <- factor(kelp.pl.long$species, levels=
                              c("Total","Macrocystis", "Nereocystis"))
                  
                  



 theme_nt   <-   theme(legend.title=element_blank(),
                   legend.background = element_blank(),
                   axis.title.y = element_text(size=8),
                   axis.text.x = element_text(size=8),
                   strip.background = element_blank(),
                   legend.text = element_text(size = 8),
                   legend.key.size = unit(0.5,'lines')
)


p.canopy <- ggplot(kelp.can.long,aes(y=canopy_ha,x=year,color=species)) +
    geom_point() +
    geom_line() +
    facet_wrap(~site,ncol=2,scale="free") +
    scale_y_continuous("Canopy area (ha)") +
    scale_color_manual(values = site.col$col[c(5,1,2,3)])+
    # scale_color_viridis_d(begin=0,end=0.75,option="plasma") +
    theme_bw()+ theme_nt 
p.canopy 
 
 

p.plan <- ggplot(kelp.pl.long,aes(y=pl_ha,x=year,color=species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site,ncol=2,scale="free") +
  scale_y_continuous("Planimeter area (ha)") +
  scale_color_manual(values = site.col$col[c(5,1,2,3)])+
  # scale_color_viridis_d(begin=0,end=0.75,option="plasma") +
  theme_bw()+ theme_nt



graphics.off()
jpeg(paste0( Fig_Loc,'Kelp-canopy-by-site-canopy-ha.jpg'), units='in', res=300, height=6, width = 6)
p.canopy
dev.off()

jpeg(paste0(Fig_Loc,'Kelp-canopy-by-site-planimeter-ha.jpg'), units='in', res=300, height=6, width = 6)
p.plan
dev.off()

saveRDS( kelp.can.long , paste0(Data_Loc, "Data_Kelp_Canopy_Long.rds") )

####







