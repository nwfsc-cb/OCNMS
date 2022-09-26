rm(list = ls())

# Paths for  ####
source("R-functions-ocnms.r")
HomeFile = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/"
Fig_Loc = paste0(HomeFile,"/Plots/")
Data_Loc = paste0(HomeFile,"/Data/")
Results_Loc = paste0(HomeFile,"/Results/")
Other_Files = paste0(HomeFile,"/Other Files/")
par(ps=10, cex=1)

# load packages ####
library(tidyverse)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(panelr)
library(lme4)

#settings ####
settings = readRDS(paste0(Data_Loc,'settings.rds') )
min.vis = as.numeric(settings['min.vis'])
years = as.numeric(unlist(settings['years']))
site.col = data.frame(settings['site.col'])
colnames(site.col) = c('site','col')
sites = site.col$site
year.pch = data.frame(settings['year.pch'])
colnames(year.pch) = c('year','pch','col')

# Some csv files for naming later

swath_codes = data.frame(read.csv( paste0(Data_Loc,"spp_codes_swath.csv") ))
kelp_codes = data.frame(read.csv( paste0(Data_Loc,"spp_codes_kelp.csv") ))

theme_nt = readRDS(paste0(Data_Loc, 'theme_nt.rds') )

#### bring in data and do some stuff ####

df_transect1 = readRDS(paste0(Data_Loc,'Data_Inverts_Kelp_transect_wide.rds')) %>% 
     mutate(Other = c(AGAFIM+ALAMAR+COSCOS+CYMTRI+DESSPP+LAMSET+PLEGAR+SACGRO+SACLAT)) %>%
     filter(site %in% sites) 

df_transect2 = df_transect1[,c('site','year','transect','area','zone', 
                               'NERLUE','MACPYR', 'PTECAL','Other',
                               'purple urchin','red urchin','green urchin')]

df_transect =  df_transect2 %>%
     pivot_longer(NERLUE:Other , names_to = 'kelp_spp', values_to = 'kelp_density') %>%
     mutate(kelp_spp = factor(kelp_spp, levels=c('MACPYR','NERLUE','PTECAL','Other'))) %>%
     pivot_longer('purple urchin':'green urchin' , 
                  names_to = 'urchin_spp', values_to = 'urchin_density')
df_transect$kelp_spp <- as.character(df_transect$kelp_spp)

df_transect$kelp_spp[ df_transect$kelp_spp == 'NERLUE'] <- 'Nereocystis'
df_transect$kelp_spp[ df_transect$kelp_spp == 'MACPYR'] <- 'Macrocystis'
df_transect$kelp_spp[ df_transect$kelp_spp == 'PTECAL'] <- 'Pterygophora'

df_transect$kelp_spp = factor(df_transect$kelp_spp , levels = 
                                   c('Macrocystis','Nereocystis','Pterygophora','Other'))

df_transect$urchin_spp <- as.character(df_transect$urchin_spp)

df_transect$urchin_spp[df_transect$urchin_spp=='purple urchin'] <- 'Purple urchins'
df_transect$urchin_spp[df_transect$urchin_spp=='red urchin'] <- 'Red urchins'
df_transect$urchin_spp[df_transect$urchin_spp=='green urchin'] <- 'Green urchins'

df_transect$urchin_spp = factor(df_transect$urchin_spp , levels=c('Purple urchins','Red urchins','Green urchins'))

# some organization ####

df_kelp <- df_transect %>% rename(depth = zone) %>% 
     select(year,site,area,depth,transect,kelp_spp, kelp_density) %>%
     # the way the file is set up, there are multiple kelp obs 
     # and multiple urchin obs...take mean
     # including transect just keeps it as a cat var
     # doesn't actually do any math for real
     group_by(year,site,area,depth,transect,kelp_spp) %>%
     summarise(kelp_density = mean(kelp_density)) %>%
     pivot_wider(id_cols = c(year,site,area,depth,transect), 
                 names_from = kelp_spp,
                 values_from = kelp_density)

df_urchin <- df_transect %>% rename(depth = zone) %>% 
     select(year,site,area,depth,transect, urchin_spp, urchin_density) %>%
     # the way the file is set up, there are multiple kelp obs 
     # and multiple urchin obs...take mean
     # including transect just keeps it as a cat var
     # doesn't actually do any math for real
     group_by(year,site,area,depth,transect,urchin_spp) %>%
     summarise(urchin_density = mean(urchin_density)) %>%
     pivot_wider(id_cols = c(year,site,area,depth,transect), 
                 names_from = urchin_spp,
                 values_from = urchin_density)

df = full_join(df_kelp,df_urchin) %>% 
     mutate(id_i = paste(site,area,depth,transect, sep="-"),
            id_a = paste(site,area,depth, sep="-"),
            urchins = `Purple urchins` + `Red urchins` + `Green urchins`)

######## REWB calculations

# B = between
# W = within
# a = area, d = day, y = year, t = transect

df1 = df %>% group_by(site, depth, area) %>% 
              mutate(B_da = mean(urchins, rm.na=TRUE)) %>%
             group_by(site, depth, area, year) %>% 
              mutate(B_day = mean(urchins, rm.na=TRUE)) %>%
              mutate(W_day = B_day - B_da,
                     W_daty = urchins - B_day) 
             # ungroup %>%
             # mutate_at(vars(begins_with("B_"),depth, area, year),
             #           funs(c = . - mean(.)))


df_rewb = df1 %>% filter(year!=2015,
                        site == 'Tatoosh Island')

m_nereo = lmer( log1p(Nereocystis) ~ depth + area + year + 
             B_da  + W_day + B_day + W_daty +
             (1 + W_day + W_daty | id_i),
             data = df_rewb)
m_nereo
summary(m_nereo)
anova(m_nereo) 


m_ptery = lmer( log1p(Pterygophora) ~ depth + area + year + 
                     B_da  + W_day + B_day + W_daty +
                     (1 + W_day + W_daty | id_i),
                data = df_rewb)
m_ptery
summary(m_ptery)
anova(m_ptery) 

