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

# prepare REWB format ####

df_kelp <- df_transect %>% rename(depth = zone) %>% 
     select(year,site,area,depth,transect,kelp_spp, kelp_density) %>%
     # the way the file is set up, there are multiple kelp obs 
     # and multiple urchin obs...take mean
     group_by(year,site,area,depth,transect,kelp_spp) %>%
     summarise(kelp_density = mean(kelp_density)) %>%
     pivot_wider(id_cols = c(year,site,area,depth,transect), 
                 names_from = kelp_spp,
                 values_from = kelp_density)

df_urchin <- df_transect %>% rename(depth = zone) %>% 
     select(year,site,area,depth,transect, urchin_spp, urchin_density) %>%
     # the way the file is set up, there are multiple kelp obs 
     # and multiple urchin obs...take mean
     group_by(year,site,area,depth,transect,urchin_spp) %>%
     summarise(urchin_density = mean(urchin_density)) %>%
     pivot_wider(id_cols = c(year,site,area,depth,transect), 
                 names_from = urchin_spp,
                 values_from = urchin_density)

df = full_join(df_kelp,df_urchin) %>% 
     mutate(ID = paste(site,area,depth,transect, sep="-"),
            urchins = `Purple urchins` + `Red urchins` + `Green urchins`,
            DEPTH = ifelse(depth==5,0,1),
            YEAR = year-2015)

# make a numeric i (id) ######
idx = data.frame(X =levels(as.factor(df$ID)))
idx$Y = 1:nrow(idx)
df$id = idx$Y[match(df$ID,idx$X)]

# tatoosh and destruction ####
# both sites have Nereo and Urchins

df1 = df %>% filter(site %in% c('Tatoosh Island','Destruction Island') )
sx = data.frame(X = levels(as.factor(as.character(df1$site))))
sx$Y = 0:1
df1$SITE = sx$Y[ match(df1$site,sx$X)]

# Just messing around with models....

# panelr NEREO REWB MODEL #### 
df_nero = df1[,c('YEAR',"DEPTH",'SITE','Nereocystis',"urchins","id")]
dfx <- panel_data(df_nero, id = id, wave = YEAR)
m_nereo = wbm( log1p(Nereocystis) ~ urchins | DEPTH  | urchins*SITE +  urchins*DEPTH + (1|id) , use.wave = TRUE, data = dfx )
summary(m_nereo)
# plot(mod1)


# panelr NEREO REWB MODEL #### 
df_pter = df1[,c('YEAR',"DEPTH",'SITE','Pterygophora',"urchins","id")]
dfz <- panel_data(df_pter, id = id, wave = YEAR)
m_pter = wbm( log1p(Pterygophora) ~ urchins | DEPTH + SITE |  urchins*SITE +  (1|id) , use.wave = TRUE, data = dfz )
summary(m_pter)
# plot(mod1)



# plot x_it-x_i ####

df_b = df1 %>% group_by(id, SITE, DEPTH) %>%
       summarise(nereo = mean(log1p(Nereocystis)),
                 pter = mean(log1p(Pterygophora)),
                 urchins = mean(urchins))

p1 = ggplot(df_b, aes(x = urchins, y = nereo, colour = as.factor(DEPTH) ) ) +
     geom_point()

p1




