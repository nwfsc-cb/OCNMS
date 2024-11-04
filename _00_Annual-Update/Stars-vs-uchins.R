# starss vs urchins

# Set file locations ####

HomeFile = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper"
Fig_Loc = paste0(HomeFile,"/Plots/")
Data_Loc = paste0(HomeFile,"/Data/")
Results_Loc = paste0(HomeFile,"/Results/")
Other_Files = paste0(HomeFile,"/Other Files/")

# Load packages ###

library(tidyverse)
library(stringr)
library(RColorBrewer)
library(ggpubr) 
library(ggplot32)
library(readxl)

# Load background files ####

settings = readRDS( paste0(Data_Loc,'settings.RDS') )
theme_nt = readRDS( paste0(Data_Loc, 'theme_nt.rds') )
site.col = settings$site.col
sites = settings$sites
# bring in data

df = readRDS( paste0( Data_Loc, 'Data_Inverts_Kelp_transect_wide.rds'))
df$col = site.col$col[ match(df$site, site.col$site)]
df$urchins = df$`purple urchin`+df$`red urchin`+df$`green urchin`
# stars but not henricia

df$stars = df$`brood star`   + df$Pisaster + df$Pycnopodia + 
           df$`large star` + df$`leather star` + df$`medium star`

df$site = factor(df$site , levels = sites)

####

s_all <-  ggplot( df , aes(x = stars, y = urchins, color = site)) +
        geom_point() + 
        scale_color_manual(values = site.col$col)+
        theme_bw() + theme_nt +
        theme(legend.position = c(0.8, 0.8))

s_tat <-  ggplot( filter(df, site=="Tatoosh Island") , aes(x = stars, y = urchins, color = site)) +
      geom_point() + 
      scale_color_manual(values = site.col$col[2])+
      theme_bw() + theme_nt +
      theme(legend.position ='none')

s_des <-  ggplot( filter(df, site=="Destruction Island") , aes(x = stars, y = urchins, color =  site)) +
      geom_point() + 
      scale_color_manual(values = site.col$col[5])+
      theme_bw() + theme_nt +
      theme(legend.position = 'none')


pi_all <-  ggplot( df , aes(x = Pisaster, y = urchins, color = site)) +
     geom_point() + 
     scale_color_manual(values = site.col$col)+
     theme_bw() + theme_nt +
     theme(legend.position = 'none')

py_all <-  ggplot( df , aes(x = Pycnopodia, y = urchins, color = site)) +
  geom_point() + 
  scale_color_manual(values = site.col$col)+
  theme_bw() + theme_nt +
  theme(legend.position = 'none')

pi_tat <-  ggplot( filter(df, site=="Tatoosh Island"), aes(x = Pisaster, y = urchins, color = site)) +
     geom_point()+
     scale_color_manual(values = site.col$col[2])+
     theme_bw() + theme_nt +
     theme(legend.position = 'none')

pi_des <-  ggplot( filter(df, site=="Destruction Island") , aes(x = Pisaster, y = urchins, col=site) )+
     geom_point() + 
     scale_color_manual(values = site.col$col[5])+
     theme_bw() + theme_nt +
     theme(legend.position = 'none' )

py_tat <-  ggplot( filter(df, site=="Tatoosh Island"), aes(x =Pycnopodia, y = urchins, color = site)) +
  geom_point()+
  scale_color_manual(values = site.col$col[2])+
  theme_bw() + theme_nt +
  theme(legend.position = 'none')

py_des <-  ggplot( filter(df, site=="Destruction Island") , aes(x = Pycnopodia, y = urchins, col=site) )+
  geom_point() + 
  scale_color_manual(values = site.col$col[5])+
  theme_bw() + theme_nt +
  theme(legend.position = 'none' )

graphics.off() 

pdf( paste0(Fig_Loc, "Stars-vs-Urchins.pdf"), height=6, width = 6)

ggarrange( s_all, pi_all, py_all,
           s_tat, pi_tat, py_tat,
           s_des, pi_des, py_des,
           nrow = 3, ncol=3,
           align='v',
           labels = c('a)','b)','c)','d)', 'e)', 'f)'), 
           # labels = 'auto',
           font.label = list(face='plain', size = 10),
           hjust = c(rep(-5,5),-7), vjust = 2 )
dev.off()


dfx = df %>% filter(site == 'Tatoosh Island')

m1 = lm(log(urchins+0.01) ~ Pisaster, data = dfx)
summary(m1)
 
 