# Analyze relationships between rockfish YOY and kelp
rm(list = ls())
# libraries ####

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(vegan)
library(MASS)
library(lme4)
library(here)
library(janitor)
source("R-functions-ocnms.r")

# devtools::install_github("AckerDWM/gg3D")
# library(gg3D)

# Set parameters; import information  ####

HomeFile = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper"

Fig_Loc = paste0(HomeFile,"/Plots/")
Data_Loc = paste0(HomeFile,"/Data/")
Results_Loc = paste0(HomeFile,"/Results/")
other1_Files = paste0(HomeFile,"/other1 Files/")
theme_nt = readRDS(paste0(Data_Loc, 'theme_nt.rds') )

settings = readRDS(paste0(Data_Loc,'settings.rds') )

min.vis = as.numeric(settings['min.vis'])
years = as.numeric(unlist(settings['years']))
site.col = data.frame(settings['site.col'])
colnames(site.col) = c('site','col')
sites = site.col$site
year.pch = data.frame(settings['year.pch'])
colnames(year.pch) = c('year','pch','col')

fish_codes = data.frame(read.csv( paste0(Data_Loc,"spp_codes_fish.csv") ))
swath_codes = data.frame(read.csv( paste0(Data_Loc,"spp_codes_swath.csv") ))
kelp_codes = data.frame(read.csv( paste0(Data_Loc,"spp_codes_kelp.csv") ))

# Bring in data. Produced in PlotUnivariate. RMD ####

# MEAN DENSITY ####
df_dens = readRDS( paste0(Data_Loc,"Data_Fish_Kelp_area_wide.rds"))
df_dens <- df_dens %>% rename(Macro= MACPYR , Nereo=NERLUE, Ptery=PTECAL) %>%
        mutate(other1 = sum(AGAFIM+ALAMAR+COSCOS+CYMTRI+DESSPP+LAMSET+SACGRO+SACLAT+PLEGAR ))
df_dens$area[df_dens$year==2015] <- "D"

# TOTALCOUNT ####
df_count <- readRDS(paste0(Data_Loc,"Fish-kelp-counts-wide.rds"))
df_count <- df_count %>% 
        rename(Macro= MACPYR , Nereo=NERLUE, Ptery=PTECAL) %>%
        mutate(fish_volume = fish_area*2,
               other1 = sum(AGAFIM+ALAMAR+COSCOS+CYMTRI+DESSPP+LAMSET+SACGRO+SACLAT+PLEGAR ))


# add the area informatinto the df_dens file
# has area data for weighting

# dfx = df_count[,c('site','zone','area','year','fish_area','kelp_area')]
df_dens <- left_join(df_dens , df_count[,c('site','zone','area','year','fish_area','kelp_area','fish_volume')])

######### Quick plots #######

df_dens$site <- factor(df_dens$site, levels=settings$sites)

macro1 <- ggplot( df_dens , aes(x = Macro, y = TOTyoy, color=site)) +
     geom_point() + 
     xlab( expression(paste(italic('Macro'), ' stipes per ', m^2)) )+
     ylab( expression(paste('Rockfish YOY per 60 ',m^2) )) + 
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt

nereo1 <- ggplot( df_dens , aes(x = Nereo, y = TOTyoy, color=site)) +
     geom_point() + 
     xlab( expression(paste(italic('Nero'), ' stipes per ', m^2)) )+
     ylab( expression(paste('Rockfish YOY per 60 ',m^2) )) + 
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt

ptery1 <- ggplot( df_dens , aes(x = Ptery, y = TOTyoy, color=site)) +
     geom_point() + 
     xlab( expression(paste(italic('Ptery'), ' stipes per ', m^2)) )+
     ylab( expression(paste('Rockfish YOY per 60 ',m^2) )) + 
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt

under1 <- ggplot( df_dens , aes(x = other1, y = TOTyoy, color=site)) +
        geom_point() + 
        xlab( expression(paste(italic('other1'), ' stipes per ', m^2)) )+
        ylab( expression(paste('Rockfish YOY per 60 ',m^2) )) + 
        scale_color_manual(values = site.col$col) +
        theme_bw() + theme_nt

macro1 <- macro1 + theme(legend.position = c(0.75,0.8))
nereo1 <- nereo1 + theme(legend.position = 'none')
ptery1 <- ptery1 + theme(legend.position = 'none')
under1 <-under1 + theme(legend.position = 'none')
     
par(ps = 10, cex =1)

graphics.off()

png(paste0(Fig_Loc,"Univariate-YOY-v-Kelp.simple.png"), units = 'in', res=300, height=5, width=3)
ggarrange(macro1, nereo1, ptery1, under1,
          labels = c('a)','b)','c)', 'd)'), 
          font.label = list(face='plain', size = 10),
          hjust = -5.5,
          vjust = 2.5,
          align = 'v',
          nrow = 4, ncol=1)
dev.off()


#################### Stats #######################

# Delta-GLM ####

# chose file to use for delta-glm analyses (counts or density for fish)

dfx <- df_dens

dfx <- dfx %>% mutate(TOTyoy_pres = ifelse(TOTyoy > 0 , 1, 0)) %>%
               mutate(canopy_kelp = Macro+Nereo,
                    other2 = Ptery+other1,
                    all_kelps = Macro+Nereo+Ptery+other1,
                    transect.area.algae.weight = kelp_area/max(kelp_area),
                    fish.area.weight = fish_area/max(fish_area),
                    n = 1)
dfx$year_factor = factor(dfx$year)



sum(dfx$TOTyoy_pres)
sum(dfx$n)

sum(dfx$TOTyoy_pres)/sum(dfx$n)

###########################################################################
######## Begin occurrence models ##########################################
###########################################################################

# sum of all kelp
m_all <- glmer( TOTyoy_pres ~  all_kelps + (1|year_factor) + (1|site), 
                family = binomial, 
                weights = fish.area.weight,
                na.action = 'na.fail',
                data = dfx)

# canopy vs ptery vs under
m_CPO <- glmer( TOTyoy_pres ~  canopy_kelp + Ptery + other1 + (1|year_factor) + (1|site), 
               family = binomial, 
               weights = fish.area.weight,
               na.action = 'na.fail',
               data = dfx)
# kelp by species plus other1
m_MNPO <- glmer( TOTyoy_pres ~  Macro+Nereo+Ptery+other1 + (1|year_factor) + (1|site), 
                family = binomial, 
                weights = fish.area.weight,
                na.action = 'na.fail',
                data = dfx)


# run model selection
library(MuMIn)
dd_all  <- MuMIn::dredge(m_all)
dd_MNPO <- MuMIn::dredge(m_MNPO)
dd_CPO <- MuMIn::dredge(m_CPO)

dd = dd_all %>% full_join(.,dd_MNPO) %>% full_join(.,dd_CPO)

dd = dd[, c("(Intercept)","all_kelps", 
            "canopy_kelp",
            "Macro","Nereo","Ptery",
            "other1",
            "df","AICc","delta")]


dd$delta = dd$AICc - min(dd$AICc)
dd_occur = dd[ order(dd$delta) , ]

dd_occur$n = 1
# delete doubles
for(i in 2:nrow(dd_occur)){
        dd_occur$n[i] = ifelse(dd_occur$delta[i] == dd_occur$delta[i-1],2,1 )
}
dd_occur = dd_occur %>% filter(n!=2)

head(dd_occur)
write.csv(dd_occur, paste0(Fig_Loc,"Table.S10.Fish-YOY-Kelp-binomial-AICc-table.csv"))


m_MN <- glmer( TOTyoy_pres ~  Macro+Nereo+ (1|year_factor) + (1|site), 
                 family = binomial, 
                 weights = fish.area.weight,
                 na.action = 'na.fail',
                 data = dfx)

capture.output(summary(m_MN), file = paste(Results_Loc,"Model-MN-occurence.txt"))

m_canopy <- glmer( TOTyoy_pres ~ canopy_kelp + (1|year_factor) + (1|site), 
               family = binomial, 
               weights = fish.area.weight,
               na.action = 'na.fail',
               data = dfx)

capture.output(summary(m_canopy), file = paste(Results_Loc,"Model-canopy-occurence.txt"))


##############################################################
###### end occurrence models #################################
##############################################################

###### plot best occurrence model ############################
pred_occur <- predict(m_canopy , type = "response")

dfx$p_occur <- pred_occur

dfx$pch = ifelse(dfx$zone==5,21,19)

# plot occurence
plot_occur_1 <-
     ggplot( dfx , aes(x=canopy_kelp,y=p_occur, color = site))+
     geom_point(pch = 19)+ # dfx$pch)+
     xlab( expression(paste('Canopy kelp stipes per ', m^2)) )+
     geom_point(aes(x=canopy_kelp, y = TOTyoy_pres), color='black') + 
     ylab('Probability of occurence') + 
     geom_smooth(formula = y ~ x,
                 aes(x=canopy_kelp,y = TOTyoy_pres, group=1),
                 method = "glm", 
                 method.args = list(family = "binomial")) +
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt

plot_occur_1 + theme(legend.position = c(0.8,0.3),
                   legend.box.background = element_blank(),
                   legend.background = element_blank())

graphics.off()
png( paste0(Fig_Loc, "FishYOY-canopy-Kelp-occurence.png"), units = 'in',res=300, height=3.5 ,width = 3.5)
plot_occur_1 + theme(legend.position = c(0.8,0.3),
                   legend.box.background = element_blank(),
                   legend.background = element_blank())
dev.off()



#####################################################################
#### begin abundance models ###############################################
#####################################################################

dfa = dfx[dfx$TOTyoy_pres == 1,]

dfa$TOTyoy  = log(dfa$TOTyoy)

a_all  <- lmer( TOTyoy ~  all_kelps  + (1|year_factor) + (1|site), 
              weights = fish.area.weight,
              na.action = 'na.fail',
              data = dfa)
a_CPO  <- lmer( TOTyoy ~  canopy_kelp + Ptery + other1 + (1|year_factor) + (1|site), 
               weights = fish.area.weight,
               na.action = 'na.fail',
               data = dfa)
a_MNPO <- lmer( TOTyoy ~  Macro+Nereo+Ptery+other1 + (1|year_factor) + (1|site), 
                weights = fish.area.weight,
                 na.action = 'na.fail',
                 data = dfa)


# run model selection
library(MuMIn)
dd1 <- MuMIn::dredge(a_all)
dd2 <- MuMIn::dredge(a_CPO)
dd3 <- MuMIn::dredge(a_MNPO)



dd = dd1 %>% full_join(.,dd2) %>% full_join(.,dd3)

dd = dd[, c("(Intercept)","all_kelps", 
            "canopy_kelp",
            "Macro","Nereo","Ptery",
            "other1",
            "df","AICc","delta")]

dd$delta = dd$AICc - min(dd$AICc)
dd_abund = dd[ order(dd$delta) , ]

dd_abund$n = 1
# delete doubles
for(i in 2:nrow(dd_abund)){
        dd_abund$n[i] = ifelse(dd_abund$delta[i] == dd_abund$delta[i-1],2,1 )
}

dd_abund = dd_abund %>% filter(n!=2)
head(dd_abund)


write.csv(dd_abund, paste0(Fig_Loc,"Table.S.12.Fish-Kelp-Abundance-AICc.csv"))

a_base <- lmer( TOTyoy ~ (1|year_factor) + (1|site), 
                   weights = fish.area.weight,
                   na.action = 'na.fail',
                   data = dfa)

capture.output(summary(a_base), file = paste(Results_Loc,"Fish-kelp-abundance-base.txt"))


################################################################################
############ end abundance model ###############################################
################################################################################


################################################################################
############ plot individual kelp species model for appendix ###################
################################################################################

dfx$pred_occur_mn <-  predict(m_MN , type = 'response')
dfa$pred_abund_mn <-  exp(predict(a_base , type = 'response'))

# plot just occurrence ####
plot_occur <- 
        ggplot(dfx , aes(x = Macro, y = Nereo, color = pred_occur_mn)) +
        geom_point(pch = substring(dfx$year,4,4), size = 5) + 
        # scale_color_viridis_d() +
        xlab( expression(paste( italic(Macro),' stipes per ', m^2)) )+
        ylab( expression(paste( italic(Nereo),' stipes per ', m^2)) )+
        theme_bw()+theme_nt + theme(legend.key.size = unit(1,'lines'),
                                    legend.position = c(0.8,0.8))
plot_occur

graphics.off()
png( paste0(Fig_Loc,"Predicted-occurence-MN.png"), units = 'in',res=300, height=3.5, width = 3.5) 
plot_occur
dev.off()


# plot just occurrence and abundance ####

plot_abund <-        
         ggplot(dfa , aes(x = Macro, y = Nereo, color = pred_abund_mn)) +
         # geom_point(size = 4) +
         geom_point(pch = substring(dfa$year,4,4), size = 4) + 
         xlab( expression(paste( italic(Macro),' stipes per ', m^2)) )+
         ylab( expression(paste( italic(Nereo),' stipes per ', m^2)) )+
         theme_bw()+theme_nt + theme(legend.key.size = unit(1,'lines'),
                                     legend.position = c(0.8,0.8))

plot_abund   

 graphics.off()
 png( paste0(Fig_Loc,"Hurdle-Model-Halves.png"), units = 'in',res=300, height=3.0, width = 6)
 
 ggarrange( plot_occur, plot_abund,
            nrow =1, ncol = 2,
            align = 'v'
            )
 dev.off()

 
#################################################################################
########### Combine occurrence and abundance models##############################
#################################################################################
 
 
df_comb1 <- dfx[,c('year','site','area','zone','Nereo','Macro','pred_occur_mn')] 
df_comb2 <- dfa[,c('year','site','area','zone','pred_abund_mn')] 


# df_comb2$pred_abund_mn[is.na(df_comb2$pred_abund_mn)] <- 0

df_comb  <- left_join(df_comb1, df_comb2) %>% 
            mutate(predYOY = pred_occur_mn*pred_abund_mn)

df_comb$predYOY[is.na(df_comb$pred_abund_mn)] <- 0


library(ggrepel)
library(scales) 
 
 
df_comb$pch = ifelse(df_comb$predYOY == 0, 21, 19 )

legend.title.1 =expression( "Rockfish\njuveniles\nper 60 m"^2) 
legend.title.2 = "Probability of\noccurrence"
# fake legend


plot_comb = ggplot(df_comb , aes(x = Macro, y = Nereo), color='black') +
        geom_point(aes( fill = predYOY), size = 2,  shape = 21 , alpha = 0.5 ) +
        #geom_point(aes( size = pred_occur_mn, fill = predYOY), shape = 21 , alpha = 0.5 ) +
        scale_fill_gradient(low = "white", high = "red")+
        scale_x_sqrt(breaks = c(0, 0.5, 1,2,4,6,8,10), minor_breaks=0:10) +
        scale_y_sqrt(breaks = c(0, 0.5, 1,2,4,6,8,10), minor_breaks=0:10) +
        labs(x = expression(paste( italic(Macro),' stipes per ', m^2)),
             y = expression(paste( italic(Nereo),' stipes per ', m^2)),
             fill = legend.title.1,
             # size = legend.title.2
             ) +
        theme_bw() + theme_nt
        
plot_comb 


############## combined figure ################

plot_occur_x <- plot_occur_1 + theme(legend.box.background = element_blank(),
                                     legend.background = element_blank(),
                                     legend.key.size = unit(0.5,'lines'),
                                     # legend.position = c(0.8,0.7),
                                     legend.position = 'right')
plot_comb_x <- plot_comb     + theme(legend.title =element_text(size=8),
                                     legend.box.background = element_blank(),
                                     legend.background = element_blank(),
                                     legend.key.size = unit(0.5,'lines'),
                                     # legend.position = c(0.8,0.7),
                                     legend.position = 'right',
                                     legend.box = "vertical")
                                      
plot_comb_x
# legend.margin = margin(0,0,0,0, unit="cm")

graphics.off()

png(paste0(Fig_Loc,"Figure-6-fish-kelp-combined.png"), units='in', res = 600, width = 5, height = 5)

ggarrange(plot_occur_x, plot_comb_x, 
         labels = c('a)','b)'), 
         # labels = 'auto',
         font.label = list(face='plain', size = 10),
         hjust = -26,
         vjust = 3,
         nrow = 2, ncol=1,
         align = 'v'
 )
 
dev.off()
 
 
 
 ##################################################

 
