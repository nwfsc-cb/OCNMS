# Analyze relationships between rockfish YOY and kelp

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

devtools::install_github("AckerDWM/gg3D")
library(gg3D)
# Set parameters; import information  ####

HomeFile = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper"

Fig_Loc = paste0(HomeFile,"/Plots/")
Data_Loc = paste0(HomeFile,"/Data/")
Results_Loc = paste0(HomeFile,"/Results/")
Other_Files = paste0(HomeFile,"/Other Files/")

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

# Bring in data. Produced in Univariate RMD ####

# this file has mean density data for ordination
df_dens = readRDS( paste0(Data_Loc,"Data_Fish_Kelp_area_wide.rds"))
df_dens <- df_dens %>% rename(Macro= MACPYR , Nereo=NERLUE, Ptery=PTECAL)
df_dens$area[df_dens$year==2015] <- "D"

# this file has counts and areas for some univariate stats
df_count <- readRDS(paste0(Data_Loc,"Fish-kelp-counts-wide.rds"))
df_count <- df_count %>% rename(Macro= MACPYR , Nereo=NERLUE, Ptery=PTECAL)

# add the area informatinto the df_dens file

dfx = df_count[,c('site','zone','area','year','fish_area','kelp_area')]
df_dens <- left_join(df_dens , dfx)

################## Ordination rockfish yoy vs kelps #########################

# get fish matrix
yoy = colnames(df_dens)[grep('yoy',colnames(df_dens))]
yoy = yoy[ yoy != 'TOTyoy']
fish = df_dens[,yoy]

library(ecole)
bray.fish = bray0(sqrt(fish))

cap1 = capscale( bray.fish ~ Macro + Nereo + Ptery, distance = 'bray', data = df_dens)
sppscores(cap1) <- fish   # add fish scores sinces used distance matrix
capscores <- scores(cap1) # get scores
df_dens$LD1 = capscores$sites[,1]
df_dens$LD2 = capscores$sites[,2]
df_dens$col = site.col$col[ match(df_dens$site,site.col$site) ]

rn <- rownames(capscores$species)
rn[rn == 'SECAyoy'] <- 'Copper'
rn[rn == 'SEPIyoy'] <- 'Canary'
rn[rn == 'SEMYyoy'] <- 'Blue'
rn[rn == 'SEBYTyoy'] <- 'Black/YT'
rn[rn == 'RYOY'] <- 'RF YOY'
rownames(capscores$species) <- rn

# ordination plot ####
graphics.off()
par( ps = 10, cex = 1, pty='s')
png(paste0(Fig_Loc,"Ordination-YOY-v-kelp-area.png"), units = 'in', res=300, height=4, width=4)

ordiplot(cap1, cex = 0.8)
points(df_dens$LD1,df$LD2, col=df$col, pch=19, cex=0.8)
text( capscores$species[,1] , capscores$species[,2], rownames(capscores$species),col='red', cex = 0.8 )

dev.off()


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

macro1 <- macro1 + theme(legend.position = c(0.75,0.8))
nereo1 <- nereo1 + theme(legend.position = 'none')
ptery1 <- ptery1 + theme(legend.position = 'none')
     
par(ps = 10, cex =1)

graphics.off()

png(paste0(Fig_Loc,"Univariate-YOY-v-Kelp.simple.png"), units = 'in', res=300, height=5, width=3)
ggarrange(macro1, nereo1, ptery1,
          labels = c('a)','b)','c)'), 
          font.label = list(face='plain', size = 10),
          hjust = -5.5,
          vjust = 2.5,
          align = 'v',
          nrow = 3, ncol=1)
dev.off()


#################### Stats #######################

# Delta-GLM ####

# chose file to use for delta-glm analyses
dfx <- df_dens

dfx <- dfx %>% mutate(TOTyoy_pres = ifelse(TOTyoy > 0 , 1, 0)) %>%
             mutate(three_kelps = Macro+Nereo+Ptery) %>%
             mutate(transect.area.algae.weight = kelp_area/max(kelp_area))
dfx$year_factor = factor(dfx$year)

#(three_kelps * zone) ####
# null model with only random year effect
m_year <- glmer( TOTyoy_pres ~  (1|year_factor), 
                  family = binomial, 
                  weights = transect.area.algae.weight,
                  data = dfx)
m_rand <- glmer( TOTyoy_pres ~  (1|year_factor)+ (1|site), 
                 family = binomial, 
                 weights = transect.area.algae.weight,
                 data = dfx)
# model with kelp and zones ####
m_nozone <- glmer( TOTyoy_pres ~ three_kelps+ (1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = transect.area.algae.weight,
                  data = dfx)

# model with kelp and zones ####
m_occur <- glmer( TOTyoy_pres ~ (three_kelps * zone) + (1|year_factor) + (1|site), 
               family = binomial, 
               weights = transect.area.algae.weight,
               data = dfx)

summary(m_occur)
aic_occur = data.frame(c( AIC(m_year),AIC(m_rand),AIC(m_nozone),AIC(m_occur) ))
colnames(aic_occur) = 'AIC'
aic_occur$Model = c('Year','Year + Site', 'Kelp' , 'Kelp + Depth')
aic_occur = aic_occur[,c('Model','AIC')]
aic_occur$DeltaAIC =  aic_occur$AIC - min(aic_occur$AIC)
aic_occur = aic_occur[ order(aic_occur$DeltaAIC),]
aic_occur

pred_occur <- predict(m_occur , type = "response")

AIC(m_occur) - AIC(m_year)

dfx$p_occur <- pred_occur

dfx$pch = ifelse(dfx$zone==5,21,19)

# plot occurence
plot_occur <-
     ggplot( dfx , aes(x=three_kelps,y=p_occur, color = site))+
     geom_point(pch = dfx$pch)+
     xlab( expression(paste('Kelp stipes per ', m^2)) )+
     geom_point(aes(x=three_kelps, y = TOTyoy_pres), color='black') + 
     ylab('Probability of occurence') + 
     geom_smooth(formula = y ~ x,
                 aes(x=three_kelps,y = TOTyoy_pres, group=1),
                 method = "glm", 
                 method.args = list(family = "binomial")) +
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt

plot_occur + theme(legend.position = c(0.8,0.3))

graphics.off()
png( paste0(Fig_Loc, "FishYOY-Kelp-probability-occurence.png"), units = 'in',res=300, height=3.5 ,width = 3.5)
plot_occur + theme(legend.position = c(0.8,0.3))
dev.off()
### separate by depth

plot_occur2 <-
        ggplot( dfx , aes(x=three_kelps,y=p_occur, color = site))+
        geom_point( pch =  dfx$pch)+
        geom_point(aes(x=three_kelps, y = TOTyoy_pres), color='black', size=1) + 
        xlab( expression(paste('Kelp stipes per ', m^2)) )+
        ylab('Probability of occurence') + 
        facet_wrap(facets =  dfx$zone, nrow = 2)+
        geom_smooth(formula = y ~ x,
                    aes(x=three_kelps,y = TOTyoy_pres, group=1),
                    method = "glm", 
                    method.args = list(family = "binomial")) +
        scale_color_manual(values = site.col$col) +
        theme_bw() + theme_nt
plot_occur2 + theme(legend.position = c(0.8,0.3))




####### Abundance Only ####
# get just positives

df_pos <-  dfx %>% filter(TOTyoy > 0)

pos_rand <- lmer( log(TOTyoy) ~  (1|year_factor), 
                 weights = transect.area.algae.weight,
                 data = df_pos)

AIC(pos_rand)

# model with kelp and zones ####
pos_abund <- lmer( log(TOTyoy) ~ (three_kelps * zone) + (1|year_factor), 
                  weights = transect.area.algae.weight,
                  data = df_pos)

AIC(pos_abund)
AIC(pos_abund) - AIC(pos_rand)


pred_abund = exp(predict(pos_abund))
df_pos$pred_abund = pred_abund

# plot abundance ####

plot_abund <-
     ggplot( df_pos , aes(x=three_kelps,y=pred_abund, color = site))+
     geom_point( pch = df_pos$pch)+
     xlab( expression(paste('Kelp stipes per ', m^2)) )+
     ylab('Probability of occurence') + 
     # geom_smooth(formula = log(y) ~ x,
     #             aes(x=three_kelps,y = TOTyoy, group=1),
     #             method = "glm" ) +
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt
plot_abund

### Combine ####


df_pos1 <- df_pos[ c('site','year','area', 'zone', 'pred_abund')]
dfx <-  dfx %>% left_join(., df_pos1)

dfx$predicted = dfx$p_occur *  dfx$pred_abund

plot_predicted <-
     ggplot(  dfx , aes(x=three_kelps,y=predicted, color = site))+
     geom_point( pch =  dfx$pch)+
     xlab( expression(paste('Kelp stipes per ', m^2)) )+
     ylab('Predicted abundance') + 
        scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt
plot_predicted


#######################################################################
# Macro + Nereo + Ptery separate ####

# no kelp

m_year <- glmer( TOTyoy_pres ~  (1|year_factor), 
                family = binomial, 
                weights = transect.area.algae.weight,
                data = dfx)
m_site <- glmer( TOTyoy_pres ~  (1|site), 
                 family = binomial, 
                 weights = transect.area.algae.weight,
                 data = dfx)
m_yearsite <- glmer( TOTyoy_pres ~  (1|year_factor)+ (1|site), 
                 family = binomial, 
                 weights = transect.area.algae.weight,
                 data = dfx)
# indiv kelps

m_nereo <- glmer( TOTyoy_pres ~ Nereo +(1|year_factor) + (1|site), 
                family = binomial, 
                weights = transect.area.algae.weight,
                data = dfx)
m_macro <- glmer( TOTyoy_pres ~  Macro +  (1|year_factor) + (1|site), 
                family = binomial, 
                weights = transect.area.algae.weight,
                data = dfx)
m_ptery <- glmer( TOTyoy_pres ~  Ptery +  (1|year_factor) + (1|site), 
                family = binomial, 
                weights = transect.area.algae.weight,
                data = dfx)
# two kelps

m_MN <- glmer( TOTyoy_pres ~ Nereo + Macro +(1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = transect.area.algae.weight,
                  data = dfx)
m_MP <- glmer( TOTyoy_pres ~  Macro + Ptery+  (1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = transect.area.algae.weight,
                  data = dfx)
m_NP <- glmer( TOTyoy_pres ~  Nereo + Ptery +  (1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = transect.area.algae.weight,
                  data = dfx)
# all kelps
m_kelp <- glmer( TOTyoy_pres ~  Macro+Nereo+Ptery + (1|year_factor) + (1|site), 
                 family = binomial, 
                 weights = transect.area.algae.weight,
                 data = dfx)


m_all <- glmer( TOTyoy_pres ~  Macro+Nereo+Ptery+zone +  (1|year_factor) + (1|site), 
                family = binomial, 
                weights = transect.area.algae.weight,
                data = dfx)

m_kelp_inter <- glmer( TOTyoy_pres ~  (Macro*Nereo*Ptery) + (1|year_factor) + (1|site), 
                family = binomial, 
                weights = transect.area.algae.weight,
                data = dfx)


     
AIC(m_year)
AIC(m_site)
AIC(m_yearsite)
AIC(m_all)
summary(m_all)

aic_all = data.frame(c( AIC(m_year),
                        AIC(m_site),
                        AIC(m_yearsite),
                        AIC(m_macro),
                        AIC(m_nereo),
                        AIC(m_ptery),
                        AIC(m_MN),
                        AIC(m_MP),
                        AIC(m_NP) ))
                        #AIC(m_kelp), 
                        #AIC(m_all), 
                        #AIC(m_kelp_inter) ))
colnames(aic_all) = 'AIC'
aic_all$Model = c('Year','Site', 'Year + Site', 
                  'Macro + Year + Site',
                  'Nereo + Year + Site',
                  'Ptery + Year + Site',
                  'Mac + Ner + Year + Site', 
                  'Mac+ Pter + Year + Site', 
                  'Ner+ Pter + Year + Site')
                  # 'Kelp' , 'Kelp + Depth', 
                  # 'Kelp Interactions')

aic_all = aic_all[,c('Model','AIC')]
aic_all$DeltaAIC =  aic_all$AIC - min(aic_all$AIC)
aic_all = aic_all[ order(aic_all$DeltaAIC),]
aic_occur <- aic_all

capture.output(aic_occur , file = paste0(Fig_Loc,"AIC_occurence.txt"))


# rename here because overwritten below

m_presence <- m_MN



# double check whether zone matters: doesn't
m_MNZ <- glmer( TOTyoy_pres ~ Nereo + Macro + zone +(1|year_factor) + (1|site), 
               family = binomial, 
               weights = transect.area.algae.weight,
               data = dfx)
AIC(m_MNZ)
# abundance ####

dfa = dfx[dfx$TOTyoy_pres == 1,]

m_year <- lmer( TOTyoy ~  (1|year_factor), 
        
                 weights = transect.area.algae.weight,
                 data = dfa)
m_site <- lmer( TOTyoy ~  (1|site), 
                  
                 weights = transect.area.algae.weight,
                 data = dfa)
m_yearsite <- lmer( TOTyoy ~  (1|year_factor)+ (1|site), 
                     
                     weights = transect.area.algae.weight,
                     data = dfa)
# indiv kelps
m_nereo <- lmer( TOTyoy ~ Nereo +(1|year_factor) + (1|site), 
                   
                  weights = transect.area.algae.weight,
                  data = dfa)
m_macro <- lmer( TOTyoy ~  Macro +  (1|year_factor) + (1|site), 
                  
                  weights = transect.area.algae.weight,
                  data = dfa)
m_ptery <- lmer( TOTyoy ~  Ptery+zone +  (1|year_factor) + (1|site), 
                  
                  weights = transect.area.algae.weight,
                  data = dfa)
# two kelps

m_MN <- lmer( TOTyoy ~ Nereo+Macro +(1|year_factor) + (1|site), 
                
               weights = transect.area.algae.weight,
               data = dfa)
m_MP <- lmer( TOTyoy ~  Macro +Ptery+  (1|year_factor) + (1|site), 
               
               weights = transect.area.algae.weight,
               data = dfa)
m_NP <- lmer( TOTyoy ~  Nereo+ Ptery+zone +  (1|year_factor) + (1|site), 
               
               weights = transect.area.algae.weight,
               data = dfa)
# all kelps
m_kelp <- lmer( TOTyoy ~  Macro+Nereo+Ptery + (1|year_factor) + (1|site), 
                 
                 weights = transect.area.algae.weight,
                 data = dfa)


m_all <- lmer( TOTyoy ~  Macro+Nereo+Ptery+zone +  (1|year_factor) + (1|site), 
                
                weights = transect.area.algae.weight,
                data = dfa)

m_kelp_inter <- lmer( TOTyoy ~  (Macro*Nereo*Ptery) + (1|year_factor) + (1|site), 
                       
                       weights = transect.area.algae.weight,
                       data = dfa)

aic_abund = data.frame(c( AIC(m_year),
                        AIC(m_site),
                        AIC(m_yearsite),
                        AIC(m_macro),
                        AIC(m_nereo),
                        AIC(m_ptery),
                        AIC(m_MN),
                        AIC(m_MP),
                        AIC(m_NP) ))
                        #AIC(m_kelp), 
                        #AIC(m_all), 
                        #AIC(m_kelp_inter) 
                        #))
colnames(aic_abund) = 'AIC'
aic_abund$Model  = c('Year','Site', 'Year + Site', 
                     'Macro + Year + Site',
                     'Nereo + Year + Site',
                     'Ptery + Year + Site',
                     'Mac + Ner + Year + Site', 
                     'Mac+ Pter + Year + Site', 
                     'Ner+ Pter + Year + Site')

aic_abund =aic_abund[,c('Model','AIC')]
aic_abund$DeltaAIC =  aic_abund$AIC - min(aic_abund$AIC)
aic_abund = aic_abund[ order(aic_abund$DeltaAIC),]
aic_abund

capture.output(aic_abund , file = paste0(Fig_Loc,"AIC_positives.txt"))

# use only multi species kelp models; don't lump kelps
m_abundance = m_MP

############ put together hurdle model #############
# 

# get predictions for best model
# summarize to site x year
# comnbine
# plot

dfx$prob_occur <- predict(m_presence, type = "response")
df_occurence <- dfx %>% group_by(site,year) %>%
        summarise(p_occur = mean(prob_occur),
                  mean_macro = mean(Macro),
                  mean_nereo = mean(Nereo),
                  mean_ptery = mean(Ptery))

dfa$pred_abund <- predict(m_abundance, type = "response")
df_abundance <- dfa %>% group_by(site,year) %>%
        summarise(p_abund = mean(pred_abund) )

df_predicted <- full_join( df_occurence, df_abundance)
df_predicted$p_abund[df_predicted$year == 2015] <- 0

df_predicted = df_predicted %>% mutate(pred_yoy = p_occur*p_abund) 

# some attempt at a heat plot style pair of graphs
plot_occur <- 
        ggplot(df_predicted , aes(x = mean_macro, y = mean_nereo, color = p_occur)) +
        geom_point(pch = substring(df_predicted$year,4,4), size = 4) + 
        # scale_color_viridis_d() +
        xlab( expression(paste( italic(Macro),' stipes per ', m^2)) )+
        ylab( expression(paste( italic(Nereo),' stipes per ', m^2)) )+
        theme_bw()+theme_nt + theme(legend.key.size = unit(1,'lines'),
                                    legend.position = c(0.8,0.8))
 
plot_abund <-        
        ggplot(df_predicted , aes(x = mean_macro, y = mean_ptery, color = p_abund)) +
        # geom_point(size = 4) +
        geom_point(pch = substring(df_predicted$year,4,4), size = 4) + 
        xlab( expression(paste( italic(Macro),' stipes per ', m^2)) )+
        ylab( expression(paste( italic(Ptery),' stipes per ', m^2)) )+
        theme_bw()+theme_nt + theme(legend.key.size = unit(1,'lines'),
                                                         legend.position = c(0.8,0.8))
graphics.off()
png( paste0(Fig_Loc,"Hurdle_Model.png"), units = 'in',res=300, height=3.0, width = 6)

ggarrange( plot_occur, plot_abund,
           nrow =1, ncol = 2,
           align = 'v'
           )
dev.off()


#################################################

#### quic gam just for fun ##########
library(mgcv)
g1 = gam( TOTyoy ~ s(three_kelps), data =  dfx)
plot(g1)
summary(g1)

g2 = gam(TOTyoy ~ s(Macro) + s(Nereo) + s(Ptery), data=dfx)
summary(g2)
plot(g2)

#################################################



###############################################################
#### Use CANOPY DATA ##########################################
###############################################################

canopy = readRDS( paste0(Data_Loc, "Data_Kelp_Canopy_Long.rds") )

canopy_wide <- pivot_wider(filter(canopy, can_type == 'tot_can'), names_from = species, values_from = canopy_ha,values_fill = NA)

df_fish = df_dens %>% 
        group_by(year,site) %>%
        summarise(yoy = mean(TOTyoy)) %>%
        left_join(.,canopy_wide)
df_fish$yr = as.character(substring(df_fish$year,4,4))

df_fish$site = factor(df_fish$site, levels = settings$sites)

canopy_plot <-
        ggplot( df_fish, aes(x = Total, y = yoy, color = site ))+
        geom_point(pch = df_fish$yr, size = 4) +
        xlab('Canopy area (ha)')+
        ylab( expression(paste('Rockfish YOY per 60 ', m^2, ' transect')) )+
        scale_color_manual(values = site.col$col) +
        theme_bw() + theme_nt

graphics.off()
png( paste0(Fig_Loc, "YOY-vs-Canopy-Kelp.png") , units = 'in', res=300, width=3.5, height=3.5)

canopy_plot + theme(legend.position = c(0.8,0.85) )

dev.off()







