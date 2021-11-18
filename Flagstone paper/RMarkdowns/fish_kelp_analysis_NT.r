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

#### Delta-glm analyses ####


#################### Stats #######################

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
aic = c( AIC(m_year),AIC(m_rand),AIC(m_nozone),AIC(m_occur) )



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
plot_occur

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

graphics.off()
png( paste0(Fig_Loc, "FishYOY-Kelp-probability-occurence.png"), units = 'in',res=300, height=5 ,width = 3.5)
plot_occur2 + theme(legend.position = c(0.8,0.3))
dev.off()

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

#################################################

#### quic gam just for fun ##########
library(mgcv)
g1 = gam( TOTyoy ~ s(three_kelps), data =  dfx)
plot(g1)
summary(g1)

#################################################

# negative binomial model

m_abund_full <- glm.nb( TOTyoy ~ (three_kelps * zone) + 
                        (year_factor) + 
                        offset(log(transect.area.algae.sum/100)), # makes the units nicer
                        data =  df_count )

summary(m_abund_full)
AIC(m_abund_full)
pred_nb = predict(m_abund_full)
df_count$pred_nb <- exp(pred_nb)

dfp =  df_count[,c('three_kelps','TOTyoy')]
df_count =  df_count[ order( df_count$three_kelps) , ]

p2 <-  df_count %>% 
        ggplot(aes(x = three_kelps, y = TOTyoy, color = as.factor(site))) +
        geom_point(pch =  df_count$pch) +
        geom_line(aes(x = three_kelps, y = pred_nb), color='black')+
        xlab( expression(paste('Kelp stipes per ', m^2)) )+
        ylab( expression(paste('Rockfish YOY per 60', m^2, ' transect')) )+
        scale_color_manual(values = site.col$col) +
        labs(title= "Total YOY rockfish count as a function of 3 kelps") +
        theme_bw() + theme_nt

print(p2)














