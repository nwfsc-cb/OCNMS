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

# Bring in data. Produced in PlotUnivariate. RMD ####

# MEAN DENSITY ####
df_dens = readRDS( paste0(Data_Loc,"Data_Fish_Kelp_area_wide.rds"))
df_dens <- df_dens %>% rename(Macro= MACPYR , Nereo=NERLUE, Ptery=PTECAL)
df_dens$area[df_dens$year==2015] <- "D"

# TOTALCOUNT ####
df_count <- readRDS(paste0(Data_Loc,"Fish-kelp-counts-wide.rds"))
df_count <- df_count %>% 
        rename(Macro= MACPYR , Nereo=NERLUE, Ptery=PTECAL) %>%
        mutate(fish_volume = fish_area*2)

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
               mutate(three_kelps = Macro+Nereo+Ptery , 
                    canopy_kelp = Macro+Nereo,
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

m_year <- glmer( TOTyoy_pres ~  (1|year_factor), 
                  family = binomial, 
                  weights = fish.area.weight,
                  data = dfx)

m_rand <- glmer( TOTyoy_pres ~  (1|year_factor)+ (1|site), 
                 family = binomial, 
                 weights = fish.area.weight,
                 data = dfx)

# models with kelp summed ####

m_three <- glmer( TOTyoy_pres ~ three_kelps+ (1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = fish.area.weight,
                  data = dfx)

m_canopy <- glmer( TOTyoy_pres ~ canopy_kelp + (1|year_factor) + (1|site), 
                    family = binomial, 
                    weights = fish.area.weight,
                    data = dfx)

# indiv kelps ####

m_nereo <- glmer( TOTyoy_pres ~ Nereo +(1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = fish.area.weight,
                  data = dfx)
m_macro <- glmer( TOTyoy_pres ~  Macro +  (1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = fish.area.weight,
                  data = dfx)
m_ptery <- glmer( TOTyoy_pres ~  Ptery +  (1|year_factor) + (1|site), 
                  family = binomial, 
                  weights = fish.area.weight,
                  data = dfx)

# two kelps ####

m_MN <- glmer( TOTyoy_pres ~ Nereo + Macro + (1|year_factor) + (1|site), 
               family = binomial, 
               weights = fish.area.weight,
               data = dfx)

m_MP <- glmer( TOTyoy_pres ~  Macro + Ptery+  (1|year_factor) + (1|site), 
               family = binomial, 
               weights = fish.area.weight,
               data = dfx)
m_NP <- glmer( TOTyoy_pres ~  Nereo + Ptery +  (1|year_factor) + (1|site), 
               family = binomial, 
               weights = fish.area.weight,
               data = dfx)

# three kelps ####

m_kelp <- glmer( TOTyoy_pres ~  Macro+Nereo+Ptery + (1|year_factor) + (1|site), 
                 family = binomial, 
                 weights = fish.area.weight,
                 data = dfx)
# AIC table

x = ls()
y = grep("m_",x)
z = x[y]
z = z[z != "Sum_Stats"]


for(i in 1:length(z)){
        x = get(z[i])
        aicc = AICcmodavg::AICc(x)
        aic = AIC(x)
        sx = summary(x)
        npar = nrow(sx$coefficients)
        mod.name = z[i]
        x2 = data.frame(cbind(mod.name,aicc,aic, npar))
        if(i == 1){aic_table <- x2}else{aic_table = rbind(aic_table,x2)}
}

colnames(aic_table) = c('Model','AICc', 'AIC','Parms')
aic_table$AICc = as.numeric(aic_table$AICc)
aic_table$d_AICc <- aic_table$AICc - min(aic_table$AICc)
aic_table$AIC = as.numeric(aic_table$AIC)
aic_table$d_AIC <- aic_table$AIC - min(aic_table$AIC)
# order table by AICc or AIC. Set here
aic_table = aic_table[order(aic_table$d_AICc),]
aic_table_occur <- aic_table
aic_table_occur
rm(aic_table)

capture.output(aic_table_occur , file = paste0(Fig_Loc,"AIC_occurence.txt"))

write.csv(aic_table_occur, paste0(Fig_Loc,"Fish-YOY-Kelp-binomial-probability-occurence.csv"))

summary(m_canopy)
summary(m_MN)

##############################################################
###### end occurrence models #################################
##############################################################

###### plot best occurrence model ############################
pred_occur <- predict(m_canopy , type = "response")

dfx$p_occur <- pred_occur

dfx$pch = ifelse(dfx$zone==5,21,19)

# plot occurence
plot_occur_1 <-
     ggplot( dfx , aes(x=three_kelps,y=p_occur, color = site))+
     geom_point(pch = 19)+ # dfx$pch)+
     xlab( expression(paste('Canopy kelp stipes per ', m^2)) )+
     geom_point(aes(x=three_kelps, y = TOTyoy_pres), color='black') + 
     ylab('Probability of occurence') + 
     geom_smooth(formula = y ~ x,
                 aes(x=three_kelps,y = TOTyoy_pres, group=1),
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

 # dfa$fish.area.weight = 1

a_year <- lmer( TOTyoy ~  (1|year_factor), 
                weights = fish.area.weight,
                 data = dfa)
a_site <- lmer( TOTyoy ~  (1|site), 
                weights = fish.area.weight,
                 data = dfa)
a_yearsite <- lmer( TOTyoy ~  (1|year_factor)+ (1|site), 
                    weights = fish.area.weight,
                     data = dfa)
# summed kelps ####

a_canopy <- lmer( TOTyoy ~ canopy_kelp +(1|year_factor) + (1|site), 
                  weights = fish.area.weight,
                 data = dfa)
a_three  <- lmer( TOTyoy ~ three_kelps +(1|year_factor) + (1|site), 
                  weights = fish.area.weight,
                 data = dfa)

# indiv kelps ####
a_nereo <- lmer( TOTyoy ~ Nereo +(1|year_factor) + (1|site), 
                 weights = fish.area.weight,
                  data = dfa)
a_macro <- lmer( TOTyoy ~  Macro +  (1|year_factor) + (1|site), 
                 weights = fish.area.weight,
                  data = dfa)
a_ptery <- lmer( TOTyoy ~  Ptery+zone +  (1|year_factor) + (1|site), 
                 weights = fish.area.weight,
                  data = dfa)

# two kelps

a_MN <- lmer( TOTyoy ~ Nereo+Macro +(1|year_factor) + (1|site), 
              weights = fish.area.weight,
               data = dfa)
a_MP <- lmer( TOTyoy ~  Macro +Ptery+  (1|year_factor) + (1|site), 
              weights = fish.area.weight,
               data = dfa)
a_NP <- lmer( TOTyoy ~  Nereo+ Ptery+zone +  (1|year_factor) + (1|site), 
              weights = fish.area.weight,
               data = dfa)

# all kelps

a_MNP <- lmer( TOTyoy ~  Macro+Nereo+Ptery + (1|year_factor) + (1|site), 
               weights = fish.area.weight,
               data = dfa)

# AIC
x = ls()
y = grep("a_",x)
z = x[y]
z = z[ !( z %in% c("Sum_Stats", "Data_Loc")) ]

for(i in 1:length(z)){
        print(i)
        x = get(z[i])
        aicc = AICcmodavg::AICc(x)
        aic = AIC(x)
        sx = summary(x)
        npar = nrow(sx$coefficients)
        mod.name = z[i]
        x2 = data.frame(cbind(mod.name,aicc,aic, npar))
        if(i == 1){aic_table <- x2}else{aic_table = rbind(aic_table,x2)}
}

colnames(aic_table) = c('Model','AICc','AIC', 'Parms')
aic_table$AIC = as.numeric(aic_table$AIC)
aic_table$AICc = as.numeric(aic_table$AICc)
aic_table$d_AICc <- aic_table$AICc - min(aic_table$AICc)
aic_table$d_AIC  <- aic_table$AIC  - min(aic_table$AIC)
aic_table = aic_table[order(aic_table$d_AIC),]
aic_table_abund <- aic_table
aic_table_abund


write.csv(aic_table_abund, paste0(Fig_Loc,"Fish-YOY-Kelp-binomial-abundance.csv"))

summary(a_MNP)


capture.output(summary(m_canopy), file = paste0(Fig_Loc,"Fish-YOY-Kelp-occurence-canopy-table.txt"))
capture.output(summary(m_MN), file = paste0(Fig_Loc,"Fish-YOY-Kelp-occurence-MN-table.txt"))
capture.output(summary(a_MNP), file = paste0(Fig_Loc,"Fish-YOY-Kelp-abundance-table.txt"))
################################################################################
############ end abundance model ###############################################
################################################################################


################################################################################
############ plot individual kelp species model for appendix ###################
################################################################################

dfx$pred_occur_mn <-  predict(m_MN , type = 'response')
dfa$pred_abund_mn <-  exp(predict(a_year , type = 'response'))

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

# df_comb$predYOY[is.na(df_comb$pred_abund_mn)] <- 0

df_comb = na.omit(df_comb)

library(ggrepel)
library(scales) 
 
 
df_comb$pch = ifelse(df_comb$predYOY == 0, 21, 19 )

legend.title.1 =expression( "Rockfish\njuveniles\nper 60 m"^2) 
legend.title.2 = "Probability of\noccurrence"
# fake legend


plot_comb = ggplot(df_comb , aes(x = Macro, y = Nereo), color='black') +
        geom_point(aes( size = pred_occur_mn, fill = predYOY), shape = 21 , alpha = 0.5 ) +
        # geom_point(data = filter(df_comb, predYOY == 0), size=1, shape = 19, color='black')+
        scale_fill_gradient(low = "white", high = "red")+
        scale_x_sqrt(breaks = c(0, 0.5, 1,2,4,6,8,10), minor_breaks=0:10) +
        scale_y_sqrt(breaks = c(0, 0.5, 1,2,4,6,8,10), minor_breaks=0:10) +
        labs(x = expression(paste( italic(Macro),' stipes per ', m^2)),
             y = expression(paste( italic(Nereo),' stipes per ', m^2)),
             size = legend.title.2,
             fill = legend.title.1) +
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

 
