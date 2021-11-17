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

df = readRDS( paste0(Data_Loc,"Data_Fish_Kelp_area_wide.rds"))

################## Ordination rockfish yoy vs kelps #########################

# get fish matrix
yoy = colnames(df)[grep('yoy',colnames(df))]
yoy = yoy[ yoy != 'TOTyoy']
fish = df[,yoy]

library(ecole)
bray.fish = bray0(sqrt(fish))

df <- df %>% rename(Macro= MACPYR , Nereo=NERLUE, Ptery=PTECAL) 

cap1 = capscale( bray.fish ~ Macro + Nereo + Ptery, distance = 'bray', data = df)
sppscores(cap1) <- fish   # add fish scores sinces used distance matrix
capscores <- scores(cap1) # get scores
df$LD1 = capscores$sites[,1]
df$LD2 = capscores$sites[,2]
df$col = site.col$col[ match(df$site,site.col$site) ]

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
points(df$LD1,df$LD2, col=df$col, pch=19, cex=0.8)
text( capscores$species[,1] , capscores$species[,2], rownames(capscores$species),col='red', cex = 0.8 )

dev.off()


######### Quick plots #######

df$site <- factor(df$site, levels=settings$sites)

macro1 <- ggplot( df , aes(x = Macro, y = TOTyoy, color=site)) +
     geom_point() + 
     xlab( expression(paste(italic('Macro'), ' stipes per 60 ', m^2)) )+
     ylab( expression(paste('Rockfish YOY per 60 ',m^2) )) + 
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt

nereo1 <- ggplot( df , aes(x = Nereo, y = TOTyoy, color=site)) +
     geom_point() + 
     xlab( expression(paste(italic('Nero'), ' stipes per 60 ', m^2)) )+
     ylab( expression(paste('Rockfish YOY per 60 ',m^2) )) + 
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt

ptery1 <- ggplot( df , aes(x = Ptery, y = TOTyoy, color=site)) +
     geom_point() + 
     xlab( expression(paste(italic('Ptery'), ' stipes per 60 ', m^2)) )+
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

# get transect area for weighting. Need swath file
swath <- read_rds(paste0(Data_Loc,'Swath_2015-2021.rds'))
swath1 <- swath %>% filter(site %in% settings$sites)
swath1$site <- factor(swath1$site, levels=settings$sites)

dat.algae   <- swath1 %>% filter(group=="Algae")
# assign dummy area and zone for 2015 data
dat.algae$area[dat.algae$year == 2015] <- "D"
dat.algae$zone[dat.algae$year == 2015] <- 5

# 
dat.algae1 <- dat.algae %>%
     rename(transect.area.algae = Transect.area, count = Count) %>%
     mutate(density = count/transect.area.algae )
dat.algae2 <- dat.algae1 %>%
     complete(species,
              nesting(year, site, area, zone, transect, transect.area.algae),
              fill=list(fun_gr = "COMPLETED",
                        count = NA,
                        density = NA)
     )
# make sure everything looks ok. it does
dat.algae2 %>% tabyl(site, area, year)

#### combine files ####

# make match
df$area[df$year == 2015] <- "D"
dat.algae3 <- dat.algae2[,c('year','site','area','zone','transect.area.algae')]
     
df <- left_join(df, dat.algae3) %>%
       mutate(transect.area.algae.weight = transect.area.algae/max(transect.area.algae)) 




#################### Stats #######################
df <- df %>% mutate(TOTyoy_pres = ifelse(TOTyoy > 0 , 1, 0)) %>%
             mutate(three_kelps = Macro+Nereo+Ptery)
df$year_factor = factor(df$year)

#(three_kelps * zone) ####
# null model with only random year effect
m_rand <- glmer( TOTyoy_pres ~  (1|year_factor), 
                  family = binomial, 
                  weights = transect.area.algae.weight,
                  data = df
)

AIC(m_rand)

# model with kelp and zones ####
m_occur <- glmer( TOTyoy_pres ~ (three_kelps * zone) + (1|year_factor), 
               family = binomial, 
               weights = transect.area.algae.weight,
               data = df
)

summary(m_occur)
anova(m_occur)

# delta AIC for model comparison

AIC(m_occur)

p_occur <- predict(m_occur , type = "response")

AIC(m_occur) - AIC(m_rand)

df$p_occur <- p_occur
df$pch = ifelse(df$zone==5,21,19)

# plot occurence
plot_occur <-
     ggplot( df , aes(x=three_kelps,y=p_occur, color = site))+
     geom_point( pch = df$pch)+
     xlab(expression("Total kelp stipes per 60 ", m^2)) +
     ylab('Probability of occurence') + 
     geom_smooth(formula = y ~ x,
                 aes(x=three_kelps,y = TOTyoy_pres, group=1),
                 method = "glm", 
                 method.args = list(family = "binomial")) +
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt
plot_occur


####### Abundance Only ####
# get just positives

df_pos <- df %>% filter(TOTyoy > 0)

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
     xlab(expression("Total kelp stipes per 60 ", m^2)) +
     ylab('Probability of occurence') + 
     # geom_smooth(formula = log(y) ~ x,
     #             aes(x=three_kelps,y = TOTyoy, group=1),
     #             method = "glm" ) +
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt
plot_abund

### Combine ####


df_pos1 <- df_pos[ c('site','year','area', 'zone', 'pred_abund')]
df <- df %>% left_join(., df_pos1)

df$predicted = df$p_occur * df$pred_abund

plot_predicted <-
     ggplot( df , aes(x=three_kelps,y=predicted, color = site))+
     geom_point( pch = df$pch)+
     xlab(expression("Total kelp stipes per 60 ", m^2)) +
     ylab('Predicted abundance') + 
     scale_color_manual(values = site.col$col) +
     theme_bw() + theme_nt
plot_predicted
























