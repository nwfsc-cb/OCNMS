library(tidyverse)
# clean up
rm(list=ls())

Data_Loc = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/Data/"
Fig_Loc = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/Plots/"
##### area by area files  ########################################################
ntfish <- readRDS("C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/Data/Data_Fish_Kelp_area_wide.rds")

jsfish <- readRDS("C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper/Data/yoy_kelp_2015_2021_year_site_area_zone.rds")

# renames some colums to match different files; easier coding
area_area <- jsfish %>% rename( present = num_trans_TOTyoy_Y,
                             absent  = num_trans_TOTyoy_N)

##### transect by area files #####################################################
# combine the fish transect data with kelp area files

fish_transect <- readRDS(paste0(Data_Loc,  "Data_FISH_taxa_wide.rds") ) %>% 
     select(year, site, transect, area, zone, TOTyoy)

kelp_area <- ntfish %>% select(year, site, area, zone, NERLUE, MACPYR, PTECAL)

transect_area <- fish_transect %>% left_join(., kelp_area) %>%
     mutate(canopy_kelp = NERLUE+MACPYR,
            three_kelps = MACPYR+NERLUE+PTECAL,
            present = ifelse(TOTyoy>0,1,0),
            absent  = ifelse(TOTyoy>0,0,1),
            n = 1) 

##### choose data set to analyze ##################################################

# area by area or transect by area

analysis_data <- transect_area
analysis_data$year_factor = as.factor(analysis_data$year)
###################################################################################

#### quasibinomial abundance #########

# nereo.m1 <- glm( num_trans_TOTyoy_Y/(num_trans_TOTyoy_Y+num_trans_TOTyoy_N) ~ 
#                         (NERLUE*zone) + year_factor, data = analysis_data, 
#                         family = quasibinomial(link = 'logit'), 
#                         weights = (num_trans_TOTyoy_Y+num_trans_TOTyoy_N))
library(lme4)

# base models ####

m_site <- glm( present/(present+absent) ~ site, 
                 data = analysis_data, 
                 family = quasibinomial(link = 'logit'), 
                 weights = (present+absent))

m_year <- glm( present/(present+absent) ~ year_factor, 
               data = analysis_data, 
               family = quasibinomial(link = 'logit'), 
               weights = (present+absent))

m_site_year <- glm( present/(present+absent) ~ site + year_factor, 
               data = analysis_data, 
               family = quasibinomial(link = 'logit'), 
               weights = (present+absent))

# individual algae ####

m_nereo <- glm( present/(present+absent) ~ (NERLUE*zone) + site + year_factor, 
                 data = analysis_data, 
                 family = quasibinomial(link = 'logit'), 
                 weights = (present+absent))

m_macro <- glm( present/(present+absent) ~ (MACPYR*zone) + site + year_factor, 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))

m_ptery <- glm( present/(present+absent) ~ (PTECAL*zone) + site + year_factor, 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))

# two kelps

m_macner <- glm( present/(present+absent) ~ (MACPYR*NERLUE*zone) + site + year_factor, 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))
m_macpte <- glm( present/(present+absent) ~ (MACPYR*PTECAL*zone) + site + year_factor, 
                 data = analysis_data, 
                 family = quasibinomial(link = 'logit'), 
                 weights = (present+absent))
m_nerpte <- glm( present/(present+absent) ~ (NERLUE*PTECAL*zone) + site + year_factor, 
                 data = analysis_data, 
                 family = quasibinomial(link = 'logit'), 
                 weights = (present+absent))

# all three ####

m_three <- glm( present/(present+absent) ~ (MACPYR*NERLUE*PTECAL*zone) + site + year_factor, 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))
s3 = summary(m_three)

# canopy kelp ####

m_canopy <- glm( present/(present+absent) ~ (canopy_kelp*zone) + site + year_factor, 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))

# model table
models = ls()
models = models[grep('m_',models)]
xtable = data.frame(Terms = rownames(s3$coefficients))

for(i in 1: length(models)){
     df = data.frame(summary(get(models[i]))$coefficients)
     df$Estimate = round(df$Estimate,2)
     df$SE = round(df[,2],2)
     df$x = paste0(df$Estimate," (",df$SE,")")
     df$y = ifelse(df[,4]<=0.05,"*"," ")
     df$x[df$x == '<NA>'] <- ""
     df$y[is.nan(df$y)] <- ""
     df$xy = paste0(df$x, df$y)
     xtable[,i+1] = ""
     xtable[,i+1] = df$xy[match(xtable$Terms,rownames(df))]

     
     
     
}

colnames(xtable) = c('Term',paste0('M',2:ncol(xtable)))
xtable[is.na(xtable)] <- ""
xtable

write.csv(xtable,paste0(Fig_Loc,"Table_Quasibinomial_parameters.csv"), row.names = FALSE)







