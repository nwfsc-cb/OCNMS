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
analysis_data$year_factor = factor(analysis_data$year, 
                                      levels = c(2016,2017,2018,2019,2021,2015))

# how many transects have YOY?
sum(analysis_data$present)
sum(analysis_data$absent)

sum(analysis_data$present)/(sum(analysis_data$present)+sum(analysis_data$absent))

###################################################################################

# note for binomial or quasibinomial with success proportions, weights should specify total samples.


#### quasibinomial occurence #########

m_MNs <- glm( present/(present+absent) ~ MACPYR + NERLUE +  site + year_factor, 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))

s_MNs = summary(m_MNs)

m_MNPs <- glm( present/(present+absent) ~ MACPYR + NERLUE + PTECAL + site + year_factor, 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))
s_MNPs = summary(m_MNPs)


m_canopys <- glm( present/(present+absent) ~ canopy_kelp + site + year_factor , 
                data = analysis_data, 
                family = quasibinomial(link = 'logit'), 
                weights = (present+absent))
s_canopys <- summary(m_canopys)

m_threes <- glm( present/(present+absent) ~ three_kelps + site + year_factor , 
                  data = analysis_data, 
                  family = quasibinomial(link = 'logit'), 
                  weights = (present+absent))
s_threes <- summary(m_threes)


#### no site

m_MN <- glm( present/(present+absent) ~ MACPYR + NERLUE +  year_factor, 
             data = analysis_data, 
             family = quasibinomial(link = 'logit'), 
             weights = (present+absent))
s_MN = summary(m_MN)

m_MNP <- glm( present/(present+absent) ~ MACPYR + NERLUE + PTECAL + year_factor, 
              data = analysis_data, 
              family = quasibinomial(link = 'logit'), 
              weights = (present+absent))
s_MNP = summary(m_MNP)

m_canopy <- glm( present/(present+absent) ~ canopy_kelp +  year_factor, 
                 data = analysis_data, 
                 family = quasibinomial(link = 'logit'), 
                 weights = (present+absent))
s_canopy <- summary(m_canopy)

m_three <- glm( present/(present+absent) ~ three_kelps +  year_factor, 
                 data = analysis_data, 
                 family = quasibinomial(link = 'logit'), 
                 weights = (present+absent))
s_three <- summary(m_three)

# model table
models = ls()
models = models[grep('m_',models)]
xtable = data.frame(Terms = c('(Intercept)','three_kelps','canopy_kelp',names(m_MNPs$coefficients)[-1]) )

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

colnames(xtable) = c('Term',paste0('M',1:(ncol(xtable)-1)) )
xtable[is.na(xtable)] <- ""
xtable

write.csv(xtable,paste0(Fig_Loc,"Table_Quasibinomial_parameters-simplified-occurence.csv"), row.names = FALSE)


abundance_data = analysis_data[analysis_data$present == 1,]
 
# with site      
a_MNPs <- glm( TOTyoy  ~ MACPYR + NERLUE + PTECAL + site + year_factor, 
               family = poisson(link = 'log'),
               data = abundance_data )
sa_MNPs = summary(a_MNPs)

a_MNs <- glm( TOTyoy  ~ MACPYR + NERLUE + site + year_factor, 
              family = poisson(link = 'log'),
               data = abundance_data)
sa_MNs = summary(a_MNs)

a_canopys <- glm( TOTyoy  ~ canopy_kelp + site + year_factor, 
                  family = poisson(link = 'log'),
               data = abundance_data )
sa_canopys = summary(a_canopys)

a_threes <- glm( TOTyoy  ~ three_kelps + site + year_factor, 
                 family = poisson(link = 'log'),
                  data = abundance_data )
sa_threes = summary(a_threes)

# no site

a_MNP <- glm( TOTyoy  ~ MACPYR + NERLUE + PTECAL +year_factor, 
              family = poisson(link = 'log'),
               data = abundance_data )
sa_MNP = summary(a_MNP)

a_MN <- glm( TOTyoy  ~ MACPYR + NERLUE + year_factor, 
             family = poisson(link = 'log'),
              data = abundance_data)
sa_MN = summary(a_MN)

a_canopy <- glm( TOTyoy  ~ canopy_kelp +  year_factor, 
                 family = poisson(link = 'log'),
                  data = abundance_data )
sa_canopy = summary(a_canopy)

a_three <- glm( TOTyoy  ~ three_kelps +  year_factor, 
                family = poisson(link = 'log'),
                 data = abundance_data )
sa_three = summary(a_three)


###

models = ls()
models = models[grep('a_',models)]
ytable = data.frame(Terms = c('(Intercept)','canopy_kelp',names(a_MNPs$coefficients)[-1]) )

for(i in 1: length(models)){
        df = data.frame(summary(get(models[i]))$coefficients)
        df$Estimate = round(df$Estimate,2)
        df$SE = round(df[,2],2)
        df$x = paste0(df$Estimate," (",df$SE,")")
        df$y = ifelse(df[,4]<=0.05,"*"," ")
        df$x[df$x == '<NA>'] <- ""
        df$y[is.nan(df$y)] <- ""
        df$xy = paste0(df$x, df$y)
        ytable[,i+1] = ""
        ytable[,i+1] = df$xy[match(ytable$Terms,rownames(df))]
        ytable['AIC',i+1] = AIC(get(models[i]))
      
}

colnames(ytable) = c('Term',paste0('M',1:(ncol(ytable)-1)) )
ytable[is.na(ytable)] <- ""

for(i in 2:ncol(ytable)){
        print(i)
     ytable['deltaAIC',i] =  round(as.numeric(ytable['AIC',i]) - min(as.numeric(ytable['AIC',2:ncol(ytable)])),2)
}

ytable

write.csv(ytable,paste0(Fig_Loc,"Table_Quasibinomial_parameters-simplified-abundance.csv"), row.names = FALSE)






