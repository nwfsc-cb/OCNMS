
# simple plots of relief and substratum type for the appendix

# Preliminaries ####

library(ggplot2)
library(tidyverse)
library(readxl)

# setwd('..')
HomeFile = getwd()
Fig_Loc = paste0(HomeFile,"/Plots/")
Data_Loc = paste0(HomeFile,"/Data/")
settings = readRDS( paste0(Data_Loc,'settings.RDS') )
site_col = settings$site.col

# Import Data ####


upc = read_xlsx( paste0( Data_Loc,"NWFSC_UPC_ALLYEARS_data_2021.xlsm" ) , sheet = "DATA")

#### RELIEF ####
# drop some columns
relief = upc %>% filter( CATEGORY == 'RELIEF')
relief = relief[ c('YEAR','SITE','SIDE','ZONE','TRANSECT','SEGMENT','CLASSCODE','COUNT')]

# fix some codes

relief$CLASSCODE[relief$CLASSCODE == "0-10m"] <- "0-10cm"
relief$CLASSCODE[relief$CLASSCODE == ">2M"]   <- ">2m"
relief$CLASSCODE[relief$CLASSCODE == "<2m"]   <- ">2m"

# pivot wide and back to fillin zeros
relief_transect = relief %>% group_by( YEAR, SITE, ZONE, SIDE, TRANSECT, SEGMENT, CLASSCODE) %>%
                   summarise(count = sum(COUNT)) %>%
                   # pivot wide and back to fill in zeros
                   pivot_wider(., names_from = CLASSCODE, values_from = count, values_fill = 0) %>%
                   pivot_longer(., '10cm-1m':'>2m' , names_to = 'classcode', values_to = 'count') %>%
                   # summarise at the segment level
                   group_by(YEAR, SITE, ZONE, SIDE, TRANSECT, SEGMENT, classcode) %>%
                   summarise(total.count = sum(count), segments = length(SEGMENT) ) %>%
                   # summarise at the transect level
                   group_by(YEAR,SITE, TRANSECT, ZONE,SIDE,classcode) %>% 
                   summarise( tot.count = sum(total.count), N = length(segments))

# total count by transect. divide by

tran_count <-      relief_transect %>% group_by(YEAR, SITE, TRANSECT, ZONE, SIDE) %>%
                   summarise(transect.total = sum(tot.count))

relief_site <-     relief_transect %>%
                   left_join(., tran_count) %>%
                   mutate(prop = tot.count/transect.total) %>%
                   # summarise at the year x site level
                   group_by(YEAR, SITE, ZONE, classcode) %>%
                   summarise(mean.prop = mean(prop)) %>%
                   group_by(SITE, classcode) %>%
                   summarise(mean_prop = mean(mean.prop), 
                             SD = sd(mean.prop),
                             N = length(SITE),
                             SE = sqrt(SD)/N)
                    
# set plot order
relief_site$classcode = factor(relief_site$classcode, levels=c( "0-10cm","10cm-1m", "1m-2m",">2m" ))
relief_site$SITE = factor(relief_site$SITE, levels=c(  rev(c("Neah Bay",
                                                          "Tatoosh Island",
                                                          "Cape Alava",
                                                          "Cape Johnson",
                                                        "Destruction Island"))))

# Relief plot ####

relief.plot <- ggplot(relief_site,aes(x=SITE)) +
        geom_bar(aes(y=mean_prop, fill= classcode), 
                 position="fill", stat="identity",width = 0.75, colour="black")+
        scale_fill_manual(values = site_col$col,
                          labels = c('0-0.1 m','0.1-1 m', '1-2 m', '> 2 m')) +  
        theme(legend.position = "left", aspect.ratio = 3/1)  +
        theme_bw()+
        ylab("Mean percent cover") +
        xlab("") +
        scale_x_discrete(expand = c(0,0), labels=c("DI","CJ","CA","TI","NB"))+
        scale_y_continuous(expand = c(0,0)) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 10 ),
              axis.text.x = element_text(size=10), 
              strip.text.x = element_text(size = 7))


     
     
#### SUBSTRATE ####

substrate = upc %>% filter( CATEGORY == 'SUBSTRATE')
substrate = substrate[ c('YEAR','SITE','SIDE','ZONE','TRANSECT','SEGMENT','CLASSCODE','COUNT')]   

# fix some names
substrate$CLASSCODE[ substrate$CLASSCODE == 'sand'] <- "SAND"
substrate$CLASSCODE[ substrate$CLASSCODE == 'cob'] <- "COB"
     
substrate_transect = substrate %>% group_by( YEAR, SITE, ZONE, SIDE, TRANSECT, SEGMENT, CLASSCODE) %>%
        summarise(count = sum(COUNT)) %>%
        # pivot wide and back to fill in zeros
        pivot_wider(., names_from = CLASSCODE, values_from = count, values_fill = 0) %>%
        pivot_longer(., BOULD:SAND , names_to = 'classcode', values_to = 'count') %>%
        # summarise at the segment level
        group_by(YEAR, SITE, ZONE, SIDE, TRANSECT, SEGMENT, classcode) %>%
        summarise(total.count = sum(count), segments = length(SEGMENT) ) %>%
        # summarise at the transect level
        group_by(YEAR,SITE, TRANSECT, ZONE,SIDE,classcode) %>% 
        summarise( tot.count = sum(total.count), N = length(segments))

# total count by transect. divide by

tran_count <-  substrate_transect %>% group_by(YEAR, SITE, TRANSECT, ZONE, SIDE) %>%
        summarise(transect.total = sum(tot.count))

substrate_site <-    substrate_transect %>%
        left_join(., tran_count) %>%
        mutate(prop = tot.count/transect.total) %>%
        # summarise at the year x site level
        group_by(YEAR, SITE, ZONE, classcode) %>%
        summarise(mean.prop = mean(prop)) %>%
        group_by(SITE, classcode) %>%
        summarise(mean_prop = mean(mean.prop), 
                  SD = sd(mean.prop),
                  N = length(SITE),
                  SE = sqrt(SD)/N)


substrate_site$subs = factor(substrate_site$classcode, levels= c( 'SAND','COB','BOULD','BEDRK' ))

substrate_site$SITE = factor(substrate_site$SITE, levels=c(  rev(c("Neah Bay",
                                                             "Tatoosh Island",
                                                             "Cape Alava",
                                                             "Cape Johnson",
                                                             "Destruction Island"))))

# substrate plot ####
substrate.plot <- ggplot(substrate_site,aes(x=SITE)) +
        geom_bar(aes(y=mean_prop, fill= subs), 
                 position="fill", stat="identity",width = 0.75, colour="black")+
        scale_fill_manual(values = site_col$col,
                          labels = c('Sand','Cobble','Boulder','Bedrock')) +  
        theme(legend.position = "left", aspect.ratio = 3/1)  +
        theme_bw()+
        ylab("Mean percent cover") +
        xlab("") +
        scale_x_discrete(expand = c(0,0), labels=c("DI","CJ","CA","TI","NB"))+
        scale_y_continuous(expand = c(0,0)) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 10 ),
              axis.text.x = element_text(size=10), 
              strip.text.x = element_text(size = 7))

library(ggpubr) 


graphics.off()

jpeg( paste0(Fig_Loc,'Substrate.jpg'), res=300, units='in', height = 4, width = 5)
ggarrange(
        substrate.plot,relief.plot,
        labels = c('a) Substrate','b) Relief'), 
        # labels = 'auto',
        font.label = list(face='plain', size = 10),
         hjust = c(-5.4,-7.8),
        # vjust = -1,
        nrow = 2, ncol=1,
        align = 'v'
)

dev.off()
