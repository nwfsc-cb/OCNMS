#### Analysis of the UPC for the OCNMS survey region.

# Libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)
library(janitor)
library(magrittr)

#base.dir <- "/Users/ole.shelton/GitHub/OCNMS"
base.dir <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/OCNMS/OCNMS"
data.dir <- paste0(base.dir,"/Data/CSV_2015_on")
setwd(data.dir)

#dat.2015 <- read.csv("2015_OCNMSDataComplete_standardized_122116.csv")
dat.2016.on.upc <- clean_names(read.csv("NWFSC_UPC_ALLYEARS_data_2019.csv"), case = "all_caps") %>%
  remove_empty(c("rows", "cols")) 
# fix data entry issues
dat.2016.on.upc %<>% mutate(
  CLASSCODE = toupper(CLASSCODE)
)
unique(dat.2016.on.upc$CLASSCODE)
unique(dat.2016.on.upc$SITE)
glimpse(dat.2016.on.upc)

substrate.codes <- clean_names(read.csv("substrate_codes.csv"), case = "all_caps") %>%
  remove_empty(c("rows", "cols")) # js checked on 072720 and codes are same in 2019 data
  
substrate <- dat.2016.on.upc %>% filter(CATEGORY=="SUBSTRATE")
relief <- dat.2016.on.upc %>% filter(CATEGORY=="RELIEF")
#cover <- dat.2016.on.upc %>% filter(CATEGORY=="COVER")

 
### Analyze Substrate
  # Pad with zero observations
  cat <- data.frame(merge(unique(substrate$SITE),unique(substrate$CLASSCODE))); colnames(cat) <- c("SITE","CLASSCODE")
  dat.sub <- substrate %>% group_by(YEAR,SITE,SEGMENT,TRANSECT,SIDE,ZONE,OBSERVER) %>% summarise(a=length(SEGMENT)) %>% 
          full_join(.,cat,by="SITE") %>% dplyr::select(-a) %>% left_join(.,substrate)
  dat.sub$COUNT[is.na(dat.sub$COUNT)==TRUE] <- 0
  
  dat.sub$SITE <- factor(dat.sub$SITE,
               levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  dat.sub$YEAR <- as.factor(dat.sub$YEAR)
  
  sub.summary <- dat.sub %>% group_by(YEAR,SITE,ZONE,CLASSCODE) %>% dplyr::summarise(Mean=mean(COUNT),SD=sd(COUNT),N=length(COUNT),SE=SD/sqrt(N))
  sub.summary$YEAR <- as.factor(sub.summary$YEAR)

  #### END BASIC SUMMARIES.
  
  ####################
  ####################
  # do we want to summarise by transect before calculating means and other stats? js thinks yes
  ####################
  ####################
  
  # plot of number of occurrences of substrate types for each site and year (substrate type on x axis, site as facets)
  sub.plot <- ggplot() +
      geom_jitter(data=dat.sub,aes(y=COUNT,x=CLASSCODE,color=YEAR),width=0.2,height=0.01,alpha=0.3 )+
      geom_point(data=sub.summary,aes(y=Mean,x=CLASSCODE,fill=YEAR),color="black",size=3,shape=21)+
      geom_errorbar(data=sub.summary,
                  aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=CLASSCODE),width=0.1)  +
      facet_grid(SITE~ZONE) +
      theme_bw()
  sub.plot
  
  # plot of number of occurrences of substrate types for each site and year (site on x axis, substrate type as facets)
  sub.plot.site <- ggplot() +
    geom_jitter(data=dat.sub,aes(y=COUNT,x=SITE,color=YEAR),width=0.2,height=0.01,alpha=0.5 )+
    geom_point(data=sub.summary,aes(y=Mean,x=SITE,fill=YEAR),color="black",size=3,shape=21)+
    geom_errorbar(data=sub.summary,
                  aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=SITE),width=0.1)  +
    facet_grid(CLASSCODE~ZONE) +
    theme_bw()
  sub.plot.site
  
  sub.plot.zone1 <- ggplot() +
    geom_jitter(data=dat.sub,aes(y=COUNT,x=ZONE),width=0.2,height=0.01,alpha=0.5 )+
    geom_point(data=sub.summary,aes(y=Mean,x=ZONE,color=SITE),size=2)+
    geom_line(data=sub.summary,aes(y=Mean,x=ZONE,color=SITE),size=2)+
    geom_errorbar(data=sub.summary,
                  aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=ZONE,color=SITE),width=0.1)  +
    facet_grid(YEAR~CLASSCODE) +
    theme_bw()
  sub.plot.zone1
  
  sub.plot.zone2 <- ggplot() +
    geom_jitter(data=dat.sub,aes(y=COUNT,x=ZONE,color=YEAR),width=0.2,height=0.01,alpha=0.3 )+
    geom_point(data=sub.summary,aes(y=Mean,x=ZONE,fill=YEAR),size=3,shape=21,color="black")+
    #geom_line(data=sub.summary,aes(y=Mean,x=ZONE),size=2)+
    geom_errorbar(data=sub.summary,
                  aes(ymin=Mean-SE-1e-10,ymax=Mean+SE,x=ZONE,fill=YEAR),color="black",width=0.1)  +
    facet_grid(CLASSCODE~SITE) +
    theme_bw()
  sub.plot.zone2

  
  pdf(file=paste0(base.dir,"/Plots/Substrate.pdf"),onefile=T)
  print(sub.plot)  
  print(sub.plot.site)
  print(sub.plot.zone1)
  print(sub.plot.zone2)
  dev.off()

###
##############
### LOOK AT RELIEF.
##############
  
  
  
  
###
##############
### LOOK AT PERCENT COVER FRACTIONS.
##############
  # cover is the relevant data here.
  
  cat <- data.frame(merge(unique(cover$SITE),unique(cover$CLASSCODE))); colnames(cat) <- c("SITE","CLASSCODE")
  dat.cov <- cover %>% group_by(YEAR,SITE,SEGMENT,TRANSECT,SIDE,ZONE,OBSERVER) %>% summarise(a=length(SEGMENT)) %>% 
    full_join(.,cat,by="SITE") %>% dplyr::select(-a) %>% left_join(.,cover)
  dat.cov$COUNT[is.na(dat.cov$COUNT)==T] <- 0
  
  cov.summary <- dat.cov %>% group_by(YEAR,SITE,ZONE,CLASSCODE) %>% dplyr::summarise(Mean=mean(COUNT),SD=sd(COUNT),N=length(COUNT),SE=SD/sqrt(N)) %>% as.data.frame()
  cov.summary$YEAR <- as.factor(cov.summary$YEAR)
  
  cov.among.site <- cov.summary %>% group_by(YEAR,CLASSCODE,ZONE) %>% dplyr::summarise(among.mean=mean(Mean),among.sd=sd(Mean)) %>% arrange(desc(among.mean))
  cov.order <- cov.among.site %>% group_by(CLASSCODE) %>% dplyr::summarise(MEAN=mean(among.mean)) %>% arrange(desc(MEAN))
  
  cov.summary$CLASSCODE <- factor(cov.summary$CLASSCODE,levels=cov.order$CLASSCODE)
  cov.among.site$CLASSCODE <- factor(cov.among.site$CLASSCODE,levels=cov.order$CLASSCODE)
  
  cov.summary$SITE <- factor(cov.summary$SITE,
                         levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  cov.summary$YEAR <- as.factor(cov.summary$YEAR)
  
  # Across site comparison
  cov.among.plot <- ggplot(cov.among.site) +
      geom_col(aes(y=among.mean,x=CLASSCODE,fill=YEAR),position="dodge") +
      facet_wrap(~ZONE)+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=8))
  
  cov.by.site <- ggplot(cov.summary) +
    geom_col(aes(y=Mean,x=CLASSCODE,fill=YEAR),position="dodge") +
    facet_grid(SITE~ZONE)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=8))
  
    