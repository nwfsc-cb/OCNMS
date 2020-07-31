#### Analysis of the UPC for the OCNMS survey region.

# Libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(reshape2)
library(janitor)
library(magrittr)

#base.dir <- "/Users/ole.shelton/GitHub/OCNMS"
base.dir <- "/Users/jameal.samhouri/Dropbox/Projects/In progress/OCNMS/OCNMS"
data.dir <- paste0(base.dir,"/Data/CSV_2015_on")
data.summary.output.dir <- paste0(base.dir,"/Data/Summarized data")
setwd(data.dir)

# 2015 data. tricky because in 2015 we took % cover data in quadrats, and that included things that we/PISCO now call substrate but also that we/PISCO now call % cover. exclude for now
# dat_2015 <- clean_names(read.csv("2015_OCNMSDataComplete_standardized_122116.csv"), case = "all_caps") %>%
#   remove_empty(c("rows", "cols")) 
# glimpse(dat_2015)
# head(dat_2015)
# upc_2015 <- dat_2015 %>% filter(PISCO_DATATYPE=="UPC")
# head(upc_2015)


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
#relief <- dat.2016.on.upc %>% filter(CATEGORY=="RELIEF")
#cover <- dat.2016.on.upc %>% filter(CATEGORY=="COVER")

 
### Analyze Substrate
  # Pad with zero observations. i.e., complete the data so that each segment has all 4 substrate categories
  cat <- data.frame(merge(unique(substrate$SITE),unique(substrate$CLASSCODE))); colnames(cat) <- c("SITE","CLASSCODE")
  dat.sub <- substrate %>% 
    group_by(YEAR,SITE,SEGMENT,TRANSECT,SIDE,ZONE,OBSERVER) %>% 
    summarise(a=length(SEGMENT)) %>% 
    full_join(.,cat,by="SITE") %>% 
    dplyr::select(-a) %>% 
    left_join(.,substrate)
  dat.sub$COUNT[is.na(dat.sub$COUNT)==TRUE] <- 0
  
  # order sites from south to north
  dat.sub$SITE <- factor(dat.sub$SITE,
               levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  
  # make year a factor
  dat.sub$YEAR <- as.factor(dat.sub$YEAR)
  
  # not sure if we have to account for SIDE 
  # (eg, multiple observers on a segment)? looks like no.
  tmp<-substrate %>% 
    group_by(YEAR,SITE,SEGMENT,TRANSECT,SIDE,ZONE,OBSERVER) %>% 
    summarise(a=length(SEGMENT), b=length(OBSERVER))
  which(tmp$a != tmp$b) # integer(0)
  # with(dat.sub, table(SITE, ZONE, TRANSECT, SIDE))
  # unique(dat.sub$SIDE)
  # length(which(dat.sub$SIDE != 1))/dim(dat.sub)[1]
  
  # but it does look like there are some issues with duplicate entries. so fix those
  dat.sub <- dat.sub %>% 
    group_by(YEAR,SITE,ZONE,SIDE,TRANSECT) %>%
    distinct(CLASSCODE, SEGMENT, COUNT, .keep_all = TRUE) %>% # remove duplicate entries based on CLASSCODE, SEGMENT, COUNT
    mutate(
      TOTAL_COUNT = sum(COUNT),
      PROPORTION = COUNT/TOTAL_COUNT
    )
  unique(dat.sub$TOTAL_COUNT) # 30 29
  View(dat.sub[which(dat.sub$TOTAL_COUNT !=30),])
  glimpse(dat.sub)
  
  # create summary df. 1) sum segments for each transect. 2) calculate summary stats for site-year-zone. 3) calculate summary stats for site-year. 4) calculate summary stats for site-zone. 5) calculate summary stats for site
  #proportion data so use binomial distribution for summary stats. https://sites.warnercnr.colostate.edu/gwhite/wp-content/uploads/sites/73/2017/04/BinomialDistribution.pdf
  
  # 1) sum segments for each transect to get total proportion of each substrate CLASSCODE
  sub.summary.year.transect <- dat.sub %>% 
    group_by(YEAR,SITE,ZONE,SIDE,TRANSECT,CLASSCODE) %>%
    dplyr::summarise(
      PROPORTION=sum(PROPORTION),
      .groups = "keep"
    )
  sub.summary.year.transect$YEAR <- as.factor(sub.summary.year.transect$YEAR)
  # order sites from south to north
  sub.summary.year.transect$SITE <- factor(sub.summary.year.transect$SITE,
                                       levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  glimpse(sub.summary.year.transect)
  
  # 2) calculate summary stats for site-year-zone
  sub.summary.year.zone <- sub.summary.year.transect %>% 
    group_by(YEAR,SITE,ZONE,CLASSCODE) %>%
    dplyr::summarise(
      MEAN=mean(PROPORTION),
      SD=sqrt(MEAN * (1 - MEAN)),
      N=length(PROPORTION),
      SE=SD/sqrt(N),
      .groups = "keep"
      )
  sub.summary.year.zone$YEAR <- as.factor(sub.summary.year.zone$YEAR)
  # order sites from south to north
  sub.summary.year.zone$SITE <- factor(sub.summary.year.zone$SITE,
                         levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  glimpse(sub.summary.year.zone)
  
  # 3) calculate summary stats for site-year
  sub.summary.year <- sub.summary.year.transect %>% 
    group_by(YEAR,SITE,CLASSCODE) %>%
    dplyr::summarise(
      MEAN=mean(PROPORTION),
      SD=sqrt(MEAN * (1 - MEAN)),
      N=length(PROPORTION),
      SE=SD/sqrt(N),
      .groups = "keep"
    )
  sub.summary.year$YEAR <- as.factor(sub.summary.year$YEAR)
  sub.summary.year$SITE <- factor(sub.summary.year$SITE,
                                       levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  glimpse(sub.summary.year)
  
  # 4) calculate summary stats for site-zone
  sub.summary.zone <- sub.summary.year.transect %>% 
    group_by(SITE,ZONE,CLASSCODE) %>%
    dplyr::summarise(
      MEAN=mean(PROPORTION),
      SD=sqrt(MEAN * (1 - MEAN)),
      N=length(PROPORTION),
      SE=SD/sqrt(N),
      .groups = "keep"
    )
  sub.summary.zone$SITE <- factor(sub.summary.zone$SITE,
                                  levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  glimpse(sub.summary.zone)
  
  
  
  # 5) calculate summary stats for site
  sub.summary.site <- sub.summary.year.transect %>% 
    group_by(SITE,CLASSCODE) %>%
    dplyr::summarise(
      MEAN=mean(PROPORTION),
      SD=sqrt(MEAN * (1 - MEAN)),
      N=length(PROPORTION),
      SE=SD/sqrt(N),
      .groups = "keep"
    )
  sub.summary.site$SITE <- factor(sub.summary.site$SITE,
                                  levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
  glimpse(sub.summary.site)

  #### END BASIC SUBSTRATE SUMMARIES.
  
  ### WRITE OUT SUBSTRATE DATA FRAMES
  
  
  write_csv(sub.summary.year.transect, paste0(data.summary.output.dir, "/SUBSTRATE summary by YEAR-SITE-DEPTH-SIDE-TRANSECT 2016-2019.csv"))
  write_rds(sub.summary.year.transect, paste0(data.summary.output.dir, "/SUBSTRATE summary by YEAR-SITE-DEPTH-SIDE-TRANSECT 2016-2019.rds"))
  write_csv(sub.summary.year.zone, paste0(data.summary.output.dir, "/SUBSTRATE summary by YEAR-SITE-DEPTH 2016-2019.csv"))
  write_rds(sub.summary.year.zone, paste0(data.summary.output.dir, "/SUBSTRATE summary by YEAR-SITE-DEPTH 2016-2019.rds"))
  write_csv(sub.summary.year, paste0(data.summary.output.dir, "/SUBSTRATE summary by YEAR-SITE 2016-2019.csv"))
  write_rds(sub.summary.year, paste0(data.summary.output.dir, "/SUBSTRATE summary by YEAR-SITE 2016-2019.rds"))
  write_csv(sub.summary.zone, paste0(data.summary.output.dir, "/SUBSTRATE summary by SITE-DEPTH 2016-2019.csv"))
  write_rds(sub.summary.zone, paste0(data.summary.output.dir, "/SUBSTRATE summary by SITE-DEPTH 2016-2019.rds"))
  write_csv(sub.summary.site, paste0(data.summary.output.dir, "/SUBSTRATE summary by SITE 2016-2019.csv"))
  write_rds(sub.summary.site, paste0(data.summary.output.dir, "/SUBSTRATE summary by SITE 2016-2019.rds"))
  
  #### END WRITING OUT SUBSTRATE DATA FRAMES
  
  #### MAKE SUBSTRATE PLOTS
  
  # 1) plots for site-year-zone. 
  # a) substrate type on x axis, site as facets
  sub.plot.year.zone_sub <- ggplot() +
    geom_jitter(data=sub.summary.year.transect,aes(y=PROPORTION,x=CLASSCODE,colour=YEAR),alpha=0.5, position=position_dodge(width=0.5))+
    geom_point(data=sub.summary.year.zone,aes(y=MEAN,x=CLASSCODE,fill=YEAR),color="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.year.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=CLASSCODE, colour=YEAR),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(SITE~ZONE) +
    theme_bw()
  sub.plot.year.zone_sub
  
  # b) site on x axis, substrate type as facets
  sub.plot.year.zone_site <- ggplot() +
    geom_jitter(data=sub.summary.year.transect,aes(y=PROPORTION,x=SITE, colour=YEAR),alpha=0.5, position=position_dodge(width=0.5) )+
    geom_point(data=sub.summary.year.zone,aes(y=MEAN,x=SITE,fill=YEAR),color="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.year.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=SITE, colour=YEAR),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(CLASSCODE~ZONE) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
  sub.plot.year.zone_site
  
  # c) compare zones, years as facets, lines connecting 5m v 10m
  
  sub.plot.year.zone_compare1 <- ggplot() +
    geom_jitter(data=sub.summary.year.transect,aes(y=PROPORTION,x=ZONE, colour=SITE),alpha=0.5, position=position_dodge(width=0.5))+
    geom_point(data=sub.summary.year.zone,aes(y=MEAN,x=ZONE,colour=SITE),size=2, position=position_dodge(width=0.5))+
    geom_line(data=sub.summary.year.zone,aes(y=MEAN,x=ZONE,colour=SITE),size=1)+
    geom_errorbar(data=sub.summary.year.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=ZONE,colour=SITE),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    facet_grid(YEAR~CLASSCODE) +
    theme_bw()
  sub.plot.year.zone_compare1
  
  # d) compare zones, CLASSCODE as facets, no lines connecting 5m v 10m
  sub.plot.year.zone_compare2 <- ggplot() +
    geom_jitter(data=sub.summary.year.transect,aes(y=PROPORTION,x=ZONE,colour=YEAR),alpha=0.3 , position=position_dodge(width=0.5))+
    geom_point(data=sub.summary.year.zone,aes(y=MEAN,x=ZONE,fill=YEAR),size=2,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.year.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=ZONE,colour=YEAR),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    facet_grid(CLASSCODE~SITE) +
    theme_bw()
  sub.plot.year.zone_compare2
  
  # 2) plots for site-year. 
  # a) substrate type on x axis, site as facets
  sub.plot.year_sub <- ggplot() +
    geom_point(data=sub.summary.year,aes(y=MEAN,x=CLASSCODE,fill=YEAR),colour="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.year,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=CLASSCODE, colour=YEAR),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(SITE~.) +
    theme_bw()
  sub.plot.year_sub
  

  # b) site on x axis, substrate type as facets
  sub.plot.year_site <- ggplot() +
    geom_point(data=sub.summary.year,aes(y=MEAN,x=SITE,fill=YEAR),color="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.year,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=SITE, colour=YEAR),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(CLASSCODE~.) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
  sub.plot.year_site
  
  # c) time series, CLASSCODE as facets
  
  sub.plot.year_ts1 <- ggplot() +
    geom_point(data=sub.summary.year,aes(y=MEAN,x=YEAR,colour=SITE, group=SITE),size=2, position=position_dodge(width=0.5))+
    geom_line(data=sub.summary.year,aes(y=MEAN,x=YEAR,colour=SITE, group=SITE),size=1, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.year,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=YEAR,colour=SITE, group=SITE),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    facet_grid(.~CLASSCODE) +
    theme_bw()
  sub.plot.year_ts1
  
  # d) time series, SITE as facets
  
  sub.plot.year_ts2 <- ggplot() +
    geom_point(data=sub.summary.year,aes(y=MEAN,x=YEAR,colour=CLASSCODE, group=CLASSCODE),size=2, position=position_dodge(width=0.5))+
    geom_line(data=sub.summary.year,aes(y=MEAN,x=YEAR,colour=CLASSCODE, group=CLASSCODE),size=1, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.year,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=YEAR,colour=CLASSCODE, group=CLASSCODE),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    facet_grid(.~SITE) +
    theme_bw()
  sub.plot.year_ts2
  
  # 3) plots for site-zone. 
  
  # a) substrate type on x axis, site as facets
  sub.plot.zone_sub <- ggplot() +
    geom_point(data=sub.summary.zone,aes(y=MEAN,x=CLASSCODE,fill=factor(ZONE)),colour="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=CLASSCODE, colour=factor(ZONE)),width=0.1, position=position_dodge(width=0.5))  +
    labs(fill='ZONE', colour='ZONE') +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(SITE~.) +
    theme_bw()
  sub.plot.zone_sub
  
  
  # b) site on x axis, substrate type as facets
  sub.plot.zone_site <- ggplot() +
    geom_point(data=sub.summary.zone,aes(y=MEAN,x=SITE,fill=factor(ZONE)),color="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=SITE, colour=factor(ZONE)),width=0.1, position=position_dodge(width=0.5))  +
    labs(fill='ZONE', colour='ZONE') +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(CLASSCODE~.) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
  sub.plot.zone_site
  
  # c) compare zones, years as facets, lines connecting 5m v 10m
  
  sub.plot.zone_compare1 <- ggplot() +
    geom_point(data=sub.summary.zone,aes(y=MEAN,x=factor(ZONE),colour=SITE, group=SITE),size=2, position=position_dodge(width=0.5))+
    geom_line(data=sub.summary.zone,aes(y=MEAN,x=factor(ZONE),colour=SITE, group=SITE),size=1, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=factor(ZONE),colour=SITE, group=SITE),width=0.1, position=position_dodge(width=0.5))  +
    xlab("ZONE") +
    scale_colour_viridis_d()+
    facet_grid(.~CLASSCODE) +
    theme_bw()
  sub.plot.zone_compare1
  
  # d) compare zones, CLASSCODE as facets, no lines connecting 5m v 10m
  
  sub.plot.zone_compare2 <- ggplot() +
    geom_point(data=sub.summary.zone,aes(y=MEAN,x=factor(ZONE),colour=CLASSCODE, group=CLASSCODE),size=2, position=position_dodge(width=0.5))+
    geom_line(data=sub.summary.zone,aes(y=MEAN,x=factor(ZONE),colour=CLASSCODE, group=CLASSCODE),size=1, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.zone,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=factor(ZONE),colour=CLASSCODE, group=CLASSCODE),width=0.1, position=position_dodge(width=0.5))  +
    xlab("ZONE") +
    scale_colour_viridis_d()+
    facet_grid(.~SITE) +
    theme_bw()
  sub.plot.zone_compare2
  
  # 4) plots for site
  
  # a) substrate type on x axis, site as facets
  sub.plot.site_sub <- ggplot() +
    geom_point(data=sub.summary.site,aes(y=MEAN,x=CLASSCODE, fill=CLASSCODE),colour="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.site,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=CLASSCODE, colour=CLASSCODE),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(SITE~.) +
    theme_bw()
  sub.plot.site_sub
  
  
  # b) site on x axis, substrate type as facets
  sub.plot.site_site <- ggplot() +
    geom_point(data=sub.summary.site,aes(y=MEAN,x=SITE,fill=SITE),color="black",size=3,shape=21, position=position_dodge(width=0.5))+
    geom_errorbar(data=sub.summary.site,
                  aes(ymin=MEAN-SE-1e-10,ymax=MEAN+SE,x=SITE, colour=SITE),width=0.1, position=position_dodge(width=0.5))  +
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_grid(CLASSCODE~.) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
  sub.plot.site_site
  
  
#### PRINT OUT THE PLOTS IN A SINGLE PDF
  
  pdf(file=paste0(base.dir,"/Plots/Substrate v2.pdf"),onefile=T)
  
  print(sub.plot.site_sub)
  print(sub.plot.site_site)
  
  print(sub.plot.zone_sub)
  print(sub.plot.zone_site)
  print(sub.plot.zone_compare1)
  print(sub.plot.zone_compare2)
  
  print(sub.plot.year_sub)
  print(sub.plot.year_site)
  print(sub.plot.year_ts1)
  print(sub.plot.year_ts2)
  
  print(sub.plot.year.zone_sub)  
  print(sub.plot.year.zone_site)
  print(sub.plot.year.zone_compare1)
  print(sub.plot.year.zone_compare2)
  
  dev.off()

###
##############
### LOOK AT RELIEF.
##############
  
  
#   
#   
# ###
# ##############
# ### LOOK AT PERCENT COVER FRACTIONS.
# ##############
#   # cover is the relevant data here.
#   
#   cat <- data.frame(merge(unique(cover$SITE),unique(cover$CLASSCODE))); colnames(cat) <- c("SITE","CLASSCODE")
#   dat.cov <- cover %>% group_by(YEAR,SITE,SEGMENT,TRANSECT,SIDE,ZONE,OBSERVER) %>% summarise(a=length(SEGMENT)) %>% 
#     full_join(.,cat,by="SITE") %>% dplyr::select(-a) %>% left_join(.,cover)
#   dat.cov$COUNT[is.na(dat.cov$COUNT)==T] <- 0
#   
#   cov.summary <- dat.cov %>% group_by(YEAR,SITE,ZONE,CLASSCODE) %>% dplyr::summarise(Mean=mean(COUNT),SD=sd(COUNT),N=length(COUNT),SE=SD/sqrt(N)) %>% as.data.frame()
#   cov.summary$YEAR <- as.factor(cov.summary$YEAR)
#   
#   cov.among.site <- cov.summary %>% group_by(YEAR,CLASSCODE,ZONE) %>% dplyr::summarise(among.mean=mean(Mean),among.sd=sd(Mean)) %>% arrange(desc(among.mean))
#   cov.order <- cov.among.site %>% group_by(CLASSCODE) %>% dplyr::summarise(MEAN=mean(among.mean)) %>% arrange(desc(MEAN))
#   
#   cov.summary$CLASSCODE <- factor(cov.summary$CLASSCODE,levels=cov.order$CLASSCODE)
#   cov.among.site$CLASSCODE <- factor(cov.among.site$CLASSCODE,levels=cov.order$CLASSCODE)
#   
#   cov.summary$SITE <- factor(cov.summary$SITE,
#                          levels=c("Destruction Island","Cape Johnson","Cape Alava","Tatoosh Island","Neah Bay"))
#   cov.summary$YEAR <- as.factor(cov.summary$YEAR)
#   
#   # Across site comparison
#   cov.among.plot <- ggplot(cov.among.site) +
#       geom_col(aes(y=among.mean,x=CLASSCODE,fill=YEAR),position="dodge") +
#       facet_wrap(~ZONE)+
#       theme_bw()+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=8))
#   
#   cov.by.site <- ggplot(cov.summary) +
#     geom_col(aes(y=Mean,x=CLASSCODE,fill=YEAR),position="dodge") +
#     facet_grid(SITE~ZONE)+
#     theme_bw()+
#     theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=8))
  
    