### 
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(lubridate)

dat <- read.csv("./Data/Offshore Data/AllHaulsbyRegion.Rockfish.May2020.csv") %>% 
          rename_all(tolower) 

#### Let's take a look at some basic summaries of where and when these samples occur
tab.1 <- dat %>% group_by(year,survey) %>% 
            summarise(n.tow=n()) %>% pivot_wider(values_from="n.tow",names_from="survey")

dat <- dat %>% mutate(month=month(as.Date(net_in_tim,"%m/%d/%y")),
                      day=day(as.Date(net_in_tim,"%m/%d/%y")),
                      day.of.year = yday(as.Date(net_in_tim,"%m/%d/%y")))

# When during each year does each survey happen?
ggplot(dat,aes(y=day.of.year,x=year)) + 
    geom_point(alpha=0.3)+
    facet_wrap(~survey)

# Make some basic maps of where the data occurs.
# set up lat and lon limits

lat.lims <- c(min(dat$latitude,na.rm=T),max(dat$latitude,na.rm=T))
lon.lims <- c(min(dat$long,na.rm=T),max(dat$long[dat$long<0],na.rm=T))

lat.lims.trim <- c(34,48.5)
lon.lims.trim <- c(-126,-121)

# Run Base_map.R
source("./R scripts/Base_map.R")

simple.map <- base_map + 
  scale_color_manual(values=c("blue","red","green"))+
  geom_point(data=dat,aes(x=long,y=latitude,color=survey),alpha=0.4,size=0.8) 

simple.map 
simple.map + facet_wrap(~year)

simple.map.trim <- base_map_trim + 
  scale_color_manual(values=c("blue","red","green"))+
  geom_point(data=dat,aes(x=long,y=latitude,color=survey),alpha=0.4,size=0.8) 

simple.map.trim.facet<- simple.map.trim + facet_wrap(~year,nrow=3)

## OK. # Lets collapse the rockfish data 
these <- which(colnames(dat) %in% c("brown_rock","unidentifi"))

dat.long <-pivot_longer(dat,cols = these[1]:these[2],names_to="sp",values_to = "count") %>%
              mutate(log.count=log10(count),ifelse(is.infinite(log.count)==T,NA,log.count),
                     pres=ifelse(count>0,1,0))

# Here are the species
dat.summary     <- dat.long %>% group_by(sp) %>% summarise(n.tow=n(),
                                        n.occur = sum(pres),
                                        p.occur = n.occur/n.tow,
                                        n.tot=sum(count),
                                        Mean=mean(count),
                                        SD=sd(count)) %>%
                                  arrange(desc(p.occur)) %>% as.data.frame()

dat.summary$sp <- factor(dat.summary$sp,levels=dat.summary$sp)

occur.freq <- ggplot(dat.summary,aes(y=p.occur,x=sp)) +
          geom_col() +
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

dat.summary.year <- dat.long %>% group_by(sp,year) %>% 
                                  summarise(n.tow=n(),
                                            n.occur = sum(pres),
                                            p.occur = n.occur/n.tow,
                                            n.tot=sum(count),
                                            Mean=mean(count),
                                            SD=sd(count)) %>%
                                  arrange(desc(p.occur))
dat.summary.year$sp <- factor(dat.summary.year$sp,levels=dat.summary$sp)

# Time-series for top 15 each species
ggplot(dat.summary.year %>% filter(sp %in% dat.summary$sp[1:16]), 
                          aes(y=p.occur,x=year))+
  geom_line() +
  geom_point() +
  facet_wrap(~sp,nrow=4) +
  theme_bw()


abund.facet <- ggplot(dat.summary.year %>% filter(sp %in% dat.summary$sp[1:16]), 
       aes(y=Mean,x=year))+
  geom_line() +
  geom_point() +
  facet_wrap(~sp,nrow=4,scales="free") +
  theme_bw()

abund.facet2 <- ggplot(dat.summary.year %>% filter(sp %in% dat.summary$sp[17:31]), 
                      aes(y=Mean,x=year))+
  geom_line() +
  geom_point() +
  facet_wrap(~sp,nrow=4,scales="free") +
  theme_bw()


log.abund.facet <- ggplot(dat.summary.year %>% filter(sp %in% dat.summary$sp[1:16]), 
       aes(y=log(Mean),x=year))+
  geom_line() +
  geom_point() +
  facet_wrap(~sp,nrow=4,scales="free") +
  theme_bw()

log.abund.facet2 <- ggplot(dat.summary.year %>% filter(sp %in% dat.summary$sp[17:32]), 
                          aes(y=log(Mean),x=year))+
  geom_line() +
  geom_point() +
  facet_wrap(~sp,nrow=4,scales="free") +
  theme_bw()

occur.facet <- ggplot(dat.summary.year %>% filter(sp %in% dat.summary$sp[1:16]), 
       aes(y=Mean,x=p.occur))+
  geom_point(alpha=0.5) +
  facet_wrap(~sp,nrow=4,scales="free") +
  theme_bw()

occur.facet2 <- ggplot(dat.summary.year %>% filter(sp %in% dat.summary$sp[17:32]), 
                      aes(y=Mean,x=p.occur))+
  geom_point(alpha=0.5) +
  facet_wrap(~sp,nrow=4,scales="free") +
  theme_bw()


#########################################
#########################################
#########################################
#########################################
#########################################
# Make some maps
N.sp <- nrow(dat.summary)
SP <- list()

for(i in 1:N.sp){
  SP[[as.character(dat.summary$sp[i])]] <- base_map_trim +
    geom_point(data=dat.long %>% filter(sp==dat.summary$sp[i]),
               aes(x=long,y=latitude,size=log.count),shape=21,color="red") +
    scale_size("Log Count",range=c(0.01,5))+
    geom_point(data=dat.long %>% filter(sp==dat.summary$sp[i],count==0),
              aes(x=long,y=latitude),shape="x",size=1,color="black") +
    facet_wrap(~year,nrow=3)+
    ggtitle(dat.summary$sp[i])
}


pdf(file="./Plots/_Offshore/Species maps.pdf",onefile=T,width=8.5,height=11)
  for(i in 1:N.sp){print(SP[i])}
dev.off()


pdf(file="./Plots/_Offshore/Survey maps.pdf",onefile=T,width=8.5,height=11)
  simple.map.trim.facet
dev.off()

pdf(file="./Plots/_Offshore/Fish time-series.pdf",onefile=T)
  print(occur.freq)  
  print(abund.facet)
  print(abund.facet2)
  print(log.abund.facet)
  print(log.abund.facet2)
  print(occur.facet)
  print(occur.facet2)
dev.off()



