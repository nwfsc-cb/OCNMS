rm(list=ls())
options(max.print=99999)
library(dplyr)
library(ggplot2)

##################################################################################
##################################################################################
######-------------- KELP ANALYSIS ---------------------------------##############
##################################################################################
##################################################################################

base.dir <- "/Users/ole.shelton/GitHub/OCNMS"
setwd(paste(base.dir,"/Data/csv files",sep=""))

kelp.dat    <- read.csv("kelp_area.csv")
area.dat    <- read.csv("site_area_by_depth.csv")

kelp.dat$ID <- "kelp"
kelp.dat$REF[kelp.dat$Year.surveyed <=1991] <- 1991
kelp.dat$REF[kelp.dat$Year.surveyed > 1991] <- kelp.dat$Year.surveyed[kelp.dat$Year.surveyed > 1991]

#Summarise area 
area.dat$area.less.than.20 <- (area.dat$m.20.to.15 + area.dat$m.15.to.10 + area.dat$m.10.to.5 +area.dat$m.5.to.0) *0.0001
area.dat$area.less.than.15 <- (area.dat$m.15.to.10 + area.dat$m.10.to.5  + area.dat$m.5.to.0) * 0.0001
area.dat$area.less.than.10 <- (area.dat$m.10.to.5  + area.dat$m.5.to.0) * 0.0001
area.dat$area.less.than.5  <- (area.dat$m.5.to.0 ) * 0.0001

kelp.area <- group_by(kelp.dat,Year.surveyed,ID,REF,ESP.site.name,Buffer.radius) %>%
                summarise(.,tot.area=sum(Polygon.area.m2) * 0.0001) %>%
                as.data.frame()
kelp.area.ref <- group_by(kelp.area,REF,ID,ESP.site.name,Buffer.radius) %>%
                    summarise(.,tot.area=mean(tot.area) ) %>%
                    as.data.frame()

temp <- kelp.area.ref[kelp.area.ref$REF == 1991,]
colnames(temp)[which(colnames(temp)=="tot.area")] <- "ref.area"
kelp.area.ref <- merge(kelp.area.ref,temp,by=c("ID","ESP.site.name","Buffer.radius"),all=T)
kelp.area.ref$rel.area <- kelp.area.ref$tot.area / kelp.area.ref$ref.area
kelp.area.ref$Year <- kelp.area.ref$REF.x

### Make time-series plot for each site
  # Make site order from north to south
site.order <- c(
  "Neah Bay",
  "Chibadehl Rocks",
  "Tatoosh Island",
  #"Cape Flattery",
  "Anderson Point",
  "Makah Bay",
  "Point of the Arches",
  "Cape Alava",
  "Cape Johnson",
  "Rock 305",
  "Teahwhit Head",
  "Destruction Island SW")

kelp.area$ESP.site.name <-  factor(kelp.area$ESP.site.name, 
                               levels = site.order)
kelp.area.ref$ESP.site.name <-  factor(kelp.area.ref$ESP.site.name, 
                               levels = site.order)

### START PLOT

pdf(file = paste(base.dir,"/Plots/Kelp trends.pdf",sep=""),onefile=T, width=4,height=10)

  temp.dat <- kelp.area[kelp.area$ID =="kelp" & kelp.area$Buffer.radius == "1000 m",]
  p <- ggplot(temp.dat,aes(y=tot.area,x=Year.surveyed)) +
        geom_point() +
        geom_smooth(span=0.5,color="black",method="loess") +
        facet_wrap(~ESP.site.name,ncol=2) +
        labs(y="kelp area(ha)") +
        ggtitle("1000m buffer") +
        theme_bw() 
  print(p)

  temp.dat <- kelp.area[kelp.area$ID =="kelp" & kelp.area$Buffer.radius == "500 m",]
  p <- ggplot(temp.dat,aes(y=tot.area,x=Year.surveyed)) +
        geom_point() +
        geom_smooth(span=0.5,color="black",method="loess") +
        facet_wrap(~ESP.site.name,ncol=2) +
        labs(y="kelp area(ha)") +
        ggtitle("500m buffer") +
        theme_bw() 
  print(p)

  #### Repeat time-series for standardization relative to the average of the first 3 year of the time series (1989-91)
  temp.dat <- kelp.area.ref[kelp.area.ref$ID =="kelp" & kelp.area.ref$Buffer.radius == "1000 m" &
                            kelp.area.ref$ESP.site.name != "Teahwhit Head",]
  p <- ggplot(temp.dat,aes(y=rel.area,x=Year)) +
    geom_point() +
    geom_hline(yintercept=1,linetype=2) +
    geom_smooth(span=0.5,color="black",method="loess") +
    facet_wrap(~ESP.site.name,ncol=2) +
    labs(y="relative kelp area") +
    ggtitle("Area relative to 89-91 avg (1000m buffer)") +
    theme_bw() 
  print(p)

  #### Repeat time-series for standardization relative to the average of the first 3 year of the time series (1989-91)
  temp.dat <- kelp.area.ref[kelp.area.ref$ID =="kelp" & kelp.area.ref$Buffer.radius == "500 m" &
                              kelp.area.ref$ESP.site.name != "Teahwhit Head",]
  p <- ggplot(temp.dat,aes(y=rel.area,x=Year)) +
    geom_point() +
    geom_smooth(span=0.5,color="black",method="loess") +
    geom_hline(yintercept=1,linetype=2) +
    facet_wrap(~ESP.site.name,ncol=2) +
    labs(y="relative kelp area") +
    ggtitle("Area relative to 89-91 avg (500m buffer)") +
    theme_bw() 
  print(p)

  temp.dat <- kelp.area.ref[kelp.area.ref$ID =="kelp" & kelp.area.ref$Buffer.radius == "500 m" &
                              kelp.area.ref$ESP.site.name != "Teahwhit Head",]
  p <- ggplot(temp.dat,aes(y=rel.area,x=Year,color=ESP.site.name)) +
    geom_point() +
    geom_line() +
    geom_smooth(span=0.5,color="black",method="loess") +
    geom_hline(yintercept=1,linetype=2) +
#    facet_wrap(~ESP.site.name,ncol=2) +
    labs(y="relative kelp area") +
    ggtitle("Area relative to 89-91 avg (500m buffer)") +
    theme_bw() 
  print(p)
dev.off()


















