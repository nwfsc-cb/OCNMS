rm(list=ls())
options(max.print=99999)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)

# for OLE
 base.dir <- "/Users/ole.shelton/GitHub/OCNMS"

# for JAMEAL
#base.dir <- "~/Documents/GitHub/OCNMS/"

source(paste(base.dir,"/R scripts/multiplot.r",sep=""))

##################################################################################
##################################################################################
######-------------- KELP ANALYSIS ---------------------------------##############
##################################################################################
##################################################################################

# FILTER TO INCLUDE ONLY year <= 2015.

# Use discrete areas for 
setwd(paste(base.dir,"/Data/csv files",sep=""))

kelp.dat      <- read.csv("annual canopy area by index.csv")
weight.dat    <- read.csv("Kelp area index weights.csv")
kelp.coastwide.dat <- read.csv("kelp canopy all sites.csv")
area.available <- read.csv("WADNR kelp index map bathymetry, kelp & substrate data table.csv")
area.available[,2:ncol(area.available)] <- area.available[,2:ncol(area.available)] * 0.0001


## Coastwide summary of kelp
kelp.coastwide.dat <- kelp.coastwide.dat %>% filter(map_index >=15.1 & map_index <= 25.2) %>% 
         rename(year = year_) %>% filter(year<=2015) %>% group_by(year) %>% summarise(total.area = sum(tot_can)) %>% as.data.frame()

p <- ggplot(kelp.coastwide.dat,aes(y=total.area,x=year)) +
  geom_point() +
  geom_line(linetype="dashed") +
  geom_smooth(,span=0.5,color="black",method="loess") +
  #facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
  #  ggtitle("1000m buffer") +
  theme_bw() 
print(p)

A <- kelp.coastwide.dat %>% filter(year <= 2001) 
B <- kelp.coastwide.dat %>% filter(year >= 2002) 
sd(A$total.area)
sd(B$total.area)
#####


kelp.dat <- melt(kelp.dat,id.vars = "kelp.map.index")
kelp.dat <- kelp.dat %>% rename(year=variable)
kelp.dat$year <- substr(kelp.dat$year,2,5)
kelp.dat$year <- as.numeric(kelp.dat$year)
kelp.dat <- kelp.dat %>% filter(year<=2015)

weight.dat <- weight.dat %>% rename(Site = Site.100m.radius)
           
kelp.ts <- merge(kelp.dat,weight.dat)
kelp.ts$Area <- kelp.ts$value * kelp.ts$weight

A<- area.available %>% select(Index.Map.ID,Area.0to20m,Area.20to30m,Total.area.0to30m,Rock.area.0to20m,Rock.area.20to30m,Total.Rock.area0to30m) %>% as.data.frame()
kelp.ts <- merge(kelp.ts,A,
                 by.x=c("kelp.map.index"),by.y=c("Index.Map.ID"))

kelp.ts$area.avail.rock <- kelp.ts$weight * kelp.ts$Rock.area.0to20m
kelp.ts$area.avail.tot  <- kelp.ts$weight * kelp.ts$Area.0to20m

kelp.ts.all <- kelp.ts %>% group_by(Site,year) %>% summarise(total.area = sum(Area))

kelp.ts.all$Site <- as.character(kelp.ts.all$Site)
kelp.ts.all$Site[kelp.ts.all$Site == "Chibadehl Rocks"] <- "Chibahdehl Rock"

site.order <- c(
  "Chibahdehl Rock",
  "Neah Bay",
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

kelp.ts.all$Site <-  factor(kelp.ts.all$Site, 
                                   levels = site.order)

#V
kelp.ts.all <- kelp.ts.all %>% mutate(Region = as.character(Site)) %>%
  mutate(Region = replace(Region,Site%in%c("Neah Bay","Chibahdehl Rock","Tatoosh Island"), "Northern")) %>%
  mutate(Region = replace(Region,Site%in%c("Anderson Point","Point of the Arches","Cape Alava"), "Central")) %>%                                                                  
  mutate(Region = replace(Region,Site%in%c("Cape Johnson","Rock 305","Teahwhit Head","Destruction Island SW"), "Southern")) %>%
  as.data.frame()
kelp.ts.all$Region <- as.factor(kelp.ts.all$Region)

kelp.ts.all$year <- as.numeric(as.character(kelp.ts.all$year))

kelp.ts.site <- kelp.ts.all %>% group_by(Site) %>% summarise(Mean=mean(total.area),SD=sd(total.area)) %>%
                as.data.frame()

kelp.ts.all <- kelp.ts.all %>% group_by(Site) %>% summarise(Mean=mean(total.area),SD=sd(total.area)) %>%
                as.data.frame() %>% merge(.,kelp.ts.all) %>% mutate(Dev = (total.area - Mean) / SD)


# Plots by area

p1 <- ggplot(kelp.ts.all) +
  geom_point(aes(y=total.area,x=year)) +
  geom_smooth(aes(y=total.area,x=year),span=0.5,color="black",method="loess") +
  facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
#  ggtitle("1000m buffer") +
  theme_bw() 
print(p1)

p2 <- ggplot(kelp.ts.all) +
  geom_point(aes(y=total.area,x=year)) +
  geom_smooth(aes(y=total.area,x=year),span=0.5,color="black",method="loess") +
  facet_wrap(~Site,ncol=2,scales="free_y") +
  labs(y="kelp area(ha)") +
  #ggtitle("1000m buffer") +
  theme_bw() 
print(p2)

p3 <- ggplot(kelp.ts.all,aes(y=Dev,x=year)) +
  geom_point() +
  geom_smooth(span=0.5,color="black",method="loess") +
  facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
  #  ggtitle("1000m buffer") +
  theme_bw() +
  geom_hline(yintercept =0,linetype="dashed")
print(p3)

p4 <- ggplot(kelp.ts.all,aes(y=Dev,x=year)) +
  geom_point() +
  geom_line()+
  facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
  #  ggtitle("1000m buffer") +
  theme_bw() +
  geom_hline(yintercept =0,linetype="dashed")
print(p4)


pdf(file=paste(base.dir,"/Plots/Kelp time-series plots.pdf",sep=""),onefile=T,width=11,height=8.5)
  print(p)
  print(p1)
  print(p2)
  print(p3)
  print(p4)
dev.off()


############


#################################################################################################
### MEAN, CV at each site.

## Raw area data. 1000m buffer

kelp.ts.site$Site <-  factor(kelp.ts.site$Site,
                             levels = site.order)
kelp.ts.site$CV <- kelp.ts.site$SD / kelp.ts.site$Mean

X.AX <- element_text(angle=45,size=8,hjust=1)

A.1 <- ggplot(kelp.ts.site) +
  geom_bar(aes(y=Mean,x=Site),stat="identity") +
  labs(y="Mean Area (ha)") +
  theme_bw()  +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank()) 
  

A.2 <- ggplot(kelp.ts.site) +
  geom_bar(aes(y=CV,x=Site),stat="identity") +
  labs(x="Site") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1)) +
  theme_bw()  +
  theme(axis.text.x = X.AX)

# Proportional cover data
# kelp.summary.prop <- kelp.area %>% group_by(ESP.site.name,Buffer.radius) %>%
#   summarise(Mean=mean(rel.area),SD=sd(rel.area),N=length(rel.area)) %>%
#   mutate(SE.mean = SD/sqrt(N),CV=SD/Mean)
# kelp.summary.prop$ESP.site.name <-  factor(kelp.summary.prop$ESP.site.name,
#                                            levels = site.order)

# B.1 <- ggplot(kelp.ts.site) +
#   geom_bar(aes(y=Mean,x=Site),stat="identity") +
#   labs(y="Area",x=NULL) +
#   scale_y_continuous(limits=c(0,0.7),expand = c(0, 0)) +
#   theme_bw()  +
#   theme(axis.text.x = element_blank())
B.2 <- ggplot(kelp.ts.site) +
  geom_point(aes(y=CV,x=Mean),stat="identity")+
  labs(x="Mean Prop",y="CV") +
 # scale_x_continuous(limits=c(0,0.7),expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + #limits=c(0,0.85),
  theme_bw()

quartz(file = paste(base.dir,"/Plots/Kelp Area, CV.pdf",sep=""),type="pdf",dpi=300,height=6,width=7 )
  Layout= matrix(c(1,1,2,2,2,3,3,3,4,4),nrow=5,ncol=2,byrow=F)
  QQ <- list(A.1,A.2,B.2)
  multiplot(plotlist=QQ ,layout= Layout)
dev.off()





###### MAKE PLOT OF KELP TIME SERIES RELATIVE TO mean of values during first three years (89-91)

kelp.start <- kelp.ts.all %>% filter(year <=1991) %>% group_by(Site) %>% summarise(mean.init= mean(total.area))
kelp.ts.all <- merge(kelp.ts.all,kelp.start)
kelp.ts.all$abund.ratio <- kelp.ts.all$total.area / kelp.ts.all$mean.init
kelp.ts.all$log.ratio <- log(kelp.ts.all$abund.ratio)

theme_os <- function(base_size = 12, base_family = "") {
  theme_bw()+
    theme(
      text=element_text(size=11),
      legend.title = element_blank(),
      legend.text  = element_text(size=7.5),
      legend.justification = c("left", "top"),
      #legend.key   = element_blank(),
      legend.key.size = unit(0.7, 'lines'),
      # legend.background =  element_rect(colour = "white"),
      legend.position   = c(0.02,0.98),
      # #legend.text.align = 0,
      legend.key =         element_rect(fill = "white", color="white",size=0.5),
      panel.background =   element_rect(fill = "white", colour = "black",size=1.5),
      panel.border =       element_blank(),
      panel.grid.major =   element_blank(),
      panel.grid.minor =   element_blank(),
      # panel.spacing =       unit(0.25, "lines"),
      #strip.background =   element_rect(fill = "black", colour = "black"),
      strip.text.x = element_blank(),
      strip.background = element_blank(),
      plot.background =    element_rect(colour = "white"),
      plot.title =         element_text(size = rel(0.9),hjust = 0),
      plot.margin =        unit(c(0.2, 0.1, -0.7, 0.1), "lines")
    )
}  


COL   <- viridis(3,begin=0,end=0.7)
COL.2 <- viridis(4,begin=0,end=0.8)
y.lim=c(0,7.86)

K.index1 <- ggplot(kelp.ts.all %>% filter(Region =="Northern"),aes(x=year,y=abund.ratio,color=Site)) +
  geom_line(linetype="dashed") +
  geom_point() +
  geom_hline(yintercept = 1,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  #scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  ggtitle("a) Northern") +
  theme_os() #+ theme(legend.position="none")
K.index2 <- ggplot(kelp.ts.all %>% filter(Region =="Central"),aes(x=year,y=abund.ratio,color=Site)) +
  geom_line(linetype="dashed") +
  geom_point() +
  geom_hline(yintercept = 1,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  #scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  ggtitle("b) Central") +
  theme_os() #+ theme(legend.position="none")
K.index3 <- ggplot(kelp.ts.all %>% filter(Region =="Southern"),aes(x=year,y=abund.ratio,color=Site)) +
  geom_line(linetype="dashed") +
  geom_point() +
  geom_hline(yintercept = 1,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL.2) +
  #scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  ggtitle("c) Southern") +
  theme_os() #+ theme(legend.position="none")

quartz(file = paste(base.dir,"/Plots/Kelp Index by region.pdf",sep=""),type="pdf",dpi=300,height=6,width=4 )
Layout= matrix(c(1,2,3),nrow=3,ncol=1,byrow=F)
QQ <- list(K.index1,
           K.index2,
           K.index3)
multiplot(plotlist=QQ ,layout= Layout)
dev.off()


# setwd(paste(base.dir,"/Data/csv files",sep=""))
# 
# kelp.dat    <- read.csv("kelp_area.csv")
# area.dat    <- read.csv("site_area_by_depth.csv")
# 
# kelp.dat$ID <- "kelp"
# # kelp.dat$REF[kelp.dat$Year.surveyed <=1991] <- 1991
# # kelp.dat$REF[kelp.dat$Year.surveyed > 1991] <- kelp.dat$Year.surveyed[kelp.dat$Year.surveyed > 1991]
# 
# #Summarise area 
# area.dat$area.less.than.20 <- (area.dat$m.20.to.15 + area.dat$m.15.to.10 + area.dat$m.10.to.5 +area.dat$m.5.to.0) *0.0001
# area.dat$area.less.than.15 <- (area.dat$m.15.to.10 + area.dat$m.10.to.5  + area.dat$m.5.to.0) * 0.0001
# area.dat$area.less.than.10 <- (area.dat$m.10.to.5  + area.dat$m.5.to.0) * 0.0001
# area.dat$area.less.than.5  <- (area.dat$m.5.to.0 ) * 0.0001
# 
# ggplot(area.dat) +
#   geom_bar(aes(x=Site,y=area.less.than.15,fill=Buffer.radius),position="dodge",stat="identity")
# 
# 
# kelp.area <- group_by(kelp.dat,Year.surveyed,ESP.site.name,Buffer.radius) %>%
#                 summarise(.,tot.area=sum(Polygon.area.m2) * 0.0001) %>%
#                 as.data.frame()
# 
# 
# kelp.area <- merge(kelp.area,area.dat[,c("Site","Buffer.radius","area.less.than.20","area.less.than.15")],
#                    by.x=c("ESP.site.name","Buffer.radius"),by.y=c("Site","Buffer.radius"))
# 
# #temp <- kelp.area.ref[kelp.area.ref$REF == 1991,]
# #colnames(temp)[which(colnames(temp)=="tot.area")] <- "ref.area"
# #kelp.area.ref <- merge(kelp.area.ref,temp,by=c("ID","ESP.site.name","Buffer.radius"),all=T)
# kelp.area$rel.area <- kelp.area$tot.area / kelp.area$area.less.than.20
# 
# 
# ### Make time-series plot for each site
#   # Make site order from north to south
# site.order <- c(
#   "Neah Bay",
#   "Chibadehl Rocks",
#   "Tatoosh Island",
#   #"Cape Flattery",
#   "Anderson Point",
#   "Makah Bay",
#   "Point of the Arches",
#   "Cape Alava",
#   "Cape Johnson",
#   "Rock 305",
#   "Teahwhit Head",
#   "Destruction Island SW")
# 
# kelp.area$ESP.site.name <-  factor(kelp.area$ESP.site.name, 
#                                levels = site.order)
# # kelp.area.ref$ESP.site.name <-  factor(kelp.area.ref$ESP.site.name, 
# #                                levels = site.order)
# 
# ### START PLOT
# 
# pdf(file = paste(base.dir,"/Plots/Kelp trends.pdf",sep=""),onefile=T, width=4,height=10)
# 
#   temp.dat <- kelp.area[kelp.area$Buffer.radius == "1000 m",]
#   p <- ggplot(temp.dat,aes(y=tot.area,x=Year.surveyed)) +
#         geom_point() +
#         geom_smooth(span=0.5,color="black",method="loess") +
#         facet_wrap(~ESP.site.name,ncol=2) +
#         labs(y="kelp area(ha)") +
#         ggtitle("1000m buffer") +
#         theme_bw() 
#   print(p)
# 
#   temp.dat <- kelp.area[kelp.area$Buffer.radius == "500 m",]
#   p <- ggplot(temp.dat,aes(y=tot.area,x=Year.surveyed)) +
#         geom_point() +
#         geom_smooth(span=0.5,color="black",method="loess") +
#         facet_wrap(~ESP.site.name,ncol=2) +
#         labs(y="kelp area(ha)") +
#         ggtitle("500m buffer") +
#         theme_bw() 
#   print(p)
# 
#   #### Repeat time-series for standardization relative to the average of the first 3 year of the time series (1989-91)
#   temp.dat <- kelp.area[kelp.area$Buffer.radius == "1000 m",]
#   p <- ggplot(temp.dat,aes(y=rel.area,x=Year.surveyed)) +
#     geom_point() +
#     #geom_hline(yintercept=1,linetype=2) +
#     geom_smooth(span=0.5,color="black",method="loess") +
#     facet_wrap(~ESP.site.name,ncol=2) +
#     labs(y="relative kelp area") +
#     ggtitle("Area relative to total area < 20m (1000m buffer)") +
#     theme_bw() 
#   print(p)
# 
#   #### Repeat time-series for standardization relative to the average of the first 3 year of the time series (1989-91)
#   temp.dat <- kelp.area[ kelp.area$Buffer.radius == "500 m" ,]
#   p <- ggplot(temp.dat,aes(y=rel.area,x=Year.surveyed)) +
#     geom_point() +
#     geom_smooth(span=0.5,color="black",method="loess") +
#     #geom_hline(yintercept=1,linetype=2) +
#     facet_wrap(~ESP.site.name,ncol=2) +
#     labs(y="relative kelp area") +
#     ggtitle("Area relative to total area < 20m (500m buffer)") +
#     theme_bw() 
#   print(p)
# 
# #   temp.dat <- kelp.area.ref[kelp.area.ref$ID =="kelp" & kelp.area.ref$Buffer.radius == "500 m" &
# #                               kelp.area.ref$ESP.site.name != "Teahwhit Head",]
# #   p <- ggplot(temp.dat,aes(y=rel.area,x=Year,color=ESP.site.name)) +
# #     geom_point() +
# #     geom_line() +
# #     geom_smooth(span=0.5,color="black",method="loess") +
# #     geom_hline(yintercept=1,linetype=2) +
# # #    facet_wrap(~ESP.site.name,ncol=2) +
# #     labs(y="relative kelp area") +
# #     ggtitle("Area relative to 89-91 avg (500m buffer)") +
# #     theme_bw() 
# #   print(p)
# dev.off()
# 
# 
# kelp.combined <- kelp.area %>% filter(!ESP.site.name %in% c("Teahwhit Head","Anderson Point")) %>%
#                         group_by(Year.surveyed,Buffer.radius) %>% summarise(Area =sum(tot.area))
# 
# 
# tot.kelp.area.plot <- ggplot(kelp.combined %>% filter(Buffer.radius == "500 m"),aes(y=Area,x=Year.surveyed)) +
#   lims(x=c(1978,2015),y=c(0,200)) +
#   geom_point() +
#   geom_smooth(span=0.5,color="black",method="loess") +
#   #geom_hline(yintercept=1,linetype=2) +
#   #facet_wrap(~ESP.site.name,ncol=2) +
#   labs(y=expression("Kelp Area (ha)"),x="Year") +
#   ggtitle("Total area at index sites (500m buffer)") +
#   theme_bw() 
# 
# quartz(file = paste(base.dir,"/Plots/Total Kelp area 500m buffer.pdf",sep=""), width=5,height=3.5,dpi=300,type="pdf")
#   print(tot.kelp.area.plot)
# dev.off()
# 
# tot.kelp.area.plot <- ggplot(kelp.combined %>% filter(Buffer.radius == "1000 m"),aes(y=Area,x=Year.surveyed)) +
#   lims(x=c(1978,2015),y=c(0,400)) +
#   geom_point() +
#   geom_smooth(span=0.5,color="black",method="loess") +
#   #geom_hline(yintercept=1,linetype=2) +
#   #facet_wrap(~ESP.site.name,ncol=2) +
#   labs(y=expression("Kelp Area (ha)"),x="Year") +
#   ggtitle("Total area at index sites (1000m buffer)") +
#   theme_bw() 
# 
# quartz(file = paste(base.dir,"/Plots/Total Kelp area 1000m buffer.pdf",sep=""), width=5,height=3.5,dpi=300,type="pdf")
#   print(tot.kelp.area.plot)
# dev.off()
# 
# ####################################################################################
# ####################################################################################
# ### Derive summaries of Kelp at each site
# ####################################################################################
# ####################################################################################
# 
# ## Raw area data. 500m buffer
# kelp.summary <- kelp.area %>% group_by(ESP.site.name,Buffer.radius) %>% summarise(Mean=mean(tot.area),SD=sd(tot.area),N=length(tot.area)) %>%
#                   mutate(SE.mean = SD/sqrt(N),CV=SD/Mean)
# kelp.summary$ESP.site.name <-  factor(kelp.summary$ESP.site.name, 
#                                    levels = site.order)
# 
# X.AX <- element_text(angle=45,size=8,hjust=1)
# 
# A.1 <- ggplot(kelp.summary %>% filter(Buffer.radius=="500 m")) +
#     geom_bar(aes(y=Mean,x=ESP.site.name),stat="identity") +
#     geom_errorbar(aes(x=ESP.site.name,ymax=Mean+SD,ymin=Mean-SD),width=0) +
#     labs(y="Mean Area (ha)",x="Site") +
#     theme_bw()  +
#     theme(axis.text.x = X.AX) 
# 
# A.2 <- ggplot(kelp.summary %>% filter(Buffer.radius=="500 m")) +
#     geom_bar(aes(y=CV,x=ESP.site.name),stat="identity") +
#     labs(x="Site") +
#     scale_y_continuous(expand = c(0, 0),limits=c(0,0.85)) +
#     theme_bw()  +
#     theme(axis.text.x = X.AX) 
# 
# # Proportional cover data
# kelp.summary.prop <- kelp.area %>% group_by(ESP.site.name,Buffer.radius) %>% 
#   summarise(Mean=mean(rel.area),SD=sd(rel.area),N=length(rel.area)) %>%
#   mutate(SE.mean = SD/sqrt(N),CV=SD/Mean)
# kelp.summary.prop$ESP.site.name <-  factor(kelp.summary.prop$ESP.site.name, 
#                                       levels = site.order)
# 
# B.1 <- ggplot(kelp.summary.prop %>% filter(Buffer.radius=="500 m")) +
#   geom_bar(aes(y=Mean,x=ESP.site.name),stat="identity") +
#   geom_errorbar(aes(x=ESP.site.name,ymax=Mean+SE.mean,ymin=Mean-SE.mean),width=0) +
#   labs(y="Mean Proportion",x=NULL) +
#   scale_y_continuous(limits=c(0,0.7),expand = c(0, 0)) +
#   theme_bw()  +
#   theme(axis.text.x = element_blank()) 
# 
# B.2 <- ggplot(kelp.summary.prop %>% filter(Buffer.radius=="500 m")) +
#   geom_point(aes(y=CV,x=Mean),stat="identity")+
#   labs(x="Mean Proportion",y="CV") +
#   scale_x_continuous(limits=c(0,0.7),expand = c(0, 0)) +
#   scale_y_continuous(limits=c(0,0.85),expand = c(0, 0)) +
#   theme_bw()  
# 
# quartz(file = paste(base.dir,"/Plots/Kelp Area, CV 500m buffer.pdf",sep=""),type="pdf",dpi=300,height=6,width=7 )
#   Layout= matrix(c(1,1,2,2,2,3,3,3,4,4),nrow=5,ncol=2,byrow=F)
#   QQ <- list(B.1,A.2,B.2)
#   multiplot(plotlist=QQ ,layout= Layout)
# dev.off()
# 
# ################################################################################################
# ################################################################################################
# ################################################################################################
# ################################################################################################
# 
# ## Raw area data. 1000m buffer
# kelp.summary <- kelp.area %>% group_by(ESP.site.name,Buffer.radius) %>% summarise(Mean=mean(tot.area),SD=sd(tot.area),N=length(tot.area)) %>%
#   mutate(SE.mean = SD/sqrt(N),CV=SD/Mean)
# kelp.summary$ESP.site.name <-  factor(kelp.summary$ESP.site.name, 
#                                       levels = site.order)
# 
# X.AX <- element_text(angle=45,size=8,hjust=1)
# 
# A.1 <- ggplot(kelp.summary %>% filter(Buffer.radius=="1000 m")) +
#   geom_bar(aes(y=Mean,x=ESP.site.name),stat="identity") +
#   labs(y="Mean Area (ha)",x="Site") +
#   theme_bw()  +
#   theme(axis.text.x = X.AX) 
# 
# A.2 <- ggplot(kelp.summary %>% filter(Buffer.radius=="1000 m")) +
#   geom_bar(aes(y=CV,x=ESP.site.name),stat="identity") +
#   labs(x="Site") +
#   lims(ylim=c(0,0.85))+
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw()  +
#   theme(axis.text.x = X.AX) 
# 
# # Proportional cover data
# kelp.summary.prop <- kelp.area %>% group_by(ESP.site.name,Buffer.radius) %>% 
#   summarise(Mean=mean(rel.area),SD=sd(rel.area),N=length(rel.area)) %>%
#   mutate(SE.mean = SD/sqrt(N),CV=SD/Mean)
# kelp.summary.prop$ESP.site.name <-  factor(kelp.summary.prop$ESP.site.name, 
#                                            levels = site.order)
# 
# B.1 <- ggplot(kelp.summary.prop %>% filter(Buffer.radius=="1000 m")) +
#   geom_bar(aes(y=Mean,x=ESP.site.name),stat="identity") +
#   labs(y="Mean Prop",x=NULL) +
#   scale_y_continuous(limits=c(0,0.7),expand = c(0, 0)) +
#   theme_bw()  +
#   theme(axis.text.x = element_blank()) 
# B.2 <- ggplot(kelp.summary.prop %>% filter(Buffer.radius=="1000 m")) +
#   geom_point(aes(y=CV,x=Mean),stat="identity")+
#   labs(x="Mean Prop",y="CV") +
#   scale_x_continuous(limits=c(0,0.7),expand = c(0, 0)) +
#   scale_y_continuous(limits=c(0,0.85),expand = c(0, 0)) +
#   theme_bw()  
# 
# quartz(file = paste(base.dir,"/Plots/Kelp Area, CV 1000m buffer.pdf",sep=""),type="pdf",dpi=300,height=6,width=7 )
#   Layout= matrix(c(1,1,2,2,2,3,3,3,4,4),nrow=5,ncol=2,byrow=F)
#   QQ <- list(B.1,A.2,B.2)
#   multiplot(plotlist=QQ ,layout= Layout)
#dev.off()

