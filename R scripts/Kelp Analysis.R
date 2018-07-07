rm(list=ls())
options(max.print=99999)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(synchrony)
library(stats)

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
  kelp.coastwide.dat.raw<-kelp.coastwide.dat
area.available <- read.csv("WADNR kelp index map bathymetry, kelp & substrate data table.csv")
area.available[,2:ncol(area.available)] <- area.available[,2:ncol(area.available)] * 0.0001

survey.locations.linear.shore <- read.csv("Survey locations linear shore.csv")
survey.locations.linear.shore$distance.km <- survey.locations.linear.shore$Simplified.Distance..m. / 1000

## Coastwide summary of kelp
kelp.coastwide.dat <- kelp.coastwide.dat %>% filter(map_index >=15.1 & map_index <= 25.2) %>% 
         rename(year = year_) %>% filter(year<=2015) %>% group_by(year) %>% 
          summarise(total.area = sum(tot_can),nereo=sum(tot_ne_can),macro=sum(tot_ma_can)) %>% as.data.frame()
kelp.coastwide.dat[kelp.coastwide.dat < 0] <- NA

p <- ggplot(kelp.coastwide.dat) +
  geom_point(aes(y=total.area,x=year)) +
  geom_line(aes(y=total.area,x=year),linetype="dashed") +
  geom_smooth(aes(y=total.area,x=year),span=0.5,color="black",method="loess") +
  #facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
  #  ggtitle("1000m buffer") +
  theme_bw() 
print(p)

p.nereo <- ggplot(kelp.coastwide.dat) +
  geom_point(aes(y=nereo,x=year)) +
  geom_line(aes(y=nereo,x=year),linetype="dashed") +
  geom_smooth(aes(y=nereo,x=year),span=0.5,color="red",method="loess") +
  #facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
  #  ggtitle("1000m buffer") +
  theme_bw() 
print(p.nereo)

p.macro <- ggplot(kelp.coastwide.dat,aes(y=macro,x=year)) +
  geom_point() +
  geom_line(linetype="dashed") +
  geom_smooth(span=0.5,color="red",method="loess") +
  #facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
  #  ggtitle("1000m buffer") +
  theme_bw() 
print(p.macro)

A <- kelp.coastwide.dat %>% filter(year <= 2001) 
B <- kelp.coastwide.dat %>% filter(year >= 2002) 
sd(A$total.area)
sd(B$total.area)
####################
### THIS IS FOR THE SUM OF NEREO AND MACRO 
####################
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

####################
### THIS IS FOR THE NEREO AND MACRO SEPARATELY 
####################

kelp.dat.ne.ma <- kelp.coastwide.dat.raw %>% dplyr::select(kelp.map.index=map_index,nereo=tot_ne_can,macro=tot_ma_can,total=tot_can,year=year_)
kelp.dat.ne.ma <- merge(kelp.dat.ne.ma,weight.dat)
kelp.dat.ne.ma[kelp.dat.ne.ma== -9999] <- NA
kelp.dat.ne.ma <- kelp.dat.ne.ma %>% filter(year<=2015)

kelp.dat.ne.ma$Area.ne <- kelp.dat.ne.ma$nereo * kelp.dat.ne.ma$weight
kelp.dat.ne.ma$Area.ma <- kelp.dat.ne.ma$ma * kelp.dat.ne.ma$weight

kelp.ts.ne.ma <- kelp.dat.ne.ma %>% group_by(Site,year) %>% summarise(total.area.ne = sum(Area.ne),total.area.ma = sum(Area.ma))
kelp.ts.ne.ma$Site <- as.character(kelp.ts.ne.ma$Site)
kelp.ts.ne.ma$Site[kelp.ts.ne.ma$Site=="Chibadehl Rocks"] <- "Chibahdehl Rock"

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

kelp.ts.ne.ma$Site <-  factor(kelp.ts.ne.ma$Site, 
                            levels = site.order)

kelp.ts.ne.ma <- kelp.ts.ne.ma %>% mutate(Region = as.character(Site)) %>%
  mutate(Region = replace(Region,Site%in%c("Neah Bay","Chibahdehl Rock","Tatoosh Island"), "Northern")) %>%
  mutate(Region = replace(Region,Site%in%c("Anderson Point","Point of the Arches","Cape Alava"), "Central")) %>%                                                                  
  mutate(Region = replace(Region,Site%in%c("Cape Johnson","Rock 305","Teahwhit Head","Destruction Island SW"), "Southern")) %>%
  as.data.frame()
kelp.ts.ne.ma$Region <- as.factor(kelp.ts.ne.ma$Region)

kelp.ts.ne.ma$year <- as.numeric(as.character(kelp.ts.ne.ma$year))

kelp.ts.ne.ma <- kelp.ts.ne.ma %>% group_by(Site) %>% 
  summarise(Mean.ne=mean(total.area.ne,na.rm=T),SD.ne=sd(total.area.ne,na.rm=T), 
            Mean.ma=mean(total.area.ma,na.rm=T),SD.ma=sd(total.area.ma,na.rm=T)) %>%
  as.data.frame() %>% merge(.,kelp.ts.ne.ma) %>% 
  mutate(Dev.ne = (total.area.ne - Mean.ne) / SD.ne,Dev.ma = (total.area.ma - Mean.ma) / SD.ma) %>%
  arrange(Site,year)


# Plots by area
p1 <- ggplot(kelp.ts.ne.ma) +
  geom_point(aes(y=total.area.ne,x=year)) +
  geom_smooth(aes(y=total.area.ne,x=year),span=0.5,color="black",method="loess") +
  facet_wrap(~Site,ncol=2) +
  labs(y="kelp area(ha)") +
#  ggtitle("1000m buffer") +
  theme_bw() 
print(p1)

p2 <- ggplot(kelp.ts.ne.ma) +
  geom_point(aes(y=total.area.ne,x=year)) +
  geom_smooth(aes(y=total.area.ne,x=year),span=0.5,color="black",method="loess") +
  geom_point(aes(y=total.area.ma,x=year),color="red") +
  geom_smooth(aes(y=total.area.ma,x=year),span=0.5,color="red",method="loess") +
  facet_wrap(~Site,ncol=2,scales="free_y") +
  labs(y="kelp area(ha)") +
  #ggtitle("1000m buffer") +
  theme_bw() 
print(p2)

p3 <- ggplot(kelp.ts.ne.ma) +
  geom_point(aes(y=Dev.ne,x=year)) +
  geom_smooth(aes(y=Dev.ne,x=year),span=0.5,color="black",method="loess") +
  geom_point(aes(y=Dev.ma,x=year),color="red") +
  geom_smooth(aes(y=Dev.ma,x=year),span=0.5,color="red",method="loess") +
      facet_wrap(~Site,ncol=2) +
  labs(y="kelp area (mean standardized) ") +
  #  ggtitle("1000m buffer") +
  theme_bw() +
  geom_hline(yintercept =0,linetype="dashed")
print(p3)

bivariate.ne.ma <- ggplot(kelp.ts.ne.ma) +
  geom_point(aes(x=Dev.ne,y=Dev.ma,color=Region))+
  theme_bw() +
  geom_hline(yintercept =0,linetype="dashed") +
  geom_vline(xintercept =0,linetype="dashed") +
  labs(x="Nereocystis area (mean standardized)",y="Macrocystis area (mean standardized)") 
bivariate.ne.ma

pdf(file=paste(base.dir,"/Plots/Kelp time-series plots by species.pdf",sep=""),onefile=T,width=11,height=8.5)
  print(p)
  print(p1)
  print(p2)
  print(p3)
dev.off()
pdf(file=paste(base.dir,"/Plots/Kelp bivariate plots.pdf",sep=""),onefile=T,width=5,height=4)
  print(bivariate.ne.ma)
dev.off()

 
############


#################################################################################################
### MEAN, CV at each site.
#################################################################################################

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

kelp.start.ne.ma <- kelp.ts.ne.ma %>% filter(year <=1991) %>% group_by(Site) %>% 
                summarise(mean.init.ne= mean(total.area.ne,na.rm=T),mean.init.ma= mean(total.area.ma,na.rm=T))
kelp.ts.ne.ma <- merge(kelp.ts.ne.ma,kelp.start.ne.ma)
kelp.ts.ne.ma$abund.ratio.ne <- kelp.ts.ne.ma$total.area.ne / kelp.ts.ne.ma$mean.init.ne
kelp.ts.ne.ma$abund.ratio.ma <- kelp.ts.ne.ma$total.area.ma / kelp.ts.ne.ma$mean.init.ma
kelp.ts.ne.ma$log.ratio.ne <- log(kelp.ts.ne.ma$abund.ratio.ne)
kelp.ts.ne.ma$log.ratio.ma <- log(kelp.ts.ne.ma$abund.ratio.ma)

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


####################################################################
#### Calculate some synchrony metrics
locs <- survey.locations.linear.shore %>% filter(!Area %in% c("Pillar Point","Point Grenville")) %>% 
  dplyr::select(-Simplified.Distance..m.) %>% arrange(desc(distance.km))
loc.dist <- dist(locs$distance.km,diag=T)

site.order2 <- c(
  "Neah Bay",
  "Chibahdehl Rock",
  "Tatoosh Island",
  #"Cape Flattery",
  "Anderson Point",
  #"Makah Bay",
  "Point of the Arches",
  "Cape Alava",
  "Cape Johnson",
  "Rock 305",
  "Teahwhit Head",
  "Destruction Island")


# perform some analyses on total kelp (macro + nereo)
# Cast a data from to be site x raw data
kelp.ts.all$Site <- as.character(kelp.ts.all$Site)
kelp.ts.all$Site[kelp.ts.all$Site == "Destruction Island SW"] <- "Destruction Island"
kelp.ts.all.by.loc <- left_join(kelp.ts.all,locs,by=c("Site"="Area")) %>% arrange(desc(distance.km),year)

kelp.ts.all.by.loc$Site <- factor(kelp.ts.all.by.loc$Site, levels=site.order2)

kelp.dev <- dcast(kelp.ts.all.by.loc,year~Site,value.var = "Dev")
kelp.raw <- dcast(kelp.ts.all.by.loc,year~Site,value.var = "total.area")

N.rep <- 10

sync.dev.start <- list() 
sync.dev.end <- list() 
boot.dev.start <- NULL
boot.dev.end <- NULL

sync.dev.all <- community.sync(kelp.dev %>% select(-year) %>% select(-2),nrands=N.rep)
for(i in 1:9){
  sync.dev.start[[i]] <- community.sync(kelp.dev %>% filter(year <=2001) %>% select(-year)%>% select(-2) %>% select(-i),nrands=N.rep)
  sync.dev.end[[i]] <- community.sync(kelp.dev %>% filter(year >2001) %>% select(-year)%>% select(-2) %>% select(-i),nrands=N.rep)
  boot.dev.start[i] <- sync.dev.start[[i]]$obs  
  boot.dev.end[i]   <- sync.dev.end[[i]]$obs  
}
sync.dev.start[[10]] <- community.sync(kelp.dev %>% filter(year <=2001) %>% select(-year)%>% select(-2),nrands=N.rep) 
sync.dev.end[[10]]   <- community.sync(kelp.dev %>% filter(year >2001) %>% select(-year)%>% select(-2),nrands=N.rep)    

sd(boot.dev.start)
sd(boot.dev.end)

sync.raw.all <- community.sync(kelp.raw %>% select(-year)%>% select(-2),nrands=N.rep)
sync.raw.start <- community.sync(kelp.raw %>% filter(year <=2001) %>% select(-year)%>% select(-2),nrands=N.rep)
sync.raw.end <- community.sync(kelp.raw %>% filter(year >2001) %>% select(-year)%>% select(-2),nrands=N.rep)

######################################
# Pairwise correlations of kelp at each site.
MAT <- matrix(0,length(site.order2),length(site.order2)); MAT[lower.tri(MAT)] <- 1
pairwise.corr <- rbind(melt(cor(kelp.dev %>% filter(year <=2001) %>% select(-year)) * MAT) %>% mutate(period="early"),
                      melt(cor(kelp.dev %>% filter(year >2001) %>% select(-year)) * MAT) %>% mutate(period="late")) %>%
                      as.data.frame() %>%
                      rename(site.1 = Var1,site.2=Var2,COR = value) %>%
                      filter(!site.1 =="Chibahdehl Rock",!site.2=="Chibahdehl Rock",COR != 0)

# Distance Matrix for sites
locs <- survey.locations.linear.shore %>% filter(!Area %in% c("Pillar Point","Point Grenville")) %>% 
          dplyr::select(-Simplified.Distance..m.) %>% arrange(desc(distance.km))

loc.dist <- dist(locs$distance.km,diag=T,upper=T) %>% as.matrix()* MAT  %>% as.data.frame() 
colnames(loc.dist) = site.order2; 
loc.dist$site.1 = site.order2; 

pairwise.corr  <- melt(loc.dist,id.vars = "site.1") %>% rename(site.2=variable,DIST=value) %>%
                  filter(DIST > 0) %>%
                  left_join(pairwise.corr,.) %>%
                  filter(COR<0.999) %>%
                  #mutate(plot.lab=period) %>% 
                  mutate(plot.lab=case_when(period=="early" ~ "1989-2001",period=="late"~"2002-2015")) %>%
                  as.data.frame() 


###
## exponential decay
###

mod.early <- nls(COR ~ exp(-(DIST)/(V)),
                data=pairwise.corr %>% filter(period=="early"),
                start=list(V=10))
summary(mod.early)
mod.late <- nls(COR ~ exp(-(DIST)/(V)),
                 data=pairwise.corr %>% filter(period=="late"),
                 start=list(V=10))
summary(mod.late)

est.early <- as.numeric(mod.early$m$getAllPars())
pred.early <- data.frame(DIST = seq(0.01,100,length.out=500)) %>% mutate(period="early",PRED = exp(-(DIST)/(est.early)))
est.late <- as.numeric(mod.late$m$getAllPars())
pred.late<- data.frame(DIST = seq(0.01,100,length.out=500)) %>% mutate(period="late",PRED = exp(-(DIST)/(est.late)))


plot(PRED~DIST,pred.early,col="black",ylim=c(0,1))
par(new=T)
plot(PRED~DIST,pred.late,col="red",ylim=c(0,1))



theme_os2 <- function(base_size = 12, base_family = "") {
  theme_bw()+
    theme(
      text=element_text(size=11),
      #legend.title = element_blank(),
      legend.text  = element_text(size=7.5),
      legend.justification = c("left", "top"),
      #legend.key   = element_blank(),
      legend.key.size = unit(0.7, 'lines'),
      # legend.background =  element_rect(colour = "white"),
      legend.position   = c(0.7,0.98),
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
      plot.margin =        unit(c(0.2, 0.1, 0.2, 0.1), "lines")
    )
}  

Pairwise.plot  <-  ggplot(pairwise.corr) +
    geom_point(data=pairwise.corr,aes(y=COR,x=DIST,fill=plot.lab),shape=21)+
    geom_line(data=pred.early,aes(y=PRED,x=DIST),size=1.5) +
    geom_line(data=pred.late,aes(y=PRED,x=DIST),linetype="dashed",size=1.5) +
    scale_fill_manual(name="Period",values=c("black","white")) +
    ylim(-0.25,1)+
    ylab("Pairwise correlation")+
    xlab("Distance (km)") +
    theme_os2()

quartz(file = paste(base.dir,"/Plots/Pairwise Correlation Kelp.pdf",sep=""),type="pdf",dpi=300,height=4,width=4 )
  print(Pairwise.plot)
dev.off()









######################
### Go get some PDO and Upwelling information
######################

setwd(paste(base.dir,"/Data/oceanographic variables",sep=""))
pdo.dat <- read.csv("PDO.csv")
cui.dat <- read.csv("CUI index.csv")

# filter to include appropriate years and months
pdo.sum.dat <- pdo.dat %>% dplyr::select(-JAN,-FEB,-MAR,-OCT,-NOV,-DEC) %>% filter(YEAR >=1989)
cui.sum.dat <- cui.dat %>% dplyr::select(-jan,-feb,-mar,-oct,-nov,-dec,-lat,-long) %>% filter(year >=1989)

pdo.summer <- melt(pdo.sum.dat,id.vars = "YEAR") %>% group_by(YEAR) %>% summarise(mean.PDO=mean(value)) %>%
                mutate( period = case_when(YEAR <= 2001 ~ "early",YEAR > 2001 ~ "late"))
cui.summer <- melt(cui.sum.dat,id.vars = "year") %>% group_by(year) %>% summarise(mean.CUI=mean(value)) %>%
                mutate( period = case_when(year <= 2001 ~ "early",year > 2001 ~ "late"))

pdo.period <- pdo.summer %>% group_by(period) %>% dplyr::summarize(MEAN = mean(mean.PDO),SD=sd(mean.PDO))
cui.period <- cui.summer %>% group_by(period) %>% dplyr::summarize(MEAN = mean(mean.CUI),SD=sd(mean.CUI))

t.test(mean.PDO~period,data=pdo.summer)
t.test(mean.CUI~period,data=cui.summer)














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

