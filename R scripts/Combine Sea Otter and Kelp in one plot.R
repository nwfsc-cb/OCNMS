library(viridis)
library(grid)
### Combining Kelp and Sea otter data for publication plots

# for OLE 
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Kelp Analysis.R",sep=""))
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Sea otter time series script.R",sep=""))
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"

otter.kern.dat<- read.csv(paste(base.dir,"Data/csv files/Kernel otter abundances; kern=10.2.csv",sep=""))

# # Relevant data frames for plots are 
  #  kelp.coastwise.dat
  #  kelp.ts.all
  #  otter.kern.dat
  #  nwfsc.otter.by.year
  nwfsc.otter.by.year$Year <- as.numeric(nwfsc.otter.by.year$Year)

# Start with making cumulative totals for the entire Olympic coast of WA
otter.kern.dat$location <- as.character(otter.kern.dat$location)
otter.kern.dat$location[otter.kern.dat$location=="Chibadehl Rock"] <- "Chibadehl Rocks"

  NOM <- c(
    "Neah Bay",
    "Chibadehl Rocks",
    "Tatoosh Island",
    "Anderson Point",
    "Point of the Arches",
    "Cape Alava",
    "Cape Johnson",
    "Rock 305",
    "Teahwhit Head",
    "Destruction Island"
  )
  
  otter.kern.dat$location <- factor(otter.kern.dat$location,levels=NOM)
  
## Color palette
COL   <- viridis(3,begin=0,end=0.7)
COL.2 <- viridis(4,begin=0,end=0.8)
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

  x.lim <- c(min(otter.kern.dat$Year)-1,max(otter.kern.dat$Year))
   
SPAN = 0.25  
   
O.1 <- ggplot(nwfsc.otter.by.year,aes(x=Year,y=Total)) +
        geom_line(linetype="dashed") +
        geom_smooth(span=SPAN,method="loess",se=F,color=grey(0.4) )+
        geom_point() +        
        ylab("Sea otters") +
        xlab("")+ 
        scale_x_continuous(limits = x.lim) +
        theme_os() 
O.2 <- ggplot(otter.kern.dat %>% filter(Region=="Northern"),aes(x=Year,y=tot.pop,color=location)) +
          geom_line(linetype="dashed") +
          geom_smooth(span=SPAN,method="loess",se=F) +
          geom_point() +
          scale_colour_manual(name="location",values=COL) +
          scale_x_continuous(limits = x.lim) +      
          #ylab("Sea otters") +
          xlab("")+
          ylab("") +
          ggtitle("a) Northern") +
          theme_os() #+ theme(legend.position="none")
O.3 <- ggplot(otter.kern.dat %>% filter(Region=="Central"),aes(x=Year,y=tot.pop,color=location)) +
            geom_line(linetype="dashed") +
            geom_smooth(span=SPAN,method="loess",se=F) +
            geom_point() +        
            scale_colour_manual(name="location",values=COL) +
            scale_x_continuous(limits = x.lim) +        
            ylab("Sea otters (individuals)") +
            ggtitle("b) Central") +
            xlab("")+
            #ylab("") +
            theme_os() #+ theme(legend.position="none")
          
O.4 <- ggplot(otter.kern.dat %>% filter(Region=="Southern"),aes(x=Year,y=tot.pop,color=location)) +
            geom_line(linetype="dashed") +
            geom_smooth(span=SPAN,method="loess",se=F) +
            geom_point() +        
            scale_colour_manual(name="location",values=COL.2) +
            scale_x_continuous(limits = x.lim) +          
            #ylab("Sea otters") +
            ggtitle("c) Southern") +
            xlab("") + #+ theme(legend.position="none")
            ylab("")  +
            theme_os()

y.lim=c(-4,4)

Otter.index2 <- ggplot(otter.kern.dat %>% filter(Region=="Northern"),aes(x=Year,y=log.ratio,color=location)) +
  geom_line(linetype="dashed") +
  geom_smooth(span=SPAN,method="loess",se=F) +
  geom_point() +
  geom_hline(yintercept = 0,linetype="dotted")+ 
  scale_colour_manual(name="location",values=COL) +
  scale_x_continuous(limits = x.lim) +
  coord_cartesian(ylim = y.lim)  +
    #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  ggtitle("a) Northern") +
  theme_os() #+ theme(legend.position="none")
Otter.index3 <- ggplot(otter.kern.dat %>% filter(Region=="Central"),aes(x=Year,y=log.ratio,color=location)) +
  geom_line(linetype="dashed") +
  geom_smooth(span=SPAN,method="loess",se=F) +
  geom_point() +        
  geom_hline(yintercept = 0,linetype="dotted")+
  scale_colour_manual(name="location",values=COL) +
  scale_x_continuous(limits = x.lim) +        
  coord_cartesian(ylim = y.lim)  +
  ylab("Sea otters (log index)") +
  ggtitle("b) Central") +
  xlab("")+
  #ylab("") +
  theme_os() #+ theme(legend.position="none")
Otter.index4 <- ggplot(otter.kern.dat %>% filter(Region=="Southern"),aes(x=Year,y=log.ratio,color=location)) +
  geom_line(linetype="dashed") +
  geom_smooth(span=SPAN,method="loess",se=F) +
  geom_point() +
  geom_hline(yintercept = 0,linetype="dotted")+
  scale_colour_manual(name="location",values=COL.2) +
  scale_x_continuous(limits = x.lim) +          
  coord_cartesian(ylim = y.lim)  +
  #ylab("Sea otters") +
  ggtitle("c) Southern") +
  xlab("") + #+ theme(legend.position="none")
  ylab("")  +
  theme_os()

###########################################################
###########################################################
###########################################################
###########################################################
y.lim <- c(-2.5,2.5)

K.1 <- ggplot(kelp.coastwide.dat,aes(x=year,y=total.area)) +
         geom_point() +
         geom_line(linetype="dashed") +
          geom_smooth(span=SPAN,method="loess",se=F,color=grey(0.4) )+
         scale_x_continuous(limits = x.lim) +
         ylab("Kelp canopy area (ha)") +
         xlab("Year")+
        theme_os() +
        theme(plot.title=element_text(color="white")) +
        theme(legend.position="none")

K.2 <- ggplot(kelp.ts.all %>% filter(Region=="Northern"),aes(x=year,y=Dev,color=Site)) +
          geom_line(linetype="dashed") +
          geom_hline(yintercept = 0,linetype="dotted",color="black") +
          geom_smooth(span=SPAN,method="loess",se=F) +
          geom_point() +        
          scale_colour_manual(name="Site",values=COL) +
          scale_x_continuous(limits = x.lim) +
          scale_y_continuous(limits = y.lim) +
          xlab("")+
          ylab("") +
          ggtitle(" ") +       
          theme_os() +
          theme(plot.title=element_text(color="white")) +
          theme(legend.position="none")
K.3 <- ggplot(kelp.ts.all %>% filter(Region=="Central"),aes(x=year,y=Dev,color=Site)) +
          geom_hline(yintercept = 0,linetype="dotted",color="black") +
          geom_line(linetype="dashed") +
          geom_smooth(span=SPAN,method="loess",se=F) +
          geom_point() +
          scale_colour_manual(name="Site",values=COL) +
          scale_x_continuous(limits = x.lim) +
          scale_y_continuous(limits = y.lim) +
          xlab("")+
          ylab("Kelp Deviation") +
          theme_os() +
          theme(plot.title=element_text(color="white")) +
          theme(legend.position="none")
 
K.4 <- ggplot(kelp.ts.all %>% filter(Region=="Southern"),aes(x=year,y=Dev,color=Site)) +
          geom_hline(yintercept = 0,linetype="dotted",color="black") +
          geom_line(linetype="dashed") +
          geom_smooth(span=SPAN,method="loess",se=F) +
          geom_point() +
          scale_colour_manual(name="Site",values=COL.2) +
          scale_x_continuous(limits = x.lim) +
          scale_y_continuous(limits = y.lim) +
          ylab("") +
          xlab("")+
          theme_os()+
          ggtitle("b) Central") + 
          theme(plot.title=element_text(color="white")) +
           theme(legend.position="none")
##### ALTERNATE KELP TREND PLOTTING 

y.lim=c(-2,2)

K.index1 <- ggplot(kelp.ts.all %>% filter(Region =="Northern"),aes(x=year,y=log.ratio,color=Site)) +
  geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(linetype="dashed") +
  geom_point() +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  theme_os() +
  ggtitle("a) Northern") + 
  theme(plot.title=element_text(color="white")) +
  theme(legend.position="none")
K.index2 <- ggplot(kelp.ts.all %>% filter(Region =="Central"),aes(x=year,y=log.ratio,color=Site)) +
  geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(linetype="dashed") +
  geom_point() +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("Kelp area (log index)") +
  ggtitle(" ") +        
  theme_os() +
  ggtitle("b) Central") + 
  theme(plot.title=element_text(color="white")) +
  theme(legend.position="none")
K.index3 <- ggplot(kelp.ts.all %>% filter(Region =="Southern"),aes(x=year,y=log.ratio,color=Site)) +
  geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(linetype="dashed") +
  geom_point() +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL.2) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  ggtitle(" ") +        
  theme_os() +
  ggtitle("c) Southern") + 
  theme(plot.title=element_text(color="white")) +
  theme(legend.position="none")



quartz(file = paste(base.dir,"/Plots/Otters and Kelp coastwide.pdf",sep=""),type="pdf",dpi=300,height=6,width=4 )
Layout= matrix(c(1,2),nrow=2,ncol=1,byrow=F)
QQ <- list(O.1,
           K.1)
multiplot(plotlist=QQ ,layout= Layout)
dev.off()


quartz(file = paste(base.dir,"/Plots/Otters and Kelp centered by region.pdf",sep=""),type="pdf",dpi=300,height=8,width=7 )
  Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=F)
  QQ <- list(O.2,O.3,O.4,
             K.2,K.3,K.4)
  multiplot(plotlist=QQ ,layout= Layout)
dev.off()

quartz(file = paste(base.dir,"/Plots/Otters and Kelp index by region.pdf",sep=""),type="pdf",dpi=300,height=8,width=7 )
Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=F)
QQ <- list(O.2,O.3,O.4,
           K.index1,K.index2,K.index3)
multiplot(plotlist=QQ ,layout= Layout)
dev.off()

quartz(file = paste(base.dir,"/Plots/Otters index and Kelp index by region.pdf",sep=""),type="pdf",dpi=300,height=8,width=7 )
Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=F)
QQ <- list(Otter.index2,Otter.index3,Otter.index4,
           K.index1,K.index2,K.index3)
multiplot(plotlist=QQ ,layout= Layout)
dev.off()


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################


# CROSS CORRELATIONS BETWEEN OTTERS AND KELP

# 
# head(otter.kern.dat)
# head(kelp.ts.all)

### Pretty up some names
kelp.ts.all$Site <- as.character(kelp.ts.all$Site)
otter.kern.dat$location <- as.character(otter.kern.dat$location)
kelp.ts.all$Site[kelp.ts.all$Site=="Destruction Island SW"] <- "Destruction Island"
otter.kern.dat$location[otter.kern.dat$location=="Chibadehl Rock"] <- "Chibadehl Rocks"

otter.kern.dat <- otter.kern.dat %>% dplyr::rename(otter.numb = tot.pop)
kelp.otter.dat <- merge(kelp.ts.all,otter.kern.dat,by.y=c("location","Region","Year"),by.x=c("Site","Region","year"),all=T)




kelp.otter.dat$Site <- factor(kelp.otter.dat$Site,levels=NOM)


C1 <- ggplot(kelp.otter.dat) +
        geom_point(aes(y=otter.numb,x=Dev))+
        facet_wrap(~Site,scales="free")
C1        


######################
## Kelp cross-correlation & moving window cross correlation



# drop Chib Rocks
A <- dcast(kelp.ts.all %>% filter(Site != "Chibadehl Rocks"),year ~ Site,value.var="Dev")
# Add in missing years of data 
A <- merge(A,data.frame(year=seq(min(A$year),max(A$year))),all=T)
YEARS <- sort(unique(A$year))
WIN <- 6

cor.window.dat <- NULL
for( i in 1:(nrow(A)-WIN)){
  temp <- A %>% filter(year>=YEARS[i],year<(YEARS[i]+WIN))
  COR <- cor(temp[,2:ncol(temp)],use="complete.obs")
  COR <- COR * lower.tri(COR)
  cor.window.dat <- rbind(cor.window.dat,data.frame(start.year = YEARS[i],window =WIN,  melt(COR)))
}

cor.window.dat <- cor.window.dat %>% dplyr::rename(Site.1=Var1,Site.2=Var2) %>% filter(value!=1,value != 0)

S.R <- kelp.ts.all %>% group_by(Site,Region) %>% summarise(length(Site)) %>% select(Site,Region)
cor.window.dat <- merge(cor.window.dat,S.R,by.y="Site",by.x="Site.2")
cor.window.dat <- cor.window.dat %>% dplyr::rename(Region.2=Region)
cor.window.dat <- merge(cor.window.dat,S.R,by.y="Site",by.x="Site.1")
cor.window.dat <- cor.window.dat %>% dplyr::rename(Region.1=Region)

cor.window.dat$pair <- ""
cor.window.dat$pair[cor.window.dat$Region.1 =="Northern" & cor.window.dat$Region.2 =="Northern"] <- "North"
cor.window.dat$pair[cor.window.dat$Region.1 =="Northern" & cor.window.dat$Region.2 =="Central"] <- "N.C"
cor.window.dat$pair[cor.window.dat$Region.1 =="Central"  & cor.window.dat$Region.2 =="Northern"] <- "N.C"
cor.window.dat$pair[cor.window.dat$Region.1 =="Northern" & cor.window.dat$Region.2 =="Southern"] <- "N.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Southern" & cor.window.dat$Region.2 =="Northern"] <- "N.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Central" & cor.window.dat$Region.2 =="Central" ]  <- "Central"
cor.window.dat$pair[cor.window.dat$Region.1 =="Southern" & cor.window.dat$Region.2 =="Central"]  <- "C.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Central"  & cor.window.dat$Region.2 =="Southern"] <- "C.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Southern" & cor.window.dat$Region.2 =="Southern"] <- "South"


COL   <- viridis(3,begin=0,end=0.7)
SPAN <- 0.5
kelp.cor.1 <- ggplot(cor.window.dat %>% filter(pair %in% c("North","Central","South"))) +
    geom_point(aes(y=value,x=start.year,color=pair),alpha=0.5) +
    geom_smooth(aes(y=value,x=start.year,color=pair),method="loess",span=SPAN,se=F) +
    scale_colour_manual(name="Region",values=COL) +
  geom_hline(yintercept = 0,linetype="dotted") +
    theme_os()

kelp.cor.2 <- ggplot(cor.window.dat %>% filter(pair %in% c("N.C","C.S","N.S"))) +
  geom_point(aes(y=value,x=start.year,color=pair),alpha=0.5) +
  geom_smooth(aes(y=value,x=start.year,color=pair),method="loess",span=SPAN,se=F) +
  scale_colour_manual(name="Region",values=COL) +
  geom_hline(yintercept = 0,linetype="dotted") +
  theme_os()

Layout= matrix(c(1,2),nrow=2,ncol=1,byrow=F)
QQ <- list(kelp.cor.1,
           kelp.cor.2)
multiplot(plotlist=QQ ,layout= Layout)

# for JAMEALtheme_js

# setwd("~/Documents/Github/OCNMS/Data/csv files")
# base.dir <- "~/Documents/Github/OCNMS/"
