library(AICcmodavg)
library(viridis)
library(grid)
### Combining Kelp and Sea otter data for publication plots

# for OLE 
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Kelp Analysis.R",sep=""))
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Sea otter time series script.R",sep=""))
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"

# for JAMEAL
# base.dir <- "~/Documents/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Kelp Analysis.R",sep=""))
# base.dir <- "~/Documents/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Sea otter time series script.R",sep=""))
# base.dir <- "~/Documents/GitHub/OCNMS/"

otter.kern.dat<- read.csv(paste(base.dir,"Data/csv files/Kernel otter abundances; kern=10.2.csv",sep=""))

# # Relevant data frames for plots are 
  #  kelp.coastwise.dat
  #  kelp.ts.all
  #  otter.kern.dat
  #  nwfsc.otter.by.year
  nwfsc.otter.by.year$Year <- as.numeric(nwfsc.otter.by.year$Year)
  # make index for coastwide otters.
  nwfsc.otter.by.year$Mean <- nwfsc.otter.by.year %>%  filter(Year >= 1989, Year<=1991) %>% summarize(Mean=mean(Total)) %>% as.numeric()
  nwfsc.otter.by.year <- nwfsc.otter.by.year %>% mutate(abund.ratio=Total/Mean,log.ratio= log(abund.ratio))
  
  kelp.coastwide.dat[,"Mean"] <- kelp.coastwide.dat %>% filter(year >= 1989, year<=1991) %>% summarize(Mean=mean(total.area))
  kelp.coastwide.dat[,"Mean.ne"] <- kelp.coastwide.dat %>% filter(year >= 1989, year<=1991) %>% summarize(Mean.ne=mean(nereo,na.rm=T))
  kelp.coastwide.dat[,"Mean.ma"] <- kelp.coastwide.dat %>% filter(year >= 1989, year<=1991) %>% summarize(Mean.ma=mean(macro,na.rm=T))
  kelp.coastwide.dat <- kelp.coastwide.dat %>% 
      mutate(abund.ratio=total.area/Mean,log.ratio= log(abund.ratio),
             abund.ratio.ne=nereo/Mean.ne,log.ratio.ne= log(abund.ratio.ne),
             abund.ratio.ma=macro/Mean.ma,log.ratio.ma= log(abund.ratio.ma))

# Start with making cumulative totals for the entire Olympic coast of WA
#otter.kern.dat$location <- as.character(otter.kern.dat$location)
#otter.kern.dat$location[otter.kern.dat$location=="Chibadehl Rock"] <- "Chibahdehl Rocks"

  NOM <- c(
    "Chibahdehl Rock",
    "Neah Bay",
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
  
  ### ADD MEAN VALUE FOR 89-2001, and 2002-2015
  
  otter.mean.summary <- rbind(otter.kern.dat %>% mutate(Start=1989) %>% filter(Year >=1989,Year<=2001) %>% group_by(location,Start) %>% summarize(otter.mean=mean(tot.pop)) %>% as.data.frame(),
        otter.kern.dat %>%mutate(Start=2002) %>% filter(Year >=2002,Year<=2015) %>% group_by(location,Start) %>% summarize(otter.mean=mean(tot.pop)) %>% as.data.frame())
  

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
        #    geom_smooth(span=SPAN,method="loess",se=F,color=grey(0.4) )+
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
O.2
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

##3 Otter.index with no smoothes

y.lim=c(-4,4)

Otter.index2.no.s <- ggplot() +
  geom_line(data=otter.kern.dat %>% filter(Region=="Northern"),mapping=aes(x=Year,y=log.ratio,color=location),linetype="dashed") +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_point(data=otter.kern.dat %>% filter(Region=="Northern"),mapping=aes(x=Year,y=log.ratio,color=location)) +
  geom_hline(yintercept = 0,linetype="dotted")+ 
  scale_colour_manual(name="location",values=COL) +
  scale_x_continuous(limits = x.lim) +
  coord_cartesian(ylim = y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  ggtitle("a) Northern") +
  theme_os() +#+ theme(legend.position="none")
  geom_line(mapping=aes(x=Year,y=log.ratio),data=nwfsc.otter.by.year)  

Otter.index3.no.s <- ggplot() +
  geom_line(data=otter.kern.dat %>% filter(Region=="Central"),mapping=aes(x=Year,y=log.ratio,color=location),linetype="dashed") +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_point(data=otter.kern.dat %>% filter(Region=="Central"),mapping=aes(x=Year,y=log.ratio,color=location)) +        
  geom_hline(yintercept = 0,linetype="dotted")+
  scale_colour_manual(name="location",values=COL) +
  scale_x_continuous(limits = x.lim) +        
  coord_cartesian(ylim = y.lim)  +
  ylab("Sea otters (log index)") +
  ggtitle("b) Central") +
  xlab("")+
  #ylab("") +
  theme_os() +
  geom_line(mapping=aes(x=Year,y=log.ratio),data=nwfsc.otter.by.year)  

Otter.index4.no.s <- ggplot() +
  geom_line(data=otter.kern.dat %>% filter(Region=="Southern"),mapping=aes(x=Year,y=log.ratio,color=location),linetype="dashed") +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_point(data=otter.kern.dat %>% filter(Region=="Southern"),mapping=aes(x=Year,y=log.ratio,color=location)) +
  geom_hline(yintercept = 0,linetype="dotted")+
  scale_colour_manual(name="location",values=COL.2) +
  scale_x_continuous(limits = x.lim) +          
  coord_cartesian(ylim = y.lim)  +
  #ylab("Sea otters") +
  ggtitle("c) Southern") +
  xlab("") + #+ theme(legend.position="none")
  ylab("")  +
  theme_os() +
  geom_line(mapping=aes(x=Year,y=log.ratio),data=nwfsc.otter.by.year)  

###########################################################
###########################################################
###########################################################
###########################################################
y.lim <- c(-2.5,2.5)

K.1 <- ggplot(kelp.coastwide.dat,aes(x=year,y=total.area)) +
         geom_point() +
         geom_line(linetype="dashed") +
        #  geom_smooth(span=SPAN,method="loess",se=F,color=grey(0.4) )+
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
          ggtitle("d)") +
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
          ggtitle("e)") +
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
          ggtitle("f)") + 
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


# Kelp index no smooth


y.lim=c(-2,2)

K.index1.no.s <- ggplot() +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(data=kelp.ts.all %>% filter(Region =="Northern"),mapping=aes(x=year,y=log.ratio,color=Site),linetype="dashed") +
  geom_point(data=kelp.ts.all %>% filter(Region =="Northern"),mapping=aes(x=year,y=log.ratio,color=Site)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  ggtitle("d) ") + 
  xlab("")+
  ylab("") +
  theme_os() +
  #theme(plot.title=element_text(color="white")) +
  theme(legend.position="none") +
  geom_line(mapping=aes(x=year,y=log.ratio),data=kelp.coastwide.dat)  

K.index2.no.s <- ggplot() +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(data=kelp.ts.all %>% filter(Region =="Central"),mapping=aes(x=year,y=log.ratio,color=Site),linetype="dashed") +
  geom_point(data=kelp.ts.all %>% filter(Region =="Central"),mapping=aes(x=year,y=log.ratio,color=Site)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("Kelp area (log index)") +
  theme_os() +
  ggtitle("e) ") + 
  #theme(plot.title=element_text(color="white")) +
  theme(legend.position="none") +
  geom_line(mapping=aes(x=year,y=log.ratio),data=kelp.coastwide.dat)  


K.index3.no.s <- ggplot() +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(data=kelp.ts.all %>% filter(Region =="Southern"),mapping=aes(x=year,y=log.ratio,color=Site),linetype="dashed") +
  geom_point(data=kelp.ts.all %>% filter(Region =="Southern"),mapping=aes(x=year,y=log.ratio,color=Site)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL.2) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  theme_os() +
  ggtitle("f) ") + 
  #theme(plot.title=element_text(color="white")) +
  theme(legend.position="none") +
  geom_line(mapping=aes(x=year,y=log.ratio),data=kelp.coastwide.dat)  

###############################################################
#### REPEAT WITH MACROCYSTIS ONLY
###############################################################

K.MA.index1.no.s <- ggplot() +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(data=kelp.ts.ne.ma %>% filter(Region =="Northern"),mapping=aes(x=year,y=log.ratio.ma,color=Site),linetype="dashed") +
  geom_point(data=kelp.ts.ne.ma %>% filter(Region =="Northern"),mapping=aes(x=year,y=log.ratio.ma,color=Site)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  ggtitle("d) ") + 
  xlab("")+
  ylab("") +
  theme_os() +
  #theme(plot.title=element_text(color="white")) +
  theme(legend.position="none") +
  geom_line(mapping=aes(x=year,y=log.ratio.ma),data=kelp.coastwide.dat)  

K.MA.index2.no.s <- ggplot() +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(data=kelp.ts.ne.ma %>% filter(Region =="Central"),mapping=aes(x=year,y=log.ratio.ma,color=Site),linetype="dashed") +
  geom_point(data=kelp.ts.ne.ma %>% filter(Region =="Central"),mapping=aes(x=year,y=log.ratio.ma,color=Site)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("Kelp area (log index)") +
  theme_os() +
  ggtitle("e) ") + 
  #theme(plot.title=element_text(color="white")) +
  theme(legend.position="none") +
  geom_line(mapping=aes(x=year,y=log.ratio.ma),data=kelp.coastwide.dat)  


K.MA.index3.no.s <- ggplot() +
  #geom_smooth(span=SPAN,method="loess",se=F) +
  geom_line(data=kelp.ts.ne.ma %>% filter(Region =="Southern"),mapping=aes(x=year,y=log.ratio.ma,color=Site),linetype="dashed") +
  geom_point(data=kelp.ts.ne.ma %>% filter(Region =="Southern"),mapping=aes(x=year,y=log.ratio.ma,color=Site)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  scale_colour_manual(name="Site",values=COL.2) +
  scale_x_continuous(limits = x.lim) +
  scale_y_continuous(limits=y.lim)  +
  #ylab("Sea otters") +
  xlab("")+
  ylab("") +
  theme_os() +
  ggtitle("f) ") + 
  #theme(plot.title=element_text(color="white")) +
  theme(legend.position="none") +
  geom_line(mapping=aes(x=year,y=log.ratio.ma),data=kelp.coastwide.dat)  

#################################################################################

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


quartz(file = paste(base.dir,"/Plots/Otters index and Kelp index by region (no smoothes).pdf",sep=""),type="pdf",dpi=300,height=8,width=7 )
Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=F)
QQ <- list(Otter.index2.no.s,Otter.index3.no.s,Otter.index4.no.s,
           K.index1.no.s,K.index2.no.s,K.index3.no.s)
multiplot(plotlist=QQ ,layout= Layout)
dev.off()

quartz(file = paste(base.dir,"/Plots/Otters index and MACRO index by region (no smoothes).pdf",sep=""),type="pdf",dpi=300,height=8,width=7 )
Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=F)
QQ <- list(Otter.index2.no.s,Otter.index3.no.s,Otter.index4.no.s,
           K.MA.index1.no.s,K.MA.index2.no.s,K.MA.index3.no.s)
multiplot(plotlist=QQ ,layout= Layout)
dev.off()


#######################################################################################
#######################################################################################
#######################################################################################
 #####   Change in Kelp versus Change in Otters
#######################################################################################
#######################################################################################
#######################################################################################

kelp.ts.all$Site <- as.character(kelp.ts.all$Site)
kelp.ts.all$Site[kelp.ts.all$Site=="Destruction Island SW"] <-"Destruction Island"

kelp.ts.ne.ma$Site <- as.character(kelp.ts.ne.ma$Site)
kelp.ts.ne.ma$Site[kelp.ts.ne.ma$Site=="Destruction Island SW"] <-"Destruction Island"

kelp.otter.dat <- merge(otter.kern.dat %>% select(location,Year,tot.pop,Region),
                          kelp.ts.all %>% select(Site,year,total.area,Region),
                          by.x=c("location","Year","Region"),
                          by.y=c("Site","year","Region"),all=T)

kelp.MA.otter.dat <- merge(otter.kern.dat %>% select(location,Year,tot.pop,Region),
                        kelp.ts.ne.ma %>% select(Site,year,total.area.ma,Region),
                        by.x=c("location","Year","Region"),
                        by.y=c("Site","year","Region"),all=T)


kelp.otter.dat <- kelp.otter.dat %>% dplyr::rename(otter.n=tot.pop,kelp.area=total.area)
kelp.otter.dat$log.otter <- log(kelp.otter.dat$otter.n)
kelp.otter.dat$log.kelp  <- log(kelp.otter.dat$kelp.area)

kelp.MA.otter.dat <- kelp.MA.otter.dat %>% dplyr::rename(otter.n=tot.pop,kelp.area=total.area.ma)
kelp.MA.otter.dat$log.otter <- log(kelp.MA.otter.dat$otter.n)
kelp.MA.otter.dat$log.kelp  <- log(kelp.MA.otter.dat$kelp.area)

temp <- expand.grid(Year=min(kelp.otter.dat$Year):max(kelp.otter.dat$Year),location=sort(unique(kelp.otter.dat$location)))
kelp.otter.dat <- merge(kelp.otter.dat,temp,all=T)
kelp.MA.otter.dat <- merge(kelp.MA.otter.dat,temp,all=T)

k.o.dat <- NULL
reg.coef1 <- NULL
reg.coef2 <- NULL
kelp.cv   <- NULL
for(i in 1:length(NOM)){
  temp <- kelp.otter.dat %>% filter(location == NOM[i]) 
  temp$log.diff.otter <- NA
  temp$log.diff.otter[1:(nrow(temp)-1)] <- log(temp$otter.n[2:nrow(temp)] / temp$otter.n[1:(nrow(temp)-1)])
  temp$log.diff.kelp <- NA
  temp$log.diff.kelp[1:(nrow(temp)-1)] <- log(temp$kelp.area[2:nrow(temp)] / temp$kelp.area[1:(nrow(temp)-1)])
  k.o.dat <- rbind(k.o.dat,temp)

  BREAK <- 2001
  temp <- kelp.MA.otter.dat %>% filter(location == NOM[i],Year >= 1989,Year<= BREAK) 
  
  mod <- lm(log.otter~Year,data=temp)  
  reg.coef1 <- rbind(reg.coef1,data.frame(Site=NOM[i],Start=1989,slope.otter=coef(summary(mod))["Year","Estimate"],slope.otter.se=coef(summary(mod))["Year","Std. Error"]))
  
  mod <- lm(log.kelp~Year,data=temp)  
  reg.coef2 <- rbind(reg.coef2,data.frame(Site=NOM[i],Start=1989,slope.kelp=coef(summary(mod))["Year","Estimate"],slope.kelp.se=coef(summary(mod))["Year","Std. Error"]))
  
  temp <- temp %>% filter(Year<=BREAK)
  A<- matrix(sample(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values),10000,replace = T),10,1000)
  B<- c(quantile(apply(A,1,sd),probs=c(0.025,0.975)))
  lower.95.sd <- B[1]
  upper.95.sd <- B[2]
  C <- mean(apply(A,1,sd),na.rm=T)

  kelp.cv <- rbind(kelp.cv,data.frame(Site=NOM[i],Start=1989, 
                                      kelp.cv= sd(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values)) / mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.boot= C/ mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.lower = lower.95.sd/ mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.upper = upper.95.sd/ mean(temp$kelp.area,na.rm=T),                                  
                                      kelp.mean= mean(temp$kelp.area,na.rm=T)))
  
  ### middle 13 years
  temp <- kelp.otter.dat %>% filter(location == NOM[i],Year >= 1996,Year<= 2008) 
  
  mod <- lm(log.otter~Year,data=temp)  
  reg.coef1 <- rbind(reg.coef1,data.frame(Site=NOM[i],Start=1996,slope.otter=coef(summary(mod))["Year","Estimate"],slope.otter.se=coef(summary(mod))["Year","Std. Error"]))
  
  mod <- lm(log.kelp~Year,data=temp)  
  reg.coef2 <- rbind(reg.coef2,data.frame(Site=NOM[i],Start=1996,slope.kelp=coef(summary(mod))["Year","Estimate"],slope.kelp.se=coef(summary(mod))["Year","Std. Error"]))
  
  temp <- temp %>% filter(Year >= 1996,Year<= 2008)
  A<- matrix(sample(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values),10000,replace = T),10,1000)
  B<- c(quantile(apply(A,1,sd),probs=c(0.025,0.975)))
  lower.95.sd <- B[1]
  upper.95.sd <- B[2]
  C <- mean(apply(A,1,sd),na.rm=T)
  
  kelp.cv <- rbind(kelp.cv,data.frame(Site=NOM[i],Start=1996, 
                                      kelp.cv= sd(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values)) / mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.boot= C/ mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.lower = lower.95.sd/ mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.upper = upper.95.sd/ mean(temp$kelp.area,na.rm=T),                                  
                                      kelp.mean= mean(temp$kelp.area,na.rm=T)))
  
  
  ### last 13 years
  temp <- kelp.otter.dat %>% filter(location == NOM[i],Year >= BREAK+1,Year<= 2015) 
  
  mod <- lm(log.otter~Year,data=temp)  
  reg.coef1 <- rbind(reg.coef1,data.frame(Site=NOM[i],Start=BREAK+1,slope.otter=coef(summary(mod))["Year","Estimate"],slope.otter.se=coef(summary(mod))["Year","Std. Error"]))
  
  mod <- lm(log.kelp~Year,data=temp)  
  reg.coef2 <- rbind(reg.coef2,data.frame(Site=NOM[i],Start=BREAK+1,slope.kelp=coef(summary(mod))["Year","Estimate"],slope.kelp.se=coef(summary(mod))["Year","Std. Error"]))
  
  temp <- temp %>% filter(Year>=BREAK+1)
  A<- matrix(sample(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values),10000,replace = T),10,1000)
  B<- c(quantile(apply(A,1,sd),probs=c(0.025,0.975)))
  lower.95.sd <- B[1]
  upper.95.sd <- B[2]
  C <- mean(apply(A,1,sd),na.rm=T)
  
  kelp.cv <- rbind(kelp.cv,data.frame(Site=NOM[i],Start=BREAK+1, 
                                      kelp.cv= sd(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values)) / mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.boot= C / mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.lower = lower.95.sd/ mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.upper = upper.95.sd/ mean(temp$kelp.area,na.rm=T),                                  
                                      kelp.mean= mean(temp$kelp.area,na.rm=T)))
  
  ### 1989-2015
  temp <- kelp.MA.otter.dat %>% filter(location == NOM[i],Year >= 1989,Year<= 2015) 
  
  mod <- lm(log.otter~(Year),data=temp)  
  reg.coef1 <- rbind(reg.coef1,data.frame(Site=NOM[i],Start="All",slope.otter=coef(summary(mod))["Year","Estimate"],slope.otter.se=coef(summary(mod))["Year","Std. Error"]))
  
  mod <- lm(log.kelp~Year,data=temp)  
  reg.coef2 <- rbind(reg.coef2,data.frame(Site=NOM[i],Start="All",slope.kelp=coef(summary(mod))["Year","Estimate"],slope.kelp.se=coef(summary(mod))["Year","Std. Error"]))
  
  temp <- temp %>% filter(Year>=1989)
  A<- matrix(sample(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values),10000,replace = T),10,1000)
  B<- c(quantile(apply(A,1,sd),probs=c(0.025,0.975)))
  lower.95.sd <- B[1]
  upper.95.sd <- B[2]
  C <- mean(apply(A,1,sd),na.rm=T)
  
  kelp.cv <- rbind(kelp.cv,data.frame(Site=NOM[i], Start="All",
                                      kelp.cv= sd(as.numeric(na.omit(temp$kelp.area)) - exp(mod$fitted.values)) / mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.boot= C / mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.lower = lower.95.sd/ mean(temp$kelp.area,na.rm=T),
                                      kelp.cv.upper = upper.95.sd/ mean(temp$kelp.area,na.rm=T),                                  
                                      kelp.mean= mean(temp$kelp.area,na.rm=T)))
  
}

reg.coef <- merge(reg.coef1,reg.coef2)
reg.coef <- merge(reg.coef,kelp.cv)
reg.coef$all.ts <- 0
reg.coef$all.ts[is.na(reg.coef$Start)==T] <- 1

A <-  merge(kelp.otter.dat %>% filter(Year>=1989,Year<=1991) %>% group_by(location,Region) %>% summarise(otter=mean(otter.n))%>%mutate(Start=1989)%>%as.data.frame(),
            kelp.otter.dat %>% filter(Year>=1996,Year<=1998) %>% group_by(location,Region) %>% summarise(otter=mean(otter.n))%>%mutate(Start=1996)%>%as.data.frame(),all=T)
A <- merge(A,
           kelp.otter.dat %>% filter(Year>=BREAK+1,Year<=BREAK+3) %>% group_by(location,Region) %>% summarise(otter=mean(otter.n))%>%mutate(Start=BREAK+1)%>%as.data.frame(),all=T)
reg.coef <- merge(reg.coef,A,by.x=c("Site","Start"),by.y=c("location","Start"),all=T)

reg.coef          <- merge(reg.coef%>% dplyr::select(-Region),reg.coef %>% filter(Start ==1989) %>% dplyr::select(Site,Region) ,all=T)
# reg.coef <- merge(reg.coef,kelp.otter.dat %>% filter(Year>=BREAK+1,Year<=BREAK+3) %>% group_by(location,Region) %>% summarise(otter.2003=mean(otter.n))%>%as.data.frame(),
#                   by.x=c("Site","Region"),by.y=c("location","Region"))
reg.coef$Region <- factor(reg.coef$Region, levels=c("Northern","Central","Southern"))

#### ADD coastwide estimates of growth for otters and kelp.
otter.89to15 <- nwfsc.otter.by.year %>% filter(Year>=1989) %>% lm(log(Total)~ Year,data=.) %>% summary(.)
otter.89to01 <- nwfsc.otter.by.year %>% filter(Year>=1989, Year <=2001) %>% lm(log(Total)~ Year,data=.) %>% summary(.)
otter.02to15 <- nwfsc.otter.by.year %>% filter(Year>=2002, Year <=2015) %>% lm(log(Total)~ Year,data=.) %>% summary(.)

kelp.89to15  <- kelp.coastwide.dat  %>% filter(year>=1989) %>% lm(log(total.area)~ year,data=.) %>% summary(.)
kelp.89to01  <- kelp.coastwide.dat  %>% filter(year>=1989, year <=2001) %>% lm(log(total.area)~ year,data=.) %>% summary(.)
kelp.02to15  <- kelp.coastwide.dat  %>% filter(year>=2002, year <=2015) %>% lm(log(total.area)~ year,data=.) %>% summary(.)

coast.reg.coef<- data.frame(Site = "All",Start = c("All",1989,2002,"All",1989,2002),Region="All",
           slope.otter = c(otter.89to15$coefficients['Year','Estimate'],
                           otter.89to01$coefficients['Year','Estimate'],
                           otter.02to15$coefficients['Year','Estimate']),
           slope.otter.se = c(otter.89to15$coefficients['Year','Std. Error'],
                              otter.89to01$coefficients['Year','Std. Error'],
                              otter.02to15$coefficients['Year','Std. Error']),
           slope.kelp = c(kelp.89to15$coefficients['year','Estimate'],
                          kelp.89to01$coefficients['year','Estimate'],
                          kelp.02to15$coefficients['year','Estimate']),
           slope.kelp.se = c(kelp.89to15$coefficients['year','Std. Error'],
                             kelp.89to01$coefficients['year','Std. Error'],
                             kelp.02to15$coefficients['year','Std. Error']))

reg.coef <- merge(reg.coef,coast.reg.coef,all=T)
reg.coef<- merge(reg.coef,otter.mean.summary,by.x=c("Site","Start"),by.y=c("location","Start"),all=T)

#### GLM for kelp x otter interaction.
library(lme4)
slope.time1 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>% 
                  lmer(slope.kelp ~ (1|Site) + Region + slope.otter*as.factor(Start), data=.) 
slope.time2 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>% 
                  lm(slope.kelp ~ Region + slope.otter*as.factor(Start), data=.) 
summary(slope.time2)
anova(slope.time2)

### THIS IS FOR ALL KELP TOGETHER (NEREO + MACRO)
# Call:
#   lm(formula = slope.kelp ~ Region + slope.otter * as.factor(Start), 
#      data = .)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039652 -0.014942 -0.001978  0.016036  0.048590 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                       0.02901    0.01520   1.909   0.0770 .
# RegionCentral                     0.02243    0.01720   1.304   0.2133  
# RegionSouthern                    0.03243    0.03257   0.996   0.3363  
# slope.otter                       0.28527    0.28273   1.009   0.3301  
# as.factor(Start)2002             -0.05311    0.01841  -2.885   0.0120 *
#   slope.otter:as.factor(Start)2002 -0.79260    0.35973  -2.203   0.0448 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02832 on 14 degrees of freedom
# Multiple R-squared:  0.8023,	Adjusted R-squared:  0.7316 
# F-statistic: 11.36 on 5 and 14 DF,  p-value: 0.0001583

slope.all  <- reg.coef %>% filter(Region != "All", Start =="All") %>% 
                  lm(slope.kelp ~ Region + slope.otter, data=.) 
summary(slope.all)
anova(slope.all)
# Call:
#   lm(formula = slope.kelp ~ Region + slope.otter, data = .)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.027456 -0.006491 -0.004480  0.002049  0.033419 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    -0.003599   0.012824  -0.281   0.7884  
# RegionCentral   0.059504   0.018110   3.286   0.0167 *
#   RegionSouthern  0.079172   0.031825   2.488   0.0473 *
#   slope.otter    -0.222335   0.245468  -0.906   0.4000  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02018 on 6 degrees of freedom
# Multiple R-squared:  0.7215,	Adjusted R-squared:  0.5822 
# F-statistic:  5.18 on 3 and 6 DF,  p-value: 0.04202
############################################################

### THIS IS JUST FOR MACROCYSTIS ALONE
slope.time1 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>% 
  lmer(slope.kelp ~ (1|Site) + Region + slope.otter*as.factor(Start), data=.) 
slope.time2 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>% 
  lm(slope.kelp ~ Region + slope.otter*as.factor(Start), data=.) 
summary(slope.time2)
anova(slope.time2)

# Call:
#   lm(formula = slope.kelp ~ Region + slope.otter * as.factor(Start), 
#      data = .)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.069791 -0.022458  0.003686  0.017281  0.085275 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                       0.05532    0.02269   2.438  0.02867 * 
#   RegionCentral                     0.03449    0.02568   1.343  0.20065   
# RegionSouthern                    0.01712    0.04863   0.352  0.72998   
# slope.otter                       0.16284    0.42212   0.386  0.70546   
# as.factor(Start)2002             -0.09148    0.02748  -3.329  0.00497 **
#   slope.otter:as.factor(Start)2002 -0.58834    0.53709  -1.095  0.29182   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04229 on 14 degrees of freedom
# Multiple R-squared:  0.7404,	Adjusted R-squared:  0.6477 
# F-statistic: 7.986 on 5 and 14 DF,  p-value: 0.0009617
# 
# > anova(slope.time2)
# Analysis of Variance Table
# 
# Response: slope.kelp
# Df   Sum Sq  Mean Sq F value    Pr(>F)    
# Region                        2 0.004082 0.002041  1.1415 0.3473284    
# slope.otter                   1 0.030702 0.030702 17.1707 0.0009936 ***
#   as.factor(Start)              1 0.034465 0.034465 19.2754 0.0006161 ***
#   slope.otter:as.factor(Start)  1 0.002146 0.002146  1.2000 0.2918171    
# Residuals                    14 0.025033 0.001788                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# DATA FROM BLAKE on Exposure.
# 
exposure.dat <- data.frame(matrix(c(
  "Teahwhit Head"   ,       130129,
  "Destruction Island"      ,       119929,
  "Rock 305"        ,       101715,
  "Cape Johnson"    ,       141234,
  "Cape Alava"      ,       155151,
  "Point of the Arches"     ,       111405,
  "Anderson Point"  ,       65867,
  "Tatoosh Island"  ,       14400,
  "Chibahdehl Rock" ,       23469,
  "Neah Bay"        ,       13258),
  10,2,
  byrow=TRUE))
colnames(exposure.dat) <- c("Site","expose.value")
exposure.dat$expose.value <- as.numeric(as.character(exposure.dat$expose.value))

exposure.dat$expose.value.short <- exposure.dat$expose.value/1e4

#### Sundry analyses for change in kelp CV.
cv.diff.89 <- reg.coef %>% filter(Start==1989) %>% dplyr::select(Site,Start,kelp.cv.boot,kelp.mean,otter,otter.mean,Region)
cv.diff.02 <- reg.coef %>% filter(Start==2002) %>% dplyr::select(Site,Start,kelp.cv.boot,kelp.mean,otter,otter.mean,Region)

cv.diff    <- cv.diff.89 %>% mutate(kelp.mean.89 = kelp.mean, otter.89=otter,otter.mean.89=otter.mean,kelp.cv.89=kelp.cv.boot) %>%
                      dplyr::select(Site,Region,kelp.mean.89,otter.89,otter.mean.89,kelp.cv.89)
cv.diff    <- cbind(cv.diff,cv.diff.02 %>%  mutate(kelp.mean.02 = kelp.mean, otter.02=otter,otter.mean.02=otter.mean,kelp.cv.02=kelp.cv.boot)%>%
                  dplyr::select(kelp.mean.02,kelp.cv.02,otter.02,otter.mean.02) ) %>% as.data.frame()
                  
cv.diff$CV.DIFF <- cv.diff$kelp.cv.02 - cv.diff$kelp.cv.89 
cv.diff         <- cv.diff %>% filter(Site!="All")                    

cv.diff <- cv.diff %>% mutate(diff.otter = otter.02 - otter.89, 
                              diff.kelp = kelp.mean.02- kelp.mean.89, 
                              diff.otter.mean = otter.mean.02- otter.mean.89) 
cv.diff$expose.value.short <- exposure.dat$expose.value.short[match(cv.diff$Site,exposure.dat$Site)]


M.0a <- lm(kelp.cv.02~Region,data=cv.diff)
summary(M.0a)
anova(M.0a)

M.0b <- lm(kelp.cv.02~kelp.cv.89,data=cv.diff)
summary(M.0b)
anova(M.0b)

M.0c <- lm(kelp.cv.02~diff.otter.mean,data=cv.diff)
summary(M.0c)
anova(M.0c)

M.1 <- lm(kelp.cv.02~kelp.cv.89+Region,data=cv.diff)
summary(M.1)
anova(M.1)

M.2 <- lm(kelp.cv.02~kelp.cv.89+ diff.otter,data=cv.diff)
summary(M.2)
anova(M.2)

M.2b <- lm(kelp.cv.02~kelp.cv.89 + diff.otter.mean ,data=cv.diff)
summary(M.2b)
anova(M.2b)

M.3 <- lm(kelp.cv.02~Region+diff.otter,data=cv.diff)
summary(M.3)
anova(M.3)

M.3b <- lm(kelp.cv.02~Region+diff.otter.mean,data=cv.diff)
summary(M.3b)
anova(M.3b)

M.4 <- lm(kelp.cv.02~kelp.cv.89 + diff.otter + Region,data=cv.diff)
summary(M.4)
anova(M.4)

M.4b <- lm(kelp.cv.02~kelp.cv.89 + diff.otter.mean + Region,data=cv.diff)
summary(M.4b)
anova(M.4b)

# M.5 <- lm(CV.DIFF~kelp.cv.89 * diff.otter + Region,data=cv.diff)
# summary(M.5)
# anova(M.5)

AICc(M.0a)
AICc(M.0b)
AICc(M.0c)
AICc(M.1)
AICc(M.2)
AICc(M.2b)
AICc(M.3)
AICc(M.3b)
AICc(M.4)
AICc(M.4b)


AIC(M.0a)
AIC(M.0b)
AIC(M.0c)
AIC(M.1)
AIC(M.2)
AIC(M.2b)
AIC(M.3)
AIC(M.3b)
AIC(M.4)
AIC(M.4b)

### Models without cv.kelp.89
AICc(M.0a)
AICc(M.0c)
AICc(M.3)
AICc(M.3b)
#AICc(M.5)

#########################################################################################
#########################################################################################
#########################################################################################

# Analyses of kelp CV for the entire time period

kelp.cv.all <- reg.coef %>% filter(Start=="All", Site != "All")

plot(kelp.cv~expose.value.short,data= kelp.cv.all)


#########################################################################################
#########################################################################################
#########################################################################################

reg.coef$expose.value.short <- exposure.dat$expose.value.short[match(reg.coef$Site,exposure.dat$Site)]
reg.coef$log.otter.mean <-log(reg.coef$otter.mean)
reg.coef$log.otter <-log(reg.coef$otter)

CV.lmNull <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) , data=.) 
CV.lm0a <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Region , data=.) 
CV.lm0b <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start , data=.) 
summary(CV.lm0a)
summary(CV.lm0b)

CV.lm1 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start+Region , data=.) 
summary(CV.lm1)
AIC(CV.lm1)

CV.lm1b <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start*Region , data=.) 
summary(CV.lm1b)

CV.lm2 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start+ Region + log.otter.mean, data=.) 
summary(CV.lm2)
anova(CV.lm2)
AIC(CV.lm2)

CV.lm3 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start+ Region*log.otter, data=.) 
summary(CV.lm3)
anova(CV.lm3)

AIC(CV.lmNull)
AIC(CV.lm0a)
AIC(CV.lm0b)
AIC(CV.lm1)
AIC(CV.lm1b)
AIC(CV.lm2)
AIC(CV.lm3)

# OK.  CV.lm0b wins.  Add effects of otters
CV.lm0.1 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start + otter, data=.) 
CV.lm0.2 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start + otter.mean, data=.) 
CV.lm0.3 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start + log.otter, data=.) 
CV.lm0.4 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start + log.otter.mean, data=.) 
CV.lm0.5 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start * log.otter, data=.) 
CV.lm0.6 <- reg.coef %>% filter(Region != "All", Start != 1996,Start!="All") %>%  lmer(kelp.cv ~ (1|Site) + Start * log.otter.mean, data=.) 


AIC(CV.lm0b)
AIC(CV.lm0.1)
AIC(CV.lm0.2)
AIC(CV.lm0.3)
AIC(CV.lm0.4)
AIC(CV.lm0.5)
AIC(CV.lm0.6)



#################
## Plotting
#################
theme_os2 <- function(base_size = 12, base_family = "") {
  theme_bw()+
    theme(
      text=element_text(size=11),
     # legend.title = element_blank(),
      legend.text  = element_text(size=7.5),
      #legend.justification = c("left", "top"),
      #legend.key   = element_blank(),
      legend.key.size = unit(0.7, 'lines'),
      # legend.background =  element_rect(colour = "white"),
      #legend.position   = c(0.02,0.98),
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
      plot.title =         element_text(size = rel(0.9),hjust = 0,vjust=-1),
      plot.margin =        unit(c(0, -0.1, 0.1, 0.1), "lines")
    )
}  

x.lim=c(-0.06,0.22)
y.lim=c(-0.12,0.18)

COL   <- viridis(32,begin=0,end=0.8)

A.1989 <-  ggplot(reg.coef %>% filter(Start ==1989),aes(y=slope.kelp,x=slope.otter)) +
  geom_errorbarh(aes(xmin=slope.otter-slope.otter.se, xmax=slope.otter+slope.otter.se)) +
  geom_errorbar(aes(ymin=slope.kelp-slope.kelp.se, ymax=slope.kelp+slope.kelp.se)) +
  geom_point(aes(fill=otter,shape=Region),size=3) +
  scale_shape_manual(values=c(21,22,23,4),name = "Region")+
  scale_fill_gradientn(name = "Otters",colors = COL,breaks=seq(0,200,by=50),labels=as.character(seq(0,200,by=50)),limits=c(0,max(reg.coef$otter))) +
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_vline(xintercept = 0,linetype="dotted")+
  #xlab("Otter growth rate") + #+ theme(legend.position="none")
  scale_y_continuous(limits=y.lim,name = "Kelp growth rate")+
  scale_x_continuous(limits=x.lim,name = " ")+
  ggtitle("a) 1989-2001") +
  theme_os2 ()

A.1996 <-  ggplot(reg.coef %>% filter(Start ==1996),aes(y=slope.kelp,x=slope.otter)) +
  geom_errorbarh(aes(xmin=slope.otter-slope.otter.se, xmax=slope.otter+slope.otter.se)) +
  geom_errorbar(aes(ymin=slope.kelp-slope.kelp.se, ymax=slope.kelp+slope.kelp.se)) +
  geom_point(aes(fill=otter,shape=Region),size=3) + 
  scale_shape_manual(values=c(21,22,23,4),name = "Region")+
  scale_fill_gradientn(name = "Otters",colors = COL,breaks=seq(0,200,by=50),labels=as.character(seq(0,200,by=50)),limits=c(0,max(reg.coef$otter))) +
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_y_continuous(limits=y.lim,name = "Kelp growth rate")+
  scale_x_continuous(limits=x.lim,name = "Otter growth rate")+
  ggtitle("b) 2002-2015") +
  theme_os2 ()

A.2002 <-  ggplot(reg.coef %>% filter(Start ==2002),aes(y=slope.kelp,x=slope.otter)) +
  geom_errorbarh(aes(xmin=slope.otter-slope.otter.se, xmax=slope.otter+slope.otter.se)) +
  geom_errorbar(aes(ymin=slope.kelp-slope.kelp.se, ymax=slope.kelp+slope.kelp.se)) +
  geom_point(aes(fill=otter,shape=Region),size=3) + 
  scale_shape_manual(values=c(21,22,23,4),name = "Region")+
  scale_fill_gradientn(name = "Otters",colors = COL,breaks=seq(0,200,by=50),labels=as.character(seq(0,200,by=50)),limits=c(0,max(reg.coef$otter))) +
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_y_continuous(limits=y.lim,name = "Kelp growth rate")+
  scale_x_continuous(limits=x.lim,name = "Otter growth rate")+
  ggtitle("b) 2002-2015") +
  theme_os2 ()

quartz(file = paste(base.dir,"/Plots/Otters vs. MA growth 2 halves.pdf",sep=""),type="pdf",dpi=300,height=7,width=5 )
  Layout= matrix(c(1,2),nrow=2,ncol=1,byrow=F)
  QQ <- list(A.1989,
           A.2002)
  multiplot(plotlist=QQ ,layout= Layout)
dev.off()


###################################################
###################################################
###################################################
###################################################
###################################################
## Change in kelp CV with change in otter numbers.
COL=viridis(2,begin=0,end=0.6)
y.lim=c(-0.005,1.01)
x.lim=c(-1,max(reg.coef$otter.mean,na.rm=T)+8)
A <- ggplot(reg.coef %>% filter(Start!=1996,Start!="All",Region!="All"),aes(y=kelp.cv.boot,x=otter.mean,group=Site,shape=Region,fill=as.factor(Start))) +
  geom_point(size=4)+
  scale_fill_manual(name = "Period",values = COL,labels=c("1989-2001","2002-2015"),
                    guide = guide_legend(override.aes = list(shape = c(21,21),fill=COL))) +
  scale_shape_manual(values=c(21,22,23),name = "Region")+
  geom_errorbar(aes(ymin= kelp.cv.lower, ymax=kelp.cv.upper)) +
  geom_line(color="black",linetype="dashed") +
  scale_y_continuous(limits=y.lim,name = "Kelp CV",expand=c(0,0))+
  scale_x_continuous(limits=x.lim,name = "Otters",expand=c(0,0))+
  theme_os2 () +
  theme(legend.justification = c("right", "top"),
        legend.position   = c(0.98,0.98),
        plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines"))

x.lim=c(-1,max(reg.coef$otter,na.rm=T)+8)
B <- ggplot(reg.coef %>% filter(Start!=1996,Start!="All",Region!="All"),aes(y=kelp.cv.boot,x=otter,group=Site,shape=Region,fill=as.factor(Start))) +
  geom_point(size=4)+
  scale_fill_manual(name = "Period",values = COL,labels=c("1989-2001","2002-2015"),
                    guide = guide_legend(override.aes = list(shape = c(21,21),fill=COL))) +
  scale_shape_manual(values=c(21,22,23),name = "Region")+
  geom_errorbar(aes(ymin= kelp.cv.lower, ymax=kelp.cv.upper)) +
  geom_line(color="black",linetype="dashed") +
  scale_y_continuous(limits=y.lim,name = "Kelp CV",expand=c(0,0))+
  scale_x_continuous(limits=x.lim,name = "Otters",expand=c(0,0))+
  theme_os2 () +
  theme(legend.justification = c("right", "top"),
        legend.position   = c(0.98,0.98),
        plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines"))




C <- ggplot(reg.coef %>% filter(Start!=1996,Start!="All",Region!="All"),aes(y=kelp.cv.boot,x=kelp.mean,group=Site,shape=Region,fill=as.factor(Start))) +
  geom_point(size=4)+
  scale_fill_manual(name = "Period",values = COL,labels=c("1989-2001","2002-2015"),
                    guide = guide_legend(override.aes = list(shape = c(21,21),fill=COL))) +
  scale_shape_manual(values=c(21,22,23),name = "Region")+
  geom_errorbar(aes(ymin= kelp.cv.lower, ymax=kelp.cv.upper)) +
  geom_line(color="black",linetype="dashed") +
  scale_y_continuous(limits=y.lim,name = "Kelp CV",expand=c(0,0))+
  scale_x_continuous(limits=x.lim,name = "Otters",expand=c(0,0))+
  theme_os2 () +
  theme(legend.justification = c("right", "top"),
        legend.position   = c(0.98,0.98),
        plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines"))

quartz(file = paste(base.dir,"/Plots/Otters vs. MA CV change (MEAN).pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
  print(A)
dev.off()

quartz(file = paste(base.dir,"/Plots/Otters vs. MA CV change (START).pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
  print(B)
dev.off()



# kelp.otter.dat <- k.o.dat
# 
# 
# ggplot(kelp.otter.dat %>% filter(Year >= 1989),aes(y=log.diff.kelp,x=log.diff.otter)) +
#     geom_point(aes(color=log.otter)) + 
#     scale_color_gradientn(colors = COL) +
#     facet_wrap(~location)
# 
# 
# ggplot(kelp.otter.dat %>% filter(Year >= 1989),aes(y=log.diff.kelp,x=Year)) +
#   geom_point(aes(color=log.otter)) + 
#   scale_color_gradientn(colors = COL) +
#   facet_wrap(~location)
# 
# ggplot(kelp.otter.dat %>% filter(Year >= 1989),aes(y=log.diff.otter,x=Year)) +
#   geom_point(aes(color=log.otter)) + 
#   scale_color_gradientn(colors = COL) +
#   facet_wrap(~location)
# 
# 
# 
# 
# 
# 
# 
# ####  Calculate some statistics
#   A <- reg.coef %>% filter(Start ==1989) 
#   cor.test(A$slope.otter,A$slope.kelp)
# 
#   A <- reg.coef %>% filter(Start ==2002) 
#   cor.test(A$slope.otter,A$slope.kelp)
#   
