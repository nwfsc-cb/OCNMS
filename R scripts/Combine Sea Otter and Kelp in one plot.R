library(viridis)
library(grid)
### Combining Kelp and Sea otter data for publication plots

# for OLE 
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Kelp Analysis.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Sea otter time series script.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"

# for JAMEAL
base.dir <- "~/Documents/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Kelp Analysis.R",sep=""))
base.dir <- "~/Documents/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Sea otter time series script.R",sep=""))
base.dir <- "~/Documents/GitHub/OCNMS/"


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

Otter.index2.no.s <- ggplot(otter.kern.dat %>% filter(Region=="Northern"),aes(x=Year,y=log.ratio,color=location)) +
  geom_line(linetype="dashed") +
  #geom_smooth(span=SPAN,method="loess",se=F) +
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
Otter.index3.no.s <- ggplot(otter.kern.dat %>% filter(Region=="Central"),aes(x=Year,y=log.ratio,color=location)) +
  geom_line(linetype="dashed") +
  #geom_smooth(span=SPAN,method="loess",se=F) +
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
Otter.index4.no.s <- ggplot(otter.kern.dat %>% filter(Region=="Southern"),aes(x=Year,y=log.ratio,color=location)) +
  geom_line(linetype="dashed") +
  #geom_smooth(span=SPAN,method="loess",se=F) +
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


# Kelp index no smooth


y.lim=c(-2,2)

K.index1.no.s <- ggplot(kelp.ts.all %>% filter(Region =="Northern"),aes(x=year,y=log.ratio,color=Site)) +
  #geom_smooth(span=SPAN,method="loess",se=F) +
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
K.index2.no.s <- ggplot(kelp.ts.all %>% filter(Region =="Central"),aes(x=year,y=log.ratio,color=Site)) +
  #geom_smooth(span=SPAN,method="loess",se=F) +
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
K.index3.no.s <- ggplot(kelp.ts.all %>% filter(Region =="Southern"),aes(x=year,y=log.ratio,color=Site)) +
  #geom_smooth(span=SPAN,method="loess",se=F) +
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
#######


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

#######################################################################################
#######################################################################################
#######################################################################################
 #####   Change in Kelp versus Change in Otters
#######################################################################################
#######################################################################################
#######################################################################################

kelp.ts.all$Site <- as.character(kelp.ts.all$Site)
kelp.ts.all$Site[kelp.ts.all$Site=="Destruction Island SW"] <-"Destruction Island"

kelp.otter.dat <- merge(otter.kern.dat %>% select(location,Year,tot.pop,Region),
                          kelp.ts.all %>% select(Site,year,total.area,Region),
                          by.x=c("location","Year","Region"),
                          by.y=c("Site","year","Region"),all=T)

kelp.otter.dat <- kelp.otter.dat %>% dplyr::rename(otter.n=tot.pop,kelp.area=total.area)
kelp.otter.dat$log.otter <- log(kelp.otter.dat$otter.n)
kelp.otter.dat$log.kelp  <- log(kelp.otter.dat$kelp.area)
  
temp <- expand.grid(Year=min(kelp.otter.dat$Year):max(kelp.otter.dat$Year),location=sort(unique(kelp.otter.dat$location)))
kelp.otter.dat <- merge(kelp.otter.dat,temp,all=T)

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
  temp <- kelp.otter.dat %>% filter(location == NOM[i],Year >= 1989,Year<= BREAK) 
  
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
}

reg.coef <- merge(reg.coef1,reg.coef2)
reg.coef <- merge(reg.coef,kelp.cv)

A <-  merge(kelp.otter.dat %>% filter(Year>=1989,Year<=1991) %>% group_by(location,Region) %>% summarise(otter=mean(otter.n))%>%mutate(Start=1989)%>%as.data.frame(),
            kelp.otter.dat %>% filter(Year>=1996,Year<=1998) %>% group_by(location,Region) %>% summarise(otter=mean(otter.n))%>%mutate(Start=1996)%>%as.data.frame(),all=T)
A <- merge(A,
           kelp.otter.dat %>% filter(Year>=BREAK+1,Year<=BREAK+3) %>% group_by(location,Region) %>% summarise(otter=mean(otter.n))%>%mutate(Start=BREAK+1)%>%as.data.frame(),all=T)
reg.coef <- merge(reg.coef,A,by.x=c("Site","Start"),by.y=c("location","Start"),all=T)
# reg.coef <- merge(reg.coef,kelp.otter.dat %>% filter(Year>=BREAK+1,Year<=BREAK+3) %>% group_by(location,Region) %>% summarise(otter.2003=mean(otter.n))%>%as.data.frame(),
#                   by.x=c("Site","Region"),by.y=c("location","Region"))
reg.coef$Region <- factor(reg.coef$Region, levels=c("Northern","Central","Southern"))
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
y.lim=c(-0.12,0.165)

COL   <- viridis(32,begin=0,end=0.8)

A.1989 <-  ggplot(reg.coef %>% filter(Start ==1989),aes(y=slope.kelp,x=slope.otter)) +
  geom_errorbarh(aes(xmin=slope.otter-slope.otter.se, xmax=slope.otter+slope.otter.se)) +
  geom_errorbar(aes(ymin=slope.kelp-slope.kelp.se, ymax=slope.kelp+slope.kelp.se)) +
  geom_point(aes(fill=otter,shape=Region),size=3) + 
  scale_shape_manual(values=c(21,22,23),name = "Region")+
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
  scale_shape_manual(values=c(21,22,23),name = "Region")+
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
  scale_shape_manual(values=c(21,22,23),name = "Region")+
  scale_fill_gradientn(name = "Otters",colors = COL,breaks=seq(0,200,by=50),labels=as.character(seq(0,200,by=50)),limits=c(0,max(reg.coef$otter))) +
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_y_continuous(limits=y.lim,name = "Kelp growth rate")+
  scale_x_continuous(limits=x.lim,name = "Otter growth rate")+
  ggtitle("b) 2002-2015") +
  theme_os2 ()

quartz(file = paste(base.dir,"/Plots/Otters vs. Kelp growth 2 halves.pdf",sep=""),type="pdf",dpi=300,height=7,width=5 )
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
x.lim=c(-1,max(reg.coef$otter)+8)
A <- ggplot(reg.coef %>% filter(Start!=1996),aes(y=kelp.cv.boot,x=otter,group=Site,shape=Region,fill=as.factor(Start))) +
  geom_point(size=4)+
  geom_errorbar(aes(ymin= kelp.cv.lower, ymax=kelp.cv.upper)) +
    scale_fill_manual(name = "Period",values = COL,labels=c("1989-2001","2002-2015"),
                    guide = guide_legend(override.aes = list(shape = c(21,21),fill=COL))) +
  scale_shape_manual(values=c(21,22,23),name = "Region")+
  geom_line(color="black",linetype="dashed") +
  scale_y_continuous(limits=y.lim,name = "Kelp CV",expand=c(0,0))+
  scale_x_continuous(limits=x.lim,name = "Otters",expand=c(0,0))+
  theme_os2 () +
  theme(legend.justification = c("right", "top"),
        legend.position   = c(0.98,0.98),
        plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines"))
  
quartz(file = paste(base.dir,"/Plots/Otters vs. Kelp CV change.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
  print(A)
dev.off()

  



kelp.otter.dat <- k.o.dat



ggplot(kelp.otter.dat %>% filter(Year >= 1989),aes(y=log.diff.kelp,x=log.diff.otter)) +
    geom_point(aes(color=log.otter)) + 
    scale_color_gradientn(colors = COL) +
    facet_wrap(~location)


ggplot(kelp.otter.dat %>% filter(Year >= 1989),aes(y=log.diff.kelp,x=Year)) +
  geom_point(aes(color=log.otter)) + 
  scale_color_gradientn(colors = COL) +
  facet_wrap(~location)

ggplot(kelp.otter.dat %>% filter(Year >= 1989),aes(y=log.diff.otter,x=Year)) +
  geom_point(aes(color=log.otter)) + 
  scale_color_gradientn(colors = COL) +
  facet_wrap(~location)







####  Calculate some statistics
  A <- reg.coef %>% filter(Start ==1989) 
  cor.test(A$slope.otter,A$slope.kelp)

  A <- reg.coef %>% filter(Start ==2002) 
  cor.test(A$slope.otter,A$slope.kelp)
  














