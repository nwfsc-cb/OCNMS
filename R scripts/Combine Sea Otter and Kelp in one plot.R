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

  NOM <- c(
    "Neah Bay",
    "Chibadehl Rock",
    "Tatoosh Island",
    "Anderson Point",
    "Point of the Arches",
    "Cape Alava",
    "Cape Johnson",
    "Teahwhit Head",
    "Destruction Island"
  )
  
  otter.kern.dat$location <- factor(otter.kern.dat$location,levels=NOM)
  
## Color palette
  COL <- viridis(3,begin=0,end=0.7)
    
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
        legend.position   = c(-0.02,1.04),
        # #legend.text.align = 0,
        legend.key =         element_rect(fill = "white", color="white",size=0.5),
        panel.background =   element_rect(fill = "white", colour = "black",size=1.5),
        panel.border =       element_blank(),
        panel.grid.major =   element_blank(),
        panel.grid.minor =   element_blank(),
        # panel.spacing =       unit(0.25, "lines"),
        strip.background =   element_rect(fill = "black", colour = "black"),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        plot.background =    element_rect(colour = "white"),
        plot.title =         element_text(size = rel(0.9),hjust = 0),
        plot.margin =        unit(c(0.2, 0.1, -0.7, 0.1), "lines")
      )
  }  

  x.lim <- c(min(otter.kern.dat$Year)-1,max(otter.kern.dat$Year))
    
O.1 <- ggplot(nwfsc.otter.by.year,aes(x=Year,y=Total)) +
        geom_point() +
        geom_line(linetype="dashed") +
        geom_smooth(span=0.5,method="loess",se=F,color=grey(0.4) )+
        ylab("Sea otters") +
        xlab("")+ 
        scale_x_continuous(limits = x.lim) +
        theme_os() 
O.2 <- ggplot(otter.kern.dat %>% filter(Region=="Northern"),aes(x=Year,y=tot.pop,color=location)) +
          geom_point() +
          geom_line(linetype="dashed") +
          geom_smooth(span=0.25,method="loess",se=F) +
          scale_colour_manual(name="location",values=COL) +
          scale_x_continuous(limits = x.lim) +      
          #ylab("Sea otters") +
          xlab("")+
          ylab("") +
          ggtitle("a) Northern") +
          theme_os() #+ theme(legend.position="none")
O.3 <- ggplot(otter.kern.dat %>% filter(Region=="Central"),aes(x=Year,y=tot.pop,color=location)) +
            geom_point() +
            geom_line(linetype="dashed") +
            geom_smooth(span=0.25,method="loess",se=F) +
            scale_colour_manual(name="location",values=COL) +
            scale_x_continuous(limits = x.lim) +        
            ylab("Sea otters (individuals)") +
            ggtitle("b) Central") +
            xlab("")+
            #ylab("") +
            theme_os() #+ theme(legend.position="none")
          
O.4 <- ggplot(otter.kern.dat %>% filter(Region=="Southern"),aes(x=Year,y=tot.pop,color=location)) +
  theme_os() +geom_point() +
            geom_line(linetype="dashed") +
            geom_smooth(span=0.25,method="loess",se=F) +
            scale_colour_manual(name="location",values=COL) +
            scale_x_continuous(limits = x.lim) +          
            #ylab("Sea otters") +
            ggtitle("c) Southern") +
            xlab("") + #+ theme(legend.position="none")
            ylab("")

y.lim <- c(-2.5,2.5)

K.1 <- ggplot(kelp.coastwide.dat,aes(x=year,y=total.area)) +
         geom_point() +
         geom_line(linetype="dashed") +
          geom_smooth(span=0.5,method="loess",se=F,color=grey(0.4) )+
         scale_x_continuous(limits = x.lim) +
         ylab("Kelp canopy area (ha)") +
         xlab("Year")+
         theme_os() + theme(legend.position="none")

K.2 <- ggplot(kelp.ts.all %>% filter(Region=="Northern"),aes(x=year,y=Dev,color=Site)) +
          geom_point() +
          geom_line(linetype="dashed") +
          geom_hline(yintercept = 0,linetype="dotted",color="black") +
          geom_smooth(span=0.25,method="loess",se=F) +
          scale_colour_manual(name="Site",values=COL) +
          scale_x_continuous(limits = x.lim) +
          scale_y_continuous(limits = y.lim) +
          xlab("")+
          ylab("") +
          ggtitle(" ") +       
          theme_os() + theme(legend.position="none")
K.3 <- ggplot(kelp.ts.all %>% filter(Region=="Central"),aes(x=year,y=Dev,color=Site)) +
          geom_point() +
          geom_hline(yintercept = 0,linetype="dotted",color="black") +
          geom_line(linetype="dashed") +
          geom_smooth(span=0.25,method="loess",se=F) +
          scale_colour_manual(name="Site",values=COL) +
          scale_x_continuous(limits = x.lim) +
          scale_y_continuous(limits = y.lim) +
          xlab("")+
          ylab("Kelp Deviation") +        
          ggtitle(" ") +
          theme_os() + theme(legend.position="none")
K.4 <- ggplot(kelp.ts.all %>% filter(Region=="Southern"),aes(x=year,y=Dev,color=Site)) +
          geom_point() +
          geom_hline(yintercept = 0,linetype="dotted",color="black") +
          geom_line(linetype="dashed") +
          geom_smooth(span=0.25,method="loess",se=F) +
          scale_colour_manual(name="Site",values=COL) +
          scale_x_continuous(limits = x.lim) +
          scale_y_continuous(limits = y.lim) +
          ylab("") +
          xlab("")+
          ggtitle(" ") +        
          theme_os() + theme(legend.position="none")

print(K.2)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


quartz(file = paste(base.dir,"/Plots/Otters and Kelp coastwide.pdf",sep=""),type="pdf",dpi=300,height=6,width=4 )
Layout= matrix(c(1,2),nrow=2,ncol=1,byrow=F)
QQ <- list(O.1,
           K.1)
multiplot(plotlist=QQ ,layout= Layout)
dev.off()


quartz(file = paste(base.dir,"/Plots/Otters and Kelp by region.pdf",sep=""),type="pdf",dpi=300,height=8,width=7 )
  Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=F)
  QQ <- list(O.2,O.3,O.4,
             K.2,K.3,K.4)
  multiplot(plotlist=QQ ,layout= Layout)
dev.off()
















# for JAMEALtheme_js

# setwd("~/Documents/Github/OCNMS/Data/csv files")
# base.dir <- "~/Documents/Github/OCNMS/"
