
# for OLE 
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Combine Sea Otter and Kelp in one plot.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Data process 2017.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"

# for JAMEAL
base.dir <- "~/Documents/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Combine Sea Otter and Kelp in one plot.R",sep=""))
base.dir <- "~/Documents/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Data process 2017.R",sep=""))
base.dir <- "~/Documents/GitHub/OCNMS/"

# Important data frames are 
# dat.trim 
# dat.trim.coastwide
# dat.otter.food[1:10,]

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

# Indicator for Before otter invasion
dat.trim$otter.absence.indicator <- "N"
dat.trim$otter.absence.indicator[dat.trim$Year==1987 & dat.trim$Site %in% c("Neah Bay","Anderson Point","Point of the Arches")] <- "Y" 

# Get rid of non-focal sites.
dat.trim <- dat.trim %>% filter(Region != "")
# unique(dat.trim$Region)

# Groups of interest
GROUP <-  c("urchin","gastropod","bivalve","crab","seastar","cucumber")
GROUP.NAME <-  c("a) Sea Urchins", "b) Gastropods","c) Bivalves","d) Crabs","e) Seastars","f) Sea cucumbers")

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

dat.trim$Year.jit <- dat.trim$Year + runif(nrow(dat.trim),-0.5,0.5)
set.seed(1)
FILL <- c("N"="white","Y"="red")
ALP <- 0.75
PT.SIZE <- 3
STROKE <- 1.25
A<- list()
for( i in 1: length(GROUP)){
  if(i==1){
  A[[i]] <- ggplot(dat.trim %>% filter(group == GROUP[i]) ,aes(y=MEAN,x=Year.jit,group=Site,shape=Region,fill=otter.absence.indicator)) +
    geom_line(linetype="dashed",color="black") +
    geom_point(size=PT.SIZE,alpha=ALP,stroke=STROKE) +
    scale_shape_manual(values=c(21,22,23),name = "Region") +
    scale_fill_manual(values=FILL,name="Otters present",labels=c("Yes","No") ) + 
    guides(fill=guide_legend(override.aes=list(colour=FILL))) +
    geom_errorbar(aes(ymin= MEAN - SE, ymax= MEAN+SE),width=0) +
    scale_y_continuous(name = expression("Number m"^-2)) +
    scale_x_continuous(name = "Year")+
    ggtitle(GROUP.NAME[i]) +
    theme_os2() +
    theme(legend.justification = c("right", "top"),
          legend.position   = c(0.98,0.98),
          plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines"))
}else{
  A[[i]] <- ggplot(dat.trim %>% filter(group == GROUP[i]) ,aes(y=MEAN,x=Year.jit,group=Site,shape=Region,fill=otter.absence.indicator)) +
    geom_line(linetype="dashed",color="black") +
    geom_point(size=PT.SIZE,alpha=ALP,stroke=STROKE) +
    scale_shape_manual(values=c(21,22,23),name = "Region") +
    scale_fill_manual(values=FILL,name="Otters present",labels=c("Yes","No") ) + 
    guides(fill=guide_legend(override.aes=list(colour=FILL))) +
    geom_errorbar(aes(ymin= MEAN - SE, ymax= MEAN+SE),width=0) +
    scale_y_continuous(name = expression("Number m"^-2)) +
    scale_x_continuous(name = "Year")+
    ggtitle(GROUP.NAME[i]) +
    theme_os2() +
    theme(legend.justification = c("right", "top"),
          legend.position   = "none",
          plot.margin =        unit(c(0.2, 0.2, 0.2, 0.2), "lines"))
}
}  
###############  



quartz(file = paste(base.dir,"/Plots/Invertebrate panels v1.pdf",sep=""),type="pdf",dpi=300,height=8,width=7 )
  Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=T)
  multiplot(plotlist=A ,layout= Layout)
dev.off()

quartz(file = paste(base.dir,"/Plots/Invertebrate panels v2.pdf",sep=""),type="pdf",dpi=300,height=5,width=9 )
Layout= matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=T)
multiplot(plotlist=A ,layout= Layout)
dev.off()


