rm(list=ls())
options(max.print=99999)
library(dplyr)
library(ggplot2)
#### Script for importing historical Kviteck data and our OCNMS data
base.dir <- "/Users/ole.shelton/GitHub/OCNMS"
setwd(paste(base.dir,"/Data/csv files",sep=""))

#### FUNCTION
WMean <- function(MEAN,SD){
  if(length(MEAN)==2){
    w <- SD[1]^2 / sum(SD^2)  
    mean.out <- w*MEAN[1] + (1-w)*MEAN[2]
    return(mean.out)
  }else{
    return(MEAN)
  }
}
WSD <- function(MEAN,SD){
  if(length(MEAN)==2){
    w <- SD[1]^2 / sum(SD^2)                
    var.out  <- w^2*SD[1]^2 + (1-w)^2*SD[2]^2
    return(sqrt(var.out))
  }else{
    return(SD)
  }
}

dat.2016 <- read.csv("NWFSC_SWATH_2016_data_entry.csv")
dat.2015 <- read.csv("2015_OCNMSDataComplete_standardized_122116.csv")
dat.kvitek <- read.csv("Kvitek.Data.SummaryByYr.MASTER.csv")
# drop Destruction Island E
dat.kvitek <- filter(dat.kvitek,SITE != "Destruction Island E")
site.order <- read.csv("site.order.csv") # ordering useful for plotting.

### RULES for otter food:
# Sea Stars         - rare
# Urchins           - common
# Anenomes          - No
# Tunicates         - No
# Crabs             - occassional
# Sea Cucs          - rare
# Nudibranch        - No
# Snails & Limpets  - common
# Chitons           - rare
# Mollusks          - common

# import directly the otter food rules:
otter.food <- read.csv("otter_food_categories.csv")


## Cull 2015 data to only include swath data
dat.2015 <- filter(dat.2015,data.type == "swath",!is.na(Transect),PISCO.Classcode !="",PISCO.Classcode !="NO_ORG")
dat.2015 <- merge(dat.2015,otter.food,by.x="PISCO.Classcode",by.y="PISCO.sp")

# Convert all of the observations to m2 units
dat.2015$Transect.area..inverts. <- as.numeric(as.character(dat.2015$Transect.area..inverts.))
dat.2015$Count.m2 <- dat.2015$Count / dat.2015$Transect.area..inverts.

output        <- dat.2015 %>% 
  group_by(Site,Transect,Observer,PISCO.Classcode,otter.food,group) %>% 
  summarise(.,N = sum(Count.m2,na.rm=T)) %>%
  as.data.frame()
ref          <- output %>% group_by(.,Site,Transect,Observer) %>% summarise(.,length(Observer)) %>% as.data.frame()

output2  <-       merge(
  merge(
    expand.grid(Site=unique(output$Site),PISCO.Classcode=unique(output$PISCO.Classcode)),
    otter.food,
    by.x="PISCO.Classcode",by.y="PISCO.sp",all=T),
  ref,all=T)

output2  <- select(output2, -starts_with("length"))
output   <- merge(output,output2,all=T)
output$N[is.na(output$N)==T] <- 0
output$ID <- paste(output$Transect,output$Observer,sep=".")
output <- output[is.na(output$Site)==F,]  

out.by.sp     <- data.frame(summarise(group_by(output,Site,PISCO.Classcode,otter.food,group,Sp.name),
                                      MEAN= mean(N),SD=sd(N),N.obs=length(N),SE=sd(N)/sqrt(length(N))))

out.by.group  <- output %>% group_by(.,Site,ID,group) %>% 
  summarise(SUM= sum(N)) %>% group_by(.,Site,group) %>%
  summarise(MEAN=mean(SUM),SD=sd(SUM),SE=sd(SUM)/sqrt(length(unique(ID)))) %>%
  as.data.frame()
out.by.group$Year <- 2015

##################################################################################################
##################################################################################################
##################################################################################################

# Make comparable data file for historical data
# Trim out deeper surveys (get rid of everything deep than 15m)
N.assume <- 20

dat.kvitek <- dat.kvitek %>% filter(.,depth.m.simple <= 15) %>% 
  select(.,-matches("Source"))

# summarise the 1987 seastars
dat.seastar87 <- dat.kvitek %>% filter(.,group =="seastar" & Year == 1987) %>% 
  group_by(.,SITE,Site,Year,n.quad) %>%
  summarise(MEAN=sum(mean),SD = sqrt(sum(sd^2)))
dat.seastar99 <- dat.kvitek %>% filter(.,group =="seastar" & Year == 1999) %>% 
  group_by(.,SITE,Site,Year,n.quad) %>%
  summarise(MEAN=sum(mean),SD = sqrt(sum(sd^2)))

dat.seastar <-rbind(dat.seastar87,dat.seastar99)
dat.seastar$n.quad[dat.seastar$Year == 1987] <- N.assume
dat.seastar$SE <- dat.seastar$SD / sqrt(dat.seastar$n.quad)
  
#dat.seastar$N <- N.assume
#dat.seastar$SE <- dat.seastar$SD / sqrt(dat.seastar$N)
dat.seastar   <- dat.seastar %>%
  group_by(.,Site,Year) %>%
  summarise(wMEAN= WMean(MEAN,SD),wSE = WSD(MEAN,SE)) %>%
  as.data.frame()

colnames(dat.seastar)[grep("w",colnames(dat.seastar))] <- c("MEAN","SE")
dat.seastar$group <- "seastar"
dat.seastar$Survey <- "Quadrat"
dat.seastar$depth.m.simple <- 4

dat.seastar <- dat.seastar[,c("Site","Year","group","MEAN","SE")]

# Do the remaining species  
dat.trim <- dat.kvitek[,c("Site","Year","Survey","depth.m.simple","group","mean","sd","n.quad")]
dat.trim <- filter(dat.trim,group !="seastar")
dat.trim$SE <- dat.trim$sd / sqrt(dat.trim$n.quad)
dat.trim$n.quad[dat.trim$Year == 1987] <- N.assume
#dat.trim$N <- N.assume   # this is the assumed sample size for each observation 

# Calculate the MEAN and SE for each type (quadrat, transect) then combine into a weighted average
dat.trim <- dat.trim %>% group_by(.,Site,Year,Survey,depth.m.simple,group,n.quad) %>%
  summarise(Mean=sum(mean),se=  sqrt(sum((sd/sqrt(n.quad))^2))) %>%
  group_by(.,Site,Year,depth.m.simple,group) %>%
  summarise(.,MEAN=WMean(Mean,se), SE=WSD(Mean,se)) %>%
  as.data.frame()

dat.trim[is.nan(dat.trim[,c("MEAN")])==T,c("MEAN","SE")] <- 0

# Combine the multiple depths (less than 15m deep) into on value for each site
dat.trim <- dat.trim %>% group_by(.,Site,Year,group) %>%
  summarise(Mean=WMean(MEAN,SE),se=WSD(MEAN,SE),n.obs.check=length(MEAN)) %>%
  as.data.frame
colnames(dat.trim)[4:5] <- c("MEAN","SE")
dat.trim[is.nan(dat.trim[,c("MEAN")])==T,c("MEAN","SE")] <- 0            

dat.trim <- merge(dat.trim,dat.seastar,all=T)

#### COMBINE THE 2015 and pre-2000 data
dat.group <- merge(out.by.group,dat.trim,all=T)

# Unify Merge the site names
dat.group$Site <- as.character(dat.group$Site)
dat.group$Site[dat.group$Site =="Anderson Point"] = "Anderson Pt."
dat.group$Site[dat.group$Site =="Rock #305"] = "Rock 305"
dat.group$Site[dat.group$Site =="Point of the Arches"] = "Pt. of the Arches"
dat.group$Site[dat.group$Site =="Teawhit Head"] = "Teahwhit Head"
dat.group$Site[grep("Chiba",dat.group$Site)] <- "Chibahdehl"
dat.group$Site[grep("Destruct",dat.group$Site)] <- "Destruction Is."
dat.group$Site[grep("Tatoosh",dat.group$Site)] <- "Tatoosh Is."
### Exclude a few sites that lack surveys in multiple time periods

site.order <- c(
  "Neah Bay",
  "Chibahdehl",
  "Tatoosh Is.",
  #"Cape Flattery",
  "Anderson Pt.",
  "Makah Bay",
  "Pt. of the Arches",
  "Cape Alava",
  "Cape Johnson",
  "Rock 305",
  "Teahwhit Head",
  "Destruction Is.")

dat.group$Site.plot <- dat.group$Site
dat.group$Site.plot <-  factor(dat.group$Site.plot, 
                               levels = site.order)
dat.group <- dat.group[is.na(dat.group$Site.plot)==F,]
dat.group$year.plot <- as.Date(as.character(dat.group$Year),"%Y")
#dat.group <- merge(dat.group,expand.grid(unique(dat.group$Site),unique(dat.group$Year),unique(dat.group$group)),all=T)

# CREATE PLOTS FOR EACH ORGANISM TYPE
GROUP <- sort(as.character(unique(dat.group$group)))

pdf(file = paste(base.dir,"/Plots/Inverts panels.pdf",sep=""),onefile=T, width=4,height=10)

for(i in 1:length(GROUP)){
  temp.dat <- dat.group[dat.group$group == GROUP[i],]
  temp.dat <- merge(temp.dat,  expand.grid(Year=unique(dat.group$year.plot),Site.plot=unique(dat.group$Site.plot)),all=T)
  
  p <- ggplot(temp.dat) +
    geom_point(aes(y=MEAN,x=year.plot),stat="identity") +   
    geom_bar(aes(y=MEAN,x=year.plot),stat="identity") +
    geom_errorbar(aes(ymin=MEAN,ymax=MEAN+SE,x=year.plot)) +
    geom_line(aes(y=MEAN,x=year.plot),stat="identity") +
    facet_wrap(~Site.plot,ncol=2) +
    ggtitle(GROUP[i]) +
    #scale_y_continuous(expand=c(0,0))+
    scale_x_date(#date_breaks = "4 year", 
      date_labels = "%Y",limits = c(as.Date("1985-1-1"), as.Date("2017-1-1")),
      breaks=c(as.Date("1987-1-1"), as.Date("1995-1-1"), as.Date("1999-1-1"), as.Date("2015-1-1")))+
    #      scale_x_continuous(breaks=c(1987,1995,1999,2015),labels=c("1987","1995","1999","2015"),expand = c(0, 0)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5))
  print(p)
  
  if(GROUP[i]=="urchin"){
      p <- ggplot(temp.dat) +
        #geom_point(aes(y=MEAN,x=year.plot),stat="identity") +   
        geom_bar(aes(y=MEAN,x=year.plot),stat="identity") +
        geom_errorbar(aes(ymin=MEAN,ymax=MEAN+SE,x=year.plot)) +
        geom_line(aes(y=MEAN,x=year.plot),stat="identity") +
        facet_wrap(~Site.plot,ncol=2) +
        ggtitle(paste(GROUP[i], "small y-axis")) +
        #scale_y_continuous(expand=c(0,0))+
        scale_x_date(#date_breaks = "4 year", 
          date_labels = "%Y",limits = c(as.Date("1985-1-1"), as.Date("2017-1-1")),
          breaks=c(as.Date("1987-1-1"), as.Date("1995-1-1"), as.Date("1999-1-1"), as.Date("2015-1-1"))) +
        scale_y_continuous(limits=c(0,10))+
      #      scale_x_continuous(breaks=c(1987,1995,1999,2015),labels=c("1987","1995","1999","2015"),expand = c(0, 0)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5))
      print(p)
    }
}
dev.off()

pdf(file = paste(base.dir,"/Plots/Inverts one plot .pdf",sep=""),onefile=T, width=6,height=6)
### Combine plots across all sites
for(i in 1:length(GROUP)){
  temp.dat <- dat.group[dat.group$group == GROUP[i],]
  temp.dat <- merge(temp.dat,  expand.grid(Year=unique(dat.group$year.plot),Site.plot=unique(dat.group$Site.plot)),all=T)
  
  p <- ggplot(temp.dat) +
    geom_point(aes(y=MEAN,x=year.plot,color=Site),stat="identity") +   
    #geom_bar(aes(y=MEAN,x=year.plot),stat="identity") +
    geom_errorbar(aes(ymin=MEAN,ymax=MEAN+SE,x=year.plot,color=Site),width=0) +
    geom_line(aes(y=MEAN,x=year.plot,color=Site),stat="identity") +
    #facet_wrap(~Site.plot,ncol=2) +
    ggtitle(GROUP[i]) +
    #scale_y_continuous(expand=c(0,0))+
    scale_x_date(#date_breaks = "4 year", 
      date_labels = "%Y",limits = c(as.Date("1985-1-1"), as.Date("2017-1-1")),
      breaks=c(as.Date("1987-1-1"), as.Date("1995-1-1"), as.Date("1999-1-1"), as.Date("2015-1-1")))+
    #      scale_x_continuous(breaks=c(1987,1995,1999,2015),labels=c("1987","1995","1999","2015"),expand = c(0, 0)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5))
  print(p)
}  

dev.off()

