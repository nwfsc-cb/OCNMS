options(max.print=99999)
library(dplyr)
#### Script for importing historical Kviteck data and our OCNMS data
base.dir <- getwd()
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

output        <- dat.2015 %>% 
                              group_by(Site,Transect,Observer,PISCO.Classcode,otter.food,group) %>% 
                              summarise(.,N = sum(Count,na.rm=T)) %>%
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

# Make comparable data file for 2016 data
  # Trim out deeper surveys (get rid of everything deep than 15m)
  dat.kvitek <- dat.kvitek %>% filter(.,depth.m.simple <= 15) %>% 
                   select(.,-matches("Source"))
  
    # summarise the 1987 seastars
  dat.seastar <- dat.kvitek %>% filter(.,group =="seastar" & Year == 1987) %>% 
                    group_by(.,SITE,Site,Year) %>%
                    summarise(MEAN=sum(mean),SD = sqrt(sum(sd^2)))
  dat.seastar$N <- 10
  dat.seastar   <- dat.seastar %>%
                    group_by(.,Site,Year) %>%
                    summarise(wMEAN= WMean(MEAN,SD),wSD = WSD(MEAN,SD),N = sum(N)) %>%
                    as.data.frame()
  
  colnames(dat.seastar)[grep("w",colnames(dat.seastar))] <- c("mean","sd")
  dat.seastar$group <- "seastar"
  dat.seastar$Survey <- "Quadrat"
  dat.seastar$depth.m.simple <- 4
  
  # Do the remaining species  
  dat.trim <- dat.kvitek[,c("Site","Year","Survey","depth.m.simple","group","mean","sd")]
  dat.trim <- filter(dat.trim,group !="seastar")
  dat.trim$N <- 10
  dat.trim <- merge(dat.trim,dat.seastar,all=T)
  
  dat.trim <- dat.trim %>% group_by(.,Site,Year,Survey,depth.m.simple,group) %>%
                summarise(Mean=sum(mean),SD=sqrt(sum(sd^2)),N=max(N) ) %>%
                group_by(.,Site,Year,depth.m.simple,group) %>%
                summarise(MEAN=WMean(Mean,SD), SD=WSD(Mean,SD),N=sum(N),n.obs.check=length(N)) %>%
                as.data.frame()
  dat.trim[is.nan(dat.trim[,c("MEAN")])==T,c("MEAN","SD")] <- 0            

  dat.trim <- dat.trim %>% group_by(.,Site,Year,group) %>%
                summarise(Mean=WMean(MEAN,SD),sd=WSD(MEAN,SD),N=sum(N),n.obs.check=length(N)) %>%
                as.data.frame
  dat.trim[is.nan(dat.trim[,c("Mean")])==T,c("Mean","sd")] <- 0            
  
  
  

  # Combine multiple data types to arrive at one number for each site-year combination
      # Don't have SE on the scale of the observations... so need to do something else
        # Also it is abundantly clear that the quadrats are not independent from each other (due to proximity)
    # Assume the observed mean and SD come from an estimated 8 m2 quadrats (for both quadrat and transects)
  
  out.kvitek <- dat.kvitek %>% group_by(SITE,Year,Survey,group,depth.m.simple) %>%
                      summarise(N =length(mean) ) %>% as.data.frame()
  
  
  
  
  
  
  
  
  
  
  
  
  # Rules for combining two estimates - calculate a weighted mean 
  
  
  



















# make comparable data file for Kvitek data (1987, 1995, 1999)
    # This is a mess that combines quadrat and transect sampling across different methodologies
    # Mostly extracted information from tables







dat.kvitek