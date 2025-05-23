

options(max.print=99999)
library(dplyr)
library(ggplot2)
#library(plyr)
#### Script for importing historical Kviteck data and our OCNMS data
#base.dir <- "/Users/ole.shelton/GitHub/OCNMS"
base.dir <- "~/Documents/GitHub/OCNMS"
setwd(paste(base.dir,"/Data/csv files",sep=""))

#### FUNCTION
WMean <- function(MEAN,W){
  if(length(MEAN)>=2){
    w <- W / sum(W)
    mean.out <- sum(w*MEAN)
    return(mean.out)
  }
  else{
    return(MEAN)
  }
}
WSD <- function(MEAN,W,SD){
  if(length(MEAN)>=2){
      w <- (W) / sum(W)                
      var.out  <- sum(w^2*SD^2)
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
otter.food$otter.food.long <-NA
otter.food$otter.food.long [which(otter.food$otter.food=="N")] <- "Not Otter Food"
otter.food$otter.food.long [which(otter.food$otter.food=="C")] <- "Common"
otter.food$otter.food.long [which(otter.food$otter.food=="O")] <- "Occassional"
otter.food$otter.food.long [which(otter.food$otter.food=="R")] <- "Rare"

## Cull 2015 data to only include swath data
dat.2015 <- filter(dat.2015,data.type == "swath",!is.na(Transect),PISCO.Classcode !="",PISCO.Classcode !="NO_ORG")
dat.2015 <- merge(dat.2015,otter.food,by.x="PISCO.Classcode",by.y="PISCO.sp")

# Convert all of the observations to m2 units
dat.2015$Transect.area..inverts. <- as.numeric(as.character(dat.2015$Transect.area..inverts.))
dat.2015$Count.m2 <- dat.2015$Count / dat.2015$Transect.area..inverts.

output        <- dat.2015 %>% 
  group_by(Site,Transect,Observer,PISCO.Classcode,otter.food,otter.food.long,group) %>% 
  summarise(.,N = sum(Count.m2,na.rm=T)) %>%
  as.data.frame()
ref          <- output %>% group_by(.,Site,Transect,Observer) %>% summarise(.,length(Observer)) %>% as.data.frame()

output2  <-       merge(
  merge(
    expand.grid(Site=unique(output$Site),PISCO.Classcode=unique(output$PISCO.Classcode)),
    otter.food,
    otter.food.long,
    by.x="PISCO.Classcode",by.y="PISCO.sp",all=T),
  ref,all=T)

output2  <- select(output2, -starts_with("length"))
output   <- merge(output,output2,all=T)
output$N[is.na(output$N)==T] <- 0
output$ID <- paste(output$Transect,output$Observer,sep=".")
output <- output[is.na(output$Site)==F,]  

out.by.sp     <- data.frame(summarise(group_by(output,Site,PISCO.Classcode,otter.food,otter.food.long,group,Sp.name),
                                      MEAN= mean(N),SD=sd(N),N.obs=length(N),SE=sd(N)/sqrt(length(N))))


#### ASSUME A MINIMUM PRECISION FOR 0 observations - based on the smallest observed SE for non-0 observations.
MIN.SE <- 1 ### THIS IS FUCKED, DON'T USE IT

out.by.group  <- output %>% group_by(.,Site,ID,group) %>% 
  summarise(SUM= sum(N)) %>% group_by(.,Site,group) %>%
  summarise(MEAN=mean(SUM),SD=sd(SUM),SE=sd(SUM)/sqrt(length(unique(ID)))) %>%
  as.data.frame()
out.by.group$Year <- 2015

out.by.group.coastwide  <- out.by.group %>% group_by(.,group) %>% 
  summarise(simp.mean=mean(MEAN),simp.se=sd(MEAN)/sqrt(length(MEAN))) 
out.by.group.coastwide$Year <- 2015

out.by.otter.food  <- output %>% group_by(.,Site,ID,otter.food,otter.food.long) %>% 
  summarise(SUM= sum(N)) %>% group_by(.,Site,otter.food,otter.food.long) %>%
  summarise(MEAN=mean(SUM),SD=sd(SUM),SE=sd(SUM)/sqrt(length(unique(ID)))) %>%
  as.data.frame()
out.by.otter.food$Year <- 2015

out.by.otter.food.coastwide  <- out.by.otter.food %>% group_by(.,otter.food,otter.food.long) %>% 
  summarise(simp.mean=mean(MEAN),simp.se=sd(MEAN)/sqrt(length(MEAN))) %>%
  as.data.frame()
out.by.otter.food.coastwide$Year <- 2015
out.by.otter.food.coastwide
##################################################################################################
##################################################################################################
##################################################################################################

# Make comparable data file for historical data
# Trim out deeper surveys (get rid of everything deep than 10m)
N.assume <- 20

dat.kvitek <- dat.kvitek %>% filter(.,depth.m.simple <= 10) %>% 
  select(.,-matches("Source"))

# summarise the 1987 seastars for each site
dat.seastar87 <- dat.kvitek %>% filter(.,group =="seastar" & Year == 1987) %>% 
  group_by(.,SITE,Site,Year,n.quad) %>%
  summarise(MEAN=sum(mean),SD = sqrt(sum(sd^2)))
# summarise the 1999 seastars for each site
dat.seastar99 <- dat.kvitek %>% filter(.,group =="seastar" & Year == 1999) %>% 
  group_by(.,SITE,Site,Year,n.quad) %>%
  summarise(MEAN=sum(mean),SD = sqrt(sum(sd^2)))

dat.seastar <-rbind(dat.seastar87,dat.seastar99)
dat.seastar$n.quad[dat.seastar$Year == 1987] <- N.assume
dat.seastar$SE <- dat.seastar$SD / sqrt(dat.seastar$n.quad)
dat.seastar <- as.data.frame(dat.seastar)

# Combine multiple measurements within a given site using a weighted mean based on 
dat.seastar   <- dat.seastar %>%
   group_by(.,Site,Year) %>%
   summarise(wMEAN= WMean(MEAN,n.quad),wSE = WSD(MEAN,n.quad,SE)) %>%
   as.data.frame()

colnames(dat.seastar)[grep("w",colnames(dat.seastar))] <- c("MEAN","SE")
dat.seastar$group <- "seastar"
dat.seastar$Survey <- "Quadrat"
dat.seastar$depth.m.simple <- 4

dat.seastar <- dat.seastar[,c("Site","Year","group","MEAN","SE")]

# Seastars coastwide - treat each site as a replicate, pretend no measurement error at each site.
dat.seastar.coastwide <- dat.seastar %>% filter(.,group =="seastar" ) %>% 
  group_by(.,Year) %>%
  summarise(simp.mean=mean(MEAN),simp.se=sqrt(sd(MEAN)))

dat.seastar$group = "seastar"
dat.seastar.coastwide$group = "seastar"

# Do the remaining species - site by site  
dat.trim <- dat.kvitek[,c("Site","Year","Survey","depth.m.simple","group","mean","sd","n.quad")]
dat.trim <- filter(dat.trim,group !="seastar")
dat.trim$SE <- dat.trim$sd / sqrt(dat.trim$n.quad)
dat.trim$n.quad[dat.trim$Year == 1987] <- N.assume
dat.trim$area.for.w <- 0
dat.trim$area.for.w[dat.trim$Survey=="Transect"] <- dat.trim$n.quad[dat.trim$Survey=="Transect"] * 25
dat.trim$area.for.w[dat.trim$Survey=="Quadrat"] <- dat.trim$n.quad[dat.trim$Survey=="Quadrat"] * 0.25
dat.trim$area.for.w[dat.trim$Survey=="Quadrat" & dat.trim$Year==1987] <- dat.trim$n.quad[dat.trim$Survey=="Quadrat"& dat.trim$Year==1987] 

dat.trim <- dat.trim[order(dat.trim$group,dat.trim$Site,dat.trim$Year),]

# Calculate the MEAN and SE for each type (quadrat, transect) then combine into a weighted average
# Weight values by the area searched.
dat.trim <- dat.trim %>% group_by(.,Site,Year,Survey,depth.m.simple,group,n.quad,area.for.w,sd) %>%
  summarise(Mean=WMean(mean,n.quad),se=  WSD(mean,n.quad,sd/sqrt(n.quad))) %>%
  group_by(.,Site,Year,depth.m.simple,group) %>%
  summarise(.,MEAN=WMean(Mean,area.for.w), SE=WSD(Mean,area.for.w,se)) %>% 
  as.data.frame()

dat.trim[is.nan(dat.trim[,c("MEAN")])==T,c("MEAN","SE")] <- 0

<<<<<<< HEAD
# # Do the remaining species coastwide - treat each site as a replicate, pretend no measurement error at each site.
dat.trim.coastwide <- dat.trim %>% filter(group !="seastar") %>%
  group_by(.,Year,group) %>%
  summarise(simp.mean=mean(MEAN),simp.se=sqrt(sd(MEAN)))
=======
# Combine the multiple depths (less than 15m deep) into on value for each site
dat.trim <- dat.trim %>% group_by(.,Site,Year,group) %>%
  summarise(Mean=WMean(MEAN,SE),se=WSD(MEAN,SE),n.obs.check=length(MEAN)) %>%
  as.data.frame
colnames(dat.trim)[4:5] <- c("MEAN","SE")
dat.trim[is.nan(dat.trim[,c("MEAN")])==T,c("MEAN","SE")] <- 0            

dat.trim <- merge(dat.trim,dat.seastar,all=T)

# Do the remaining species - coastwide  
dat.trim.coastwide <- dat.kvitek[,c("Site","Year","Survey","depth.m.simple","group","mean","sd","n.quad")]
dat.trim.coastwide <- filter(dat.trim.coastwide,group !="seastar")
dat.trim.coastwide$SE <- dat.trim.coastwide$sd / sqrt(dat.trim.coastwide$n.quad)
dat.trim.coastwide$n.quad[dat.trim.coastwide$Year == 1987] <- N.assume
#dat.trim$N <- N.assume   # this is the assumed sample size for each observation 

 ####JAMEAL NEEDS TO PICK UP HERE

# Calculate the MEAN and SE for each type (quadrat, transect) then combine into a weighted average
dat.trim.coastwide <- dat.trim.coastwide %>% group_by(.,Year,Survey,depth.m.simple,group) %>%
  summarise(Mean=sum(mean),se=  sqrt(sum((sd/sqrt(n.quad))^2))) %>% as.data.frame()
  group_by(.,Year,depth.m.simple,group) %>%
  summarise(.,MEAN=mean(Mean), SE=WSD(Mean,se)) %>%
  as.data.frame()

dat.trim[is.nan(dat.trim[,c("MEAN")])==T,c("MEAN","SE")] <- 0

# Combine the multiple depths (less than 15m deep) into on value for each site
dat.trim <- dat.trim %>% group_by(.,Site,Year,group) %>%
  summarise(Mean=WMean(MEAN,SE),se=WSD(MEAN,SE),n.obs.check=length(MEAN)) %>%
  as.data.frame
colnames(dat.trim)[4:5] <- c("MEAN","SE")
dat.trim[is.nan(dat.trim[,c("MEAN")])==T,c("MEAN","SE")] <- 0            
>>>>>>> 1016d4e004feb6e1693b57627488ef51d396449f

# Combine Seastar and non-seastar data into two data.frames
dat.trim <- merge(dat.trim,dat.seastar,all=T)
dat.trim.coastwide <- merge(dat.trim.coastwide,dat.seastar.coastwide,all=T)

# add otter food categories. problematic because not all bivalves are common diet items etc etc
unique(otter.food$group)
# need to decide on a single prey category
table(otter.food[which(otter.food$group == "bivalve"),]$otter.food.long) # change to common, Kvitek 1988 focused on scallops
table(otter.food[which(otter.food$group == "gastropod"),]$otter.food.long) # change to occassional, depends on the species
table(otter.food[which(otter.food$group == "seastar"),]$otter.food.long) # change to rare, depends on the species
table(otter.food[which(otter.food$group == "crab"),]$otter.food.long) # change to occassional, depends on the species
table(otter.food[which(otter.food$group == "chiton"),]$otter.food.long) # change to rare
table(otter.food[which(otter.food$group == "cucumber"),]$otter.food.long) # change to rare
table(otter.food[which(otter.food$group == "urchin"),]$otter.food.long) # change to common

# no changes needed
table(otter.food[which(otter.food$group == "nudibranch"),]$otter.food.long)
table(otter.food[which(otter.food$group == "anenome"),]$otter.food.long) 
table(otter.food[which(otter.food$group == "sponge"),]$otter.food.long) 
table(otter.food[which(otter.food$group == "tunicate"),]$otter.food.long)

otter.food2 <- otter.food %>%
  select(group,otter.food,otter.food.long) %>%
  distinct(group,.keep_all=TRUE)

otter.food2$otter.food <- c(
  "N","O","N","R","C","O","R","N","R","C","N","N"
  )

otter.food2$otter.food.long <- c(
  "Not Otter Food",
  "Occassional",
  "Not Otter Food",
  "Rare",
  "Common",
  "Occassional",
  "Rare",
  "Not Otter Food",
  "Rare",
  "Common",
  "Not Otter Food",
  "Not Otter Food"
)

unique(otter.food2$group) 
unique(dat.trim$group)


#head(left_join(dat.trim,otter.food2,by="group"),10)


#### COMBINE THE 2015 and pre-2000 data
dat.trim <- merge(out.by.group,dat.trim,all=T)
#make new merged df for pre-2000 data
dat.trim <- merge(dat.trim,otter.food2,by="group")
dat.trim <- subset(dat.group,select=-c(otter.food,otter.food.long))

dat.otter.food <- merge(out.by.otter.food,dat.trim,all=T)
dat.otter.food <- subset(dat.otter.food, select=-c(group))

dat.trim.coastwide <- merge(dat.trim.coastwide, out.by.group.coastwide,all=T) 


#### MAKE BINARY OTTER FOOD COLUMN
dat.otter.food$otter.food.binary <- ifelse(
  dat.otter.food$otter.food == "N",
  "Not Sea Otter Prey",
  "Sea Otter Prey"
)

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

# repeat for otter food df
# Unify Merge the site names
dat.otter.food$Site <- as.character(dat.otter.food$Site)
dat.otter.food$Site[dat.otter.food$Site =="Anderson Point"] = "Anderson Pt."
dat.otter.food$Site[dat.otter.food$Site =="Rock #305"] = "Rock 305"
dat.otter.food$Site[dat.otter.food$Site =="Point of the Arches"] = "Pt. of the Arches"
dat.otter.food$Site[dat.otter.food$Site =="Teawhit Head"] = "Teahwhit Head"
dat.otter.food$Site[grep("Chiba",dat.otter.food$Site)] <- "Chibahdehl"
dat.otter.food$Site[grep("Destruct",dat.otter.food$Site)] <- "Destruction Is."
dat.otter.food$Site[grep("Tatoosh",dat.otter.food$Site)] <- "Tatoosh Is."
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


food.order <- c(
  "Common",
  "Occassional",
  "Rare",
  "Not Otter Food"
)

dat.otter.food$Site.plot <- dat.otter.food$Site
dat.otter.food$Site.plot <-  factor(dat.otter.food$Site.plot, 
                               levels = site.order)
dat.otter.food <- dat.otter.food[is.na(dat.otter.food$Site.plot)==F,]
dat.otter.food$year.plot <- as.Date(as.character(dat.otter.food$Year),"%Y")
dat.otter.food$otter.food.plot <- dat.otter.food$otter.food.long
dat.otter.food$otter.food.plot <-  factor(dat.otter.food$otter.food.plot,
                                    levels = food.order)

########################################################################
########################################################################

## Data frames of interest are "dat.group" and "dat.otter.food"

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
### CALL A PLOTTING SCRIPT IF DESIRED:
  # source(paste(base.dir,"/R scripts/Plot inverts.R",sep=""))
#####
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

# CONTINUE ANALYSIS
