library(reshape2)
# library(ggplot2)
# library(ggdendro)
library(ecodist)
library(vegan)
# library(mclust)
# library(cluster)
library(plyr)
# library(zoo)
# library(fpc)

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
source(paste0(base.dir,"R scripts/theme_acs.R"))

# Important data frames are 
# dat.trim 
# dat.trim.coastwide

# Groups of interest
GROUP <-  c("urchin","gastropod","bivalve","crab","seastar","cucumber")

# Years of interest
  # 1995 is missing data for seastars, gastropods
FOCAL_YEARS <- c(1987, 1999, 2015)

head(dat.trim)
dim(dat.trim)

# Get rid of non-focal sites.
dat.trim <- dat.trim %>% filter(Region != "")
dim(dat.trim)
# Get rid of non-focal years.
dat.trim <- dat.trim %>% filter(Year != 1995)
dim(dat.trim)
# Get rid of non-focal groups.
dat.trim <- dat.trim %>% filter(group != "anenome" &
                                  group != "chiton" &
                                  group != "nudibranch" &
                                  group != "tunicate"
)
dim(dat.trim)

#format group densities for nMDS
dat.trim.wide <- dcast(dat.trim, Year + Site + Region ~ group, value.var="MEAN")


#Calculate distance matrix
#dj <- vegdist(dat.trim.wide[,4:9],method = "jaccard",binary=TRUE, na.rm=TRUE)
#dg <- vegdist(sqrt(dat.trim.wide[,4:9]),method = "altGower")
dbc <- vegdist(dat.trim.wide[,4:9],method = "bray", na.rm=TRUE)


#### NMDS Jaccard
# allits_j=nmds(dj,maxdim=2)
# bestnmds_j=nmds.min(allits_j)
# NMDS_j = data.frame(bestnmds_j,cov$category,cov$Site.code,cov$pair.number,cov$Year)
# names(NMDS_j) = c("MDS1","MDS2","Category","Site","Pair","Year")
# NMDS_j$dist<-c("Jaccard")

#### NMDS Bray Curtis
allits_bc=nmds(dbc,maxdim=2)
bestnmds_bc=nmds.min(allits_bc)
NMDS_bc = data.frame(bestnmds_bc,dat.trim.wide$Year,dat.trim.wide$Site,dat.trim.wide$Region)
names(NMDS_bc) = c("MDS1","MDS2","Year","Site","Region")
NMDS_bc$dist<-c("BrayCurtis")

#### NMDS Gower biomass
# allits_g=nmds(dg,maxdim=2)
# bestnmds_g=nmds.min(allits_g)
# NMDS_g = data.frame(bestnmds_g,cov$category,cov$Site.code,cov$pair.number,cov$Year)
# names(NMDS_g) = c("MDS1","MDS2","Category","Site","Pair","Year")
# NMDS_g$dist<-c("modGower")

#HULLS
find_hull <- function(df) df[chull(df$MDS1, df$MDS2),]

# hulls_u_j <- ddply(NMDS_j,"Category", find_hull)
# hulls_u_j$dist<-c("Jaccard")
hulls_u_bc_region <- ddply(NMDS_bc,"Region", find_hull)
hulls_u_bc_region$dist<-c("BrayCurtis")
# hulls_u_g <- ddply(NMDS_g,"Category", find_hull)
# hulls_u_g$dist<-c("modGower")

hulls_u_bc_year <- ddply(NMDS_bc,"Year", find_hull)
hulls_u_bc_year$dist<-c("BrayCurtis")

#bind together urban and less urban 
#df_jg_h<-rbind(hulls_u_j,hulls_u_g)
#df_jg<-rbind(NMDS_j,NMDS_g)


#plot(NMDS_g$MDS1, NMDS_g$MDS2,type="p", pch=16, xlab="MDS1", ylab="MDS2")
#ordihull(NMDS_g,groups=hulls_u_g,draw="polygon",col="grey90",label=F)
# vec.sp<-envfit(bestnmds_g, biomass, perm=1000)
# vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
# vec.sp.df$species<-rownames(vec.sp.df)
# names(vec.sp.df) <- c("MDS1","MDS2","species")
# 
# #subset out strongest vectors - top 10 biom spp?
# vec.sp.df$rank <- ifelse(abs(vec.sp.df$MDS1)>abs(vec.sp.df$MDS2),rank(abs(vec.sp.df$MDS1)),rank(abs(vec.sp.df$MDS2)) )
# vec.sp.df <- vec.sp.df[which(vec.sp.df$rank > 36),]

mds_plot1 <- ggplot(data=NMDS_bc,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_point(aes(colour=factor(Region),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_bc_region , alpha = 0.2,aes(fill=factor(Region),colour=factor(Region),group=factor(Region))) +
  ggtitle("Regional similarities all years") +
  theme_acs()

mds_plot1

