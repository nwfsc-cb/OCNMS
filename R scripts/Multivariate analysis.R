library(reshape2)
# library(ggplot2)
# library(ggdendro)
library(ecodist)
library(vegan)
# library(mclust)
# library(cluster)
library(plyr)
library(ggrepel)
# library(zoo)
# library(fpc)
library(gridExtra)

# for OLE 
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Combine Sea Otter and Kelp in one plot.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Data process 2017.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"

# for JAMEAL
# base.dir <- "~/Documents/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Combine Sea Otter and Kelp in one plot.R",sep=""))
# base.dir <- "~/Documents/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Data process 2017.R",sep=""))
base.dir <- "~/Documents/GitHub/OCNMS/"
source(paste0(base.dir,"R scripts/theme_Publication.R"))

# Important data frames are 
# dat.trim 
# dat.trim.coastwide

# Groups of interest
GROUP <-  c("urchin","gastropod","bivalve","crab","seastar","cucumber")

# Years of interest
  # 1995 is missing data for seastars, gastropods
FOCAL_YEARS <- c(1987, 1999, 2015)

# head(dat.trim)
# dim(dat.trim)
# 
# # Get rid of non-focal sites.
# dat.trim <- dat.trim %>% filter(Region != "")
# dim(dat.trim)
# # Get rid of non-focal years.
# dat.trim <- dat.trim %>% filter(Year != 1995)
# dim(dat.trim)
# # Get rid of non-focal groups.
# dat.trim <- dat.trim %>% filter(group != "anenome" &
#                                   group != "chiton" &
#                                   group != "nudibranch" &
#                                   group != "tunicate"
# )
# dim(dat.trim)
# 
# write.csv(dat.trim, paste0(base.dir,"/Data/csv files/Annual mean, SD, and SE invert densities by site and region.csv"), row.names=FALSE)


### READ IN DATA
dat.trim <- read.csv(paste0(base.dir,"/Data/csv files/Annual mean, SD, and SE invert densities by site and region.csv"), header=TRUE)

#format group densities for nMDS
dat.trim.wide <- dcast(dat.trim, Year + Site + Region ~ group, value.var="MEAN")


## COMPARE 1987, 1995, 2015. DROP GASTROPODS.
dat.trim.wide.allyears <- subset(dat.trim.wide, select = -c(gastropod))

## COMPARE 1987 to 2015 SO AS TO INCLUDE GASTROPODS.
dat.trim.wide.1987_2015 <- dat.trim.wide[which(dat.trim.wide$Year == 1987 | dat.trim.wide$Year == 2015),]

### 1)
###########################################################################
#### DO SOME NMDS - ALL YEARS
###########################################################################

#Calculate distance matrix. Jaccard is for pres-abs, excludes joint absences, and similarity should decay across space. Bray-Curtis also excludes joint absences, but relies on abundance information to consider compositional differences. Modified Gower is similar to BC, but considers log abundance. Euclidean or Manhattan include joint absences, which can be appropriate for predation studies, best to log x+1 transform first.

#dj <- vegdist(dat.trim.wide.allyears[,4:8],method = "jaccard",binary=TRUE, na.rm=TRUE)
# dg <- vegdist(sqrt(dat.trim.wide[,4:8]),method = "altGower")
dbc <- vegdist(dat.trim.wide.allyears[,4:8],method = "bray", na.rm=TRUE)
dm <- vegdist(log10(dat.trim.wide.allyears[,4:8]+1), method = "manhattan")


#### NMDS Jaccard
# allits_j=nmds(dj,maxdim=2)
# bestnmds_j=nmds.min(allits_j)
# NMDS_j = data.frame(bestnmds_j,cov$category,cov$Site.code,cov$pair.number,cov$Year)
# names(NMDS_j) = c("MDS1","MDS2","Category","Site","Pair","Year")
# NMDS_j$dist<-c("Jaccard")

#### NMDS Bray Curtis
allits_bc=nmds(dbc,maxdim=2)
bestnmds_bc=nmds.min(allits_bc)
NMDS_bc = data.frame(bestnmds_bc,dat.trim.wide.allyears$Year,dat.trim.wide.allyears$Site,dat.trim.wide.allyears$Region)
names(NMDS_bc) = c("MDS1","MDS2","Year","Site","Region")
NMDS_bc$dist<-c("BrayCurtis")

#### NMDS Manhattan
allits_m=nmds(dm,maxdim=2)
bestnmds_m=nmds.min(allits_m)
NMDS_m = data.frame(bestnmds_m,dat.trim.wide.allyears$Year,dat.trim.wide.allyears$Site,dat.trim.wide.allyears$Region)
names(NMDS_m) = c("MDS1","MDS2","Year","Site","Region")
NMDS_m$dist<-c("Manhattan")

#### VECTORS
vectors.df<-envfit(bestnmds_bc, dat.trim.wide.allyears[,4:8], perm=1000, na.rm=TRUE)
vectors.df<-as.data.frame(vectors.df$vectors$arrows*sqrt(vectors.df$vectors$r))
vectors.df$groups<-rownames(vectors.df)
names(vectors.df) <- c("MDS1","MDS2","group")

vectors.df.m<-envfit(bestnmds_m, dat.trim.wide.allyears[,4:8], perm=1000, na.rm=TRUE)
vectors.df.m<-as.data.frame(vectors.df.m$vectors$arrows*sqrt(vectors.df.m$vectors$r))
vectors.df.m$groups<-rownames(vectors.df.m)
names(vectors.df.m) <- c("MDS1","MDS2","group")

###########################################################################


###########################################################################
#### PLOTTING - ALL YEARS
###########################################################################

#HULLS
find_hull <- function(df) df[chull(df$MDS1, df$MDS2),]

# hulls_u_j <- ddply(NMDS_j,"Category", find_hull)
# hulls_u_j$dist<-c("Jaccard")
hulls_u_bc_region <- ddply(NMDS_bc,"Region", find_hull)
hulls_u_m_region <- ddply(NMDS_m,"Region", find_hull)

hulls_u_bc_year <- ddply(NMDS_bc,"Year", find_hull)
hulls_u_m_year <- ddply(NMDS_m,"Year", find_hull)


#### BRAYCURTIS

mds_plot_byRegion <- ggplot(data=NMDS_bc,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_point(aes(colour=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_bc_region , alpha = 0.2,aes(fill=factor(Region),colour=factor(Region),group=factor(Region))) +
  ggtitle("Regional similarities all years") +
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=min(NMDS_bc$MDS1), y=min(NMDS_bc$MDS2), label=paste('Stress =',round(min(allits_bc$stress),3)), hjust=0)+
  theme_Publication()
  # theme_bw() +
  # theme(
  #   text=element_text(size=14),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   panel.background = element_rect(fill = NA,colour = "black",size=2),
  #   plot.title = element_text(hjust = 0.5)
  # )

mds_plot_byRegion

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 BrayCurtis grouped by region.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion)
dev.off()

mds_plot_byYear <- ggplot(data=NMDS_bc,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_point(aes(colour=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_bc_year , alpha = 0.2,aes(fill=factor(Year),colour=factor(Year),group=factor(Year))) +
  ggtitle("Year similarities all regions") +
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=min(NMDS_bc$MDS1), y=min(NMDS_bc$MDS2), label=paste('Stress =',round(min(allits_bc$stress),3)), hjust=0)+
  theme_Publication()
# theme_bw() +
# theme(
#   text=element_text(size=14),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = NA,colour = "black",size=2),
#   plot.title = element_text(hjust = 0.5)
# )

mds_plot_byYear

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 BrayCurtis grouped by year.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear)
dev.off()

mds_plot_byRegion_vectors <- ggplot(data=NMDS_bc)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_bc_region, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_segment(data=vectors.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df,aes(x=MDS1,y=MDS2,label=group),size=5)+
  ggtitle("Regional similarities all years")+
  #scale_colour_manual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=min(NMDS_bc$MDS1), y=min(NMDS_bc$MDS2), label=paste('Stress =',round(min(allits_bc$stress),3)), hjust=0)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byRegion_vectors

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 BrayCurtis grouped by region with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_vectors)
dev.off()

mds_plot_byYear_vectors <- ggplot(data=NMDS_bc)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_bc_year, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_segment(data=vectors.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df,aes(x=MDS1,y=MDS2,label=group),size=5)+
  #geom_segment(aes(x=MDS1,y=MDS2, group=Site))+
  # 
  # geom_line(data=NMDS_bc,aes(group=Site))+
  # 
ggtitle("Year similarities all regions")+
  #scale_colour_manual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=min(NMDS_bc$MDS1), y=min(NMDS_bc$MDS2), label=paste('Stress =',round(min(allits_bc$stress),3)), hjust=0)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byYear_vectors

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 BrayCurtis grouped by year with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_vectors)
dev.off()


#### MANHATTAN

mds_plot_byRegion_m <- ggplot(data=NMDS_m,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_point(aes(colour=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_m_region , alpha = 0.2,aes(fill=factor(Region),colour=factor(Region),group=factor(Region))) +
  ggtitle("Regional similarities all years") +
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=min(NMDS_m$MDS1), y=min(NMDS_m$MDS2), label=paste('Stress =',round(min(allits_m$stress),3)), hjust=0)+
  theme_Publication()
# theme_bw() +
# theme(
#   text=element_text(size=14),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = NA,colour = "black",size=2),
#   plot.title = element_text(hjust = 0.5)
# )

mds_plot_byRegion_m

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 Manhattan grouped by region.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_m)
dev.off()

mds_plot_byYear_m <- ggplot(data=NMDS_m,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_point(aes(colour=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_m_year , alpha = 0.2,aes(fill=factor(Year),colour=factor(Year),group=factor(Year))) +
  ggtitle("Year similarities all regions") +
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=min(NMDS_m$MDS1), y=min(NMDS_m$MDS2), label=paste('Stress =',round(min(allits_m$stress),3)), hjust=0)+
  theme_Publication()
# theme_bw() +
# theme(
#   text=element_text(size=14),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = NA,colour = "black",size=2),
#   plot.title = element_text(hjust = 0.5)
# )

mds_plot_byYear_m

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 Manhattan grouped by year.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_m)
dev.off()

mds_plot_byRegion_vectors_m <- ggplot(data=NMDS_m)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_m_region, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_segment(data=vectors.df.m,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df.m,aes(x=MDS1,y=MDS2,label=group),size=5)+
  ggtitle("Regional similarities all years")+
  #scale_colour_manual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=max(vectors.df.m$MDS1), y=max(NMDS_m$MDS2), label=paste('Stress =',round(min(allits_m$stress),3)), hjust=1)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byRegion_vectors_m

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 Manhattan grouped by region with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_vectors_m)
dev.off()

mds_plot_byYear_vectors_m <- ggplot(data=NMDS_m)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_m_year, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_segment(data=vectors.df.m,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df.m,aes(x=MDS1,y=MDS2,label=group),size=5)+
  #geom_segment(aes(x=MDS1,y=MDS2, group=Site))+
  # 
  # geom_line(data=NMDS_bc,aes(group=Site))+
  # 
  ggtitle("Year similarities all regions")+
  #scale_colour_manual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=max(vectors.df.m$MDS1), y=max(NMDS_m$MDS2), label=paste('Stress =',round(min(allits_m$stress),3)), hjust=1)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byYear_vectors_m

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 Manhattan grouped by year with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_vectors_m)
dev.off()


###########################################################################

################################################################################
#Analysis to look at full permanova and permadisp results - BRAYCURTIS
################################################################################ 

# PERMANOVA test for effect of region
write.csv(adonis(dbc~dat.trim.wide.allyears$Region,strata=dat.trim.wide$Year)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-1995-2015 BrayCurtis grouped by region.csv",sep=""),row.names=TRUE)
# PERMANOVA test for effect of year
write.csv(adonis(dbc~dat.trim.wide.allyears$Year,strata=dat.trim.wide$Region)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-1995-2015 BrayCurtis grouped by year.csv",sep=""),row.names=TRUE)


# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG REGIONS - NS
m1 <- betadisper(dbc,dat.trim.wide.allyears$Region)
permutest(m1)
write.csv(permutest(m1)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-1995-2015 BrayCurtis grouped by region.csv",sep=""),row.names=FALSE)
# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG YEARS - NS
m2 <- betadisper(dbc,dat.trim.wide.allyears$Year)
permutest(m2)
write.csv(permutest(m2)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-1995-2015 BrayCurtis grouped by year.csv",sep=""),row.names=FALSE)


# #write.csv(permutest(m3)[1],"Betadisper nomud year.all.csv",row.names=FALSE)

################################################################################
#Analysis to look at full permanova and permadisp results - MANHATTAN
################################################################################ 

# PERMANOVA test for effect of region
write.csv(adonis(dm~dat.trim.wide.allyears$Region,strata=dat.trim.wide$Year)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-1995-2015 Manhattan grouped by region.csv",sep=""),row.names=TRUE)
# PERMANOVA test for effect of year
write.csv(adonis(dm~dat.trim.wide.allyears$Year,strata=dat.trim.wide$Region)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-1995-2015 Manhattan grouped by year.csv",sep=""),row.names=TRUE)


# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG REGIONS - NS
m3 <- betadisper(dm,dat.trim.wide.allyears$Region)
permutest(m3)
write.csv(permutest(m3)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-1995-2015 Manhattan grouped by region.csv",sep=""),row.names=FALSE)

# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG YEARS - NS
m4 <- betadisper(dm,dat.trim.wide.allyears$Year)
permutest(m4)
write.csv(permutest(m4)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-1995-2015 Manhattan grouped by year.csv",sep=""),row.names=FALSE)




################################################################################







### #2)
###########################################################################
#### DO SOME NMDS - COMPARE 1987 to 2015 SO AS TO INCLUDE GASTROPODS.
###########################################################################

#Calculate distance matrix. Jaccard is for pres-abs, excludes joint absences, and similarity should decay across space. Bray-Curtis also excludes joint absences, but relies on abundance information to consider compositional differences. Modified Gower is similar to BC, but considers log abundance. Euclidean or Manhattan include joint absences, which can be appropriate for predation studies, best to log x+1 transform first.

#dj <- vegdist(dat.trim.wide.1987_2015[,4:9],method = "jaccard",binary=TRUE, na.rm=TRUE)
# dg <- vegdist(sqrt(dat.trim.wide.1987_2015[,4:9]),method = "altGower")
dbc_87_15 <- vegdist(dat.trim.wide.1987_2015[,4:9],method = "bray", na.rm=TRUE)
dm_87_15 <- vegdist(log10(dat.trim.wide.1987_2015[,4:9]+1), method = "manhattan")


#### NMDS Jaccard
# allits_j=nmds(dj,maxdim=2)
# bestnmds_j=nmds.min(allits_j)
# NMDS_j = data.frame(bestnmds_j,cov$category,cov$Site.code,cov$pair.number,cov$Year)
# names(NMDS_j) = c("MDS1","MDS2","Category","Site","Pair","Year")
# NMDS_j$dist<-c("Jaccard")

#### NMDS Bray Curtis
allits_bc_87_15=nmds(dbc_87_15,maxdim=2)
bestnmds_bc_87_15=nmds.min(allits_bc_87_15)
NMDS_bc_87_15 = data.frame(bestnmds_bc_87_15,dat.trim.wide.1987_2015$Year,dat.trim.wide.1987_2015$Site,dat.trim.wide.1987_2015$Region)
names(NMDS_bc_87_15) = c("MDS1","MDS2","Year","Site","Region")
NMDS_bc_87_15$dist<-c("BrayCurtis")

#### NMDS Manhattan
allits_m_87_15=nmds(dm_87_15,maxdim=2)
bestnmds_m_87_15=nmds.min(allits_m_87_15)
NMDS_m_87_15 = data.frame(bestnmds_m_87_15,dat.trim.wide.1987_2015$Year,dat.trim.wide.1987_2015$Site,dat.trim.wide.1987_2015$Region)
names(NMDS_m_87_15) = c("MDS1","MDS2","Year","Site","Region")
NMDS_m_87_15$dist<-c("Manhattan")

#### VECTORS
vectors.df_87_15<-envfit(bestnmds_bc_87_15, dat.trim.wide.1987_2015[,4:9], perm=1000, na.rm=TRUE)
vectors.df_87_15<-as.data.frame(vectors.df_87_15$vectors$arrows*sqrt(vectors.df_87_15$vectors$r))
vectors.df_87_15$groups<-rownames(vectors.df_87_15)
names(vectors.df_87_15) <- c("MDS1","MDS2","group")

vectors.df.m_87_15<-envfit(bestnmds_m_87_15, dat.trim.wide.1987_2015[,4:9], perm=1000, na.rm=TRUE)
vectors.df.m_87_15<-as.data.frame(vectors.df.m_87_15$vectors$arrows*sqrt(vectors.df.m_87_15$vectors$r))
vectors.df.m_87_15$groups<-rownames(vectors.df.m_87_15)
names(vectors.df.m_87_15) <- c("MDS1","MDS2","group")

###########################################################################


###########################################################################
#### PLOTTING - ALL YEARS
###########################################################################

#HULLS
find_hull <- function(df) df[chull(df$MDS1, df$MDS2),]

# hulls_u_j <- ddply(NMDS_j,"Category", find_hull)
# hulls_u_j$dist<-c("Jaccard")
hulls_u_bc_87_15_region <- ddply(NMDS_bc_87_15,"Region", find_hull)
hulls_u_m_87_15_region <- ddply(NMDS_m_87_15,"Region", find_hull)

hulls_u_bc_87_15_year <- ddply(NMDS_bc_87_15,"Year", find_hull)
hulls_u_m_87_15_year <- ddply(NMDS_m_87_15,"Year", find_hull)


#### BRAYCURTIS

mds_plot_byRegion_87_15 <- ggplot(data=NMDS_bc_87_15,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_point(aes(colour=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_bc_87_15_region , alpha = 0.2,aes(fill=factor(Region),colour=factor(Region),group=factor(Region))) +
  ggtitle("Regional similarities all years") +
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=min(NMDS_bc_87_15$MDS1), y=min(NMDS_bc_87_15$MDS2), label=paste('Stress =',round(min(allits_bc_87_15$stress),3)), hjust=0)+
  theme_Publication()
# theme_bw() +
# theme(
#   text=element_text(size=14),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = NA,colour = "black",size=2),
#   plot.title = element_text(hjust = 0.5)
# )

mds_plot_byRegion_87_15

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 BrayCurtis grouped by region.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_87_15)
dev.off()

mds_plot_byYear_87_15 <- ggplot(data=NMDS_bc_87_15,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_point(aes(colour=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_bc_87_15_year , alpha = 0.2,aes(fill=factor(Year),colour=factor(Year),group=factor(Year))) +
  ggtitle("Year similarities all regions") +
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=min(NMDS_bc_87_15$MDS1), y=min(NMDS_bc_87_15$MDS2), label=paste('Stress =',round(min(allits_bc_87_15$stress),3)), hjust=0)+
  theme_Publication()
# theme_bw() +
# theme(
#   text=element_text(size=14),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = NA,colour = "black",size=2),
#   plot.title = element_text(hjust = 0.5)
# )

mds_plot_byYear_87_15

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 BrayCurtis grouped by year.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_87_15)
dev.off()

mds_plot_byRegion_87_15_vectors <- ggplot(data=NMDS_bc_87_15)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_bc_87_15_region, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_segment(data=vectors.df_87_15,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df_87_15,aes(x=MDS1,y=MDS2,label=group),size=5)+
  ggtitle("Regional similarities all years")+
  #scale_colour_m_87_15anual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=max(NMDS_bc_87_15$MDS1), y=min(vectors.df_87_15$MDS2), label=paste('Stress =',round(min(allits_bc_87_15$stress),3)), hjust=1)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byRegion_87_15_vectors

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 BrayCurtis grouped by region with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_87_15_vectors)
dev.off()

mds_plot_byYear_87_15_vectors <- ggplot(data=NMDS_bc_87_15)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_bc_87_15_year, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_segment(data=vectors.df_87_15,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df_87_15,aes(x=MDS1,y=MDS2,label=group),size=5)+
  #geom_segment(aes(x=MDS1,y=MDS2, group=Site))+
  # 
  # geom_line(data=NMDS_bc_87_15,aes(group=Site))+
  # 
  ggtitle("Year similarities all regions")+
  #scale_colour_manual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=max(vectors.df_87_15$MDS1), y=min(vectors.df_87_15$MDS2), label=paste('Stress =',round(min(allits_bc_87_15$stress),3)), hjust=1)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byYear_87_15_vectors

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 BrayCurtis grouped by year with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_87_15_vectors)
dev.off()


#### MANHATTAN

mds_plot_byRegion_m_87_15 <- ggplot(data=NMDS_m_87_15,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_point(aes(colour=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_m_87_15_region , alpha = 0.2,aes(fill=factor(Region),colour=factor(Region),group=factor(Region))) +
  ggtitle("Regional similarities all years") +
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=max(NMDS_m_87_15$MDS1), y=max(NMDS_m_87_15$MDS2), label=paste('Stress =',round(min(allits_m_87_15$stress),3)), hjust=1)+
  theme_Publication()
# theme_bw() +
# theme(
#   text=element_text(size=14),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = NA,colour = "black",size=2),
#   plot.title = element_text(hjust = 0.5)
# )

mds_plot_byRegion_m_87_15

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 Manhattan grouped by region.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_m_87_15)
dev.off()

mds_plot_byYear_m_87_15 <- ggplot(data=NMDS_m_87_15,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_point(aes(colour=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_m_87_15_year , alpha = 0.2,aes(fill=factor(Year),colour=factor(Year),group=factor(Year))) +
  ggtitle("Year similarities all regions") +
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=max(NMDS_m_87_15$MDS1), y=max(NMDS_m_87_15$MDS2), label=paste('Stress =',round(min(allits_m_87_15$stress),3)), hjust=1)+
  theme_Publication()
# theme_bw() +
# theme(
#   text=element_text(size=14),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = NA,colour = "black",size=2),
#   plot.title = element_text(hjust = 0.5)
# )

mds_plot_byYear_m_87_15

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 Manhattan grouped by year.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_m_87_15)
dev.off()

mds_plot_byRegion_vectors_m_87_15 <- ggplot(data=NMDS_m_87_15)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_m_87_15_region, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_segment(data=vectors.df.m_87_15,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df.m_87_15,aes(x=MDS1,y=MDS2,label=group),size=5)+
  ggtitle("Regional similarities all years")+
  #scale_colour_m_87_15anual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
  annotate("text", x=max(vectors.df.m_87_15$MDS1), y=max(NMDS_m_87_15$MDS2), label=paste('Stress =',round(min(allits_m_87_15$stress),3)), hjust=0.4)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byRegion_vectors_m_87_15

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 Manhattan grouped by region with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_vectors_m_87_15)
dev.off()

mds_plot_byYear_vectors_m_87_15 <- ggplot(data=NMDS_m_87_15)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_m_87_15_year, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_segment(data=vectors.df.m_87_15,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df.m_87_15,aes(x=MDS1,y=MDS2,label=group),size=5)+
  #geom_segment(aes(x=MDS1,y=MDS2, group=Site))+
  # 
  # geom_line(data=NMDS_bc_87_15,aes(group=Site))+
  # 
  ggtitle("Year similarities all regions")+
  #scale_colour_m_87_15anual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
  annotate("text", x=max(NMDS_m_87_15$MDS1), y=max(NMDS_m_87_15$MDS2), label=paste('Stress =',round(min(allits_m_87_15$stress),3)), hjust=1)+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byYear_vectors_m_87_15

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 Manhattan grouped by year with vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_vectors_m_87_15)
dev.off()


###########################################################################

################################################################################
#Analysis to look at full permanova and permadisp results - BRAYCURTIS
################################################################################ 

# PERMANOVA test for effect of region
write.csv(adonis(dbc_87_15~dat.trim.wide.1987_2015$Region,strata=dat.trim.wide.1987_2015$Year)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-2015 BrayCurtis grouped by region.csv",sep=""),row.names=TRUE)
# PERMANOVA test for effect of year
write.csv(adonis(dbc_87_15~dat.trim.wide.1987_2015$Year,strata=dat.trim.wide.1987_2015$Region)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-2015 BrayCurtis grouped by year.csv",sep=""),row.names=TRUE)


# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG REGIONS - 
m5 <- betadisper(dbc_87_15,dat.trim.wide.1987_2015$Region)
permutest(m5)
write.csv(permutest(m5)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-2015 BrayCurtis grouped by region.csv",sep=""),row.names=FALSE)

# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG YEARS - 
m6 <- betadisper(dbc_87_15,dat.trim.wide.1987_2015$Year)
permutest(m6)
write.csv(permutest(m6)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-2015 BrayCurtis grouped by year.csv",sep=""),row.names=FALSE)


################################################################################
#Analysis to look at full permanova and permadisp results - MANHATTAN
################################################################################ 

# PERMANOVA test for effect of region
write.csv(adonis(dm_87_15~dat.trim.wide.1987_2015$Region,strata=dat.trim.wide.1987_2015$Year)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-2015 Manhattan grouped by region.csv",sep=""),row.names=TRUE)
# PERMANOVA test for effect of year
write.csv(adonis(dm_87_15~dat.trim.wide.1987_2015$Year,strata=dat.trim.wide.1987_2015$Region)$aov.tab,paste(base.dir,"Plots/_Multivariate/PERMANOVA invert community 1987-2015 Manhattan grouped by year.csv",sep=""),row.names=TRUE)


# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG REGIONS - NS
m7 <- betadisper(dm_87_15,dat.trim.wide.1987_2015$Region)
permutest(m7)
write.csv(permutest(m7)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-2015 Manhattan grouped by region.csv",sep=""),row.names=FALSE)
# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG YEARS - NS
m8 <- betadisper(dm_87_15,dat.trim.wide.1987_2015$Year)
permutest(m8)
write.csv(permutest(m8)[1],paste(base.dir,"Plots/_Multivariate/Betadisper invert community 1987-2015 Manhattan grouped by year.csv",sep=""),row.names=FALSE)


################################################################################

### FIGS FOR PUBLICATION

grid.arrange(mds_plot_byYear_vectors_m,mds_plot_byRegion_vectors_m,ncol=2)
p1 <- arrangeGrob(mds_plot_byYear_vectors_m, top = textGrob("(a)", x = unit(0, "npc"),
                                    y  = unit(1, "npc"), just=c("left","top"),
                                    gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))

p2 <- arrangeGrob(mds_plot_byRegion_vectors_m, top = textGrob("(b)", x = unit(0, "npc"),
                                                                  y  = unit(1, "npc"), just=c("left","top"),
                                                                  gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
#grid.arrange(p1,p2, ncol=2, heights=unit(0.5, "npc") )

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-1995-2015 Manhattan for pub.pdf",sep=""),type="pdf",dpi=300,height=6,width=12 )
print(grid.arrange(p1,p2, ncol=2 ))
dev.off()

p3 <- arrangeGrob(mds_plot_byYear_vectors_m_87_15, top = textGrob("(a)", x = unit(0, "npc"),
                                                                  y  = unit(1, "npc"), just=c("left","top"),
                                                                  gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))

p4 <- arrangeGrob(mds_plot_byRegion_vectors_m_87_15, top = textGrob("(b)", x = unit(0, "npc"),
                                                                    y  = unit(1, "npc"), just=c("left","top"),
                                                                    gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
#grid.arrange(p1,p2, ncol=2, heights=unit(0.5, "npc") )

quartz(file = paste(base.dir,"/Plots/_Multivariate/MDS invert community 1987-2015 Manhattan for pub.pdf",sep=""),type="pdf",dpi=300,height=6,width=12 )
print(grid.arrange(p3,p4, ncol=2 ))
dev.off()

