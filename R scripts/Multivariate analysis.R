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

# for OLE 
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Combine Sea Otter and Kelp in one plot.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Data process 2017.R",sep=""))
# base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"

# for JAMEAL
# base.dir <- "~/Documents/GitHub/OCNMS/"
# source(paste(base.dir,"R scripts/Combine Sea Otter and Kelp in one plot.R",sep=""))
base.dir <- "~/Documents/GitHub/OCNMS/"
source(paste(base.dir,"R scripts/Data process 2017.R",sep=""))
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
# hulls_u_g <- ddply(NMDS_g,"Category", find_hull)
# hulls_u_g$dist<-c("modGower")

hulls_u_bc_year <- ddply(NMDS_bc,"Year", find_hull)


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

mds_plot_byRegion <- ggplot(data=NMDS_bc,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_point(aes(colour=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_bc_region , alpha = 0.2,aes(fill=factor(Region),colour=factor(Region),group=factor(Region))) +
  ggtitle("Regional similarities all years") +
  guides(fill=guide_legend(title="Region"),colour=guide_legend(title="Region"),group=guide_legend(title="Region"),pch=guide_legend(title="Region")) +
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

quartz(file = paste(base.dir,"/Plots/MDS invert community grouped by region.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion)
dev.off()

mds_plot_byYear <- ggplot(data=NMDS_bc,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_point(aes(colour=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_bc_year , alpha = 0.2,aes(fill=factor(Year),colour=factor(Year),group=factor(Year))) +
  ggtitle("Year similarities all regions") +
  guides(fill=guide_legend(title="Year"),colour=guide_legend(title="Year"),group=guide_legend(title="Year"),pch=guide_legend(title="Year")) +
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

quartz(file = paste(base.dir,"/Plots/MDS invert community grouped by year.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear)
dev.off()

vectors.df<-envfit(bestnmds_bc, dat.trim.wide[,4:9], perm=1000, na.rm=TRUE)
vectors.df<-as.data.frame(vectors.df$vectors$arrows*sqrt(vectors.df$vectors$r))
vectors.df$groups<-rownames(vectors.df)
names(vectors.df) <- c("MDS1","MDS2","group")

mds_plot_byRegion_vectors <- ggplot(data=NMDS_bc)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region),pch=factor(Region)),size=3)+
  geom_polygon(data = hulls_u_bc_region, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Region),colour=factor(Region),group=factor(Region)))+
  geom_segment(data=vectors.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df,aes(x=MDS1,y=MDS2,label=group),size=5)+
  ggtitle("Regional similarities all years")+
  #scale_colour_manual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byRegion_vectors

quartz(file = paste(base.dir,"/Plots/MDS invert community grouped by region wth vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byRegion_vectors)
dev.off()

mds_plot_byYear_vectors <- ggplot(data=NMDS_bc)+ 
  geom_point(aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year),pch=factor(Year)),size=3)+
  geom_polygon(data = hulls_u_bc_year, alpha = 0.2,aes(x=MDS1,y=MDS2,fill=factor(Year),colour=factor(Year),group=factor(Year)))+
  geom_segment(data=vectors.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
  geom_text_repel(data=vectors.df,aes(x=MDS1,y=MDS2,label=group),size=5)+
  ggtitle("Year similarities all regions")+
  #scale_colour_manual(values = c("#0bb2dd","#ec2035"))+
  #scale_fill_manual(values = c("#0bb2dd","#ec2035"))+
  theme_Publication()+
  theme(strip.background = element_rect(fill = "white", color = "white"))
mds_plot_byYear_vectors

quartz(file = paste(base.dir,"/Plots/MDS invert community grouped by year wth vectors.pdf",sep=""),type="pdf",dpi=300,height=4,width=5 )
print(mds_plot_byYear_vectors)
dev.off()

#####
#Analysis to look at full permanova and permadisp results
#### 

# PERMANOVA test for effect of region
write.csv(adonis(dbc~dat.trim.wide$Region,strata=dat.trim.wide$Year)$aov.tab,paste(base.dir,"Plots/PERMANOVA invert community grouped by region.csv",sep=""),row.names=TRUE)
# PERMANOVA test for effect of year
write.csv(adonis(dbc~dat.trim.wide$Year,strata=dat.trim.wide$Region)$aov.tab,paste(base.dir,"Plots/PERMANOVA invert community grouped by year.csv",sep=""),row.names=TRUE)


# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG REGIONS - NS
m1 <- betadisper(dbc,dat.trim.wide$Region)
permutest(m1)
# TEST FOR DIFFERENCESIN VARIANCE IN COMMUNITY STRUCTURE AMONG YEARS - NS
m2 <- betadisper(dbc,dat.trim.wide$Year)
permutest(m2)


# #write.csv(permutest(m3)[1],"Betadisper nomud year.all.csv",row.names=FALSE)