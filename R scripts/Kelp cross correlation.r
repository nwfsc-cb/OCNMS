


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################


# CROSS CORRELATIONS BETWEEN OTTERS AND KELP

# 
# head(otter.kern.dat)
# head(kelp.ts.all)

### Pretty up some names
kelp.ts.all$Site <- as.character(kelp.ts.all$Site)
otter.kern.dat$location <- as.character(otter.kern.dat$location)
kelp.ts.all$Site[kelp.ts.all$Site=="Destruction Island SW"] <- "Destruction Island"
otter.kern.dat$location[otter.kern.dat$location=="Chibadehl Rock"] <- "Chibadehl Rocks"

otter.kern.dat <- otter.kern.dat %>% dplyr::rename(otter.numb = tot.pop)
kelp.otter.dat <- merge(kelp.ts.all,otter.kern.dat,by.y=c("location","Region","Year"),by.x=c("Site","Region","year"),all=T)




kelp.otter.dat$Site <- factor(kelp.otter.dat$Site,levels=NOM)


C1 <- ggplot(kelp.otter.dat) +
  geom_point(aes(y=otter.numb,x=Dev))+
  facet_wrap(~Site,scales="free")
C1        


######################
## Kelp cross-correlation & moving window cross correlation



# drop Chib Rocks
A <- dcast(kelp.ts.all %>% filter(Site != "Chibadehl Rocks"),year ~ Site,value.var="Dev")
# Add in missing years of data 
A <- merge(A,data.frame(year=seq(min(A$year),max(A$year))),all=T)
YEARS <- sort(unique(A$year))
WIN <- 6

cor.window.dat <- NULL
for( i in 1:(nrow(A)-WIN)){
  temp <- A %>% filter(year>=YEARS[i],year<(YEARS[i]+WIN))
  COR <- cor(temp[,2:ncol(temp)],use="complete.obs")
  COR <- COR * lower.tri(COR)
  cor.window.dat <- rbind(cor.window.dat,data.frame(start.year = YEARS[i],window =WIN,  melt(COR)))
}

cor.window.dat <- cor.window.dat %>% dplyr::rename(Site.1=Var1,Site.2=Var2) %>% filter(value!=1,value != 0)

S.R <- kelp.ts.all %>% group_by(Site,Region) %>% summarise(length(Site)) %>% select(Site,Region)
cor.window.dat <- merge(cor.window.dat,S.R,by.y="Site",by.x="Site.2")
cor.window.dat <- cor.window.dat %>% dplyr::rename(Region.2=Region)
cor.window.dat <- merge(cor.window.dat,S.R,by.y="Site",by.x="Site.1")
cor.window.dat <- cor.window.dat %>% dplyr::rename(Region.1=Region)

cor.window.dat$pair <- ""
cor.window.dat$pair[cor.window.dat$Region.1 =="Northern" & cor.window.dat$Region.2 =="Northern"] <- "North"
cor.window.dat$pair[cor.window.dat$Region.1 =="Northern" & cor.window.dat$Region.2 =="Central"] <- "N.C"
cor.window.dat$pair[cor.window.dat$Region.1 =="Central"  & cor.window.dat$Region.2 =="Northern"] <- "N.C"
cor.window.dat$pair[cor.window.dat$Region.1 =="Northern" & cor.window.dat$Region.2 =="Southern"] <- "N.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Southern" & cor.window.dat$Region.2 =="Northern"] <- "N.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Central" & cor.window.dat$Region.2 =="Central" ]  <- "Central"
cor.window.dat$pair[cor.window.dat$Region.1 =="Southern" & cor.window.dat$Region.2 =="Central"]  <- "C.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Central"  & cor.window.dat$Region.2 =="Southern"] <- "C.S"
cor.window.dat$pair[cor.window.dat$Region.1 =="Southern" & cor.window.dat$Region.2 =="Southern"] <- "South"


COL   <- viridis(3,begin=0,end=0.7)
SPAN <- 0.5
kelp.cor.1 <- ggplot(cor.window.dat %>% filter(pair %in% c("North","Central","South"))) +
  geom_point(aes(y=value,x=start.year,color=pair),alpha=0.5) +
  geom_smooth(aes(y=value,x=start.year,color=pair),method="loess",span=SPAN,se=F) +
  scale_colour_manual(name="Region",values=COL) +
  geom_hline(yintercept = 0,linetype="dotted") +
  theme_os()

kelp.cor.2 <- ggplot(cor.window.dat %>% filter(pair %in% c("N.C","C.S","N.S"))) +
  geom_point(aes(y=value,x=start.year,color=pair),alpha=0.5) +
  geom_smooth(aes(y=value,x=start.year,color=pair),method="loess",span=SPAN,se=F) +
  scale_colour_manual(name="Region",values=COL) +
  geom_hline(yintercept = 0,linetype="dotted") +
  theme_os()

Layout= matrix(c(1,2),nrow=2,ncol=1,byrow=F)
QQ <- list(kelp.cor.1,
           kelp.cor.2)
multiplot(plotlist=QQ ,layout= Layout)

# for JAMEALtheme_js

# setwd("~/Documents/Github/OCNMS/Data/csv files")
# base.dir <- "~/Documents/Github/OCNMS/"
