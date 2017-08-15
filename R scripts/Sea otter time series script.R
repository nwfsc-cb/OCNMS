library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(stringr)
library(ks)


# for OLE 

setwd("~Github/OCNMS/Data/csv files")
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"

# for JAMEAL
setwd("~/Documents/Github/OCNMS/Data/csv files")
base.dir <- "~/Documents/Github/OCNMS/"


# for EVERYBODY

source(paste(base.dir,"R scripts/integral.kde.R",sep=""))
setwd(paste(base.dir,"Data/csv files",sep=""))


otter.dat <- read.csv("WDFW sea otter survey data 1977-2015.csv")
# Mapping for location names to discrete sites from 2015 surveys/ Kvitek surveys
location.names <- read.csv("Otter location names.csv")
# Linking sundry names to particular locations for kernel density estimates
linear.shore <- read.csv("OCNMS linear shoreline.csv")
linear.shore$distance.km <- linear.shore$distance.miles * 1.60934
survey.locations.linear.shore <- read.csv("Survey locations linear shore.csv")
survey.locations.linear.shore$distance.km <- survey.locations.linear.shore$distance.miles *1.60934
##### MERGE CSV files

otter.dat <- merge(otter.dat,location.names[,c("Site","Category","common.name")],all=T)
otter.dat <- merge(otter.dat,linear.shore[,c("common.name","distance.miles")],all=F)

# IMPORTANT VALUES FOR KERNEL DENSITY ESTIMATION

KERN <- 40/(1.96*2) # Standard deviation of the normal kernel density in km 
          # equivalent to ~8.9 km using an average home range size of 35km from Laidre et al. 2009 J Mammology
DIAM <- 10 # buffer diameter used for calculating the number of otters around a focal site (in km)



#######################################
### sum otters by NWFSC study sites ###
#######################################
nwfsc.otter.dat <- otter.dat %>%
  group_by(Year, Category) %>%
  summarise(
    Total.Adults = sum(Count.Independent,na.rm=T),
    Total.Otters = sum(Count.Total,na.rm=TRUE)
  )
dim(nwfsc.otter.dat)
head(nwfsc.otter.dat,10)

nwfsc.otter.dat <-nwfsc.otter.dat %>% mutate(location=Category) %>% dplyr::select(-Category)


NOM <- c(
  "Quinault",
  "Destruction Island",
  "Teahwhit Head",
  "Cape Johnson",
  "Cape Alava",
  "Point of the Arches",
  "Anderson Point",
  "Tatoosh Island",
  "Chibadehl Rock",
  "Neah Bay",
  "East Juan De Fuca"
  )
nwfsc.otter.dat$location <- factor(nwfsc.otter.dat$location,levels=NOM)

# ADD zeros where appropriate
nwfsc.otter.dat <- merge(nwfsc.otter.dat,expand.grid(location=NOM,Year=sort(unique(otter.dat$Year))),all=T)
nwfsc.otter.dat$Total.Adults[is.na(nwfsc.otter.dat$Total.Adults)==T] <- 0
nwfsc.otter.dat$Total.Otters[is.na(nwfsc.otter.dat$Total.Otters)==T] <- 0



#### PLOT@
#######################################
### plot otters by NWFSC study sites ###
#######################################
theme_js <- function(base_size = 12, base_family = "") {
  theme_bw()+
    theme(
      text=element_text(size=16),
      legend.title=element_blank(),
      legend.text = element_text(size=14),
      #legend.background =  element_rect(colour = NA),
      #legend.key =         element_rect(fill = "white", colour = "black"),
      panel.background =   element_rect(fill = "white", colour = "black",size=1.5),
      panel.border =       element_blank(),
      panel.grid.major =   element_blank(),
      panel.grid.minor =   element_blank(),
     # panel.spacing =       unit(0.25, "lines"),
      strip.background =   element_rect(fill = "black", colour = "black"),
      strip.text.x =       element_text(colour="white",size=12),
      strip.text.y =       element_text(angle = -90,colour="white",size=12),
      plot.background =    element_rect(colour = "white"),
      plot.title =         element_text(size = rel(1.2)),
      plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines")
    )
}

#setwd("~/Documents/Github/OCNMS/Figures/")

setwd(paste(base.dir,"Plots",sep=""))

otters_by_site <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position=c(0.3,0.7)) +
  ggtitle(paste("Raw population estimates, discrete areas"))
otters_by_site

otters_stacked <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, fill=location))+
         geom_area(position = 'stack') +
         theme_js()+
          ggtitle(paste("Raw population estimates, discrete areas"))
otters_stacked

otters_by_site_facet <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~location,nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none") +
  ggtitle(paste("Raw population estimates, discrete areas"))

otters_by_site_facet

otters_by_site_facet2 <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~location,scales="free_y",nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none") +
  ggtitle(paste("Raw population estimates, discrete areas"))
otters_by_site_facet2


#######################################################################
##### CONSTRUCT KERNEL DENSITIES USING SIMPLIFIED 1-D SHORELINE LOCATIONS.
#######################################################################
otter.dat$distance.km <- otter.dat$distance.miles * 1.60934 # Convert miles to km

nwfsc.otter.kernel <- otter.dat %>%
  group_by(Year, Category,distance.km) %>%
  summarise(
    Total.Adults = sum(Count.Independent,na.rm=T),
    Total.Otters = sum(Count.Total,na.rm=TRUE)
  )
#dim(nwfsc.otter.dat)

nwfsc.otter.kernel <- nwfsc.otter.kernel %>% mutate(location=Category) %>% 
                         # dplyr::select(-Category)%>%
                          filter(Total.Otters > 0)

nwfsc.otter.by.year <- otter.dat %>% group_by(Year) %>% 
                            summarise(Total=sum(Count.Total,na.rm=T)) %>% as.data.frame()

#### Kernel density function for each year. 
library(ks)

Eval.Points <- seq(-20,max(otter.dat$distance.km)*1.1,by=0.5)

YEARS <-  sort(unique(nwfsc.otter.kernel$Year))
otter.kern <- list()
for(i in 1:length(YEARS)){
  temp <- nwfsc.otter.kernel %>% filter(Year ==YEARS[i])
  otter.kern[[i]] <- kde(
        x = rep(temp$distance.km,temp$Total.Otters),
        h =KERN, # Standard deviation of normal distribution
        eval.points = Eval.Points
        )
}

# Convert output to data.frame for plotting
otter.kern.dat <- NULL
for(i in 1:length(YEARS)){
  temp <- data.frame(year=YEARS[i],loc = otter.kern[[i]]$eval.points, 
                     dens = otter.kern[[i]]$estimate)
  otter.kern.dat <- rbind(otter.kern.dat,temp)
}

MULT <- 40 # Multiple for making the density larger and easier to see.
otter.kern.dat$dens.plus <- otter.kern.dat$dens * MULT + otter.kern.dat$year
y.labels <- survey.locations.linear.shore 


otter.dist.timeseries<- ggplot(otter.kern.dat,aes(x=dens.plus,y=loc,group=year)) +
    geom_polygon(fill=grey(0.5),alpha=0.7) + 
    theme_js() +
    xlab("Year") +
    ylab("Location") +
    geom_hline(yintercept = (y.labels %>% filter(Area=="Tatoosh Island"))$distance.km,linetype="dashed" ) + 
    scale_y_continuous(breaks=y.labels$distance.km, labels=y.labels$Area,
                       limits=c(0,180))+
    ggtitle(paste("Proportional Distribution; kernel =",round(KERN,2))) 
  
otter.dist.timeseries

#########################################
##########################################
## Calculate Kernel smoothed total abundance for sections of the coast
#########################################
##########################################

# Identify breakpoints for each area

survey.locations.linear.shore <-  survey.locations.linear.shore %>% 
                                    mutate(lower=distance.km - DIAM,upper=distance.km+DIAM)
kern.pop.est <- NULL  
for(i in 1:length(YEARS)){
  lower.cum.prob <- integral.kde(survey.locations.linear.shore$lower,otter.kern[[i]],density=T)  
  upper.cum.prob <- integral.kde(survey.locations.linear.shore$upper,otter.kern[[i]],density=T)  
  prop.pop <- upper.cum.prob - lower.cum.prob
  tot.pop  <- nwfsc.otter.by.year %>% filter(Year ==YEARS[i]) %>% dplyr::select(Total) %>%
                        as.numeric() * prop.pop
  kern.pop.est <- rbind(kern.pop.est,data.frame(Year=YEARS[i],location=survey.locations.linear.shore$Area,prop.pop,tot.pop))
}
kern.pop.est.trim     <- kern.pop.est %>% filter(location %in% NOM)
kern.pop.est.trim$location <- factor(kern.pop.est.trim$location,levels=NOM)

### 08142017:: Jameal adds regions

kern.pop.est.trim$Region <- NA
northern <- c(as.character(unique(kern.pop.est.trim$location)[8]),as.character(unique(kern.pop.est.trim$location)[4]), as.character(unique(kern.pop.est.trim$location)[6]))
central <- c(as.character(unique(kern.pop.est.trim$location)[1:2]),as.character(unique(kern.pop.est.trim$location)[7]))
southern <- c(as.character(unique(kern.pop.est.trim$location)[5]),as.character(unique(kern.pop.est.trim$location)[9]),as.character(unique(kern.pop.est.trim$location)[3]))

head(kern.pop.est.trim)
kern.pop.est.trim$Region[grep(paste(northern,collapse="|"), 
                              kern.pop.est.trim$location)] <- "Northern"
kern.pop.est.trim$Region[grep(paste(central,collapse="|"), 
                              kern.pop.est.trim$location)] <- "Central"
kern.pop.est.trim$Region[grep(paste(southern,collapse="|"), 
                              kern.pop.est.trim$location)] <- "Southern"
View(kern.pop.est.trim)

# head(
#   kern.pop.est.trim %>%
#     filter(str_detect(location, paste(northern, collapse="|"))) %>%
#     mutate(Region = "Northern")
# )

kern.pop.est.trim <- transform(
  kern.pop.est.trim,
  Region=factor(Region,levels=c("Northern","Central","Southern"))
)

#### POPULATION TIME SERIES USING kernel smoothed density estimate and fixed diameters around
 ## the study sites.

otters_by_site_kern <- ggplot(kern.pop.est.trim,aes(x=Year,y=tot.pop, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position=c(0.3,0.7)) +
  ggtitle(paste("Kernel population estimates, kernel=",round(KERN,2),";",DIAM,"km buffer"))
otters_by_site_kern

otters_by_site_facet_kern <- ggplot(kern.pop.est.trim,aes(x=Year,y=tot.pop, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~location,nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none") +
  ggtitle(paste("Kernel population estimates, kernel=",round(KERN,2),";",DIAM,"km buffer"))
otters_by_site_facet_kern

otters_by_site_facet2_kern <- ggplot(kern.pop.est.trim,aes(x=Year,y=tot.pop, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~location,scales="free_y",nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none") +
  ggtitle(paste("Kernel population estimates, kernel=",round(KERN,2),";",DIAM,"km buffer"))
otters_by_site_facet2_kern

### 08142017:: Jameal adds facet plots by region

otters_by_site_facet3_kern <- ggplot(kern.pop.est.trim,aes(x=Year,y=tot.pop, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~Region,scales="free_y",nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  #theme(legend.position="none") +
  ggtitle(paste("Kernel population estimates, kernel=",round(KERN,2),";",DIAM,"km buffer"))
otters_by_site_facet3_kern

ggsave("3 Regions Otter Plots.pdf")

## Write to file
write.csv(kern.pop.est.trim,file=paste(base.dir,"Data/csv files/Kernel otter abundances; kern=",round(KERN,1),".csv",sep=""))

######################################## 
##REPEAT ANALYSES USING smaller kernel for the home range (half of original)
########################################
 KERN <- KERN/2

otter.kern <- list()
for(i in 1:length(YEARS)){
  temp <- nwfsc.otter.kernel %>% filter(Year ==YEARS[i])
  otter.kern[[i]] <- kde(
    x = rep(temp$distance.km,temp$Total.Otters),
    h =KERN, # Standard deviation of normal distribution
    eval.points = Eval.Points
  )
}

# Convert output to data.frame for plotting
otter.kern.dat <- NULL
for(i in 1:length(YEARS)){
  temp <- data.frame(year=YEARS[i],loc = otter.kern[[i]]$eval.points, 
                     dens = otter.kern[[i]]$estimate)
  otter.kern.dat <- rbind(otter.kern.dat,temp)
}

MULT <- 40 # Multiple for making the density larger and easier to see.
otter.kern.dat$dens.plus <- otter.kern.dat$dens * MULT + otter.kern.dat$year
y.labels <- survey.locations.linear.shore 


otter.dist.timeseries2 <- ggplot(otter.kern.dat,aes(x=dens.plus,y=loc,group=year)) +
  geom_polygon(fill=grey(0.5),alpha=0.7) + 
  theme_js() +
  xlab("Year") +
  ylab("Location") +
  geom_hline(yintercept = (y.labels %>% filter(Area=="Tatoosh Island"))$distance.km,linetype="dashed" ) + 
  scale_y_continuous(breaks=y.labels$distance.km, labels=y.labels$Area,
                     limits=c(0,180))+
  ggtitle(paste("Proportional Distribution; kernel =",round(KERN,2))) 
  
  
  #########################################
##########################################
## Calculate Kernel smoothed total abundance for sections of the coast
#########################################
##########################################

# Identify breakpoints for each area

survey.locations.linear.shore <-  survey.locations.linear.shore %>% 
  mutate(lower=distance.km - DIAM,upper=distance.km+DIAM)
kern.pop.est <- NULL  
for(i in 1:length(YEARS)){
  lower.cum.prob <- integral.kde(survey.locations.linear.shore$lower,otter.kern[[i]],density=T)  
  upper.cum.prob <- integral.kde(survey.locations.linear.shore$upper,otter.kern[[i]],density=T)  
  prop.pop <- upper.cum.prob - lower.cum.prob
  tot.pop  <- nwfsc.otter.by.year %>% filter(Year ==YEARS[i]) %>% dplyr::select(Total) %>%
    as.numeric() * prop.pop
  kern.pop.est <- rbind(kern.pop.est,data.frame(Year=YEARS[i],location=survey.locations.linear.shore$Area,prop.pop,tot.pop))
}
kern.pop.est.trim     <- kern.pop.est %>% filter(location %in% NOM)
kern.pop.est.trim$location <- factor(kern.pop.est.trim$location,levels=NOM)
#### POPULATION TIME SERIES USING kernel smoothed density estimate and fixed diameters around
## the study sites.

otters_by_site_kern2 <- ggplot(kern.pop.est.trim,aes(x=Year,y=tot.pop, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position=c(0.3,0.7)) +
  ggtitle(paste("Kernel population estimates, kernel=",round(KERN,2),";",DIAM,"km buffer"))
otters_by_site_kern

otters_by_site_facet_kern2 <- ggplot(kern.pop.est.trim,aes(x=Year,y=tot.pop, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~location,nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none") +
  ggtitle(paste("Kernel population estimates, kernel=",round(KERN,2),";",DIAM,"km buffer"))

otters_by_site_facet2_kern2 <- ggplot(kern.pop.est.trim,aes(x=Year,y=tot.pop, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~location,scales="free_y",nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none") +
  ggtitle(paste("Kernel population estimates, kernel=",round(KERN,2),";",DIAM,"km buffer"))
##############################################################################

pdf(file="Otter time-series plots.pdf",onefile=T,width=11,height=8.5)
  print(otters_by_site)
  print(otters_stacked)
  print(otters_by_site_facet)
  print(otters_by_site_facet2)
  print(otter.dist.timeseries)
  print(otters_by_site_kern)
  print(otters_by_site_facet_kern)
  print(otters_by_site_facet2_kern)
  print(otters_by_site_facet3_kern)

  print(otter.dist.timeseries2)
  print(otters_by_site_kern2)
  print(otters_by_site_facet_kern2)
  print(otters_by_site_facet2_kern2)
  
dev.off()  

################# Write Otter abundances by site to file that can be called in other scripts

write.csv(kern.pop.est.trim,file=paste(base.dir,"Data/csv files/Kernel otter abundances; kern=",round(KERN,1),".csv",sep=""))



##### JAMEAL's CODE STARTS HERE:
# 
# 
# #######################################
# ### sum otters by NWFSC study sites ###
# #######################################
# unique(otter.dat$Site.code.for.analysis)
# 
# nwfsc.otter.dat <- otter.dat %>%
#   filter(Site.code.for.analysis != "Other_South" & Site.code.for.analysis != "Other_Central" & Site.code.for.analysis != "Other_North") %>%
#   #droplevels() %>%
#   group_by(Year, Site.code.for.analysis) %>%
#   summarise(
#     Total.Otters = sum(Count.Total,na.rm=TRUE)
#   )
# dim(nwfsc.otter.dat)
# head(nwfsc.otter.dat,10)
# 
# nwfsc.otter.dat <- data.frame(nwfsc.otter.dat)
# 
# nwfsc.otter.dat$Site.Name <- as.character(nwfsc.otter.dat$Site.code.for.analysis)
# nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "AP"] <- "2.Anderson Point"
# nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "CA"] <- "4.Cape Alava"
# nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "CJR3"] <- "5.Cape Johnson / Rock 305"
# nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "DI"] <- "7.Destruction Island"
# nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "PA"] <- "3.Point of the Arches"
# nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "TH"] <- "6.Teawhit Head"
# nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "TI"] <- "1.Tatoosh Island"
# 
# head(nwfsc.otter.dat,10)
# #######################################
# #######################################
# 
# #######################################
# ### plot otters by NWFSC study sites ###
# #######################################
# theme_js <- function(base_size = 12, base_family = "") {
#   theme_bw()+
#   theme(
#     text=element_text(size=16),
#     legend.title=element_blank(),
#     legend.text = element_text(size=14),
#     #legend.background =  element_rect(colour = NA),
#     #legend.key =         element_rect(fill = "white", colour = "black"),
#     panel.background =   element_rect(fill = "white", colour = "black",size=1.5),
#     panel.border =       element_blank(),
#     panel.grid.major =   element_blank(),
#     panel.grid.minor =   element_blank(),
#     panel.spacing =       unit(0.25, "lines"),
#     strip.background =   element_rect(fill = "black", colour = "black"),
#     strip.text.x =       element_text(colour="white",size=12),
#     strip.text.y =       element_text(angle = -90,colour="white",size=12),
#     plot.background =    element_rect(colour = "white"),
#     plot.title =         element_text(size = rel(1.2)),
#     plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines")
#   )
# }
# 
# #setwd("~/Documents/Github/OCNMS/Figures/")
# 
# setwd(paste(base.dir,"Plots",sep=""))
# 
# 
# otters_by_site <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=Site.Name)) +
#   geom_point(aes(colour=Site.Name)) +
#   geom_line(aes(colour=Site.Name)) +
#   ylab("Number of Sea Otters") +
#   theme_js() +
#   theme(legend.position=c(0.3,0.7))
# otters_by_site
# 
# ggsave("Sea otter abundance at NWFSC sites 1977-2015 single plot.pdf")
# 
# 
# otters_by_site_facet <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=Site.Name)) +
#   geom_point(aes(colour=Site.Name)) +
#   geom_line(aes(colour=Site.Name)) +
#   facet_wrap(~Site.Name,scales="free_y",nrow=2) +
#   ylab("Number of Sea Otters") +
#   theme_js() +
#   theme(legend.position="none")
# otters_by_site_facet
# 
# ggsave("Sea otter abundance at NWFSC sites 1977-2015 facet plot.pdf")
# 



#######################################
#######################################
