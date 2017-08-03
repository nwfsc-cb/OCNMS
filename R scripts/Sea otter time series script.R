library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)


#setwd("~/Documents/Github/OCNMS/Data/csv files")


setwd("~Github/OCNMS/Data/csv files")
# for OLE 
base.dir <- "/Users/ole.shelton/GitHub/OCNMS/"
vsetwd(paste(base.dir,"Data/csv files",sep=""))


otter.dat <- read.csv("WDFW sea otter survey data 1977-2015.csv")
# Mapping for location names to discrete sites from 2015 surveys/ Kvitek surveys
location.names <- read.csv("Otter location names.csv")
# Linking sundry names to particular locations for kernel density estimates
linear.shore <- read.csv("OCNMS linear shoreline.csv")

##### MERGE CSV files

otter.dat <- merge(otter.dat,location.names[,c("Site","Category","common.name")],all=T)
otter.dat <- merge(otter.dat,linear.shore[,c("common.name","distance.miles")],all=F)

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
      panel.spacing =       unit(0.25, "lines"),
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
  theme(legend.position=c(0.3,0.7))
otters_by_site


otters_stacked <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, fill=location))+
         geom_area(position = 'stack') +
         theme_js()

otters_by_site_facet <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=location)) +
  geom_point(aes(colour=location)) +
  geom_line(aes(colour=location)) +
  facet_wrap(~location,nrow=3) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none")
otters_by_site_facet



##### CONSTRUCT KERNEL DENSITIES USING SIMPLIFIED SHORELINE LOCATIONS.

otter.dat$distance.km <- otter.dat$distance.miles * 1.60934 # Convert miles to km

nwfsc.otter.kernel <- otter.dat %>%
  group_by(Year, Category,distance.km) %>%
  summarise(
    Total.Adults = sum(Count.Independent,na.rm=T),
    Total.Otters = sum(Count.Total,na.rm=TRUE)
  )
#dim(nwfsc.otter.dat)

nwfsc.otter.kernel <- nwfsc.otter.kernel %>% mutate(location=Category) %>% dplyr::select(-Category)
nwfsc.otter.kernel <- nwfsc.otter.kernel %>% filter(Total.Otters > 0)

#### Kernel function with 










##### JAMEAL's CODE STARTS HERE:


#######################################
### sum otters by NWFSC study sites ###
#######################################
unique(otter.dat$Site.code.for.analysis)

nwfsc.otter.dat <- otter.dat %>%
  filter(Site.code.for.analysis != "Other_South" & Site.code.for.analysis != "Other_Central" & Site.code.for.analysis != "Other_North") %>%
  #droplevels() %>%
  group_by(Year, Site.code.for.analysis) %>%
  summarise(
    Total.Otters = sum(Count.Total,na.rm=TRUE)
  )
dim(nwfsc.otter.dat)
head(nwfsc.otter.dat,10)

nwfsc.otter.dat <- data.frame(nwfsc.otter.dat)

nwfsc.otter.dat$Site.Name <- as.character(nwfsc.otter.dat$Site.code.for.analysis)
nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "AP"] <- "2.Anderson Point"
nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "CA"] <- "4.Cape Alava"
nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "CJR3"] <- "5.Cape Johnson / Rock 305"
nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "DI"] <- "7.Destruction Island"
nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "PA"] <- "3.Point of the Arches"
nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "TH"] <- "6.Teawhit Head"
nwfsc.otter.dat$Site.Name[nwfsc.otter.dat$Site.code.for.analysis == "TI"] <- "1.Tatoosh Island"

head(nwfsc.otter.dat,10)
#######################################
#######################################

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
    panel.spacing =       unit(0.25, "lines"),
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


otters_by_site <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=Site.Name)) +
  geom_point(aes(colour=Site.Name)) +
  geom_line(aes(colour=Site.Name)) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position=c(0.3,0.7))
otters_by_site

ggsave("Sea otter abundance at NWFSC sites 1977-2015 single plot.pdf")


otters_by_site_facet <- ggplot(nwfsc.otter.dat,aes(x=Year,y=Total.Otters, colour=Site.Name)) +
  geom_point(aes(colour=Site.Name)) +
  geom_line(aes(colour=Site.Name)) +
  facet_wrap(~Site.Name,scales="free_y",nrow=2) +
  ylab("Number of Sea Otters") +
  theme_js() +
  theme(legend.position="none")
otters_by_site_facet

ggsave("Sea otter abundance at NWFSC sites 1977-2015 facet plot.pdf")


#######################################
#######################################
