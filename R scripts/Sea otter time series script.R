library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)


#setwd("~/Documents/Github/OCNMS/Data/csv files")

# for OLE:
setwd("~Github/OCNMS/Data/csv files")


otter.dat <- read.csv("WDFW sea otter survey data 1977-2015.csv")
head(otter.dat)


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

setwd("~/Documents/Github/OCNMS/Figures/")

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
