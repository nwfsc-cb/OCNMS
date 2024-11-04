library(maps)       #basic mapping functions and some data
#library(mapdata)    #some additional hires data
#library(maptools)   #useful tools such as reading shapefiles
#library(PBSmapping) #powerful mapping functions developed by Pacific Biological Station
loc = paste0(getwd(),'/figures/')
graphics.off()

png(paste0(loc,"map-of-coast.png"), units="in", width=6, height=5, res=300)

map('world', xlim=c(-140, -114), ylim=c(32, 48.5), col="grey", fill=TRUE, lwd=2)  

#map.scale(relwidth=0.5,ratio=FALSE,cex=1.0)
par(  yaxp=c(32,48, 16), oma = c(0,0,0,0), ps=10, cex = 1)
map.axes()
grid()  

text.color = 'red'
pt.color = 'black'
pt.bg.wc = 'red'
pt.bg.ak = 'green'
pt.bg.bc = "yellow"
CEX= 1
CEX.ch =  0.7
PCH = 21
font = 1

mtext('Degrees W', side=1, outer=TRUE, cex = 0.8, line = -2)
mtext('Degrees N', side=2, outer=TRUE, cex = 0.8, line = -1)
dev.off()


library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization

plot_usmap(include = c("WA", "OR",'CA')) + 
  theme(panel.background=element_blank())
ggsave( paste0(loc, "west-coast-map.png"))
