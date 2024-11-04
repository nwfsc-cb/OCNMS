library(maps)       #basic mapping functions and some data
#library(mapdata)    #some additional hires data
#library(maptools)   #useful tools such as reading shapefiles
#library(PBSmapping) #powerful mapping functions developed by Pacific Biological Station
loc = paste0(getwd(),'/figures/')
graphics.off()

png(paste0(loc,"map-of-coast.png"), units="in", width=6, height=5, res=300)

map('world', xlim=c(-130, -114), ylim=c(32, 48.5), col="grey", fill=TRUE, lwd=2)  

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


##### West Coast

points(-124.618116,48.370781, col= pt.color, pch=PCH, bg = pt.bg.wc, cex = CEX)
text(-124.3,47.8, pos = 2, col = text.color, font = font,cex = CEX.ch, "Neah Bay")

points(-124.211840, 41.759723, col= pt.color, pch=PCH, bg = pt.bg.wc, cex = CEX)
text(-124.211840, 41.759723, pos = 2, col = text.color, font = font,cex = CEX.ch,"Crecent City")

points(-124.208701, 40.786866, col= pt.color, pch=PCH, bg = pt.bg.wc, cex = CEX)
text(-124.208701, 40.786866, pos = 2, col = text.color, font = font,cex = CEX.ch,"Northspit")

points(-122.914144, 38.074724, col= pt.color, pch=PCH, bg = pt.bg.wc, cex = CEX)
text(-122.914144, 38.4, pos = 2, col = text.color, font = font,cex = CEX.ch,"Point Reyes")

points(-122.435779, 37.799986, col= pt.color, pch=PCH, bg = pt.bg.wc, cex = CEX)
text(-122.435779, 37.5, pos = 2, col = text.color, font = font,cex = CEX.ch,"San Francisco")

points(-117.247845, 32.712631, col= pt.color, pch=PCH, bg = pt.bg.wc, cex = CEX)
text(-117.247845, 32.1, pos = 2, col = text.color, font = font,cex = CEX.ch,"San Diego")

mtext('Degrees W', side=1, outer=TRUE, cex = 0.8, line = -2)
mtext('Degrees N', side=2, outer=TRUE, cex = 0.8, line = -1)
dev.off()