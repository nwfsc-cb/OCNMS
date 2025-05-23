# Base Map script.
library(maps)
library(mapdata)
library(ggplot2)


#### Plotting information
states <- map_data("state")
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

base_map <-ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group=group), fill = grey(0.5), color = "black")+
  coord_fixed(xlim=lon.lims,ylim=lat.lims,ratio=1.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

base_map_trim <-ggplot(data = west_coast) + 
 geom_polygon(aes(x = long, y = lat, group=group), fill = grey(0.5), color = "black")+
 coord_fixed(xlim=lon.lims.trim,ylim=lat.lims.trim,ratio=1.3) +
 xlab("Longitude") +
 ylab("Latitude") +
 theme_bw()