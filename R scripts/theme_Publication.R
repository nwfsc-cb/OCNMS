library(grid)
library(ggthemes)
library(ggplot2)


theme_Publication <- function(base_size=12, base_family="sans") {
  
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(size=14),
           panel.background = element_rect(fill = NA,colour = "black",size=2),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.ticks.length =  unit(0.15, "cm"),
           axis.text = element_text(), 
           axis.line.x = element_line(colour="black"),
           axis.line.y = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "right",
           legend.direction = "vertical",
           legend.key.size= unit(0.4, "cm"),
           # legend.key.height =  NULL,
           # legend.key.width =   NULL,
           legend.margin = margin(t=0,r=0,b=1,l=0),
           legend.title = element_text(face="italic"),
           #legend.title.align = 0.5,
           legend.text = element_text(size = rel(0.6)),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))
  
}


# scale_colour_Publication <- function(...){
#   library(scales)
#   discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }
# 
# #Test
# data(cars)
# 
# ggplot(mtcars, aes(mpg,disp,color=factor(carb),size=hp))+
#   geom_point(alpha=0.7) + labs(title="Bubble Plot")+
#   scale_size_continuous(range = c(3,10))+
#   scale_colour_Publication()+
#   theme_Publication()
