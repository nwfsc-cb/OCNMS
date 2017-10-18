require(grid)

theme_acs <- function(base_size = 12, base_family = "") {
  
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      colour = "black", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text =          element_text(size = rel(0.8), colour = "black"),
    strip.text =         element_text(size = rel(0.8)),
    
    axis.line =          element_blank(),
    axis.text.x =        element_text(vjust = 1, size = 12),
    axis.text.y =        element_text(hjust = 1, size = 12),
    axis.ticks =         element_line(colour = "black"),
    axis.title.x =       element_text(),
    axis.title.y =       element_text(angle = 90),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),
    
    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "white", colour = "black"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = "white", colour = "black",size=1.5),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.margin =       unit(0.25, "lines"),
    
    strip.background =   element_rect(fill = "black", colour = "black"),
    strip.text.x =       element_text(colour="white",size=12),
    strip.text.y =       element_text(angle = -90,colour="white",size=12),
    
    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
    
    complete = TRUE
  )
}


#Test

# r = rnorm(10,20,2)
# r2 =seq(1:length(r))
# df <-data.frame(r,r2)
# 
# ggplot(df,aes(x=r2,y=r))+
#   geom_point()+
#   theme_acs()