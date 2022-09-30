library(FSAdata) # for data
library(FSA)     # for vbFuns(), vbStarts(), confint.bootCase()
library(car)     # for Boot()
library(dplyr)   # for filter(), mutate()
library(ggplot2)
setwd('..')
## function(t,Linf,K=NULL,t0=NULL) {
##   if (length(Linf)==3) { K <- Linf[[2]]
##                          t0 <- Linf[[3]]
##                          Linf <- Linf[[1]] }
##   Linf*(1-exp(-K*(t-t0)))
##   }
## <bytecode: 0x0000026cd7aa2768>
## <environment: 0x0000026ce4e1acc8>

growthFunShow("vonBertalanffy","Typical",plot=TRUE)


(vb1 <- vbFuns() )
ages <- 0:20
tl_m = vb1(ages,Linf=50.21,K=0.14,t0=-4.64)
tl_f = vb1(ages,Linf=53.25,K=0.15,t0=-2.84)

graphics.off()

fig.name = paste0(getwd(),'/Plots/Black-rockfish-vonBert.png')
png(fig.name, units='in', res = 300, width = 5, height = 5)

plot(tl_f~ages,type="b",pch=19, 
     xlab = 'Age (years)', ylab = "Length(cm)", 
     ylim = c(0,60))
lines(ages,tl_m, type='b', col = 'black', pch=21)
segments(x0=1,y0=0,x1=1,y1=10, lty = 'dotted')
segments(x0=0,y0=10,x1=1,y1=10, lty = 'dotted')
text(5,20,"Dotted line shows the 1-yr, 10 cm cut offs", pos=4)
legend('topleft', legend = "VonBert growth curve", bty='n')
legend('topleft', legend=c('Male','Female'), pch=c(21,19), bty='n', inset=c(0.025,0.1))

dev.off()
