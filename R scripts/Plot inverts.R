# Plot Inverts


# CREATE PLOTS FOR EACH ORGANISM TYPE
GROUP <- sort(as.character(unique(dat.group$group)))

pdf(file = paste(base.dir,"/Plots/Inverts panels.pdf",sep=""),onefile=T, width=4,height=10)

for(i in 1:length(GROUP)){
  temp.dat <- dat.group[dat.group$group == GROUP[i],]
  temp.dat <- merge(temp.dat,  expand.grid(Year=unique(dat.group$year.plot),Site.plot=unique(dat.group$Site.plot)),all=T)
  
  p <- ggplot(temp.dat) +
    geom_point(aes(y=MEAN,x=year.plot),stat="identity") +   
    geom_bar(aes(y=MEAN,x=year.plot),stat="identity") +
    geom_errorbar(aes(ymin=MEAN,ymax=MEAN+SE,x=year.plot)) +
    geom_line(aes(y=MEAN,x=year.plot),stat="identity") +
    facet_wrap(~Site.plot,ncol=2) +
    ggtitle(GROUP[i]) +
    #scale_y_continuous(expand=c(0,0))+
    scale_x_date(#date_breaks = "4 year", 
      date_labels = "%Y",limits = c(as.Date("1985-1-1"), as.Date("2017-1-1")),
      breaks=c(as.Date("1987-1-1"), as.Date("1995-1-1"), as.Date("1999-1-1"), as.Date("2015-1-1")))+
    #      scale_x_continuous(breaks=c(1987,1995,1999,2015),labels=c("1987","1995","1999","2015"),expand = c(0, 0)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5))
  print(p)
  
  if(GROUP[i]=="urchin"){
    p <- ggplot(temp.dat) +
      #geom_point(aes(y=MEAN,x=year.plot),stat="identity") +   
      geom_bar(aes(y=MEAN,x=year.plot),stat="identity") +
      geom_errorbar(aes(ymin=MEAN,ymax=MEAN+SE,x=year.plot)) +
      geom_line(aes(y=MEAN,x=year.plot),stat="identity") +
      facet_wrap(~Site.plot,ncol=2) +
      ggtitle(paste(GROUP[i], "small y-axis")) +
      #scale_y_continuous(expand=c(0,0))+
      scale_x_date(#date_breaks = "4 year", 
        date_labels = "%Y",limits = c(as.Date("1985-1-1"), as.Date("2017-1-1")),
        breaks=c(as.Date("1987-1-1"), as.Date("1995-1-1"), as.Date("1999-1-1"), as.Date("2015-1-1"))) +
      scale_y_continuous(limits=c(0,10))+
      #      scale_x_continuous(breaks=c(1987,1995,1999,2015),labels=c("1987","1995","1999","2015"),expand = c(0, 0)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5))
    print(p)
  }
}
dev.off()

pdf(file = paste(base.dir,"/Plots/Inverts one plot .pdf",sep=""),onefile=T, width=6,height=6)
### Combine plots across all sites
for(i in 1:length(GROUP)){
  temp.dat <- dat.group[dat.group$group == GROUP[i],]
  temp.dat <- merge(temp.dat,  expand.grid(Year=unique(dat.group$year.plot),Site.plot=unique(dat.group$Site.plot)),all=T)
  
  p <- ggplot(temp.dat) +
    geom_point(aes(y=MEAN,x=year.plot,color=Site),stat="identity") +   
    #geom_bar(aes(y=MEAN,x=year.plot),stat="identity") +
    geom_errorbar(aes(ymin=MEAN,ymax=MEAN+SE,x=year.plot,color=Site),width=0) +
    geom_line(aes(y=MEAN,x=year.plot,color=Site),stat="identity") +
    #facet_wrap(~Site.plot,ncol=2) +
    ggtitle(GROUP[i]) +
    #scale_y_continuous(expand=c(0,0))+
    scale_x_date(#date_breaks = "4 year", 
      date_labels = "%Y",limits = c(as.Date("1985-1-1"), as.Date("2017-1-1")),
      breaks=c(as.Date("1987-1-1"), as.Date("1995-1-1"), as.Date("1999-1-1"), as.Date("2015-1-1")))+
    #      scale_x_continuous(breaks=c(1987,1995,1999,2015),labels=c("1987","1995","1999","2015"),expand = c(0, 0)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5))
  print(p)
}  

dev.off()
