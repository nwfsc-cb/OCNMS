rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(stringr)
library(lemon)
library(ggpubr) 
library(heatwaveR)
HomeFile = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper"

Fig_Loc = paste0(HomeFile,"/Plots/")
Data_Loc = paste0(HomeFile,"/Data/")
Results_Loc = paste0(HomeFile,"/Results/")
Other_Files = paste0(HomeFile,"/Other Files/")
theme_nt = readRDS(paste0(Data_Loc, 'theme_nt.rds') )
settings = readRDS(paste0(Data_Loc,'settings.rds') )
min.vis = as.numeric(settings['min.vis'])
years = as.numeric(unlist(settings['years']))
site.col = data.frame(settings['site.col'])
colnames(site.col) = c('site','col')
# correct for combined Tatoosh/Neah Bay
plot.colors = site.col$col[c(3,1)]
site.col = site.col[2:5,]
site.col$site[1] = "Tatoosh Island-Neah Bay"
sites = site.col$site
year.pch = data.frame(settings['year.pch'])
colnames(year.pch) = c('year','pch','col')

Degree_C <-"\u00B0C"
Encoding(Degree_C)<-"UTF-8"

text_sd<-"\u00b1 1.0 s.d."
Encoding(text_sd)<-"UTF-8"

# 'Mean Daily OISST (C) for OCNMS sites 2014 to 2021.xlsx'

# bring in temp data and transpose

tempC_long = data.frame(read.csv(paste0(Data_Loc,"Temperature-processed-1992-2021.csv"), header = TRUE))
tempC_long$date = as.Date(tempC_long$date)
temp_plot <- ggplot( tempC_long , aes( x=date , y=degreesC ) ) +
     geom_line() + 
     scale_color_manual(values = site.col$col) +
     scale_x_date(date_minor_breaks = "1 year" ) + 
     facet_wrap(~ tempC_long$site , ncol = 2, scales = 'free_x') +
     xlab ("") + ylab ("Degrees C") +
     theme_bw() + theme_nt
     
temp_plot

################# Temp by season #################################
# to help select months for plotting etc.
# probably don't need to change name
df_long = tempC_long %>%
     mutate(month = lubridate::month(date), 
            year = lubridate::year(date)) 

df_month <- df_long %>%
    group_by(month) %>%
    summarise(meanC = mean(degreesC),
              sdC = sd(degreesC))
xlabel = substring(month.name, 1,3)

temp_season <- ggplot(df_month , aes(x = month, y = meanC)) +
    geom_ribbon( aes(ymin = meanC+sdC, ymax = meanC-sdC), fill = 'grey90') +
    geom_line() +
    geom_point() +
    scale_x_continuous( breaks = 1:12, labels=xlabel)+
    xlab("") +
    ylab(paste0("Mean monthly temp ", Degree_C, " (",text_sd,")"))+ 
    theme_bw() + theme_nt

temp_season 

graphics.off()
png(paste0(Fig_Loc,"SST_mean_by_month.png"), units = 'in', res = 300, width = 3.5, height = 3)
temp_season + theme_bw() + theme_nt
dev.off()

# choose what classifies as summer months ####
# july  - sept plateau

summer_months = 6:10

################# Summer MEAN temperature by site NOT SMOOTHED ######################

df_sum_mean <- df_long %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(mean_temp = round(mean(degreesC),1),
              sd_temp  = round(sd(degreesC),1) ) %>%
    mutate(mean_sd = paste0(mean_temp," (",sd_temp,")"))

df_sum_mean_wide <- df_sum_mean %>%
    select(year,site,mean_sd) %>%
    pivot_wider( ., names_from = year, values_from = mean_sd) %>%
    rename(Site = site)

write.csv(df_sum_mean_wide, paste0(Fig_Loc,"SST_by_site_year.csv"), row.names = FALSE)

df_sum_mean$site = factor(df_sum_mean$site, levels = site.col$site)
df_sum_mean$col = site.col$col[ match(df_sum_mean$site, site.col$site)]

sum_mean_plot <- ggplot(df_sum_mean, aes(x = year, y = mean_temp, color=site, fill=site)) + 
    #geom_ribbon( aes(ymin = mean_temp+sd_temp, ymax =mean_temp-sd_temp)) +
    geom_line(size = 1) +
    scale_color_manual( values = site.col$col ) +
    scale_x_continuous( breaks = seq(1990,2020,5) , minor_breaks = 1992:2021 )+
    xlab("")+
    ylab( paste0('Mean summer temperature (', Degree_C,')')) +
    theme_bw()+theme_nt + theme(legend.position = c(0.4, 0.85) )
sum_mean_plot

graphics.off()
png(paste0(Fig_Loc,"SST_mean_by_site.png") , units = 'in',res=300, width = 3.5, height = 3)
sum_mean_plot
dev.off()


##############################################################################
############ 5-day smoothed data #############################################
##############################################################################

################################ 

# smooth the data 5-day average ###
# with plot daily, smoothed plot
library(slider)
df_smooth <- tempC_long %>%
        group_by(site) %>%
        arrange(site, date) %>%
        mutate(temp_smooth = slider::slide_dbl(degreesC, mean, .before = 2, .after = 2)) %>%
        ungroup() %>%
        mutate(month = lubridate::month(date), 
               year = lubridate::year(date)) 

temp_smooth_plot <- ggplot( df_smooth , aes( x=date , y=temp_smooth ) ) +
        geom_line() + 
        scale_color_manual(values = site.col$col[2:5]) +
        scale_x_date(date_minor_breaks = "1 year" ) + 
        facet_wrap(~ df_smooth$site , ncol = 2, scales = 'free_x') +
        xlab ("") + ylab (paste0("Mean temp ", Degree_C, ' (5-day smooth)')) +
        theme_bw() + theme_nt

temp_smooth_plot

graphics.off()
png(paste0(Fig_Loc,"SST_5d_mean_five_sites.png"), 
    units='in',res = 300, width=6, height = 4)
temp_smooth_plot
dev.off()

################# Summer 5-day Maxumum temperature by site - smoothed ######################
df_sum_max <- df_smooth %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(max_temp = round(max(degreesC),1) )

df_sum_max_wide <- df_sum_max %>%
    select(year,site,max_temp) %>%
    pivot_wider( ., names_from = year, values_from = max_temp) %>%
    rename(Site = site)

write.csv(df_sum_max_wide, paste0(Fig_Loc,"SST_Max_sd_by_site_year-5day.csv"), row.names = FALSE)

df_sum_max$site = factor(df_sum_max$site, levels = site.col$site)
df_sum_max$col = site.col$col[ match(df_sum_max$site, site.col$site)]

sum_max_plot <- ggplot(df_sum_max, aes(x = year, y = max_temp, color=site, fill=site)) + 
    #geom_ribbon( aes(ymin = mean_temp+sd_temp, ymax =mean_temp-sd_temp)) +
    geom_line(size = 1) +
    scale_color_manual( values = site.col$col ) +
    scale_x_continuous( breaks = seq(1990,2020,5) , minor_breaks = 1990:2021 )+
    xlab("")+
    ylab( paste0('Five-day max summer temperature ', Degree_C)) +
    theme_bw()+theme_nt + theme(legend.position = c(0.4, 0.85) )
sum_max_plot

graphics.off()
png(paste0(Fig_Loc,"SST_max_by_site-5day.png") , units = 'in',res=300, width = 3.5, height = 3)
sum_max_plot
dev.off()



############## COMBINED PLOTS ############################################################
# max = smoothed
# mean = just the mean



##### Plot max and mean together - absolute, 5-day smooth ################################

graphics.off()
png(paste0(Fig_Loc,"Temperature_summer_5d-max_1d-mean.png"),
    units = 'in',res=300, width = 3.5, height =5)

ggarrange( sum_max_plot, sum_mean_plot,
           labels = c('a)','b)'), 
           font.label = list(face='plain', size = 10),
           hjust = -4,
           vjust = 2,
           align = 'v',
           ncol = 1)
dev.off()

####### 5-day Max and Mean plots summarized across sites #################################
# these use files established above
# df_sum_max is 5-day smooth
# df_sum_mean is NOT smoothed

df_1max <- df_sum_max %>% 
    group_by(year) %>%
    summarise(tempC = mean(max_temp), stdv = sd(max_temp)) %>%
    mutate(cat = "Max" )

dfx <- df_sum_mean %>% 
    group_by(year) %>%
    summarise(tempC = mean(mean_temp), stdv = sd(mean_temp)) %>%
    mutate(cat = "Mean") %>%
    full_join(.,df_1max)

dfx$color = ifelse(dfx$cat == 'Mean', plot.colors[1], plot.colors[2])

plot_t <- ggplot(dfx, aes(x = year, y = tempC, fill = cat, color=cat )) +
    scale_color_manual(values =  plot.colors) +
    geom_ribbon( aes(ymin = tempC-stdv, ymax =tempC+stdv ), 
                 fill = dfx$color, linetype = 0, alpha=0.5) +
    geom_line()+
    geom_point()+
    xlab("") + ylab(paste0("Temperature ", Degree_C, " (", text_sd,")")) +
    scale_x_continuous( breaks = seq(2005,2020,5), minor_breaks = 2003:2021) +
    
    theme_bw() + theme_nt

plot_t

graphics.off()

png(paste0(Fig_Loc,"SST_5d_max_1dmean.png"), 
    units = 'in', res=300, width = 3.5, height = 3)
plot_t + theme(legend.position = c(0.2,0.9))
dev.off()

########## Comparison Tables ####################################

df_sum_max_wide
df_sum_mean_wide

# df_sum_mean_wide # these are "pretty with (sd)
# df_sum_max_wide
# re-do for calculations

# Max summer temp table 5day smooth ##############################

df_max <- df_smooth %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(max_temp = max(degreesC) ) %>%
    select(year,site, max_temp) %>%
    pivot_wider( ., names_from = year, values_from = max_temp) %>%
    rename(Site = site) 





# calculate means for various time periods -- all summer
max_2003_2012 = rowSums( df_max[, as.character(c(2003:2012)) ]) / (length(2003:2012))
max_2003_2013 = rowSums( df_max[, as.character(c(2003:2013)) ]) / (length(2003:2013))
max_2014_2016 = rowSums( df_max[, as.character(c(2014:2016)) ]) / (length(2014:2016))
# calculate sds for time periods

df_max2 = cbind( df_max , 
                 B12 = max_2003_2012 , B13 = max_2003_2013 , MHW = max_2014_2016 )

df_max2 <- df_max2 %>% 
    rename(Y2013 = "2013", Y2014 = "2014")%>%
    mutate(Diff12 = MHW - B12, 
           Diff13 = MHW - B13,
           vs13 = Y2013 - B12,
           vs14 = Y2014 - B13 )

DF_MAX = df_max2 %>%
    select(Site, Y2013, Y2014, 
           B12, B13, MHW, 
           Diff12, Diff13,vs13) 
# calculate means across sites
x = apply(DF_MAX[2:ncol(DF_MAX)], MARGIN = 2, FUN=mean)    
y = apply(DF_MAX[2:ncol(DF_MAX)], MARGIN = 2, FUN=sd)
xy = rbind(x,y)
Site = c("Mean", "1.0 s.d.")
xyz = data.frame(cbind(Site,xy))
DF_MAX = data.frame(DF_MAX)
DF_MAX = rbind(DF_MAX, xyz)

write.csv(df_max , paste0(Fig_Loc,"SST_year_max-5day.csv"), row.names = FALSE)
write.csv(DF_MAX , paste0(Fig_Loc,"SSt_year_max-5day-comparison.csv"), row.names = FALSE)

#### end max comparison table

# Mean summer temp table ##############################

df_mn <- df_long %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(mean_temp = mean(degreesC),
              sd_temp  = sd(degreesC) ) %>%
    select(year,site, mean_temp) %>%
    pivot_wider( ., names_from = year, values_from = mean_temp) %>%
    rename(Site = site) 

df_sd<- df_long %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(mean_temp = mean(degreesC),
              sd_temp   = sd(degreesC) ) %>%
    select(year,site,sd_temp) %>%
    pivot_wider( ., names_from = year, values_from = sd_temp) %>%
    rename(Site = site)

# calculate means for various time periods -- all summer
mean_2003_2012 = rowSums( df_mn[, as.character(c(2003:2012)) ]) / (length(2003:2012))
mean_2003_2013 = rowSums( df_mn[, as.character(c(2003:2013)) ]) / (length(2003:2013))
mean_2014_2016 = rowSums( df_mn[, as.character(c(2014:2016)) ]) / (length(2014:2016))
# calculate sds for time periods
sd_2003_2012 = apply( df_mn[, as.character(c(2003:2012)) ] , MARGIN = 1, FUN=sd)
sd_2003_2013 = apply( df_mn[, as.character(c(2003:2013)) ] , MARGIN = 1, FUN=sd)
sd_2014_2016 = apply( df_mn[, as.character(c(2014:2016)) ] , MARGIN = 1, FUN=sd)

df_mn2 = cbind( df_mn , 
                B12.mn = mean_2003_2012 , B13.mn = mean_2003_2013 , MHW.mn = mean_2014_2016 ,
                MWH.sd = sd_2014_2016, B12.sd = sd_2003_2012 , B13.sd = sd_2003_2013)

df_mn2 <- df_mn2 %>% 
    rename(Y2013 = "2013", Y2014 = "2014")%>%
    mutate(Diff12 = MHW.mn - B12.mn, 
           Diff13 = MHW.mn - B13.mn,
           vs13 = Y2013 - B12.mn,
           vs14 = Y2014 - B13.mn )
df_mn2$MHW = paste0(round(df_mn2$MHW.mn,1) , " (", round(df_mn2$MWH.sd,2),")")
df_mn2$B12 = paste0(round(df_mn2$B12.mn,1) , " (", round(df_mn2$B12.sd,2),")")
df_mn2$B13 = paste0(round(df_mn2$B13.mn,1) , " (", round(df_mn2$B13.sd,2),")")

DF_MN = df_mn2 %>%
    select(Site, Y2013, Y2014, 
           B12, B13, MHW, 
           Diff12, Diff13,vs13, vs14) 

# export tables

write.csv(df_mn , paste0(Fig_Loc,"SST_year_mean_sd.csv"), row.names = FALSE)
write.csv(DF_MN , paste0(Fig_Loc,"SST_year_mean_comparison.csv"), row.names = FALSE)

####### end mean comparison table #################

# Max summer temp table NO SMOOTH ##############################

df_max <- df_long %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(max_temp = max(degreesC) ) %>%
    select(year,site, max_temp) %>%
    pivot_wider( ., names_from = year, values_from = max_temp) %>%
    rename(Site = site) 


# calculate means for various time periods -- all summer
max_2003_2012 = rowSums( df_max[, as.character(c(2003:2012)) ]) / (length(2003:2012))
max_2003_2013 = rowSums( df_max[, as.character(c(2003:2013)) ]) / (length(2003:2013))
max_2014_2016 = rowSums( df_max[, as.character(c(2014:2016)) ]) / (length(2014:2016))
# calculate sds for time periods

df_max2 = cbind( df_max , 
                 B12 = max_2003_2012 , B13 = max_2003_2013 , MHW = max_2014_2016 )

df_max2 <- df_max2 %>% 
    rename(Y2013 = "2013", Y2014 = "2014")%>%
    mutate(Diff12 = MHW - B12, 
           Diff13 = MHW - B13,
           vs13 = Y2013 - B12,
           vs14 = Y2014 - B13 )

x = apply(df_max[2:ncol(df_max)], MARGIN = 2, FUN=mean)    
y = apply(df_max[2:ncol(df_max)], MARGIN = 2, FUN=sd)
xy = rbind(x,y)
Site = c("Mean", "1.0 s.d.")
xyz = data.frame(cbind(Site,xy))
df_max = data.frame(df_max)
df_max = rbind(df_max, xyz)

DF_MAX = df_max2 %>%
    select(Site, Y2013, Y2014, 
           B12, B13, MHW, 
           Diff12, Diff13,vs13) 
# calculate means across sites
x = apply(DF_MAX[2:ncol(DF_MAX)], MARGIN = 2, FUN=mean)    
y = apply(DF_MAX[2:ncol(DF_MAX)], MARGIN = 2, FUN=sd)
xy = rbind(x,y)
Site = c("Mean", "1.0 s.d.")
xyz = data.frame(cbind(Site,xy))
DF_MAX = data.frame(DF_MAX)
DF_MAX = rbind(DF_MAX, xyz)

write.csv(df_max , paste0(Fig_Loc,"SST_year_max.csv"), row.names = FALSE)
write.csv(DF_MAX , paste0(Fig_Loc,"SSt_year_max--comparison.csv"), row.names = FALSE)

#### end max comparison table







############ 2013 temperature ########################

### raw data ####

plot1 <- ggplot(data = df_long %>% filter(.,year == 2013), aes(x = date, y = degreesC))+
    geom_point()

plot1

plot2 <- ggplot(data = df_long %>% filter(.,year %in% 2013:2016), aes(x = date, y = degreesC))+
         geom_point(size = 0.5) + 
         facet_wrap(~ year , ncol = 2, scales = 'free_x') 

plot2


### site averate ####

dfx = df_long %>% 
    mutate(day = lubridate::day(date)) %>%
    group_by(date,year,month,day) %>%
    summarise(meanC = mean(degreesC),
              stdev = sd(degreesC))
dfy = df_long %>% 
    mutate(day = lubridate::day(date)) %>%
    filter(year %in% 2003:2012) %>%
    group_by(month,day) %>%
    summarise(pre_mean = mean(degreesC),
              pre_stdev = sd(degreesC)) %>%
    mutate(fake_date = paste0("2013-",month,"-",day))

dfz = left_join(dfx,dfy,by=c('month','day'))


plot3 <- ggplot(data = dfx %>% filter(.,year == 2013), aes(x = date, y = meanC))+
    geom_line()

plot3

plot4 <- ggplot(data = dfz %>% filter(.,year %in% 2012:2021), aes(x = date, y = meanC))+
    #scale_color_manual(values =  plot.colors) +
    geom_ribbon( aes(ymin = pre_mean-pre_stdev, ymax =pre_mean+pre_stdev ), 
                 fill='grey',linetype = 0, alpha=0.3) +
    geom_line( aes(x = date, y = pre_mean), col='grey') +
    geom_ribbon( aes(ymin = meanC-stdev, ymax =meanC+stdev ), fill = 'red',
                 linetype = 0, alpha=0.3) +
    geom_line( color = 'red') + 
    xlab("")+
    ylab(paste0("SST ", Degree_C, " (", text_sd,")"))+
    facet_wrap(~ year , ncol = 2, scales = 'free_x') 

plot4 + theme_bw() + theme_nt

# Fig S3 in supplement ####
graphics.off()
png(paste0(Fig_Loc,"SST-MHW-years-vs-prior.png"), units='in',res=300,width=6.5, height = 7)
plot4 + theme_bw() + theme_nt
dev.off()


############## Cavanaugh , maybe #################

# use this plot...so much easier and shorter also matches Cavanaugh


#######################################################################


## across sites
df_month = df_long %>%
    group_by(year,month) %>%
    summarise(meanT = mean(degreesC), sdT = sd(degreesC))

df_max_month <- df_month %>% 
    group_by(year) %>% 
    summarise(maxT = max(meanT))
df_max_month$sdT = df_month$sdT[match(df_max_month$maxT, df_month$meanT)]

plot5 <- ggplot(df_max_month, aes(x = year, y = maxT) ) +
    geom_ribbon( aes(ymin = maxT-sdT, ymax = maxT +sdT ), 
                 fill='grey',linetype = 0, alpha=0.3) +
    geom_line() + 
    xlab("") +
    ylab(paste0("Mean SST of warmest month ", Degree_C))+
    scale_x_continuous( breaks = seq(1990,2020,5), minor_breaks = 1990:2021 )



plot5 + theme_bw()+theme_nt


graphics.off()
png( paste0(Fig_Loc,"Mean-sst-warmest-month.png"), units = 'in',res=300,width=3.5, height=3)
plot5 + theme_bw()+theme_nt
dev.off()


###########################################################################

#######################################################################

## by site
df_month_site = df_long %>%
    group_by(year,month, site) %>%
    summarise(meanT = mean(degreesC), sdT = sd(degreesC))

df_max_month_site <- df_month_site %>% 
    group_by(year, site) %>% 
    summarise(maxT = max(meanT))
df_max_month_site$sdT = df_month_site$sdT[match(df_max_month_site$maxT, df_month_site$meanT)]
## across sites
df_month = df_long %>%
    group_by(year,month) %>%
    summarise(meanT = mean(degreesC), sdT = sd(degreesC))

df_max_month <- df_month %>% 
    group_by(year) %>% 
    summarise(maxT = max(meanT))
df_max_month$sdT = df_month$sdT[match(df_max_month$maxT, df_month$meanT)]

plot6 <- ggplot(df_max_month, aes(x = year, y = maxT) ) +
    geom_ribbon( aes(ymin = maxT-sdT, ymax = maxT +sdT ), 
                 fill='grey',linetype = 0, alpha=0.3) +
    geom_line(data=df_max_month_site, aes(x=year, y = maxT, color=site), )+
    scale_color_manual(values=rev(site.col$col))+
    geom_line(size=1.2) +
    xlab("") +
    ylab(paste0("Mean SST of warmest month (", Degree_C,")"))+
    scale_x_continuous( breaks = seq(1990,2020,5), minor_breaks = 1990:2021 )



plot6 + theme_bw()+theme_nt+theme(legend.position = c(0.8,0.9))


graphics.off()
png( paste0(Fig_Loc,"Mean-sst-warmest-month-by-site-30-year.png"), units = 'in',res=300,width=3.5, height=3)
plot6 + theme_bw()+theme_nt+theme(legend.position = c(0.5,0.9))
dev.off()



#### same but only back to 2003


## by site
df_month_site = df_long %>%
  group_by(year,month, site) %>%
  summarise(meanT = mean(degreesC), sdT = sd(degreesC))

df_max_month_site <- df_month_site %>% 
  group_by(year, site) %>% 
  summarise(maxT = max(meanT))
df_max_month_site$sdT = df_month_site$sdT[match(df_max_month_site$maxT, df_month_site$meanT)]
## across sites
df_month = df_long %>%
  group_by(year,month) %>%
  summarise(meanT = mean(degreesC), sdT = sd(degreesC))

df_max_month <- df_month %>% 
  group_by(year) %>% 
  summarise(maxT = max(meanT))
df_max_month$sdT = df_month$sdT[match(df_max_month$maxT, df_month$meanT)]

plot6 <- ggplot(df_max_month, aes(x = year, y = maxT) ) +
  geom_ribbon( aes(ymin = maxT-sdT, ymax = maxT +sdT ), 
               fill='grey',linetype = 0, alpha=0.3) +
  geom_line(data=df_max_month_site, aes(x=year, y = maxT, color=site), )+
  scale_color_manual(values=site.col$col)+
  geom_line(size=1.2) +
  xlab("") +
  ylab(paste0("Mean SST of warmest month ", Degree_C))+
  scale_x_continuous(limits = c(2003,2021), breaks = seq(2003,2020,5), minor_breaks =2003:2021 )



plot6 + theme_bw()+theme_nt+theme(legend.position = c(0.8,0.9))


graphics.off()
png( paste0(Fig_Loc,"Mean-sst-warmest-month-by-site.png"), units = 'in',res=300,width=3.5, height=3)
plot6 + theme_bw()+theme_nt+theme(legend.position = c(0.3,0.9))
dev.off()

