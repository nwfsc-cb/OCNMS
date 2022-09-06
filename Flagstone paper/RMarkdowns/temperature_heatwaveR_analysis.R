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

# df <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 2003 to 2021.xlsx'), sheet = 1))
df1 <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 1992 to 2021.xlsx'), sheet = 1))
df2 <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 1992 to 2021.xlsx'), sheet = 2))

rownames(df1)
# get just our sites
df1 <- df1[ df1$SITE_NAME %in% c("Tatoosh Island", 'Destruction Island','Cape Alava','Cape Johnson'),]
df2 <- df2[ df2$SITE_NAME %in% c("Tatoosh Island", 'Destruction Island','Cape Alava','Cape Johnson'),]
# remove lat long, don't need
rownames(df1) <- df1$SITE_NAME
rownames(df2) <- df2$SITE_NAME

df1 <- df1[,-c(1:3)]
df2 <- df2[,-c(1:3)]

tempC1 = data.frame(t(df1))
tempC2 = data.frame(t(df2))
tempC = rbind(tempC1, tempC2)
tempC$id = rownames(tempC)

### put in data correctly
tempC$x = str_remove(tempC$id,"sst")
tempC$year = substring(tempC$x,1,2)
tempC$year = ifelse(tempC$year %in% 92:99, paste0(19,tempC$year), paste0(20, tempC$year))
tempC$day = substring(tempC$id,7,nchar(tempC$id))

for(i in 1:nrow(tempC)){
  # note: converts as days since origin, so subtract one
  tempC$date[i] = as.Date( (as.numeric(tempC$day[i])-1),
                        origin = as.Date(paste0(tempC$year[i],"-01-01")))
}

sites2 = c('Tatoosh.Island','Cape.Alava','Cape.Johnson','Destruction.Island')
     
tempC_long <- tempC %>% select( ., -c('id','day','x')) %>%
     pivot_longer(., names_to = 'site', values_to = 'degreesC', cols = sites2) 
    
tempC_long$site <-  stringr::str_replace_all(tempC_long$site, "\\.", " ")    
tempC_long <- data.frame(tempC_long)
tempC_long$date = as.Date(tempC_long$date)

# tempC_long$site <- factor(tempC_long$site, levels = site.col$site)

tempC_long <- tempC_long[tempC_long$site != 'Neah Bay',]
tempC_long$site[tempC_long$site == 'Tatoosh Island'] <- "Tatoosh Island-Neah Bay"
tempC_long$site <- factor(tempC_long$site, 
                          levels = sites)

temp_plot <- ggplot( tempC_long , aes( x=date , y=degreesC ) ) +
     geom_line() + 
     scale_color_manual(values = site.col$col) +
     scale_x_date(date_minor_breaks = "1 year" ) + 
     facet_wrap(~ tempC_long$site , ncol = 2, scales = 'free_x') +
     xlab ("") + ylab ("Degrees C") +
     theme_bw() + theme_nt
     
temp_plot

####### MHW Plots ##########################
# colnames(tempC_long)
# levels(tempC_long$site)
###########

df_main <- tempC_long %>% rename(t = date, temp = degreesC)

df_nb = df_main %>% filter(site=='Tatoosh Island-Neah Bay') %>% select(t,temp)
df_ca = df_main %>% filter(site=='Cape Alava') %>% select(t,temp)
df_cj = df_main %>% filter(site=='Cape Johnson') %>% select(t,temp)
df_di = df_main %>% filter(site=='Destruction Island') %>% select(t,temp)

t1 = '1992-01-03'
t2 = '2021-12-31'

ts_nb <- ts2clm(df_nb, climatologyPeriod = c(t1, t2))
ts_ca <- ts2clm(df_ca, climatologyPeriod = c(t1, t2))
ts_cj <- ts2clm(df_cj, climatologyPeriod = c(t1, t2))
ts_di <- ts2clm(df_di, climatologyPeriod = c(t1, t2))

mhw_nb <- detect_event(ts_nb)
mhw_ca <- detect_event(ts_ca)
mhw_cj <- detect_event(ts_cj)
mhw_di <- detect_event(ts_di)

nb <- event_line(mhw_nb, spread = 365*4, metric = "intensity_max", 
           start_date = t1, end_date = t2)
ca <- event_line(mhw_ca, spread = 365*4, metric = "intensity_max", 
                 start_date = t1, end_date = t2) 
cj <- event_line(mhw_cj, spread = 365*4, metric = "intensity_max", 
                 start_date = t1, end_date = t2)
di <- event_line(mhw_di, spread = 365*4, metric = "intensity_max", 
                 start_date = t1, end_date = t2)

nb <- event_line(mhw_nb, spread = 365*4, metric = "intensity_max")
ca <- event_line(mhw_ca, spread = 365*4, metric = "intensity_max") 
cj <- event_line(mhw_cj, spread = 365*4, metric = "intensity_max")
di <- event_line(mhw_di, spread = 365*4, metric = "intensity_max")

# ok but cannot control x axis...stupid
mhw1 <- ggarrange(nb,ca,cj,di,
                  nrow=4)

######### better plots ###########

c1 = RColorBrewer::brewer.pal(12,"Paired")

# functionalized...
plot_mhw <- function(dfile){
  x1 = grep("2012-01-01", dfile$climatology$t)
  x2 = grep(t2, dfile$climatology$t)
  mhw2 = dfile$climatology %>% slice(x1:x2) 
  mhw_top = mhw2
  
  ggplot(data = mhw2, aes(x = t)) +
     ylim(0,20) + 
     geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = T) +
     geom_flame(data = mhw_top, aes(y = temp, y2 = thresh, fill = "MHW"),  show.legend = T) +
     geom_line(aes(y = temp, colour = "temp"), size = 0.50) +
     geom_line(aes(y = thresh, colour = "thresh"), size = 0.250) +
     geom_line(aes(y = seas, colour = "seas"), size = 0.50) +
     scale_colour_manual(name = "",
                      values = c("temp" = "grey", 
                                 "thresh" =  c1[6], 
                                 "seas" = c1[2])) +
     scale_fill_manual(name = "", 
                    values = c("MHW" = c1[6])) +
     scale_x_date(date_labels = "%b %Y") +
     guides(colour = guide_legend(override.aes = list(fill = NA))) +
     labs(y = expression(paste("Temp (", degree, "C)")), x = NULL) + 
     theme_bw()
}

nb = plot_mhw(mhw_nb) + theme(legend.position = 'none')
ca = plot_mhw(mhw_ca) + theme(legend.position = 'none')
cj = plot_mhw(mhw_cj) + theme(legend.position = 'none')
di = plot_mhw(mhw_di) + theme(legend.position = c(0.5,0.2),
                              legend.box = 'horizontal',
                              legend.direction = 'horizontal')

p_mhw <- ggarrange(nb,ca,cj,di,
                  nrow=4,
                  align='h',
                  hjust = c(-0.525, -1, -0.81, -0.68),
                  vjust = 2,
                  labels = c('a) Tatoosh Is. & Neah Bay',
                             'b) Cape Alava',
                             'c) Cape Johnson',
                             'd) Destruction Island'),
                  font.label = list(size=10, face='plain'))
p_mhw

graphics.off()
png( paste0(Fig_Loc, "MHW-1.png"), res=600, height = 7, width = 6, units='in')
p_mhw
dev.off()

########## tables ############

xnb = mhw_nb$event %>% mutate(site = "Neah Bay")
xca = mhw_ca$event %>% mutate(site = "Cape Alava")
xcj = mhw_cj$event %>% mutate(site = "Cape Johnson")
xdi = mhw_di$event %>% mutate(site = "Destruction Island")

df = rbind(xnb, xcj, xca, xdi)
df$n = 1
df$year = as.numeric(substring(df$date_peak,1,4))

hw = df %>% group_by(site, year) %>% 
  summarise(`Events` = sum(n), 
            `MHW days` = sum(duration),
            `Min days` = min(duration),
            `Max days` = max(duration),
            `Mean intensity` = mean(intensity_mean),
            `Var intensity` = sum(intensity_var)/Events,
            `Max intensity` = max(intensity_max)
            )

hw

cnb = mhw_nb$climatology %>% mutate(site = 'Neah Bay')
cca = mhw_ca$climatology %>% mutate(site = 'Cape Alava')
ccj = mhw_cj$climatology %>% mutate(site = 'Cape Johnson')
cdi = mhw_di$climatology %>% mutate(site = 'Destruction Island')

dfc = rbind(cnb, cca, ccj, cdi)
dfc$year = as.numeric(substring(dfc$t,1,4))

dfx = dfc %>% group_by(site, year) %>%
  mutate(n = ifelse(threshCriterion==TRUE,1,0)) %>%
  summarise(Days = sum(n))

MHW = hw %>% merge(dfx) %>%
  rename(Site = site, 
         `Days above threshold` = Days,
         `MHW events` = Events,
         Year = year, 
         `HW days (5+)` = `MHW days`) %>%
  select (Site, Year, `Days above threshold`, `MHW events`, `HW days (5+)`,`Min days`,`Max days`,
          `Mean intensity`,`Var intensity`,`Max intensity`)

#reorder table; pain in the ass but done
site.order = c('Neah Bay','Cape Alava','Cape Johnson','Destruction Island')
MHW$Site = factor(MHW$Site, levels = site.order)

tx = MHW %>% mutate(n=1) %>% group_by(Site) %>% summarise(N = sum(n))
tx = tx[ order(tx$Site,site.order),]
tx$Site = as.character(tx$Site)
tx

# probably an easier way
so = c( rep(as.character(tx[1,1]), tx[1,2]),
        rep(as.character(tx[2,1]), tx[2,2]),
        rep(as.character(tx[3,1]), tx[3,2]),
        rep(as.character(tx[4,1]), tx[4,2]))
so
MHW = MHW[ order(MHW$Site,so),]
MHW$Site = as.character(MHW$Site)
MHW$Site[MHW$Site == 'Neah Bay'] <- 'Tatoosh Is. & Neah Bay'

MHW$`Mean intensity` = round(MHW$`Mean intensity`,2)
MHW$`Var intensity` = round(MHW$`Var intensity`,2)
MHW$`Max intensity` = round(MHW$`Max intensity`,2)

MHW
#### write out table

write.csv(MHW, paste0(Fig_Loc,"Table_MHW_Intensity.csv"), row.names = FALSE)

write.csv(tempC_long, paste0(Data_Loc,"Temperature-processed-1992-2021.csv"), row.names = FALSE)


############ non-mhw figures ################








