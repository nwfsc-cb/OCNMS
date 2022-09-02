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

df <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 2003 to 2021.xlsx'), sheet = 1))
rownames(df)
# get just our sites
df <- df[ df$SITE_NAME %in% c("Tatoosh Island", 'Destruction Island','Cape Alava','Cape Johnson'),]
# remove lat long, don't need
rownames(df) <- df$SITE_NAME

df <- df[,-c(1:3)]
tempC = data.frame(t(df))
tempC$id = rownames(tempC)


# bring in meta data and add some columns

att <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 2003 to 2021.xlsx'), 
                             sheet = "Attribute descriptions"))
# add in and fix date
x = att$Description[ match(tempC$id, att$Attribute)]
x_date <- substring(x , nchar(x)-10, nchar(x) )
x_month = substring(x_date , 4,6 )
month_num <- 1:12
month_name <- substring(month.name, 1,3)
z = data.frame(cbind(month_name, month_num))
x_num = z$month_num[match(x_month,z$month_name)]
month_number = ifelse(nchar(x_num) == 1, paste0('0',x_num), x_num)

tempC$date = paste(substring(x_date, 8,11), 
                    month_number,
                    substring(x_date, 1,2),
                    sep='-')
tempC$date <- as.POSIXct(tempC$date)

sites2 = c('Tatoosh.Island','Cape.Alava','Cape.Johnson','Destruction.Island')
     
tempC_long <- tempC %>% select( ., -c('id')) %>%
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

ts_nb <- ts2clm(df_nb, climatologyPeriod = c("2003-01-01", "2021-12-31"))
ts_ca <- ts2clm(df_ca, climatologyPeriod = c("2003-01-01", "2021-12-31"))
ts_cj <- ts2clm(df_cj, climatologyPeriod = c("2003-01-01", "2021-12-31"))
ts_di <- ts2clm(df_di, climatologyPeriod = c("2003-01-01", "2021-12-31"))

mhw_nb <- detect_event(ts_nb)
mhw_ca <- detect_event(ts_ca)
mhw_cj <- detect_event(ts_cj)
mhw_di <- detect_event(ts_di)

nb <- event_line(mhw_nb, spread = 365*4, metric = "intensity_max", 
           start_date = "2003-01-01", end_date = "2021-12-31")
ca <- event_line(mhw_ca, spread = 365*4, metric = "intensity_max", 
                 start_date = "2003-01-01", end_date = "2021-12-31") 
cj <- event_line(mhw_cj, spread = 365*4, metric = "intensity_max", 
                 start_date = "2003-01-01", end_date = "2021-12-31")
di <- event_line(mhw_di, spread = 365*4, metric = "intensity_max", 
                 start_date = "2003-01-01", end_date = "2021-12-31")

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
  t1 = grep("2012-01-01", dfile$climatology$t)
  t2 = grep("2021-12-31", dfile$climatology$t)
  mhw2 = dfile$climatology %>% slice(t1:t2) 
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

site.order = c('Neah Bay','Cape Alava','Cape Johnson','Destruction Island')



MHW
#### write out table

write.csv(MHW, paste0(Fig_Loc,"Table_MHW_Intensity.csv"), row.names = FALSE)














