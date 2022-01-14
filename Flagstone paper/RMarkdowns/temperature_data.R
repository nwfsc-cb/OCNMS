rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(stringr)
library(lemon)
library(ggpubr) 
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
sites = site.col$site
year.pch = data.frame(settings['year.pch'])
colnames(year.pch) = c('year','pch','col')
Degree_C <-"\u00B0C"
Encoding(Degree_C)<-"UTF-8"

# 'Mean Daily OISST (C) for OCNMS sites 2014 to 2021.xlsx'

# bring in temp data and transpose

df <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 2003 to 2021.xlsx'), sheet = 1))
rownames(df)
# get just our sites
df <- df[ df$SITE_NAME %in% sites,]
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

sites2 = stringr::str_replace(sites, " ", ".") 
     
tempC_long <- tempC %>% select( ., -c('id')) %>%
     pivot_longer(., names_to = 'site', values_to = 'degreesC', cols = sites2) 
    
tempC_long$site <-  stringr::str_replace_all(tempC_long$site, "\\.", " ")    
tempC_long <- data.frame(tempC_long)
tempC_long$date = as.Date(tempC_long$date)


tempC_long$site <- factor(tempC_long$site, levels = site.col$site)

temp_plot <- ggplot( tempC_long , aes( x=date , y=degreesC ) ) +
     geom_line() + 
     scale_color_manual(values = site.col$col) +
     scale_x_date(date_minor_breaks = "1 year" ) + 
     facet_wrap(~ tempC_long$site , ncol = 2, scales = 'free_x') +
     xlab ("") + ylab ("Degrees C") +
     theme_bw() + theme_nt
     
temp_plot


################################ 

# get rolling average 
library(slider)
df_smooth <- tempC_long %>%
        group_by(site) %>%
        arrange(site, date) %>%
        mutate(temp_smooth = slider::slide_dbl(degreesC, mean, .before = 5, .after = 5)) %>%
        ungroup() %>%
        mutate(month = lubridate::month(date), 
               year = lubridate::year(date)) 

temp_smooth_plot <- ggplot( df_smooth , aes( x=date , y=temp_smooth ) ) +
        geom_line() + 
        scale_color_manual(values = site.col$col) +
        scale_x_date(date_minor_breaks = "1 year" ) + 
        facet_wrap(~ df_smooth$site , ncol = 2, scales = 'free_x') +
        xlab ("") + ylab (paste0("Max temp ", Degree_C)) +
        theme_bw() + theme_nt

temp_smooth_plot

####################### Monthly Maximum Temperature ################################

# one day absolute ####
tempC_long$year  = lubridate::year(tempC_long$date)
tempC_long$month = lubridate::month(tempC_long$date)

df_month_max <- tempC_long %>% 
    group_by(year,month,site) %>%
    summarise(max_temp = max(degreesC)) %>%
    mutate(date = as.POSIXct( paste(as.numeric(year), as.numeric(month),15, sep="-") )) %>%
    mutate(date = lubridate::date(date))
          
# montly max plot ####

df_month_max$site = factor(df_month_max$site, levels = site.col$site)

monthly_max <- ggplot( df_month_max , aes( x=date , y=max_temp ) ) +
    geom_line() + 
    scale_color_manual(values = site.col$col) +
    scale_x_date(date_minor_breaks = "1 year" ) + 
    facet_wrap(~ df_month_max$site , ncol = 2, scales = 'free_x') +
    xlab ("") + ylab(paste0("Max temp ", Degree_C)) +
    theme_bw() + theme_nt

monthly_max

graphics.off()
png(paste0(Fig_Loc,"Temperature_monthly_maximum.png"), units = 'in', res = 300,
    width = 6, height = 5)
monthly_max
dev.off()
 

################# Temp by season #################################
# to help select months for plotting etc.

df_month <- tempC_long %>%
    group_by(month) %>%
    summarise(meanC = mean(degreesC),
              sdC = sd(degreesC))
xlabel = substring(month.name, 1,3)

temp_season <- ggplot(df_month , aes(x = month, y = meanC)) +
    geom_ribbon( aes(ymin = meanC+sdC, ymax = meanC-sdC), fill = 'grey90') +
    geom_line() +
    scale_x_continuous( breaks = 1:12, labels=xlabel)+
    xlab("") +
    ylab(paste0("Mean monthly maxumum temp ", Degree_C))+ 
    theme_bw() + theme_nt

temp_season 

graphics.off()
png(paste0(Fig_Loc,"Temperature_by_season.png"), units = 'in', res = 300, width = 3.5, height = 3)
temp_season + theme_bw() + theme_nt
dev.off()

# choose what classifies as summer months ####
# july  - sept plateau

summer_months = 7:9

########## end temp by season plotting ###########################


################# Summer MAX temperature ######################### 

df_sum_max <- tempC_long %>% # used below
     group_by(site,year) %>%
     filter(month %in% summer_months) %>%
     summarise(max_temp = max(degreesC))

df_sum_max_wide <- df_sum_max %>%
    pivot_wider( ., names_from = year, values_from = max_temp) %>%
    rename(Site = site)

write.csv(df_sum_max_wide, paste0(Fig_Loc,"Max_temp_by_site_year.csv"), row.names = FALSE)

df_sum_max$site = factor(df_sum_max$site, levels = site.col$site)

sum_max_plot <- ggplot(df_sum_max, aes(x = year, y = max_temp, color=site, fill=site)) + 
    #geom_ribbon( aes(ymin = mean_temp+sd_temp, ymax =mean_temp-sd_temp)) +
    geom_line(size = 1, show.legend = FALSE) +
    scale_color_manual( values = site.col$col ) +
    scale_x_continuous( breaks = seq(2005,2020,5) , minor_breaks = 2003:2021 )+
    xlab("")+
    ylab( paste0('Max summer temperature ', Degree_C)) +
    theme_bw()+theme_nt 
sum_max_plot


##################################################################

################# Summer MEAN temperature ######################
df_sum_mean <- tempC_long %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(mean_temp = round(mean(degreesC),1),
              sd_temp  = round(sd(degreesC),1) ) %>%
    mutate(mean_sd = paste0(mean_temp," (",sd_temp,")"))

df_sum_mean_wide <- df_sum_mean %>%
    select(year,site,mean_sd) %>%
    pivot_wider( ., names_from = year, values_from = mean_sd) %>%
    rename(Site = site)
    
write.csv(df_sum_mean_wide, paste0(Fig_Loc,"Mean_sd_temp_by_site_year.csv"), row.names = FALSE)

df_sum_mean$site = factor(df_sum_mean$site, levels = site.col$site)
df_sum_mean$col = site.col$col[ match(df_sum_mean$site, site.col$site)]

sum_mean_plot <- ggplot(df_sum_mean, aes(x = year, y = mean_temp, color=site, fill=site)) + 
    #geom_ribbon( aes(ymin = mean_temp+sd_temp, ymax =mean_temp-sd_temp)) +
    geom_line(size = 1) +
    scale_color_manual( values = site.col$col ) +
    scale_x_continuous( breaks = seq(2005,2020,5) , minor_breaks = 2003:2021 )+
    xlab("")+
    ylab( paste0('Mean summer temperature ', Degree_C)) +
    theme_bw()+theme_nt + theme(legend.position = c(0.4, 0.85) )
sum_mean_plot

##### Plot max and mean together - absolute, not smoothed ################################

graphics.off()
png(paste0(Fig_Loc,"Temperature_summer_max_mean.png"),
    units = 'in',res=300, width = 3.5, height =5)

ggarrange( sum_max_plot, sum_mean_plot,
           labels = c('a)','b)'), 
           font.label = list(face='plain', size = 10),
           hjust = -4,
           vjust = 2,
           align = 'v',
           ncol = 1)
dev.off()

############ TABLES - data and comparisons  ##############################################

# df_sum_mean_wide # these are "pretty with (sd)
# df_sum_max_wide
# re-do for calculations

# Max summer temp table ##############################

df_max <- tempC_long %>% # used below
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
           Diff12, Diff13,vs13, vs14) 

# export tables

write.csv(df_max , paste0(Fig_Loc,"Temperature_year_max.csv"), row.names = FALSE)
write.csv(DF_MAX , paste0(Fig_Loc,"Temperature_year_max_comparison.csv"), row.names = FALSE)

#### end max comparison table

# Mean summer temp table ##############################

df_mn <- tempC_long %>% # used below
    group_by(site,year) %>%
    filter(month %in% summer_months) %>%
    summarise(mean_temp = mean(degreesC),
              sd_temp  = sd(degreesC) ) %>%
    select(year,site, mean_temp) %>%
    pivot_wider( ., names_from = year, values_from = mean_temp) %>%
    rename(Site = site) 

df_sd<- tempC_long %>% # used below
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

write.csv(df_mn , paste0(Fig_Loc,"Temperature_year_mean_sd.csv"), row.names = FALSE)
write.csv(DF_MN , paste0(Fig_Loc,"Temperature_year_mean_comparison.csv"), row.names = FALSE)

####### end mean comparison table #################

####### Max and Mean plots summarized across sites #######

df_1max <- df_sum_max %>% 
    group_by(year) %>%
    summarise(tempC = mean(max_temp), stdv = sd(max_temp)) %>%
    mutate(cat = "Max" )

dfx <- df_sum_mean %>% 
    group_by(year) %>%
    summarise(tempC = mean(mean_temp), stdv = sd(mean_temp)) %>%
    mutate(cat = "Mean") %>%
    full_join(.,df_1max)

dfx$color = ifelse(dfx$cat == 'Mean', site.col$col[1], site.col$col[3])

plot_t <- ggplot(dfx, aes(x = year, y = tempC, fill = cat, color=cat )) +
    scale_color_manual(values = site.col$col[c(1,3)]) +
    geom_ribbon( aes(ymin = tempC-stdv, ymax =tempC+stdv ), 
                 fill = dfx$color, linetype = 0, alpha=0.5) +
    geom_line()+
    geom_point()+
    xlab("") + ylab(paste0("Temperature ", Degree_C)) +
    scale_x_continuous( breaks = seq(2005,2020,5), minor_breaks = 2003:2021) +
    
    theme_bw() + theme_nt

plot_t

graphics.off()

png(paste0(Fig_Loc,"Mean_max_temp_averaged.png"), units = 'in', res=300, width = 3.5, height = 3)
plot_t
dev.off()












