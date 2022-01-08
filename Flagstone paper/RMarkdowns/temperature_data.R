rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(stringr)

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


# 'Mean Daily OISST (C) for OCNMS sites 2014 to 2021.xlsx'

# bring in temp data and transpose

df <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 2014 to 2021.xlsx'), sheet = 1))
rownames(df)
# get just our sites
df <- df[ df$SITE_NAME %in% sites,]
# remove lat long, don't need
rownames(df) <- df$SITE_NAME

df <- df[,-c(1:3)]
tempC = data.frame(t(df))
tempC$id = rownames(tempC)

# bring in meta data and add some columns

att <- data.frame(read_excel( paste0(Data_Loc,'Mean Daily OISST (C) for OCNMS sites 2014 to 2021.xlsx'), 
                             sheet = "Attribute descriptions"))
# add in and fix date
x = att$Description[ match(tempC$id, att$Attribute)]
x_date <- substring(x , nchar(x)-10, nchar(x) )
x_month = substring(x_date , 4,6 )
month_num <- 1:12
month_name <- substring(month.name, 1,3)
z = data.frame(cbind(month_name, month_num))
x_num = z$month_num[match(x_name,z$month_name)]
month_number = ifelse(nchar(x_num) == 1, paste0('0',x_num), x_num)

tempC$date = paste(substring(x_date, 8,11), 
                    month_number,
                    substring(x_date, 1,2),
                    sep='-')
tempC$date <- as.POSIXct(tempC$date)

sites2 = stringr::str_replace(sites, " ", ".") 
     
tempC_long <- tempC %>% select( ., -c('id')) %>%
     pivot_longer(., names_to = 'site', values_to = 'degreesC', cols = sites2)

temp_plot <- ggplot( tempC_long , aes( x=date,y=degreesC, color = site)) +
     geom_line() + 
     scale_color_manual(values = site.col$col) +
     theme_bw()+ theme_nt
     


temp_plot










