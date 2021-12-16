# urchin sizes

# Set file locations ####

HomeFile = "C:/Users/nick.tolimieri/Documents/GitHub/OCNMS/Flagstone paper"
Fig_Loc = paste0(HomeFile,"/Plots/")
Data_Loc = paste0(HomeFile,"/Data/")
Results_Loc = paste0(HomeFile,"/Results/")
Other_Files = paste0(HomeFile,"/Other Files/")

# Load packages ###

library(tidyverse)
library(stringr)
library(RColorBrewer)
library(ggpubr) 
library(ggplot32)
library(readxl)

# Load background files ####

settings = readRDS( paste0(Data_Loc,'settings.RDS') )
theme_nt = readRDS( paste0(Data_Loc, 'theme_nt.rds') )

process_data = FALSE

if(process_data == TRUE){
# Load data files ####
swath2019 = data.frame(read.csv(paste0(Data_Loc, "NWFSC_SWATH_ALLYEARS_data_2019.csv")), header=TRUE)

swath = swath2019 %>% filter(CLASSCODE %in% c('STRPUR', 'STRDRO','MESFRA'))

# 2021 data
df = read_excel( paste0( Data_Loc, 'NWFSC_URCHINSIZES_data_2021.xlsm') )

df_combined = swath %>% bind_rows(df)

# Tatoosh urchins ####

df_all = df_combined %>% 
      group_by(YEAR, SITE, ZONE, CLASSCODE, SIZE) %>%
      summarise(Count = sum(COUNT)) %>%
      rename(Size = SIZE, 
             Species = CLASSCODE,
             Year = YEAR,
             Site=SITE)

df_all$Species[ df_all$Species == 'STRPUR'] <- 'Purple urchin'
df_all$Species[ df_all$Species == 'MESFRA'] <- 'Red urchin'
df_all$Species[ df_all$Species == 'STRDRO'] <- 'Green urchin'

df_all$Species = factor(df_all$Species , levels = c('Purple urchin',
                                                    'Red urchin',
                                                    'Green urchin'))
# drop rows with no sizes ####
df_all <- filter(df_all, !is.na(Size))

# expand to make histogram later ####
# really slow
for( i in 1:nrow(df_all)){
     print(i)
     x = df_all[i,]
     for(k in 1:x$Count){
          if(k == 1){y = x}else{y = rbind(y,x)}     
     } # end k
     y$Count = 1 # reset to one now one individual per row
     
     if(i == 1){z = y}else{z = rbind(z,y)}
} # end i

# check
nrow(z) == sum(df_all$Count)
nrow(z)

# Output file ####
minyr = min(z$Year)
maxyr = max(z$Year)
saveRDS(z, file = paste0( Data_Loc, "Data_Urchin_Sizes_Combined_",minyr, '-' ,maxyr,'.rds') )
# rename just to keep track

} # end if

# Reload to save time if not reprocessing

z <- readRDS( paste0(Data_Loc, "Data_Urchin_Sizes_Combined_2018-2021.rds") )
# Plot Tatoosh ####
df_tatoosh <- z %>% filter(., Site == "Tatoosh Island")

size_bins = max(df_tatoosh$Size) # gives 1cm bins


graphics.off()

jpeg( paste0( Fig_Loc, "Urchin_Sizes.jpg" ), units='in', res=300, width = 6 , height = 6)

ggplot(df_tatoosh, aes(Size)) +
     geom_histogram(bins = size_bins) + 
     facet_wrap(Year ~ Species)+
     xlab("Test size (cm)")+
     ylab("Count")+
     theme_bw() + theme_nt
     

dev.off()


### sizes








