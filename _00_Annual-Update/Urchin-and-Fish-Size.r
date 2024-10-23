# urchin sizes

# Set file locations ####
# setwd('..') # run only upon opening
home_dir = getwd()

fig_dir = paste0(home_dir,"/figures/")
data_dir = paste0(home_dir,"/data/")
results_dir = paste0(home_dir,"/results/")
#other_dir = paste0(home_dir,"/other/")

# Load packages ###

library(tidyverse)
library(stringr)
library(RColorBrewer)
library(ggpubr) 
library(ggplot2)
library(readxl)

# Load background files ####

settings = readRDS( paste0(data_dir,'settings.RDS') )
theme_nt = readRDS( paste0(data_dir, 'theme_nt.rds') )


### URCHINS ####
process_data = TRUE

if(process_data == TRUE){
# Load data files ####
swathx = data.frame(read.csv(paste0(data_dir, "NWFSC_SWATH_ALLYEARS_data_2024.csv")), header=TRUE)

swath = swathx %>% filter(CLASSCODE %in% c('STRPUR', 'STRDRO','MESFRA'))

# 2021 data
df = read.csv( paste0( data_dir, 'NWFSC_URCHINSIZES_data_2023.csv') )

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
saveRDS(z, file = paste0( data_dir, "Data_Urchin_Sizes_Combined_",minyr, '-' ,maxyr,'.rds') )
# rename just to keep track

} # end if

# Reload to save time if not reprocessing

z <- readRDS( paste0(data_dir, "Data_Urchin_Sizes_Combined_2018-2024.rds") )
# Plot Tatoosh ####
df_tatoosh <- z %>% filter(., Site == "Tatoosh Island")

size_bins = max(df_tatoosh$Size) # gives 1cm bins


graphics.off()

jpeg( paste0( fig_dir, "Urchin_Sizes.jpg" ), units='in', res=300, width = 6 , height = 6)

ggplot(df_tatoosh, aes(Size)) +
     geom_histogram(bins = size_bins) + 
     facet_wrap(Species ~ Year, ncol=4)+
     xlab("Test size (cm)")+
     ylab("Count")+
     theme_bw() + theme_nt
     

dev.off()


### FISH-Black Rockfish ####

fish = data.frame(read.csv( paste0(data_dir, "NWFSC_FISH_ALLYEARS_data_2024.csv"),
   header=TRUE))

df = fish %>% filter(SPECIES =='SEME',
                        SIZE..MIN. > 10,
                        VIS_M >= 2.0) %>%
                 select(YEAR,SITE,SIZE..MIN.,SIZE..MAX.,QUANTITY) %>%
                 rename(year = YEAR, 
                        site = SITE, 
                        min = SIZE..MIN., 
                        max = SIZE..MAX.,
                        number = QUANTITY)

# expand data. uncount is a super useful tidyverse command.

df_expanded = df %>% uncount(number) %>% group_by(year,site)


df_sites = df_expanded %>% mutate(n = 1) %>% group_by(site,year) %>%
  summarise(md = median(min),mn = mean(min), stdev = sd(min), n = sum(n))

df_coast = df_expanded %>% mutate(n = 1) %>% group_by(year) %>%
   summarise(md = median(min), mn = mean(min), stdev = sd(min), n = sum(n))


df_sites
df_coast





seme_plot <- ggplot(data = df_expanded , aes(x = min)) + 
   facet_wrap(facets = df_expanded$year) +
   geom_histogram(binwidth = 5, color='black',fill='white') + 
   xlab("Length (cm)") + 
   ylab("Number of fish")
         
seme_plot + theme_bw() + theme_nt


graphics.off()
png(paste0(fig_dir,"Black-rockfish-size.png"), units = 'in', res = 300, height = 5, width = 5)
seme_plot + theme_bw() + theme_nt
dev.off()


