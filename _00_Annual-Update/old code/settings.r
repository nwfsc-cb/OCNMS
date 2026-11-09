# settings #####################################################################
# common settings across files #################################################
min.vis = 2
last.year = data_year
years = 2015:data_year

##### knitr stuff and libraries ####
# tools in use 
library(tidyverse)
library(knitr)
library(stringr)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(ggplot2)
library(stringr)
library(lemon)
library(ggpubr)
library(tidyr)
library(tinytex) # for pdf output

# spp codes ####################################################################

fish_codes = data.frame(read.csv(paste0(repo_dir,"/Data/spp_codes_fish.csv"), header = TRUE))
kelp_codes = data.frame(read.csv(paste0(repo_dir,"/Data/spp_codes_kelp.csv"), header = TRUE))
swath_codes = data.frame(read.csv(paste0(repo_dir,"/Data/spp_codes_swath.csv"), header = TRUE))

# directories ##################################################################

source_dir = paste0(home_dir)
fig_dir = paste0(home_dir,"/figures/")
data_dir = paste0(repo_dir, "/Data/",data_year,"/")
results_dir = paste0(home_dir,"/results/")
other_dir = paste0(home_dir,"/other/")
par(ps=10, cex=1)

# common plot theme and colors #################################################

theme_nt   <-   theme(legend.title=element_blank(),
                      legend.background = element_blank(),
                      axis.title = element_text(size=8),
                      axis.text.x = element_text(size=8),
                      strip.background = element_blank(),
                      legend.text = element_text(size = 8),
                      legend.key.size = unit(0.25,'lines')
)

# might need to adjust here annually ###########################################
pch = c(8, 21, 22, 23, 24, 4, 25, 26, 27,1,2 )
col = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2,4,6,10,12,7,8,3,5,1,9)]
year.pch = data.frame(cbind(years, pch, col))

sites = c("Neah Bay","Tatoosh Island","Cape Alava","Cape Johnson","Destruction Island")
col = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2,4,6, 10,12)]
site.col = data.frame(cbind(sites,col))
colnames(site.col) <- c('site', 'col')

#### species  and depths #######################################################

kelp.depth = 5
fish.depth = c(5,10)
invert.depth = 5
# set for labelling plots below
depth.labs = c('5-m','10-m')
names(depth.labs) = c(5,10)

