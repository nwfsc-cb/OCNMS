library(tidyverse)
home_dir = getwd()

#### raw data look the same #####
df1 = data.frame(read.csv(paste0("~/GitHub/OCNMS/Data/CSV_2015_on/NWFSC_SWATH_ALLYEARS_data_2021.csv")))
df1 = df1 %>% filter(!is.na(YEAR))
df2 = data.frame(read.csv(paste0("~/GitHub/OCNMS/Data/2024/NWFSC_SWATH_ALLYEARS_data_2024.csv")))
df2 = df2 %>% filter(YEAR <= 2021)

####
dim(df1)
dim(df2)

spp = "NERLUE"

spp1 = df1 %>% 
  dplyr::select(!entry.order) %>% 
  filter(CLASSCODE== spp ) %>% 
  rename(count1 = COUNT)
spp2 = df2 %>% 
  dplyr::select(!entry.order) %>% 
  filter(CLASSCODE== spp ) %>% 
  rename(count2 = COUNT)

dim(spp1)
dim(spp2)

dfx = left_join(spp1, spp2)
dim(dfx)
colnames(dfx)
view(dfx)

ggplot(dfx, aes(count1, count2)) + 
  geom_point() + 
  theme_bw()

cor(dfx$count1, dfx$count2)


#### processed data look ok for counts ####
df1 = readRDS("~/GitHub/OCNMS/Data/CSV_2015_on/Swath_2015-2021.rds")
df2 = readRDS("~/GitHub/OCNMS/Data/2024/Swath_2015-2024.rds")
df2 = df2 %>% filter(year <= 2021)
colnames(df1)==colnames(df2)

spp = "MACPYR"

spp1 = df1 %>% filter(species == spp) %>% rename(count1 = Count)
spp2 = df2 %>% filter(species == spp) %>% rename(count2 = Count)

dim(spp1)
dim(spp1)

dfx = left_join(spp1,spp2)
dim(dfx)
view(dfx)
ggplot(dfx, aes(count1, count2)) + 
  geom_point() + 
  theme_bw()



sppx1 = spp1 %>% filter(site=="Cape Alava" & observer=="Ole Shelton")
sppx2 = spp2 %>% filter(site=="Cape Alava" & observer=="Ole Shelton")
view(sppx1)
view(sppx2)






