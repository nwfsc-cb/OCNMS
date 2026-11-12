get_summary_info <- function(dfile, spp_codes){
  
  # species observed by site and year 
  nspp = dfile%>% 
    group_by(year, site, species) %>% 
    mutate(n=sum(Count)) %>%
    summarize(n = sum(n)) %>%
    filter(n>0) %>% mutate(n = 1) %>%
    group_by(year,site) %>%
    summarize(Taxa = sum(n)) %>% 
    pivot_wider(id_cols='site', names_from = year, values_from = Taxa) %>%
    rename(Site = site)
  
  # sites with most species
  xspp = max(nspp[,ncol(nspp)], na.rm=T)
  xsites = nspp %>% select(Site, as.character(data_year))
  xsites = na.omit(xsites[xsites[,2]==xspp,'Site']  )
  if(nrow(xsites)==1){Xsites = xsites}
  if(nrow(xsites>1)){Xsites = knitr::combine_words(xsites)}
  

  Yspp = dfile %>% filter(size_class != 'small') %>%
    group_by(year, species) %>% 
    summarise(Total = sum(Count)) %>% 
    filter(Total > 0)
  
  # total observed across all years
  Tspp = Yspp %>% 
    group_by(species) %>%
    summarise(Total = sum(Total))
  
  # add in common names and correct species names
  Tspp$species[Tspp$species=='MYOP'] <- "MYPO"
  
  Tspp$Species = spp_codes$spp.name[ match(Tspp$species, spp_codes$species) ]
  Tspp$`Common name` = spp_codes$common.name[ match(Tspp$species, spp_codes$species) ]
  
  Tspp = Tspp[ order(Tspp$Total, decreasing=TRUE),]
  Tspp$cn = Tspp$`Common name`
  
  spp_yr = Yspp[Yspp$year==data_year,]
  spp_yr = spp_yr[ order(spp_yr$Total, decreasing=T), ]
  
  spp_yr_no = Yspp$Total[Yspp$year==data_year & Yspp$species==spp_yr$species[1]]
  spp_yr_nm = spp_codes$common.name[spp_codes$speci==spp_yr$species[1]]
  
  if(spp_yr_nm==Tspp$cn[1]){text1 = " also "}else{text1=" "}
  
  df_out = list(xsites = xsites, Tspp = Tspp, Yspp=Yspp,maxspp = xspp, nspp = nspp,spp_yr_no=spp_yr_no, spp_yr_nm=spp_yr_nm)
  
} #end function