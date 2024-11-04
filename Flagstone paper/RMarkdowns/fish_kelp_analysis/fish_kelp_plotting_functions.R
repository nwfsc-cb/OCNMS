

### 1
# function for plotting abundance and conditional abundance
bivariate_plot_abund <- function(kelp_sp, fish_sp, trans="none"){
  df <- fish_kelp %>% 
    filter(is.na(zone) == FALSE) %>% 
    dplyr::select(year, site, area, zone, transect, kelp_sp, fish_sp)
  
  # if(trans=="log"){
  #   df %<>%
  #     mutate(across(all_of(c("dens","upper","lower")),~pmax(.x,0))) %>% 
  #     mutate(across(all_of(c("dens","upper","lower")),~log(.x+1)))
  # }
  
  xl <- ifelse(trans=="log","Log Density (count/sq. m + 1)","Kelp Density (count/sq. m)")
  yl <- ifelse(trans=="log","Log Density (count/sq. m + 1)","YOY Abundance (# per 60 sq. m)")
  
  pl <- df %>% 
    ggplot(aes(x=.data[[kelp_sp]], y = .data[[fish_sp]], color = as.factor(site)))+
    geom_point() +
    geom_smooth(aes(x=.data[[kelp_sp]],y = .data[[fish_sp]], group=1)) +
    facet_grid(rows = vars(zone)) + #nrow=1
    #scale_x_log10() +
    #scale_y_log10() +
    scale_color_viridis(discrete = T, name = "")  +
    labs(x=xl,y=yl,col="Site",
         title=paste0(fish_sp," as a function of\n",kelp_sp)) +
    theme_classic()
  print(pl)
  ggsave(here::here('Flagstone paper', 
                    'Plots', 
                    'explore_fish_kelp', 
                    paste0(fish_sp," as a function of ",kelp_sp,".pdf")))
  
  
}

### 2
# function for occurrence
# # https://www.ericrscott.com/post/plot-logistic-regressions/
bivariate_plot_occur <- function(kelp_sp, fish_sp, trans="none"){
  df <- yoy_kelp %>% 
    filter(is.na(zone) == FALSE) %>% 
    dplyr::select(year, site, area, zone, kelp_sp, fish_sp)
  
  # if(trans=="log"){
  #   df %<>%
  #     mutate(across(all_of(c("dens","upper","lower")),~pmax(.x,0))) %>% 
  #     mutate(across(all_of(c("dens","upper","lower")),~log(.x+1)))
  # }
  
  # m <- glm(fish_sp ~ kelp_sp, family = binomial, data = df)
  # plot_df <- broom::augment(m, type.predict = "response")
  # 
  # base <-
  #   ggplot(plot_df, aes(x = .data[[kelp_sp]])) +
  #   geom_line(aes(y = .fitted), color = "blue") +
  #   theme_classic()
  
  # xl <- ifelse(trans=="log","Log Density (count/sq. m + 1)","Kelp Density (count/sq. m)")
  xl <- "Kelp Density (count/sq. m)"
  yl <- "YOY Probability of Occurrence"
  
  pl <- df %>% 
    filter(is.na(zone) == FALSE) %>%
    ggplot(aes(x=.data[[kelp_sp]],y = .data[[fish_sp]], color = as.factor(site))) +
    stat_summary_bin(geom = "point", fun = mean, aes(y = .data[[fish_sp]])) + # , bins = 60
    geom_smooth(aes(x=.data[[kelp_sp]],y = .data[[fish_sp]], group=1),
                method = "glm", 
                method.args = list(family = "binomial")) +
    facet_grid(rows = vars(zone)) +
    scale_color_viridis(discrete = T, name = "")  +
    labs(x=xl,y=yl,col="Site",
         title=paste0(fish_sp," occurrence as a function of\n",kelp_sp, " with fitted logistic regression")) +
    theme_classic() #+ 
  #base
  
  print(pl)
  
  ggsave(here::here('Flagstone paper', 
                    'Plots', 
                    'explore_fish_kelp', 
                    paste0(fish_sp," occurrence as a function of ",kelp_sp,".pdf")))
  
  
}