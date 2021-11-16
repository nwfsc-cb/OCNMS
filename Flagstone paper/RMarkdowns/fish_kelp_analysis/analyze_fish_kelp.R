
library(MASS)

### 3
# Analysis and plot of Total YOY occurrence vs 3 kelps

# based on multivariate analysis, site not a big deal for TOTyoy. on the other hand, year is a big deal. so we include year asa random effect.
# For the logit you want the weights argument, not the offset argument. Essentually you want to treat the 60m^2 as having 6 times the weight as a 10m^2 transect. so I think you can call your 60m2 a weight of 1 and the 20m2 with a weight of 0.33, 10m2 with a weight of 1/6, etc.

# length(which(is.na(fish_kelp$zone) == TRUE))
# length(which(is.na(fish_kelp$three_kelps) == TRUE))
# length(which(is.na(fish_kelp$TOTyoy) == TRUE))
# %>% 
#   filter(is.na(zone) == FALSE) %>%
#   filter(is.na(three_kelps) == FALSE) %>% # note that 2017       Cape Alava W 10 has no kelp data. fix later
#   filter(is.na(TOTyoy) == FALSE)


m_occur_full <- glmer( TOTyoy_pres ~ (three_kelps * zone) + (1|year_factor), 
                       family = binomial, 
                       weights = transect.area.algae.weight,
                       data = fish_kelp
)
summary(m_occur_full)
AIC(m_occur_full)

# # drop zone. could be problematic b/c too much variability among kelp densities when we drop zone
# m_occur <- glmer( TOTyoy_pres ~ three_kelps + (1|year_factor), 
#                        family = binomial, 
#                        offset = area_transects/100, # makes the units nicer
#                        data = fish_kelp %>% 
#                          filter(is.na(zone) == FALSE) %>%
#                          filter(is.na(three_kelps) == FALSE) %>% # note that 2017       Cape Alava W 10 has no kelp data. fix later
#                          filter(is.na(TOTyoy_pres) == FALSE)
# )
# summary(m_occur)
# AIC(m_occur)

### plotting
plot_df <- broom::augment(m_occur, type.predict = "response")

base <-
  ggplot(plot_df, aes(x = three_kelps, color = factor(zone))) +
  geom_line(aes(y = .fitted), color = "blue") +
  theme_classic()

base + geom_point(aes(y = TOTyoy_pres), alpha = 0.2)
base + stat_summary_bin(geom = "point", fun = mean, aes(y = TOTyoy_pres), bins=60) 

p1 <- fish_kelp %>% 
  filter(is.na(zone) == FALSE) %>%
  ggplot(aes(x=three_kelps,y = TOTyoy_pres, color = as.factor(site))) +
  stat_summary_bin(geom = "point", fun = mean, aes(y = TOTyoy_pres)) + # , bins = 60
  geom_smooth(aes(x=three_kelps,y = TOTyoy_pres, group=1),
              method = "glm", 
              method.args = list(family = "binomial")) +
  scale_color_viridis(discrete = T, name = "")  +
  labs(x="Density of 3 kelps",y="Probability of occurrence",col="Site",
       title= "Total YOY rockfish occurrence as a function of 3 kelps,\nwith fitted logistic regression") +
  theme_classic() 

print(p1)

ggsave(here::here('Flagstone paper', 
                  'Plots', 
                  'Total YOY rockfish occurrence as a function of 3 kelps, with fitted logistic regression.pdf'))


### 4
# Analysis and plot of Total YOY abundance vs 3 kelps
# make sure TOTyoy is count data, use negative binomial model
# use glm.nb is glmer.nb is wonky, and just make year a fixed effect
# use each fish transect as data for neg binom. Use area sampled for each fish transect as weights, use mean kelp density for that area as covariate. Same kelp density for all transects in an area.
 
# length(which(is.na(fish_kelp$zone) == TRUE))
# length(which(is.na(fish_kelp$three_kelps) == TRUE))
# length(which(is.na(fish_kelp$TOTyoy) == TRUE))
# %>% 
#   filter(is.na(zone) == FALSE) %>%
#   filter(is.na(three_kelps) == FALSE) %>% # note that 2017       Cape Alava W 10 has no kelp data. fix later
#   filter(is.na(TOTyoy) == FALSE)

m_abund_full <- glm.nb( TOTyoy ~ (three_kelps * zone) + (year_factor) + offset(log(transect.area.algae/100)), # makes the units nicer
                          data = fish_kelp 
)
summary(m_abund_full)
AIC(m_abund_full)


# plot_df2 <- broom::augment(m_abund_full, type.predict = "response")
# 
# base <-
#   ggplot(plot_df2, aes(x = three_kelps, color = factor(zone))) +
#   geom_line(aes(y = .fitted), color = "blue") +
#   theme_classic()
# 
# base + geom_point(aes(y = TOTyoy), alpha = 0.2)

p2 <- fish_kelp %>% 
  ggplot(aes(x = three_kelps, y = TOTyoy, color = as.factor(site))) +
  geom_point() +
  #geom_smooth(aes(x=three_kelps,y = TOTyoy_cond_abund, group=1)) +
  scale_color_viridis(discrete = T, name = "")  +
  labs(x="Density of 3 kelps",y="Density",col="Site",
       title= "Total YOY rockfish count as a function of 3 kelps") +
  theme_classic() 

print(p2)

ggsave(here::here('Flagstone paper', 
                  'Plots', 
                  'Total YOY rockfish counts as a function of 3 kelps.pdf'))

######################################################
################## GRAVEYARD ##################

# make dummy area of transects column. area_transects will differ between fish and kelp. use area_transects from fish, because kelp is predictor.
# fish_kelp <- fish_kelp %>%
#   mutate(
#     area_transects = sample(60:240, 1)
#   )