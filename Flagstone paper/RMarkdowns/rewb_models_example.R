# Replication code for:
# How social network sites and other online intermediaries increase exposure to news
# Michael Scharkow, Frank Mangold, Sebastian Stier, Johannes Breuer
# Contact us: scharkow@uni-mainz.de
setwd("C:/Users/nick.tolimieri/Desktop/REWB-models")
library(nloptr)
library(lme4)
library(broom)
library(tidyverse)

library(furrr)
plan(multiprocess)

glmer_opts = glmerControl(optimizer="nloptwrap",
                          optCtrl=list(algorithm="NLOPT_LN_BOBYQA"),
                          calc.derivs = FALSE)

cb_colors = c("#000000", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Replace 2012  with 2018  for the 2018 results
d = read_csv("2012_daily.csv.gz")

# Descriptives ------------------------------------------------------------

desc_person = d %>% group_by(person_id) %>%
  summarise(facebook = mean(facebook_visits)>0,
            twitter = mean(twitter_visits)>0,
            google = mean(google_visits)>0,
            portals = mean(portals_visits)>0,
            news = mean(news_visits)>0,
            female = mean(female),
            age = mean(age),
            ndays = length(unique(day)),
            nnews = mean(news_sites))


desc = bind_cols(
  d %>%
    select(contains("visits")) %>%
    summarise_if(is.numeric, list(m = mean, sd = sd, min = min, max = max, median = median))
  , desc_person  %>%
    summarise_at(vars(-person_id), list(m = mean, sd = sd, min = min, max = max, median = median))
)


tab1 = desc %>%
  select(ends_with("_m"), ends_with("_sd")) %>%
  select(contains("visits")) %>%
  gather(k,v) %>%
  # mutate(v = round(v, 2)) %>%
  separate(k, into=c("k", "x", "var")) %>%
  select(-x) %>%
  spread(var, v)

mean(d$news_sites); sd(d$news_sites)
tab1

# REWB transformations  --------------------------------------------------------------------

# d_rewb = d %>%
#   mutate(other_visits = total_visits - news_visits - google_visits - 
#               facebook_visits - twitter_visits - portals_visits) %>%
#   group_by(person_id) %>%
#   mutate_at(vars(matches("_visits|_sites")),
#             funs(log = log1p(.))) %>%
#   mutate_at(vars(contains("log")),
#             funs(betw = mean(.), within = .-mean(.))) %>%
#   ungroup %>%
#   mutate_at(vars(ends_with("betw"), age), funs(c = . - mean(.))) %>%
#   mutate(obs = 1:nrow(.))

d_rewb = d %>%
     # get other_visits, simple math.
     mutate(other_visits = total_visits - news_visits - google_visits - 
                 facebook_visits - twitter_visits - portals_visits) %>%
     group_by(person_id) %>%
     # log(x+1) the data for columns with visits or sites
     mutate_at(vars(matches("_visits|_sites")),
               funs(log = log1p(.))) %>%
     # calculate means and xit-xi
     # between is just the mean xi
     # within is obs-mean or xit-xi
     mutate_at(vars(contains("log")),
               funs(betw = mean(.), within = .-mean(.))) %>%
     # not sure what this does
     ungroup %>%
     # something about the age metric, vi1(xit-xi)
     mutate_at(vars(ends_with("betw"), age), funs(c = . - mean(.))) %>%
     mutate(obs = 1:nrow(.))

# REWB models --------------------------------------------------------------

# formula includes within and between_c variables

rewb_formula = "  ~
facebook_visits_log_within +
twitter_visits_log_within +
google_visits_log_within +
portals_visits_log_within +
other_visits_log_within +

facebook_visits_log_betw_c +
twitter_visits_log_betw_c +
google_visits_log_betw_c +
portals_visits_log_betw_c +
other_visits_log_betw_c +

age_c + female +

(1 + 
facebook_visits_log_within +
twitter_visits_log_within +
google_visits_log_within +
portals_visits_log_within +
other_visits_log_within | person_id) +

(1|day) + 

(1|obs)"

rewb_results = list(
  rewb_sites = paste("news_sites", rewb_formula),
  rewb_visits = paste("news_visits", rewb_formula)
) %>%
  map(as.formula) %>%
  future_map(~ glmer(.x, data = d_rewb, family = poisson, control = glmer_opts))

rewb_results %>% map(tidy)









