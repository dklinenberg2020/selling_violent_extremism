# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2025-03-06
#
# Script Name: table_4.R
#  
# Script Description: Table 4 from online appendix. Plug in the OK membership data as column "n" to recreate the graph.
#
#
# Notes:
#  
#  --------------------------------------------

rm(list=ls())
if(!("pacman" %in% installed.packages())){
  install.packages("pacman")
}
pacman::p_load("tidyverse",
               "here",
               "ggplot2",
               "lubridate")
info<-read_rds(here::here("data","videos_with_ok.rds")) %>% 
  dplyr::select(date) %>% 
  mutate(info=1)


dat <-read_rds(here::here("data","cleaned_sc_dat.R.rds")) %>% 
  filter(date>=(start_date-lubridate::days(14))) %>% 
  mutate(n=rnorm(nrow(.)))


regg<-dat %>%
  mutate(label=case_when(date<start_date~"pre_event",
                         date>end_date~"post_event",
                         TRUE~"event")) %>% 
  mutate(label=as.factor(label),
         label=relevel(label,
                       ref="pre_event")) %>% 
  left_join(info) %>% 
  mutate(info=ifelse(is.na(info),0,info),
         timess=time_length(interval(start_date,date),"days")
  ) %>% 
  fixest:::feols(n~label,
                 split = ~event,
                 vcov="newey_west", 
                 panel.id=~event+date)


regg_controls<-dat %>%
  mutate(label=case_when(date<start_date~"pre_event",
                         date>end_date~"post_event",
                         TRUE~"event")) %>% 
  mutate(label=as.factor(label),
         label=relevel(label,
                       ref="pre_event")) %>% 
  left_join(info) %>% 
  mutate(info=ifelse(is.na(info),0,info),
         timess=time_length(interval(start_date,date),"days")
  ) %>% 
  fixest:::feols(n~label+info+timess,
                 split = ~event,
                 vcov="newey_west", 
                 panel.id=~event+date)

modelsummary::modelsummary(list(
  "Veteran's Day 2014"=regg$`sample.var: event; sample: Veteran's Day Discount 2014`,
  "Constitution Day 2017"=regg$`sample.var: event; sample: Constitution Day Discount 2017`,
  "Christmas/New Years 2017"=regg$`sample.var: event; sample: Christmas/New Years Discount`,
  "Memorial Day 2018"=regg$`sample.var: event; sample: Memorial Day Discount 2018`,
  "Flash 2018"=regg$`sample.var: event; sample: Flash Discount 2018`,
  "Bundy Ranch 2014"=regg$`sample.var: event; sample: Bundy Ranch Callout 2014`,
  "Big Sky 2015"=regg$`sample.var: event; sample: Big Sky Callout 2015`,
  "DefendJ20 2017"=regg$`sample.var: event; sample: DefendJ20 2017`,
  "Nascar 2013"=regg$`sample.var: event; sample: NASCAR Sponsorship 2013`
),
stars = c("*"=.1,
          "**"=.05,
          "***"=.01),
gof_omit="R.*|AIC|BIC",
title = "Table 4 with fake data",
output = "kableExtra",
coef_map = c(
  "(Intercept)"="Intercept",
  "labelevent"="I(Event)",
  "labelpost_event"="I(Post-Event)",
  "info"="I(On Infowars)",
  "timess"="Time Trend"
)) %>% 
  kableExtra::add_header_above(c(" "=1,
                                 "Discounts"=5,
                                 "Callouts"=3,
                                 "Advertisement"=1))

