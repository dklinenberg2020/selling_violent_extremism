# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2025-02-22
#
# Script Name: figure_2.R
#  
# Script Description: This script provides the code to recreate Figure 2. Due to data restrictions,  Oath Keepers' daily recruitment is substituted with Google Trends data. 
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
                "ggplot2")


# Data construction -------------------------------------------------------

source(here::here("00_ci_functions.R"))

dat <-read_rds(here::here("data","cleaned_sc_dat.R.rds")) 

label_dat<-read_rds(here::here("data","event_data.rds"))
outcome <-readxl::read_xls(here::here("data","ok_google_trends.xls")) %>% 
  mutate(oath_keepers=as.numeric(oath_keepers))

dat<-outcome %>% 
  left_join(dat) %>% 
  rename(n=oath_keepers)

# run CIs in a loop ------------------------------------------------------------------

# puts the CI estimates into global environment with name listed in label_dat

for(i in 1:nrow(label_dat)){
  assign(label_dat$am_element[i],
         cleaner(gsub( " .*$", "", label_dat$sale[i]) ,
                 pre_period =  14,
                 placebo = T,
                 placebo_pre = 7),
         
  )
}

# Output ------------------------------------------------------------------

# save results here
ci_results<-list()

# point data

point_effects<-point_func(label=label_dat$sale[1],
                          dates=get(label_dat$am_element[1])[[2]],
                          start_date = label_dat$start_date[1],
                          end_date=label_dat$end_date[1],
                          ci_object= get(label_dat$am_element[1])[[1]]
)

for(i in 2:nrow(label_dat)){
  point_effects<-rbind(point_effects,
                       point_func(label=label_dat$sale[i],
                                  dates=get(label_dat$am_element[i])[[2]],
                                  start_date = label_dat$start_date[i],
                                  end_date=label_dat$end_date[i],
                                  ci_object= get(label_dat$am_element[i])[[1]]
                       )
  )
} 

# set the factor for ggplot2 later
point_effects<-point_effects %>% 
  mutate(sale=factor(sale,
                     levels=unique(label_dat$sale)))

ci_results[["point_effects"]]<-point_effects

# effects table with 95 CI


effects_table_ci<-effect_ci_func(label_dat$sale[1],
                                 get(label_dat$am_element[1])[[1]])

for(i in 2:nrow(label_dat)){
  effects_table_ci<-rbind(effects_table_ci,
                          effect_ci_func(label_dat$sale[i],
                                         get(label_dat$am_element[i])[[1]])
  )
} 

ci_results[["effects_tab_ci"]]<-effects_table_ci

# effect table with 95 s.e.

effects_table_se<-effect_se_func(label_dat$sale[1],
                                 get(label_dat$am_element[1])[[1]])

for(i in 2:nrow(label_dat)){
  effects_table_se<-rbind(effects_table_se,
                          effect_se_func(label_dat$sale[i],
                                         get(label_dat$am_element[i])[[1]])
  )
} 

ci_results[["effects_tab_se"]]<-effects_table_se



# coefficients graph ------------------------------------------------------

includ_prob_dat<-coef_fun(label_dat$sale[1],
                          am_vet[[1]])

for(i in 2:nrow(label_dat)){
  includ_prob_dat<-rbind(includ_prob_dat,
                         coef_fun(label_dat$sale[i],
                                  get(label_dat$am_element[i])[[1]])
  )
}

# create a factor variable and clean names of donor pool
includ_prob_dat<-includ_prob_dat %>% 
  mutate(names=case_when(
    names=="(Intercept)"~"Intercept",
    names=="light_foot_militia"~"Light Foot Militia",
    names=="civilian_defense_force"~"Civilian Defense Force",
    names=="patriot_prayer"~"Patriot Prayer",
    names=="john_birch_society"~"John Birch Society",
    names=="three_percenters"~"3 Percenters",
    names=="eagle_forum"~"Eagle Forum",
    names=="we_are_change"~"We Are Change",
    names=="proud_boys"~"Proud Boys",
    names=="rise_above_movement"~"Rise Above Movement",
  )) %>% 
  mutate(sale=factor(sale,
                     levels=unique(label_dat$sale)),
         names=factor(names,
                      levels=unique(.$names)))

ci_results[["includ_prob_dat"]]<-includ_prob_dat

# raw data graphed --------------------------------------------------------

raw_graph_dat<-ci_results$point_effects %>% 
  mutate(shape=case_when(
    (dates<start_date)~"Pre-event",
    (dates>end_date)~"Post-event",
    TRUE~"Event"
  ),
  shape=factor(shape,
               levels = c(
                 "Pre-event",
                 "Event",
                 "Post-event"
               ))
  ) 

ci_results[["raw_graph_dat"]]<-raw_graph_dat

# Figure 2 ----------------------------------------------------------------

ci_results$raw_graph_dat%>% 
  group_by(sale) %>% 
  slice(1:14) %>% 
  ggplot(aes(x=dates,y=true_dat))+
  geom_line(col="grey")+
  geom_point(aes(col=shape, shape=shape),size=3)+
  geom_line(aes(y=estimate),col="blue")+
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), col=NA,fill="blue", alpha = .2) +
  geom_vline(aes(xintercept=start_date),lty=2)+
  geom_vline(aes(xintercept=start_date-days(7)-.5),lty=2)+
  geom_hline(aes(yintercept=0), lty=2)+
  facet_wrap(~sale, scales = "free", ncol=2)+
  labs(x="Date",
       y="Treatment effect\nNew members per day",
       shape="",
       col="")+
  ggtitle("Placebo test of Oath Keepers discounts on Google Trends. The dashed lines show the beginning of the placebo period. Window limited to pre-discount. The blue line is the constructed counterfactual with 95% credibility intervals.")+
  theme_minimal()+scale_color_colorblind()+
  theme(legend.position = "bottom",
        text = element_text(size = 16),
        panel.grid.major =  element_blank())

