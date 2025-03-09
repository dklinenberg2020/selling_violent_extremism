# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2023-09-23
#
# Script Name: figures_1_2_3_table_3_4.R
#  
# Script Description: This script provides the code to recreate Figure 1, Figure 3, Table 3, and Table 4. Due to data restrictions,  Oath Keepers' daily recruitment is substituted with Google Trends data. 
#
#
# Notes:
#  
#  --------------------------------------------

rm(list=ls())
library(pacman)
pacman::p_load(
  "tidyverse",
  "here",
  "ggthemes",
  "CausalImpact",
  "readxl",
  "janitor",
  "lubridate"
)


# Source functions --------------------------------------------------------

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
         cleaner(word=gsub( " .*$", "", label_dat$sale[i]) ,
                  pre_period =  14,
                  placebo = F),
         
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


# demand data

demand_dat<-tibble("sale"=label_dat$sale,
       "actual_avg"=NA,
       "pred_avg"=NA)

for(i in 1:nrow(label_dat)){
  demand_dat$actual_avg[i]<-get(label_dat$am_element[i])[[1]]$summary$Actual[1]
  
  demand_dat$pred_avg[i]<-get(label_dat$am_element[i])[[1]]$summary$Pred[1]
    
}

ci_results[["demand_dat"]]<-demand_dat


# Figure 1 ----------------------------------------------------------------

ci_results$raw_graph_dat %>% 
  ggplot(aes(x=dates,y=true_dat, group=sale, col=shape, shape=shape))+
  geom_line(col="grey")+
  geom_point(size=3)+
  facet_wrap(~sale,scales = "free",
             ncol=2)+
  labs(x="Date",
       y="# New Oath Keepers",
       shape=" ",
       col=" ",
       group=" ")+
  ggtitle("Oath Keepers events. The graphs depict the Google Trends per day. Each pre/post period is 14 days long apart from the flash discount post period. Axis scales differ.")+
  theme_minimal()+scale_color_colorblind()+
  theme(legend.position = "bottom",
        text = element_text(size = 16),
        panel.grid.major =  element_blank())


# Figure 3 ----------------------------------------------------------------

  ci_results$raw_graph_dat %>% 
    ggplot(aes(x=dates,y=true_dat))+
    geom_line(col="grey")+
    geom_point(aes(col=shape, shape=shape),size=3)+
    geom_line(aes(y=estimate),col="blue")+
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95), col=NA,fill="blue", alpha = .2) +
    facet_wrap(~sale, scales = "free", ncol=2)+
    geom_hline(aes(yintercept=0), lty=3)+
    labs(x="Date",
         y="Treatment effect\nNew members per day",
         shape="",
         col="")+
    ggtitle("Effect of Oath Keepers tactics on Google Trends. The blue line is the constructed counterfactual with 95% credibility intervals. Axis scales differ.")+
    theme_minimal()+
    scale_color_colorblind()+
    theme(legend.position = "bottom",
          text = element_text(size = 16),
          panel.grid.major =  element_blank())
  

# Table 3 -----------------------------------------------------------------

  ci_results$effects_tab_ci %>% 
  kableExtra::kable( booktabs=TRUE, 
                     #escape = TRUE,
                     col.names = c(" ",
                                   "Relative Effect (%)",
                                   "Average Effect",
                                   "Cumulative Effect"),
                     align = "lccc",
                     linesep = "") %>% 
  kableExtra::kable_styling(font_size = 8) %>% 
  kableExtra::footnote(list("The relative effect is in terms of percent change. The average effect is the average number of new Oath Keepers per day due to the discount during the discount while the cumulative effect is the total number of new Oath Keepers due to the discount during the discount. Brackets contain 95% credibility intervals. The placebo test for the Flash discount suggests the estimated counterfactual did not accurately approximate the underlying data generating process. The results for the Flash discount should be interperted as suggestive, not causal."), threeparttable = T) %>%
  # row_spec(c(1:2,13,14),bold = T) %>% 
  kableExtra::group_rows("Membership Discounts",1,10,bold = F,
                         italic = T) %>%
  kableExtra::group_rows("Cause-Related Marketing",11,16,bold = F,
                         italic = T) %>%
  kableExtra::group_rows("Sports Sponsorship",17,18,bold = F,
                         italic = T)



# Table 4 -----------------------------------------------------------------

  elasticities<-ci_results$effects_tab_ci %>% 
    filter(str_detect(Sale,"Discount")) %>% 
    type_convert() %>% 
    clean_names() %>% 
    transmute(sale=sale,
              elast=relative_effect/25)
  
  ci_results$effects_tab_ci %>% 
    filter(!str_detect(Sale,"Discount|NASCAR"),
           Sale!=" ") %>% 
    type_convert() %>% 
    clean_names() %>% 
    dplyr::select(sale,relative_effect) %>% 
    mutate("Veteran's Day 2014 (e=-17.2)"=-relative_effect/elasticities$elast[1],
           "Constitution Day 2017 (e=-.151)"=-relative_effect/elasticities$elast[2],
           "Christmas/New Years 2017 (e=-5.53)"=-relative_effect/elasticities$elast[3],
           "Memorial Day 2018 (e=-6.67)"=-relative_effect/elasticities$elast[4]) %>% 
    dplyr::select(-relative_effect) %>% 
    pivot_longer(-sale,
                 names_to = "t",values_to = "v") %>% 
    pivot_wider(values_from=v,names_from=sale) %>% 
    rename(" "=t) %>% 
    kableExtra::kable( booktabs=TRUE, 
                       escape = TRUE,
                       caption = "Corresponding price change for callouts and sponsorships under different price elasticities. Membership is substituted with Google Trends data.",
                       digits = 2,
                       align = "lccc",
                       linesep = "") %>% 
    # kableExtra::kable_styling(font_size = 8) %>% 
    kableExtra::footnote(list("Each panel shows the percent price change such that each tactic would have the same price elasticity as a 25% discount, assuming the same ex-post price elasticity as the given sale. The assumed ex-post price elasticities are in parentheses. Due to data restrictions, these estimates use Google Trends data, not the actual membership inflows."), threeparttable = T)
  