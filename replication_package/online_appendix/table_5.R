# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2025-03-06
#
# Script Name: table_5.R from online appendix. Plug in the OK membership data as column "n" to recreate the graph.
#  
# Script Description:
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

# CI
ci<-read_rds(here::here("04_results_buying_selling","02_ci_placebo_two_weeks_sevendays.rds"))

# ferman

fp<-read_rds(here::here("04_results_buying_selling","02_ferman_pinto_placebo_two_weeks_seven.rds"))

# amdjad

# am<-read_rds(here::here("04_results_buying_selling","02_amjad_placebo_two_weeks_seven.rds"))

arco<-read_rds(here::here("04_results_buying_selling","02_arco_placebo_two_weeks_seven.rds"))

gsynth<-read_rds(here::here("04_results_buying_selling","02_xu_placebo_two_weeks_seven.rds"))

klinenberg<-read_rds(here::here("04_results_buying_selling","02_klinenberg_placebo_two_weeks_seven.rds"))

aug<-read_rds(here::here("04_results_buying_selling","02_aug_placebo_two_weeks_seven.rds"))


# Calculate msfe ----------------------------------------------------------

ci_msfe<-ci$raw_graph_dat %>% 
  filter(shape=="Pre-event") %>% 
  group_by(sale) %>% 
  filter(dates< start_date & dates>=(start_date-days(7))) %>% 
  summarise(ci_msfe=mean((true_dat-estimate)^2)
  )

fp_msfe<-fp$graph_dat %>% 
  group_by(sale) %>% 
  filter(dates< start_date & dates>=start_date-days(7)) %>% 
  summarise(fp_msfe=mean((raw_data-estimate)^2)
  )

arco_msfe<-arco$graph_dat %>% 
  group_by(sale) %>% 
  filter(dates< start_date & dates>=start_date-days(7)) %>% 
  summarise(arco_msfe=mean((raw_data-estimate)^2)
  )

gsynth_msfe<-gsynth$graph_dat %>% 
  group_by(sale) %>% 
  filter(dates< start_date & dates>=start_date-days(7)) %>% 
  summarise(gsynth_msfe=mean((raw_data-estimate)^2)
  )

klinenberg_msfe<-klinenberg$graph_dat %>% 
  group_by(sale) %>% 
  filter(dates< start_date & dates>=start_date-days(7)) %>% 
  summarise(klinenberg_msfe=mean((raw_data-estimate)^2)
  )

aug_msfe<-aug$graph_dat %>% 
  group_by(sale) %>% 
  filter(dates< start_date & dates>=start_date-days(7)) %>% 
  summarise(aug_msfe=mean((estimate)^2)
  )

msfe_table<-ci_msfe %>% 
  left_join(fp_msfe) %>% 
  left_join(aug_msfe) %>% 
  left_join(arco_msfe) %>% 
  left_join(gsynth_msfe) %>% 
  left_join(klinenberg_msfe) 

msfe_table<-msfe_table %>% 
  mutate(across(ci_msfe:gsynth_msfe,~round(.,3)))


msfe_table %>% 
  kableExtra::kable(
    format="latex", booktabs=TRUE, escape = TRUE,
    col.names = c(" ",
                  "Main Specification",
                  "Ferman and Pinto (2021)",
                  "Ben Michael et al. (2019)",
                  "Carvalho et al. (2018)",
                  "Xu (2017)",
                  "Klinenberg (2022)"),
    digits = 2,
    align = "lcccccc",
    linesep = "") %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  kableExtra::footnote(list("A horse race of multiple methods. Each method was fitted 8-14 days prior to an event, then used to create a counterfactual 1-7 days before the event. The mean squared forecast error is presented for the 1-7 day placebo window. See the appendix for results fitting the models on weeks 2-3 and weeks 2-4."), threeparttable = T) %>%
  kableExtra::group_rows("Membership Discounts",1,5, bold = F,italic = T) %>%
  kableExtra::group_rows("Cause-Related Marketing",6,8, bold = F,italic = T) %>%
  kableExtra::group_rows("Sports Sponsorship",9,9, bold = F,italic = T) 