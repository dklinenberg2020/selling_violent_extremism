# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2023-10-24
#
# Script Name: Tabl16.R
#  
# Script Description: Looking at demographic data assosciation with ever joining the Oath Keepers.
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
               "modelsummary",
               "ggplot2")

counties <-read_rds(here::here("data","cleaned_county.rds")) %>% 
  distinct(county,year) %>% 
  filter(year==2018)

fred_inequality <- read_rds(here::here("data","fred_income_inequality.rds")) %>% 
  rename("year"="date") %>% 
  filter(year==2018)

controls <-read_rds(here::here("data","cleaned_county.rds")) %>% 
  filter(year==2018) %>% 
  dplyr::select(county,
                gini,
                total_unemployed,
                median_income,
                total_pop,
                perc_males,
                perc_white,
                perc_hs_equiv,
                perc_unemployed,
                perc_republican_libertarian,
                perc_pop_rural) %>% 
  distinct()


# reads oath keepers data in if have it
# county_recs<-read_csv(here::here("data","restricted", "long_lat_aggregate_members.csv")) %>% 
#   mutate(date=lubridate::mdy(X13),
#          county=str_extract(fips,".{5}")) %>% 
#   count(county) %>% 
#   drop_na()

dat<-counties %>% 
  # left_join(county_recs) %>% 
  mutate(n=rnorm(nrow(.))) %>% 
  left_join(controls) %>% 
  left_join(fred_inequality) %>% 
  mutate(n_per_1000=100000*n/total_pop)


# OLS ---------------------------------------------------------------------

big<-dat %>% 
  fixest::feols(n_per_1000~income_inequality+median_income+perc_white+perc_republican_libertarian+perc_pop_rural,
                weights = ~total_pop,
                vcov="hetero"
  )

demo<-dat %>% 
  fixest::feols(n_per_1000~perc_white+perc_republican_libertarian+perc_pop_rural,
                weights = ~total_pop,
                vcov="hetero"
  )

econ<-dat %>% 
  fixest::feols(n_per_1000~income_inequality+median_income,
                weights = ~total_pop,
                vcov="hetero"
  )

gm<-modelsummary::gof_map

gm$clean[which(gm$raw=="nobs")]<-"Number Counties"


# probit ------------------------------------------------------------------

big_probit<-dat %>% 
  fixest::feglm(n_per_1000>0~income_inequality+median_income+perc_white+perc_republican_libertarian+perc_pop_rural,
                weights = ~total_pop,
                family = binomial(link="probit"),
                vcov="hetero"
  )

econ_probit<-dat %>% 
  fixest::feglm(n_per_1000>0~income_inequality+median_income,
                weights = ~total_pop,
                family = binomial(link="probit"),
                vcov="hetero"
  )

demo_probit<-dat %>% 
  fixest::feglm(n_per_1000>0~perc_white+perc_republican_libertarian+perc_pop_rural,
                weights = ~total_pop,
                family = binomial(link="probit"),
                vcov="hetero"
  )


# Tobit -------------------------------------------------------------------


big_aer<-AER::tobit(n_per_1000~income_inequality+median_income+perc_white+perc_republican_libertarian+perc_pop_rural,
                    data = dat,
                    robust=T,
                    weights=dat$total_pop) 

econ_aer<-AER::tobit(n_per_1000~income_inequality+median_income,
                     data = dat,
                     robust=T,
                     weights=dat$total_pop) 

demo_aer<-AER::tobit(n_per_1000~perc_white+perc_republican_libertarian+perc_pop_rural,
                     data = dat,
                     robust=T,
                     weights=dat$total_pop) 




# Table 17 ----------------------------------------------------------------
# Does not use real data - but can plug in real data to recreate main findings.

modelsummary::modelsummary(list("(1)"=big,
                                "(2)"=demo,
                                "(3)"=econ),
                           stars=c("*"=.1,
                                   "**"=.05,
                                   "***"=.01),
                           gof_omit = "R2 Adj.|RMSE|AIC|BIC",
                           fmt = fmt_significant(2,nsmall=1),
                           gof_map = gm,
                           output = "tinytable",
                           add_rows = 
                             tibble(
                               "a"="Average Oath Keepers per 100,000 per County",
                               "b"=round(mean(dat$n_per_1000),3),
                               "c"=round(mean(dat$n_per_1000),3),
                               "d"=round(mean(dat$n_per_1000),3)
                             ),
                           coef_map = 
                             c(
                               "income_inequality"="Income Inequality",
                               "perc_unemployed"="Percent Unemployed",
                               "median_income"="Median Income",
                               "perc_males"="Percent Males",
                               
                               "perc_hs_equiv"="Percent High School Equivilant",
                               
                               "perc_republican_libertarian"="Percent Libertarian/Republican",
                               "total_pop"="Total Population",
                               "perc_white"="Percent White",
                               "perc_pop_rural"="Percent Population Rural",
                               "(Intercept)"="Intercept"
                             ),
                           notes = list("The outcome is total Oath Keepers signup per county per 100,000 per county. All explanatory variables draw from 2018. Regressions are weighted by county population in 2018. The number of counties varies by the availability of demographic characteristics.")
) %>% 
  tinytable::style_tt(fontsize = .7)

