# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2023-06-29
#
# Script Name: alt_white.R
#  
# Script Description: Cleaning up the data for this new wonky idea
#
#
# Notes:
#  
#  --------------------------------------------
library(pacman)
pacman::p_load(
  "tidyverse",
  "here",
  "maps",
  "viridis",
  "ggthemes",
  "mapview",
  "parallel",
  "foreach",
  "doParallel",
  "tidygeocoder",
  "tidycensus",
  "radiant.data",
  "readxl",
  "housingData",
  "haven"
)

# Load in FRED data
fred_inequality <- read_rds(here::here("data","fred_income_inequality.rds")) %>% 
  rename("year"="date")

pop<-read_rds(here::here("data","census_explan.rds")) %>% dplyr::select("county"=GEOID,
                                                                           year,
                                                                           total_pop)

perc_white<-read_rds(here::here("data","census_explan.rds")) %>% 
  dplyr::select(county=GEOID,year,perc_white)


# Laod population data

pop<-read_rds(here::here("data","census_explan.rds")) %>% dplyr::select("county"=GEOID,
                                                                           year,
                                                                           total_pop)

# dates of events
dates<-tribble(~"event",~"pre_event",~"start_event",~"end_event",
               "Veteran's Day Discount 2014",lubridate::date("2014-11-01")-14, lubridate::date("2014-11-01"),lubridate::date("2014-11-11"),
               
               "Constitution Day Discount 2017",lubridate::date("2017-07-20")-14, lubridate::date("2017-07-20"),lubridate::date("2017-09-17"),
               
               "Christmas/New Years Discount 2017",lubridate::date("2017-12-17")-14, lubridate::date("2017-12-17"),lubridate::date("2018-02-09"),
               
               "Flash Discount 2018",lubridate::date("2018-02-27")-14, lubridate::date("2018-02-27"),lubridate::date("2018-03-01"),
               
               "Memorial Day Discount 2018",lubridate::date("2018-05-15")-14, lubridate::date("2018-05-15"),lubridate::date("2018-05-23"),
               
               "Bundy Ranch Callout 2014",lubridate::date("2014-04-04")-14, lubridate::date("2014-04-04"),lubridate::date("2014-04-27"),
               
               "Big Sky Callout 2015",lubridate::date("2015-08-06")-14, lubridate::date("2015-08-06"),lubridate::date("2015-09-03"),
               
               "DefendJ20 2017",lubridate::date("2017-01-17")-14, lubridate::date("2017-01-17"),lubridate::date("2017-01-20"),
               
               "Nascar Sponsorship 2013",lubridate::date("2013-05-04")-14, lubridate::date("2013-05-04"),lubridate::date("2013-07-13"),
) %>% 
  mutate(length_pre_event=interval(pre_event,start_event) %>%
           as.numeric('weeks'),
         length_event=interval(start_event,end_event) %>%
           as.numeric('weeks'),
         year=lubridate::year(start_event),
         year=ifelse(str_detect(event,"Chris"),2017,year),
         year=ifelse(str_detect(event,"Defend"),2017,year)
  )

# all dates

full_dates<-tibble("date"=as_date(NA))

for(i in 1:nrow(dates)){
  full_dates<-rbind(full_dates,
                    tibble("date"=seq(from=dates$pre_event[i],
                                      to=dates$end_event[i],
                                      by="day")
                    ))
}

full_dates<-full_dates %>% 
  drop_na()

# expanding to ensure don't miss any zeroes
all_combos<-expand_grid("county"=unique(fred_inequality$county), # includes all counties
                        "date"=unique(full_dates$date)
)

d2<-all_combos %>% 
  mutate(n=rnorm(nrow(.))) %>% 
  # labelling
  mutate(event=case_when(
    date>=dates$pre_event[1] & date<=dates$end_event[1]~dates$event[1],
    date>=dates$pre_event[2] & date<=dates$end_event[2]~dates$event[2],
    date>=dates$pre_event[3] & date<=dates$end_event[3]~dates$event[3],
    date>=dates$pre_event[4] & date<=dates$end_event[4]~dates$event[4],
    date>=dates$pre_event[5] & date<=dates$end_event[5]~dates$event[5],
    date>=dates$pre_event[6] & date<=dates$end_event[6]~dates$event[6],
    date>=dates$pre_event[7] & date<=dates$end_event[7]~dates$event[7],
    date>=dates$pre_event[8] & date<=dates$end_event[8]~dates$event[8],
    date>=dates$pre_event[9] & date<=dates$end_event[9]~dates$event[9]
  )
  ) %>%
  left_join(dates %>% 
              dplyr::select(event, 
                            pre_event,
                            start_event,
                            end_event)) %>% 
  mutate(event_indicator=ifelse(date<start_event,"pre","event"),
         year=lubridate::year(date),
         year=ifelse(str_detect(event,"Chris"),2017,year),
         year=ifelse(str_detect(event,"Defend"),2017,year)) 

# add in white data

linked<-d2 %>% 
  left_join(perc_white)  %>% 
  left_join(pop) %>% 
  mutate(ok_per_thousand=100000*n/total_pop)

# summary table -----------------------------------------------------------


white_p1<-linked %>% 
  distinct(county,event,perc_white) %>% 
  group_by(event) %>% 
  mutate(percentile=ntile(perc_white,4)
  ) %>% 
  filter(percentile %in% c(1,4)) %>% 
  mutate(percentile=case_when(
    percentile==1~"Lower Quantile Mean",
    TRUE~"Upper Quantile Mean"),
    percentile=factor(percentile),
    percentile=relevel(percentile,
                       ref="Lower Quantile Mean")
  )

white_diff<-white_p1 %>% 
  fixest::feols(perc_white~percentile,
                split = ~event)
pull<-function(x){
  stars<- x$coeftable$`Pr(>|t|)`[2]
  starss<-case_when(stars<=.01~"***",
                    stars<=.05~"**",
                    stars<=.1~"*",
                    TRUE~"")
  rbind(unlist(paste0(as.character(round(x$coeftable[2,1],2)),starss
  )),
  unlist(paste0("(",as.character(round(x$coeftable[2,2],2)),")"))
  )
}

a<-lapply(white_diff,pull) %>% 
  bind_rows() %>% 
  mutate(first=c("Difference"," ")) %>% 
  relocate(first,.before=1)

white_sum<-white_p1 %>% 
  group_by(event,percentile) %>% 
  reframe(m=as.character(round(mean(perc_white),2))) %>% 
  pivot_wider(
    names_from = event,
    values_from = m)

colnames(a)<-colnames(white_sum)

white_sum<-rbind(white_sum,
                 a
)


# labeling upper and lower 25 percentiles on stuff

finished<-linked %>%
  group_by(event) %>% 
  mutate(percentile=ntile(perc_white,4)
  ) %>% 
  filter(percentile %in% c(1,4)) %>% 
  mutate(percentile=case_when(
    percentile==1~"lower_25",
    TRUE~"upper_25"),
    percentile=factor(percentile),
    percentile=relevel(percentile,
                           ref="lower_25")
    ) 



# reg ---------------------------------------------------------------------


white_weighted<-finished %>% 
  mutate(treat=ifelse(str_detect(percentile,"upper") & event_indicator=="event",1,0)) %>% 
  fixest::feols(ok_per_thousand~treat|date+county,
                cluster = "county",
                weights = ~total_pop,
                split=~event)


# reg minus 100 -----------------------------------------------------------

top_ten<-finished %>% 
  distinct(county,event,total_pop) %>% 
  group_by(event) %>% 
  arrange(-total_pop) %>%
  slice(1:100) %>% 
  mutate(inn=1) %>% 
  dplyr:::select(-total_pop) %>% 
  ungroup()

white_small_weighted<-finished %>% 
  left_join(top_ten) %>% 
  filter(is.na(inn)) %>% 
  mutate(treat=ifelse(str_detect(percentile,"upper") & event_indicator=="event",1,0)) %>% 
  fixest::feols(ok_per_thousand~treat|date+county,
                cluster = "county",
                weights = ~total_pop,
                split=~event)



# event study -------------------------------------------------------------

ev_white_weighted<-finished %>% 
  mutate(ev=time_length(interval(start_event,date),"days"),
         ev=factor(ev),
         ev=relevel(ev,
                    ref="-1"),
         percentile=ifelse(str_detect(percentile,'upper'),1,0)) %>% 
  fixest::feols(ok_per_thousand~ev*percentile|county,
                cluster = "county",
                weights = ~total_pop,
                split=~event)


rm(list=ls()[-which(str_detect(ls(),".*weighted|.*sum|linked"))])
