# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2025-02-22
#
# Script Name: table_5.R
#  
# Script Description: This script provides the code to recreate table 5. Due to data restrictions,  Oath Keepers' daily recruitment is substituted a random normal variable centered at zero with variance 1. 
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

# Load in FRED data

fred_income <- read_rds(here::here("data","fred_median_income.rds")) %>% 
  rename("year"="date")

# election data

mit_election=read_csv(here::here("data","countypres_2000-2020.csv"))

cleaned_voter_dat<-mit_election %>% 
  filter(year %in% c(2012,2016),
         party %in% c("Libertarian", "REPUBLICAN")) %>% 
  group_by(year,county_fips) %>% 
  reframe(candidatevotes=sum(candidatevotes),
          totalvotes=unique(totalvotes)) %>% 
  mutate(perc_republican_libertarian=100*candidatevotes/totalvotes,
         county_fips=as.character(county_fips),
         county_fips=ifelse(str_length(county_fips)==4,paste0("0",county_fips),county_fips)) %>% 
  dplyr::select(year,county_fips,perc_republican_libertarian)

# rural
PctUrbanRural_County <- read_excel(here::here("data/PctUrbanRural_County.xls")) %>% 
  mutate(county=paste0(STATE,COUNTY)) %>% 
  dplyr::select(county,
                "perc_pop_rural"=POPPCT_RURAL)

# white

perc_white<-read_rds(here::here("data","census_explan.rds")) %>% 
  dplyr::select(county=GEOID,year,perc_white)


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
  # mutate(pre_event=lubridate::floor_date(pre_event,unit="weeks")) %>% 
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
  mutate(n=rnorm(nrow(.))) %>%  # Making fake outcome column
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
         year=ifelse(str_detect(event,"Defend"),2017,year)) %>% 
  left_join(pop) %>% 
  mutate(ok_per_thousand=100000*n/total_pop)

# add in voting data

linked<-d2 %>% 
  left_join(fred_income) %>% 
  left_join(fred_inequality) %>% 
  left_join(PctUrbanRural_County) %>% 
  left_join(perc_white)%>% 
  mutate(mit_match=ifelse(year<2016,2012,2016)) %>% 
  left_join(cleaned_voter_dat,
            by=c("mit_match"="year",
                 "county"="county_fips")) %>% 
  dplyr::select(-mit_match)



# labeling upper and lower 25 percentiles on stuff

finished<-linked %>% 
  group_by(event) %>% 
  mutate(
    median_income=ntile(median_income,4),
    income_inequality=ntile(income_inequality,4),
    
    perc_republican_libertarian=ntile(perc_republican_libertarian,4),
    perc_pop_rural=ntile(perc_pop_rural,4),
    perc_white=ntile(perc_white,4),
  ) %>% 
  pivot_longer(c(median_income,
                 income_inequality, 
                 perc_republican_libertarian, 
                 perc_pop_rural, 
                 perc_white),
               names_to = "names",
               values_to = "percentile") %>% 
  filter(percentile %in% c(1,4)) %>% 
  mutate(percentile=case_when(
    percentile==1~"lower_25",
    TRUE~"upper_25"),
    percentile=factor(percentile),
    percentile=relevel(percentile,
                       ref="lower_25")
  ) 



# reg ---------------------------------------------------------------------


f_reg<-finished %>% 
  mutate(split=paste0(event,"-",names)) %>% 
  mutate(treat=ifelse(str_detect(percentile,"upper") & event_indicator=="event",1,0)) %>% 
  fixest::feols(ok_per_thousand~treat|date+county,
                cluster = "county",
                weights = ~total_pop,
                split=~split) 

