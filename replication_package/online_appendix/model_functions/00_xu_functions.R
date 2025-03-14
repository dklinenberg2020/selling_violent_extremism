# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  dklinenberg@ucsb.edu (danny.klinenberg@gmail.com)
# 
# Date Created: 2023-05-09
#
# Script Name: 00_xu_functions.R
#  
# Script Description: Functions used to run Gsynth and clean the output
#
#
# Notes:
#  
#  --------------------------------------------

if(!("pacman" %in% installed.packages())){
  install.packages("pacman")
}
pacman::p_load(  "tidyverse",
                 "here",
                 "CausalImpact",
                 "readxl",
                 "lubridate"
               )


#  Puts data into CI format and runs CI --------------------------------------------------

xu_cleaner<-function(word,
                  pre_period_length=14,
                  placebo_pre_length=8,
                  placebo=F,
                  ...){
  
  vet_dat<-dat %>% 
    filter(str_detect(event,word)) %>% 
    filter(date>=(start_date-lubridate::days(pre_period_length)))
  
  pre_period<-which(vet_dat$date==vet_dat$start_date)
  post_period<-which(vet_dat$date==vet_dat$end_date)

  
  if(placebo==F){
    
    vet_dat1<-vet_dat %>% 
      pivot_longer(
        -seq(1,4),
        names_to="vars",
        values_to="vals"
      ) %>% 
      drop_na() %>% 
      mutate(d=case_when(
        vars=="n"& date>=start_date~1,
        TRUE~0
      ))
    
    vet_dat2<-vet_dat1 %>% 
      filter(date>=(start_date-days(pre_period_length))) %>% 
      mutate(date=lubridate::as_date(date))

    gsynth_mod<-gsynth::gsynth(vals~d,
                                  data = vet_dat2,
                                  index = c("vars","date"),
                                  force = "two-way",
                                  inference = "parametric",
                                  EM=TRUE,
                                  CV=TRUE,
                                  se=TRUE)
    
  }else if(placebo==T){
    
    vet_dat1<-vet_dat %>% 
      pivot_longer(
        -seq(1,4),
        names_to="vars",
        values_to="vals"
      ) %>% 
      drop_na() %>% 
      mutate(d=case_when(
        vars=="n"& date<=start_date & date>=(start_date-days(placebo_pre_length))~1,
        TRUE~0
      ))
    
    vet_dat2<-vet_dat1 %>% 
      filter(date>=(start_date-days(pre_period_length))) %>% 
      mutate(date=lubridate::as_date(date))
    
    gsynth_mod<-gsynth::gsynth(vals~d,
                               data = vet_dat2,
                               index = c("vars","date"),
                               force = "two-way",
                               inference = "parametric",
                               EM=TRUE,
                               CV=TRUE,
                               se=TRUE)
  }
  
  return(list("sc"=gsynth_mod,
              "dates"=vet_dat$date)
  )
}


# Creates point estimates from CI -----------------------------------------

point_func<-function(label,
                     dates,
                     start_date,
                     end_date,
                     ci_object){
  graph_dat<-tibble(
    "sale"=label, 
    "dates"=dates,
    "start_date"=lubridate::as_date(start_date),
    "end_date"=lubridate::as_date(end_date),
    "raw_data"=ci_object$Y.dat[,"n"],
    "estimate"=ci_object$est.att[,1],
    "lower_95"=ci_object$est.att[,3],
    "upper_95"=ci_object$est.att[,4]
  )
  return(graph_dat)
}

