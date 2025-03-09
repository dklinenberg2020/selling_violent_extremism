# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  dklinenberg@ucsb.edu (danny.klinenberg@gmail.com)
# 
# Date Created: 2023-05-05
#
# Script Name: 00_arco_functions.R
#  
# Script Description: Functions used to run ArCo and clean the output
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
                 "lubridate",
                 "glmnet"
               )


#  Puts data into CI format and runs CI --------------------------------------------------

arco_cleaner<-function(word,
                  pre_period_length=14,
                  placebo_pre_length=8,
                  placebo=F,
                  ...){
  
  vet_dat<-dat %>% 
    filter(str_detect(event,word)) %>% 
    filter(date>=(start_date-lubridate::days(pre_period_length)))
  
  pre_period<-which(vet_dat$date==vet_dat$start_date)
  post_period<-which(vet_dat$date==vet_dat$end_date)
  
  vet_dat2<-vet_dat %>% 
    dplyr::select(-event,
                  -date,
                  -start_date,
                  -end_date) 
  
  na_cols<-which(colnames(vet_dat2) %in% names(which(is.na(colSums(vet_dat2)))))
  
  vet_clean<-vet_dat2[,-na_cols]# %>% 
    # mutate(date=1:nrow(.))
  
  
  
  if(placebo==F){
    
    # removes trends with no variation in the pretreatment.
    # to_drop<-vet_clean %>% 
    #   filter(date %in% ((pre_period-pre_period_length):(pre_period-1))) %>% 
    #   group_by(vars) %>% 
    #   summarise(u=length(unique(vals))) %>% 
    #   filter(u==1)
    # 
    # if(dim(to_drop)[1]>0){
    #   vet_clean<-vet_clean %>% 
    #     filter(!(vars %in% to_drop$vars))
    # }
    
    arco_mod<-ArCo::fitArCo(data = list("data"=as.matrix(vet_clean)),# getting the data
                  treated.unit = which(colnames(vet_clean)=="n"), # getting right row of the data
                  fn=cv.glmnet, #using LASSO
                  p.fn = predict, # recommended prediction
                  t0=pre_period, # when treatment begins
                  VCOV.type = "nw" # preset used in help file
                  
    )
    
  }else if(placebo==T){
    
    # removes trends with no variation in the pretreatment.
    # to_drop<-vet_clean %>% 
    #   filter(date %in% ((pre_period-pre_period_length):(pre_period-placebo_pre_length-1))) %>% 
    #   group_by(vars) %>% 
    #   summarise(u=length(unique(vals))) %>% 
    #   filter(u==1)
    # 
    # vet_clean<-vet_clean %>% 
    #   filter(!(vars %in% to_drop$vars))
    
    
    arco_mod<-ArCo::fitArCo(data = list("data"=as.matrix(vet_clean)),# getting the data
                            treated.unit = which(colnames(vet_clean)=="n"), # getting right row of the data
                            fn=cv.glmnet, #using LASSO
                            p.fn = predict, # recommended prediction
                            t0=pre_period-placebo_pre_length , # when treatment begins
                            VCOV.type = "nw" # preset used in help file
                            
    )
  }
  
  return(list("sc"=arco_mod,
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
    "raw_data"=ci_object$data$data[,ci_object$treated.unit],
    "estimate"=c(ci_object$fitted.values,ci_object$cf)
  )
  return(graph_dat)
}

effect_fun<-function(label,
                     ci_object){
  t<-tibble(
    "sale"=c(label,""),
    "estimate"=c(round(ci_object$delta[2],2),paste0("[",round(ci_object$delta[1],2), " ",round(ci_object$delta[3],2),"]"))
  )
  return(t)
}
