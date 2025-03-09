# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  dklinenberg@ucsb.edu (danny.klinenberg@gmail.com)
# 
# Date Created: 2023-05-05
#
# Script Name: 00_ci_functions.R
#  
# Script Description: Functions used to run CausalImpact and clean the output
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

cleaner<-function(word,
                  pre_period=14,
                  placebo_pre=8,
                  placebo=F,
                  model_args = list()){
  
  vet_dat<-dat %>% 
    filter(str_detect(event,word)) %>% 
    filter(date>=(start_date-lubridate::days(pre_period)))
  
  pre_period<-which(vet_dat$date==vet_dat$start_date)
  post_period<-which(vet_dat$date==vet_dat$end_date)
  
  vet_dat2<-vet_dat %>% 
    dplyr::select(-event,
                  -date,
                  -start_date,
                  -end_date) 
  
  na_cols<-which(colnames(vet_dat2) %in% names(which(is.na(colSums(vet_dat2)))))
  
  if(length(na_cols)>0){
    vet_clean<-vet_dat2[,-na_cols]
  }else{
    vet_clean<-vet_dat2
  }
  
  
  if(placebo==F){
    ci_vet<-CausalImpact::CausalImpact(as.matrix(vet_clean),
                                       pre.period = c(1,(pre_period)-1),
                                       post.period = c(pre_period,post_period),
                                       model.args = model_args
    )
  }else if(placebo==T){
    ci_vet<-CausalImpact::CausalImpact(as.matrix(vet_clean),
                                       pre.period = c(1,(placebo_pre)),
                                       post.period = c(placebo_pre+1,(pre_period)-1),
                                       model.args = model_args
    )
  }
  
  return(list("ci"=ci_vet,
              "dates"=vet_dat$date)
  )
}


# Creates point estimates from CI -----------------------------------------


point_func<-function(label,
                     dates,
                     start_date,
                     end_date,
                     ci_object){
  tibble("sale"=label, 
         "dates"=lubridate::as_date(dates),
         "start_date"=lubridate::as_date(start_date),
         "end_date"=lubridate::as_date(end_date),
         "true_dat"=as.vector(ci_object$series$response),
         "estimate"=as.vector(ci_object$series$point.pred),
         "lower_95"=as.vector(ci_object$series$point.pred.lower),
         "upper_95"=as.vector(ci_object$series$point.pred.upper)
  )
}




# Create effects table with 95 CI -----------------------------------------



effect_ci_func<-function(label,
                         ci_object){
  
  if(is.null(ci_object$summary)){
    tribble(~"Sale", ~"Relative Effect", ~"Average Effect", ~"Cumulative Effect",
            label,"-","-","-",
            " ","-","-","-"
    )
  }else{
    tribble(~"Sale", ~"Relative Effect", ~"Average Effect", ~"Cumulative Effect",
            label,as.character(round(100*ci_object$summary$RelEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[2],2)),  
            " ", paste0("[",round(100*ci_object$summary$RelEffect.lower[1],2),", ",round(100*ci_object$summary$RelEffect.upper[1],2),"]"), paste0("[",round(ci_object$summary$AbsEffect.lower[1],2),", ",round(ci_object$summary$AbsEffect.upper[1],2),"]"),paste0("[",round(ci_object$summary$AbsEffect.lower[2],2),", ",round(ci_object$summary$AbsEffect.upper[2],2),"]")
    )
  }
}


# Create effects table with s.e. -----------------------------------------



effect_se_func<-function(label,ci_object){
  
  if(is.null(ci_object$summary)){
    tribble(~"Sale", ~"Relative Effect", ~"Average Effect", ~"Cumulative Effect",
            label,"-","-","-",
            " ","-","-","-"
    )
  }else{
    tribble(~"Sale", ~"Relative Effect", ~"Average Effect", ~"Cumulative Effect",
            label,as.character(round(100*ci_object$summary$RelEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[2],2)),  
            " ", paste0("(",round(100*ci_object$summary$RelEffect.sd[1],2),")"), paste0("(",round(ci_object$summary$AbsEffect.sd[1],2),")"), paste0("(",round(ci_object$summary$AbsEffect.sd[2],2),")")
    )
  }
  

}



# Create inclusion probability --------------------------------------------


coef_fun<-function(label,ci_object){
  if(is.null(ci_object$summary)){
    tibble("sale"=label,
           "names"=NA,
           "inclusion_prob"=NA,
           "coef_sign"=NA)
  }else{
    tibble("sale"=label,
           "names"=names(colMeans(ci_object$model$bsts.model$coefficients!=0)),
           "inclusion_prob"=as.vector(colMeans(ci_object$model$bsts.model$coefficients!=0)),
           "coef_sign"=colMeans(ci_object$model$bsts.model$coefficients)) 
    }
  }
  


