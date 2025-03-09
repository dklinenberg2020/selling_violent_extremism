# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  dklinenberg@ucsb.edu (danny.klinenberg@gmail.com)
# 
# Date Created: 2023-05-05
#
# Script Name: 00_sc_functions.R
#  
# Script Description: Functions used to run SC and clean the output using scpi
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

sc_cleaner<-function(word,
                  pre_period_length=14,
                  placebo_pre_length=8,
                  placebo=F,
                  w.constr=list(name = "ridge", Q = 1, Q2=NULL,constant=F),
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
  
  vet_clean<-vet_dat2[,-na_cols]
  
  #  convert to scpi format
  
  vet_clean1<-vet_clean %>% 
    mutate(date=1:nrow(.)) %>% 
    pivot_longer(-date,
                 names_to = "vars",
                 values_to = "vals")
  
  
  
  if(placebo==F){
    
    # removes trends with no variation in the pretreatment.
    to_drop<-vet_clean1 %>% 
      filter(date %in% ((pre_period-pre_period_length):(pre_period-1))) %>% 
      group_by(vars) %>% 
      summarise(u=length(unique(vals))) %>% 
      filter(u==1)
    
    vet_clean1<-vet_clean1 %>% 
      filter(!(vars %in% to_drop$vars))
    
    
    sc_dat<-vet_clean1 %>% 
      scpi::scdata(id.var = "vars",
                   time.var = "date",
                   outcome.var = "vals",
                   unit.tr = "n",
                   unit.co = setdiff(.$vars,"n"),
                   period.pre = ((pre_period-pre_period_length):(pre_period-1)),
                   period.post = (pre_period:max(.$date))
      )
    
    sc_out<-try(scpi::scpi(sc_dat,
               w.constr = w.constr,
               sims = 500
               )
    )
    
    if(class(sc_out)=="try-error"){
      sc_out<-scpi::scest(sc_dat,
                          w.constr = w.constr)
    }
    
    # post_dates<-vet_dat$date[(pre_period:nrow(vet_dat))]
    
  }else if(placebo==T){
    
    # removes trends with no variation in the pretreatment.
    to_drop<-vet_clean1 %>% 
      filter(date %in% ((pre_period-pre_period_length):(pre_period-placebo_pre_length-1))) %>% 
      group_by(vars) %>% 
      summarise(u=length(unique(vals))) %>% 
      filter(u==1)
    
    vet_clean1<-vet_clean1 %>% 
      filter(!(vars %in% to_drop$vars))
    
    
    
    sc_dat<-vet_clean1 %>% 
      scpi::scdata(id.var = "vars",
                   time.var = "date",
                   outcome.var = "vals",
                   unit.tr = "n",
                   unit.co = setdiff(.$vars,"n"),
                   period.pre = ((pre_period-pre_period_length):(pre_period-placebo_pre_length-1)),
                   period.post = ((pre_period-placebo_pre_length):max(.$date))
      )

    sc_out<-try(scpi::scpi(sc_dat,
                       w.constr = w.constr,
                       sims = 500
    ) )
    if(class(sc_out)=="try-error"){
      sc_out<-scpi::scest(sc_dat,
                     w.constr = w.constr)
    }
  }
  
  return(list("sc"=sc_out,
              "dates"=vet_dat$date)
  )
}


# Creates point estimates from CI -----------------------------------------

point_func<-function(label,
                     dates,
                     start_date,
                     end_date,
                     ci_object){
  if(is.null(ci_object$inference.results$CI.all.gaussian[,2])){
    graph_dat<-tibble(
      "sale"=label, 
      "dates"=dates,
      "start_date"=lubridate::as_date(start_date),
      "end_date"=lubridate::as_date(end_date),
      "raw_data"=c(ci_object$data$Y.pre,ci_object$data$Y.post),
      "estimate"=c(ci_object$est.results$Y.pre.fit,ci_object$est.results$Y.post.fit),
      "lower_95_all_gauss"=NA,
      "upper_95_all_gauss"=NA
    )
  }else{
    graph_dat<-tibble(
      "sale"=label, 
      "dates"=dates,
      "start_date"=lubridate::as_date(start_date),
      "end_date"=lubridate::as_date(end_date),
      "raw_data"=c(ci_object$data$Y.pre,ci_object$data$Y.post),
      "estimate"=c(ci_object$est.results$Y.pre.fit,ci_object$est.results$Y.post.fit),
      "lower_95_all_gauss"=c(rep(NA,length(ci_object$est.results$Y.pre.fit)),ci_object$inference.results$CI.all.gaussian[,1]),
      "upper_95_all_gauss"=c(rep(NA,length(ci_object$est.results$Y.pre.fit)),ci_object$inference.results$CI.all.gaussian[,2])
    )
  }
  
  return(graph_dat)
}





# Create effects table with 95 CI -----------------------------------------



effect_ci_func<-function(label,
                         ci_object){
  
  tribble(~"Sale", ~"Relative Effect", ~"Average Effect", ~"Cumulative Effect",
          label,as.character(round(100*ci_object$summary$RelEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[2],2)),  
          " ", paste0("[",round(100*ci_object$summary$RelEffect.lower[1],2),", ",round(100*ci_object$summary$RelEffect.upper[1],2),"]"), paste0("[",round(ci_object$summary$AbsEffect.lower[1],2),", ",round(ci_object$summary$AbsEffect.upper[1],2),"]"),paste0("[",round(ci_object$summary$AbsEffect.lower[2],2),", ",round(ci_object$summary$AbsEffect.upper[2],2),"]")
  )
}


# Create effects table with s.e. -----------------------------------------



effect_se_func<-function(label,ci_object){
  
  tribble(~"Sale", ~"Relative Effect", ~"Average Effect", ~"Cumulative Effect",
          label,as.character(round(100*ci_object$summary$RelEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[1],2)),as.character(round(ci_object$summary$AbsEffect[2],2)),  
          " ", paste0("(",round(100*ci_object$summary$RelEffect.sd[1],2),")"), paste0("(",round(ci_object$summary$AbsEffect.sd[1],2),")"), paste0("(",round(ci_object$summary$AbsEffect.sd[2],2),")")
  )
}



# Create inclusion probability --------------------------------------------


coef_fun<-function(label,ci_object){
  tibble("sale"=label,
         "names"=names(colMeans(ci_object$model$bsts.model$coefficients!=0)),
         "inclusion_prob"=as.vector(colMeans(ci_object$model$bsts.model$coefficients!=0)),
         "coef_sign"=colMeans(ci_object$model$bsts.model$coefficients)) 
}
