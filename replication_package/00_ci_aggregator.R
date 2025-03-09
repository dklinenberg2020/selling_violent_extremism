# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2023-09-21
#
# Script Name: main_res_aggregator.R
#  
# Script Description: Aggregates the main results togetherfor the table
#
#
# Notes:
#  
#  --------------------------------------------

if(!("pacman" %in% installed.packages())){
  install.packages("pacman")
}
pacman::p_load("tidyverse",
                "here",
                "ggplot2")

mcmc_te<-function(mod){
    te_trace<-t(mod[["ci"]][["model"]][["posterior.samples"]]) %>% 
      as_tibble() %>% 
      mutate(dat=as.vector(mod[["ci"]][["series"]][,"response"])) %>% 
      mutate(across(
        # Select cols
        -dat,
        # lambda style function -- can use others
        ~`dat`-.x
      )
      ) %>% 
      slice(mod[["ci"]][["model"]][["post.period"]][1]:mod[["ci"]][["model"]][["post.period"]][2]) %>% 
      dplyr::select(-dat)
  
  return(te_trace)
}

# cause-related marketing

## Averages
cause_avg<-rbind(mcmc_te(am_bigsky)[,200:900],
      mcmc_te(am_bundy)[,200:900],
      mcmc_te(am_j20)[,200:900]
      ) %>% 
  colMeans() %>% 
  as_tibble() %>% 
  reframe(lower=quantile(value,.025),
          upper=quantile(value,.975),
          median=quantile(value,.5),
          mean=mean(value))

## cumulative

cause_cum<-rbind(mcmc_te(am_bigsky)[,200:900],
                 mcmc_te(am_bundy)[,200:900],
                 mcmc_te(am_j20)[,200:900]
) %>% 
  colSums() %>% 
  as_tibble() %>% 
  reframe(lower=quantile(value,.025),
          upper=quantile(value,.975),
          median=quantile(value,.5),
          mean=mean(value))

## Percentages
cause_perc<-cbind(
  am_bigsky$ci$model$posterior.samples[200:900,
                                       am_bigsky[["ci"]][["model"]][["post.period"]][1]:am_bigsky[["ci"]][["model"]][["post.period"]][2]],
  
  am_bundy$ci$model$posterior.samples[200:900,
                                      am_bundy[["ci"]][["model"]][["post.period"]][1]:am_bundy[["ci"]][["model"]][["post.period"]][2]],
  
  am_j20$ci$model$posterior.samples[200:900,
                                    am_j20[["ci"]][["model"]][["post.period"]][1]:am_j20[["ci"]][["model"]][["post.period"]][2]]
  
) %>% 
  rowSums()


dat<-c(
  as.vector(am_bigsky[["ci"]][["series"]][,"response"])[am_bigsky[["ci"]][["model"]][["post.period"]][1]:am_bigsky[["ci"]][["model"]][["post.period"]][2]],
  
  as.vector(am_bundy[["ci"]][["series"]][,"response"])[am_bundy[["ci"]][["model"]][["post.period"]][1]:am_bundy[["ci"]][["model"]][["post.period"]][2]],
  
  as.vector(am_j20[["ci"]][["series"]][,"response"])[am_j20[["ci"]][["model"]][["post.period"]][1]:am_j20[["ci"]][["model"]][["post.period"]][2]]
  )

p_q<-quantile(sum(dat)/cause_perc-1,probs=c(.025,.5,.975))

cause_perc=c(100*p_q,"mean"=100*mean(sum(dat)/cause_perc-1))

cause_tot<-tribble(~"Sale",~"Relative Effect",~"Average Effect",~"Cumulative Effect",
        "Pooled Cause-Related Marketing",as.character(round(cause_perc[4],2)),as.character(round(cause_avg$mean,2)),as.character(round(cause_cum$mean,2)),
        " ", paste0("[",round(cause_perc[1]),", ",round(cause_perc[2]),"]"),paste0("[",round(cause_avg$lower,2),", ",round(cause_avg$upper,2),"]"),paste0("[",round(cause_cum$lower,2),", ",round(cause_cum$upper,2),"]"))




# Discounts

## Averages
discount_avg<-rbind(mcmc_te(am_vet)[,200:900],
                 mcmc_te(am_const)[,200:900],
                 mcmc_te(am_xmas)[,200:900],
                 mcmc_te(am_mem)[,200:900]
) %>% 
  colMeans() %>% 
  as_tibble() %>% 
  reframe(lower=quantile(value,.025),
          upper=quantile(value,.975),
          median=quantile(value,.5),
          mean=mean(value))

## cumulative

discount_cum<-rbind(mcmc_te(am_vet)[,200:900],
                    mcmc_te(am_const)[,200:900],
                    mcmc_te(am_xmas)[,200:900],
                    mcmc_te(am_mem)[,200:900]
) %>% 
  colSums() %>% 
  as_tibble() %>% 
  reframe(lower=quantile(value,.025),
          upper=quantile(value,.975),
          median=quantile(value,.5),
          mean=mean(value))

## Percentages
discount_perc<-cbind(
  am_vet$ci$model$posterior.samples[200:900,
                                    am_vet[["ci"]][["model"]][["post.period"]][1]:am_vet[["ci"]][["model"]][["post.period"]][2]],
  
  am_const$ci$model$posterior.samples[200:900,
                                      am_const[["ci"]][["model"]][["post.period"]][1]:am_const[["ci"]][["model"]][["post.period"]][2]],
  
  am_xmas$ci$model$posterior.samples[200:900,
                                     am_xmas[["ci"]][["model"]][["post.period"]][1]:am_xmas[["ci"]][["model"]][["post.period"]][2]],
  
  am_mem$ci$model$posterior.samples[200:900,
                                    am_mem[["ci"]][["model"]][["post.period"]][1]:am_mem[["ci"]][["model"]][["post.period"]][2]]
) %>% 
  rowSums()


dat<-c(
  as.vector(am_vet[["ci"]][["series"]][,"response"])[am_vet[["ci"]][["model"]][["post.period"]][1]:am_vet[["ci"]][["model"]][["post.period"]][2]],
  
  as.vector(am_const[["ci"]][["series"]][,"response"])[am_const[["ci"]][["model"]][["post.period"]][1]:am_const[["ci"]][["model"]][["post.period"]][2]],
  
  as.vector(am_xmas[["ci"]][["series"]][,"response"])[am_xmas[["ci"]][["model"]][["post.period"]][1]:am_xmas[["ci"]][["model"]][["post.period"]][2]],
  
  as.vector(am_mem[["ci"]][["series"]][,"response"])[am_mem[["ci"]][["model"]][["post.period"]][1]:am_mem[["ci"]][["model"]][["post.period"]][2]]
  

)

p_q<-quantile(sum(dat)/discount_perc-1,probs=c(.025,.5,.975))

discount_perc=c(100*p_q,"mean"=100*mean(sum(dat)/discount_perc-1))

discount_tot<-tribble(~"Sale",~"Relative Effect",~"Average Effect",~"Cumulative Effect",
                   "Pooled Discount (Excluding Flash)",as.character(round(discount_perc[4],2)),as.character(round(discount_avg$mean,2)),as.character(round(discount_cum$mean,2)),
                   " ", paste0("[",round(discount_perc[1]),", ",round(discount_perc[2]),"]"),paste0("[",round(discount_avg$lower,2),", ",round(discount_avg$upper,2),"]"),paste0("[",round(discount_cum$lower,2),", ",round(discount_cum$upper,2),"]"))

