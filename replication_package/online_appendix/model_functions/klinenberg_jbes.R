# rm(list=ls())
if(!("pacman" %in% installed.packages())){
  install.packages("pacman")
}
library(pacman)
pacman::p_load("CausalImpact",
               "here",
               "matrixStats",
               "shrinkTVP",
               "tidyverse"
)
bltvp_bitto<-function(y,
                    x,
                    t=31, # total number of periods
                    t_0=19, # number of pre-periods
                    niter=20000,
                    nburn=5000,
                    nthin=1){
  y_pre<-y[1:t_0]
  x_pre<-x[1:t_0,]
  dat_pre<-tibble(
    "y"=y_pre,
    x_pre
  )
  s<-shrinkTVP(y~+-1+., # must explicitly include intercept in the dataset
               data = dat_pre,
               niter = niter,
               nburn = nburn,
               learn_a_xi = FALSE, 
               learn_a_tau = FALSE,
               a_xi = 1, 
               a_tau = 1, 
               display_progress = FALSE)


  # In-sample prediction
  in_sample <- predict(s)

  # Out-of-sample prediction
  forecast <- forecast_shrinkTVP(s, x[(t_0 + 1):t, ])

  # Create object that holds both in-sample and out-of-sample
  joint <- cbind(in_sample[1:nrow(in_sample), 1:ncol(in_sample)], forecast$y_pred)

  # Transform into format expected by rest of code
  y <- t(joint)

  return(list("y"=y,
         "model"=s))
}



# Testing -----------------------------------------------------------------

# making data

# t_0 <- 140 # Treatment occurs
# t<-t_0+10 # total length
# len<-1:t # total length for making x1 and x2
# dat<-tibble(
#   "period"=len,
#   "x1"=sin(pi*len/12),
#   "beta1"=8,
#   "sqrt_theta1"=5,
#   "betatilde1"=NA,
#   "x2"=cos(pi*len/6),
#   "beta2"=11,
#   "sqrt_theta2"=0,
#   "betatilde2"=NA,
#   "x3"=cos(pi*len/12),
#   "beta3"=0,
#   "sqrt_theta3"=0,
#   "betatilde3"=NA,
#   "error"=rnorm(t,mean=0,sd=.01^2),
#   "y"=NA,
#   "prediction"=NA,
#   "lower_95"=NA,
#   "upper_95"=NA
# )
# # from brodersen et al.
# dat$betatilde1[1]<-0
# dat$betatilde2[1]<-03
# dat$betatilde3[1]<-0
# 
# 
# # making the simulated y
# for(i in 2:t){
#   dat$betatilde1[i]<-rnorm(1,mean=dat$betatilde1[i-1],1)
#   dat$betatilde2[i]<-rnorm(1,mean=dat$betatilde2[i-1],1)
#   dat$betatilde3[i]<-rnorm(1,mean=dat$betatilde3[i-1],1)
# }
# dat$y<-dat$x1*(dat$beta1+dat$betatilde1*dat$sqrt_theta1)+dat$x2*(dat$beta2+dat$betatilde2*dat$sqrt_theta2)+dat$x3*(dat$beta3+dat$betatilde3*dat$sqrt_theta3)+dat$error
# 
# x<-tibble(dat$x1,
#           dat$x2,
#           dat$x3)
# x<-as.data.frame(x)
# 
# test<-bltvp_bitto(dat$y,
#                 x,
#                 t=t,
#                 t_0=t_0,
#                 niter=20000,
#                 nburn=10000,
#                 nthin=1)
# 
# plot(test$model) # the parameters look good! the first one is time varying, second is constant differing from zero, and third is zero. Nice!
# 
# # CausalImpact time invariant parameters
# ci_dat<-cbind(dat$y,x)
# colnames(ci_dat)<-c("y","x1","x2","x3")
# ci_mod<-CausalImpact::CausalImpact(ci_dat,
#                            pre.period=c(1,t_0),
#                            post.period = c(t_0+1,t)
#                            )
# # CausalImpact time varying parameters
# ci_mod_tvp<-CausalImpact::CausalImpact(ci_dat,
#                                    pre.period=c(1,t_0),
#                                    post.period = c(t_0+1,t),
#                                    model.args = list(dynamic.regression=T)
# )
# 
# # compare the three
# test_dat<-tibble("model"="bitto predict",
#        "x"=1:t,
#        "simulated_y"=dat$y,
#        "prediction_y"=rowMedians(test$y),
#        "lower_95"=rowQuantiles(test$y,probs = .025),
#        "upper_95"=rowQuantiles(test$y,probs = .975)
#        ) %>%
#   rbind(
#     tibble("model"="CasualImpact",
#            "x"=1:t,
#            "simulated_y"=dat$y,
#            "prediction_y"=ci_mod$series$point.pred,
#            "lower_95"=ci_mod$series$point.pred.lower,
#            "upper_95"=ci_mod$series$point.pred.upper
#     )
#   ) %>%
#   rbind(tibble("model"="CasualImpact\nTVP",
#                "x"=1:t,
#                "simulated_y"=dat$y,
#                "prediction_y"=ci_mod_tvp$series$point.pred,
#                "lower_95"=ci_mod_tvp$series$point.pred.lower,
#                "upper_95"=ci_mod_tvp$series$point.pred.upper
#   ))
# 
# test_dat %>%
#   ggplot(aes(x=x,y=simulated_y))+
#   geom_line()+
#   geom_line(aes(x=x,y=prediction_y),lty=2)+
#   geom_ribbon(aes(ymin=lower_95,ymax=upper_95),alpha=.2)+
#   geom_vline(aes(xintercept=t_0),col="red")+ # treatment occurs here. Notice treatment is 0 so it should follow closely!
#   facet_wrap(~model)+
#   theme_minimal() +
#   coord_cartesian(xlim = c(t_0 - 20, t))
# 
# test_dat %>%
#   filter(x>=t_0) %>%
#   mutate(alpha=as.numeric(simulated_y>=lower_95 & simulated_y<=upper_95)) %>%
#   group_by(model) %>%
#   summarise(msfe=mean((simulated_y-prediction_y)^2),
#             alpha=mean(alpha),
#             cred_spread=mean(upper_95-lower_95)
#             )
# 
# # I'd expect bitto predict to perform somewhere between CasualImpact and CausalImpact TVP.
# 
# 


cleaner<-function(word,
                  pre_period_length=14,
                  placebo_pre_length=8,
                  placebo=F,
                  ...){
  # my preset is a little weird 
  pre_period_length<-pre_period_length
  
  if(!placebo){
    vday<-dat %>% 
      filter(str_detect(event,word),
             date>=(start_date-days(pre_period_length)))
  }else{
    vday<-dat %>% 
      filter(str_detect(event,word),
             date>=(start_date-days(pre_period_length)),
             date< start_date
             )
  }
  
  vday1<-vday %>% 
    dplyr::select(-(event:end_date)) 
  
  vday2<-vday1[,-which(is.na(colMeans(vday1)))]
  
  if(!placebo){
    mod<-bltvp_bitto(as.vector(vday2$n),
                     as.data.frame(vday2[,-which(colnames(vday2)=="n")]),
                     t=nrow(vday2),
                     t_0 = pre_period_length)
  }else{
    mod<-bltvp_bitto(as.vector(vday2$n),
                     as.data.frame(vday2[,-which(colnames(vday2)=="n")]),
                     t=nrow(vday2),
                     t_0 = (pre_period_length-placebo_pre_length))
  }
  
  return(list("data"=vday,
              "bltvp"=mod,
              "dates"=vday$date))
}



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
      "raw_data"=ci_object$data$n,
      "estimate"=rowMeans(ci_object$bltvp$y),
      "lower_95"=rowQuantiles(ci_object$bltvp$y,probs=.025),
      "upper_95"=rowQuantiles(ci_object$bltvp$y,probs=.975)
    )
  
  return(graph_dat)
}


coef_func<-function(label,
                    dates,
                    start_date,
                    end_date,
                    ci_object){
  
  coef_dat<-tibble(
    "sale"=label, 
    "variable"=c(colnames(ci_object$bltvp$model$beta_mean),colnames(ci_object$bltvp$model$theta_sr)),
    "estimate"=c(colMedians(ci_object$bltvp$model$beta_mean),colMedians(ci_object$bltvp$model$theta_sr)),
    "lower_95"=c(colQuantiles(ci_object$bltvp$model$beta_mean,probs=.025),colQuantiles(ci_object$bltvp$model$theta_sr,probs=.025)),
    "upper_95"=c(colQuantiles(ci_object$bltvp$model$beta_mean,probs=.975),colQuantiles(ci_object$bltvp$model$theta_sr,probs=.975))
  )
  
  coef_dat<-coef_dat %>% 
    separate_wider_regex(variable,c(parameter=".*a","_",variable=".*")) %>% 
    mutate(variable=str_replace(variable,"^.*?(?=_)","")) %>% 
    mutate(variable=substr(variable,2,nchar(variable)))
  
  return(coef_dat)
}


