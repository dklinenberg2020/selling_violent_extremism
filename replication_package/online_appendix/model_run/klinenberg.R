rm(list=ls())
library(pacman)
pacman::p_load(
  "tidyverse",
  "here",
  "CausalImpact",
  "readxl",
  "lubridate"
)


# Source functions --------------------------------------------------------

source(here::here("online_appendix","model_functions", "klinenberg_jbes.R"))

dat <-read_rds(here::here("data","cleaned_sc_dat.R.rds")) %>% 
  mutate(n=rnorm(nrow(.)))
label_dat<-read_rds(here::here("data","event_data.rds"))


# run CIs in a loop ------------------------------------------------------------------

# puts the CI estimates into global environment with name listed in label_dat

for(i in 1:nrow(label_dat)){
  assign(label_dat$am_element[i],
         cleaner(word=gsub( " .*$", "", label_dat$sale[i]) ,
                 pre_period_length =  14,
                 placebo_pre_length = 7,
                 placebo   = T),
         
         )
}



# Output ------------------------------------------------------------------

# save results here
output<-list()

# save models
for(i in 1:nrow(label_dat)){
  output[["models"]][[label_dat$am_element[i]]]<-get(label_dat$am_element[i])
}

# point data

graph_dat<-point_func(label = label_dat$sale[1],
                      dates = output$models$am_vet$dates,
                      start_date = label_dat$start_date[1],
                      end_date = label_dat$end_date[1],
                      ci_object = output$models$am_vet
) 

for(i in 2:nrow(label_dat)){
  graph_dat<-rbind(graph_dat,
                   point_func(label = label_dat$sale[i],
                              dates = output$models[[label_dat$am_element[i]]]$dates,
                              start_date = label_dat$start_date[i],
                              end_date = label_dat$end_date[i],
                              ci_object = output$models[[label_dat$am_element[i]]]
                   ) 
  )
}


output[["graph_dat"]]<-graph_dat



coef_dat<-coef_func(label = label_dat$sale[1],
                      dates = output$models$am_vet$dates,
                      start_date = label_dat$start_date[1],
                      end_date = label_dat$end_date[1],
                      ci_object = output$models$am_vet
) 


for(i in 2:nrow(label_dat)){
  coef_dat<-rbind(coef_dat,
                  coef_func(label = label_dat$sale[i],
                              dates = output$models[[label_dat$am_element[i]]]$dates,
                              start_date = label_dat$start_date[i],
                              end_date = label_dat$end_date[i],
                              ci_object = output$models[[label_dat$am_element[i]]]
                   ) 
  )
}

output[["coef_dat"]]<-coef_dat


output 

output$graph_dat %>% 
  group_by(sale) %>% 
  slice(8:14) %>% 
  summarise(msfe=mean((raw_data-estimate)^2))
