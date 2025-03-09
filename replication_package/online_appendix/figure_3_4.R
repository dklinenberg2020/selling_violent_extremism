# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2025-03-03
#
# Script Name: figure_3.R
#  
# Script Description: Figure 3 from online appendix. Plug in the OK membership data as column "n" to recreate the graph.
#
#
# Notes:
#  
#  --------------------------------------------

rm(list=ls())
library(pacman)
pacman::p_load(
  "tidyverse",
  "here",
  "maps",
  "viridis",
  "ggthemes",
  "mapview",
  "zipcodeR",
  "eiCompare",
  "parallel",
  "foreach",
  "doParallel",
  "tidygeocoder",
  "tidycensus",
  "radiant.data",
  "devtools",
  "ggpattern"
)

incidents_pb <- read_csv("data/Incidents-PB Only.csv")

events<-incidents_pb %>% 
  janitor::clean_names() %>% 
  mutate(date=lubridate::mdy(date)) %>% 
  filter(!str_detect(extra_parliamentary_groups_and_networks,"Oath"),
         str_detect(pb_role,"Organized"),
         date<=lubridate::date("2018-07-01")) %>% 
  mutate(id=paste(str_extract(name,".*,"),"\n",format(date,"%Y-%m")))

# weeks
window<- 4

hold<-events %>% 
  dplyr::select(id,name,event=date) %>% 
  slice(rep(1:n(), each=2*window*7+1)) %>% 
  group_by(id) %>% 
  mutate(day=1:length(event),
         day=day-(window*7+1),
         date=event+days(day) )

hold1<-hold %>% 
  ungroup() %>% 
  mutate(n=rnorm(nrow(.)))

order<-hold1 %>% 
  distinct(id,event) %>% 
  arrange(event)


# Figure 3 ----------------------------------------------------------------



hold1 %>% 
  filter(abs(day) <= 14) %>% 
  mutate(id=factor(id,levels=order$id),
         order=ifelse(day<0,"pre","post"),
         order=factor(order,levels=c("pre","post"))) %>% 
  ggplot(aes(x=date,y=n,group=id,shape=order,col=order))+
  geom_line()+
  geom_point()+
  facet_wrap(~id,
             scales = "free_x")+
  geom_vline(aes(xintercept=event),col="black",lty=2)+
  labs(x="Date",
       y="OK new members",
       col=" ",
       shape=" ",
       col=" ")+
  ggtitle("Figure 3 recreated with fake data")+
  theme_minimal()+
  scale_color_colorblind()+
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 22))



# Figure 4 ----------------------------------------------------------------

regs_1<-hold1 %>% 
  filter(abs(day)<=7) %>% 
  mutate(after_event=I(as.numeric(day>=0))) %>% 
  fixest::feols(n~after_event|id,
                fsplit= ~id,
                vcov="hetero"
  ) %>% 
  fixest::coeftable() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(coefficient!="(Intercept)") %>% 
  mutate(sample=ifelse(sample=="Full sample","Pooled sample",sample),
         window="One Week")

regs_2<-hold1 %>% 
  filter(abs(day)<=14) %>% 
  mutate(after_event=I(as.numeric(day>=0))) %>% 
  fixest::feols(n~after_event|id,
                fsplit= ~id,
                vcov="hetero"
  ) %>% 
  fixest::coeftable() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(coefficient!="(Intercept)") %>% 
  mutate(sample=ifelse(sample=="Full sample","Pooled sample",sample),
         window="Two Weeks")

regs_3<-hold1 %>% 
  filter(abs(day)<=21) %>% 
  mutate(after_event=I(as.numeric(day>=0))) %>% 
  fixest::feols(n~after_event|id,
                fsplit= ~id,
                vcov="hetero"
  ) %>% 
  fixest::coeftable() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(coefficient!="(Intercept)") %>% 
  mutate(sample=ifelse(sample=="Full sample","Pooled sample",sample),
         window="Three Weeks")

regs_4<-hold1 %>% 
  filter(abs(day)<=28) %>% 
  mutate(after_event=I(as.numeric(day>=0))) %>% 
  fixest::feols(n~after_event|id,
                fsplit= ~id,
                vcov="hetero"
  ) %>% 
  fixest::coeftable() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(coefficient!="(Intercept)") %>% 
  mutate(sample=ifelse(sample=="Full sample","Pooled sample",sample),
         window="Four Weeks")

regs_1 %>% 
  rbind(regs_2) %>% 
  rbind(regs_3) %>% 
  rbind(regs_4) %>% 
  mutate(sample=factor(sample,levels=c("Pooled sample",order$id)),
         window=factor(window,
                       levels=c("One Week",
                                "Two Weeks",
                                "Three Weeks",
                                "Four Weeks")
         )
  )%>% 
  ggplot(aes(x=sample,y=estimate,col=window,lty=window))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=estimate-1.96*std_error,ymax=estimate+1.96*std_error),
                position=position_dodge(width=0.5),width=0)+
  geom_hline(aes(yintercept=0),col="black")+
  theme_minimal()+
  labs(x=" ",
       y="After Event\nCoefficient",
       col="Window",
       lty="Window")+
  ggtitle("Figure 4 with fake data")+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 22),
        legend.position = "bottom"
  )
