# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2025-03-03
#
# Script Name: figure_1.R
#  
# Script Description: Figure 1 from online appendix. Plug in the OK membership data as column "n" to recreate the graph.
#
#
# Notes:
#  
#  --------------------------------------------

rm(list=ls())
if(!("pacman" %in% installed.packages())){
  install.packages("pacman")
}
pacman::p_load("tidyverse",
                "here",
                "ggplot2")

pacman::p_load(
  "here",
  "kableExtra",
  "readr",
  "tidyverse",
  "scales",
  "janitor",
  "scales",
  "lubridate",
  "ggthemes",
  "geomtextpath",
  "tidyverse",
  "here",
  "maps",
  "viridis",
  "modelsummary",
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



tibble("date"=seq(from=as_date("2009-01-01"),to=as_date("2021-10-01"),by="day")) %>% 
  mutate(n=rnorm(nrow(.))) %>% 
  mutate(date=floor_date(date,"month")) %>% 
  group_by(date) %>% 
  reframe(n=sum(n)) %>% 
  mutate(shape=case_when(
    date %in% c(
      as_date("2014-11-01"),
      as_date("2017-07-01"),
      as_date("2017-08-01"),
      as_date("2017-09-01"),
      as_date("2017-12-01"),
      as_date("2018-01-01"),
      as_date("2018-02-01"),
      as_date("2018-05-01")
    )~"Discount",
    date %in% c(
      as_date("2014-04-01"),
      as_date("2015-08-01"),
      as_date("2015-09-01"),
      as_date("2017-01-01")
    )~"Callouts",
    date %in% c(
      as_date("2013-05-01"),
      as_date("2013-06-01"),
      as_date("2013-07-01")
    )~"Sports Sponsorship"
  )
  ) %>% 
  ggplot(aes(x=date,y=n))+
  geom_line()+
  geom_point(aes(shape=shape, col=shape),size=3)+
  geom_segment(
    xend = as.Date("2013-01-10"), x = as.Date("2014-06-01"), yend = 1313, y = 1400,
    arrow = arrow(length = unit(0.03, "npc"))
  )+
  geom_label(label="Obama's Inaugaration", x = as.Date("2014-06-01"), y=1400, label.size = 1, size=6 )+
  geom_segment(
    xend = as.Date("2013-10-10"), x = as.Date("2015-06-01"), yend = 1310, y = 1250,
    arrow = arrow(length = unit(0.03, "npc"))
  )+
  geom_label(label="Introduce Civilian\nPreparedness Teams", x = as.Date("2016-03-01"), y=1250,size=6 )+
  labs(x="Month",
       y="Monthly Inflow of New Members",
       shape="",
       col="")+
  ylim(-100,1500)+
  ggtitle("Figure 1 Code. Outcome uses random number generator. Plug in OK data to recreate main findings")+
  scale_shape_discrete(breaks = c('Discount', 'Callouts','Sports Sponsorship'))+
  scale_color_discrete(breaks = c('Discount', 'Callouts','Sports Sponsorship'))+
  theme_minimal()+
  theme(legend.position = "bottom",
        text = element_text(size = 18))
