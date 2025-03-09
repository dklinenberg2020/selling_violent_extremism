# HEADER --------------------------------------------
#
# Author: Danny Klinenberg
# Email:  danny.klinenberg@gmail.com
# 
# Date Created: 2025-03-03
#
# Script Name: figure_2.R
#  
# Script Description: Figure 2 from online appendix. Plug in the OK membership data as column "n" to recreate the graph.
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
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

acs<-read_rds(here::here("data","census_explan.rds"))

counts_all<-acs %>% 
  filter(year==2018) %>% 
  dplyr::select(county=GEOID, total_pop) %>% 
  mutate(n=rnorm(nrow(.)))


counts_done<-urbnmapr::counties %>% 
  left_join(counts_all,
            by=c("county_fips"="county")) %>% 
  mutate(n_per_capita=n/total_pop)



map<-counts_done %>%
  ggplot(aes(long,lat,group=group,fill=n_per_capita))+
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "lightblue",
                      high="darkblue",
                      na.value = "white")+
  geom_polygon(data = urbnmapr::states, mapping = aes(long, lat, group = group), 
               fill = NA, color = "black", size = 0.1) +
  labs(fill = "Total new members\nper capita",
       x="",
       y="")+
  ggtitle("Recreation of Figure 2 with fake data")+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=12)
  )

map 
