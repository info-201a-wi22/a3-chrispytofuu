#goal: make a map of U.S. show the heat map of black inmates/black total prison population in each state. 
incarceration_trends<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(tidyr)
library(usmap)
library(plotly)

proportion_per_state_2018<-incarceration_trends%>%
  filter(year==2018,na.rm=TRUE)%>%
  group_by(state)%>%
  summarize(total_black_jail=sum(black_jail_pop),total_jail=sum(total_jail_pop),na.rm=TRUE)%>%
  mutate(proportion_black_inmate= total_black_jail/total_jail)%>%
  drop_na()%>%
  select(state,proportion_black_inmate)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

new_graph<-plot_usmap(data = proportion_per_state_2018,
                      values = "proportion_black_inmate") +
  scale_fill_continuous(low = "white", high = "red", name = "Proportion")+
  labs(title = "2018: Proportion of Black Inmates to Total U.S. Population")+
  blank_theme+
  theme(legend.position = "right")