incarceration_trends<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(tidyverse)
library(ggplot2)

summary_info <- list()
#Year of Latinx Jail Population that had the highest population of incarceration
summary_info$max_year_latinx<-incarceration_trends%>%
  select(year,latinx_jail_pop)%>%
  filter(latinx_jail_pop==max(latinx_jail_pop,na.rm= TRUE))%>%
  pull(year)

#Year of Black Jail Population that had the highest population of incarceration
summary_info$max_year_black<-incarceration_trends%>%
  select(year,black_jail_pop)%>%
  filter(black_jail_pop==max(black_jail_pop,na.rm= TRUE))%>%
  pull(year)

#In the highest inmate population year, what was the population differences between Latinx and Black populations?

max_pop_latinx<-incarceration_trends%>%
  filter(year==1993,na.rm= TRUE)%>%
  select(latinx_jail_pop)%>%
  drop_na()%>%
  summarize(total_latinx=sum(latinx_jail_pop)/0.01)

max_pop_black<-incarceration_trends%>%
  filter(year==1993,na.rm= TRUE)%>%
  select(black_jail_pop)%>%
  drop_na()%>%
  summarize(total_black=sum(black_jail_pop)/0.01)

#Black jail population for the highest Metro population
summary_info$highest_metro<-incarceration_trends%>%
  group_by(metro_area)%>%
  summarize(black_jail_population=mean(black_jail_pop,na.rm=TRUE))%>%
  mutate(true_jail_pop=black_jail_population/0.01)%>%
  drop_na()%>%
  filter(metro_area==max(metro_area))%>%
  pull(true_jail_pop)

#Black jail population for the lowest Metro population
summary_info$lowest_metro<-incarceration_trends%>%
  group_by(metro_area)%>%
  summarize(black_jail_population=mean(black_jail_pop,na.rm=TRUE))%>%
  mutate(true_jail_pop=black_jail_population/0.01)%>%
  drop_na()%>%
  filter(metro_area==min(metro_area))%>%
  pull(true_jail_pop)


#Highest Ratio of black inmates with respect to U.S total population in 2018
summary_info$proportion_per_state_2018_column_only<-incarceration_trends%>%
  filter(year==2018,na.rm=TRUE)%>%
  group_by(state)%>%
  summarize(total_black_jail=sum(black_jail_pop),total_jail=sum(total_jail_pop))%>%
  mutate(proportion_black_inmate= total_black_jail/total_jail)%>%
  select(state,proportion_black_inmate)%>%
  drop_na()%>%
  filter(proportion_black_inmate==max(proportion_black_inmate))%>%
  pull(proportion_black_inmate)

