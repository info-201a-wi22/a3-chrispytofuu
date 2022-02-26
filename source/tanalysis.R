incarceration_trends<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(tidyr)
library(ggplot2)

data_frame<-incarceration_trends%>%
  select(year,black_jail_pop, latinx_jail_pop)%>%
  mutate(true_black_jail_pop=black_jail_pop/0.01)%>%
  mutate(true_latinx_jail_pop=latinx_jail_pop/0.01)%>%
  select(year,true_black_jail_pop,true_latinx_jail_pop)
data_frame_new<-gather(data_frame,key="race",value=populations,-year)

plotted<-ggplot(data=data_frame_new)+
  geom_bar(aes(x=year,y=populations,fill=race),stat="identity",position=position_dodge())+
  labs(
    x = "Year",
    y = "Population",
    title = "Trends of Black and Latinx Imprisonment in the U.S.",
    subtitle = "Years:1983-2018",
    caption = "Adapted from Incarceration Trends data set by the Vera Project",
  )+
  xlim(1980,2018)+
  scale_fill_discrete(labels = c("Black Jail Population","Latinx Jail Population"))
