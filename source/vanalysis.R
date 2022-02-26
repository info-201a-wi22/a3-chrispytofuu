#Variable comparison chart

incarceration_trends<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(tidyr)
library(tidyverse)
library(plotly)


x<-incarceration_trends%>%
group_by(metro_area)%>%
  summarize(black_jail_population=mean(black_jail_pop,na.rm=TRUE))%>%
  mutate(true_jail_pop=black_jail_population/0.01)%>%
  drop_na()

variable_comparision_graph<-ggplot(data = x) +
  geom_point(mapping = aes(x = metro_area, y = true_jail_pop))+
  scale_y_continuous(labels = scales::comma)+
  labs(
  x = "Population in Metro Area",
  y = "Total Black Jail Population",
  title = "Correlation between the Metro Area Population and Black Jail Populations in the U.S.",
  subtitle = "Adapted from Incarceration Trends data set by the Vera Project",
  caption = "Variable Comparison graph"
)



