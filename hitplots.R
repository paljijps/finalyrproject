
rm(list = ls())
library(readxl)
data1 = read_excel("fig_data.xlsx")
data1$year = as.character(data1$year)
library(ggplot2)
library(ggthemes)

ggplot(data1, aes(year ,rate,fill=offense))+
  geom_bar(stat="identity",position=  'dodge')+
  labs(title= "Rates: Section-Wise yearly IT-Act Cases",
       caption = "Figure: 3.1",
       cex.labs = 0.5)+
  scale_y_continuous(labels = scales::percent, name = "Rate")+theme_clean()


ggplot(data1, aes(year ,rate,fill=offense))+
  geom_bar(stat="identity",position=  'stack')+
  labs(title= "Rates: Section-Wise yearly IT-Act Cases",
       caption = "Figure 3.1",
       cex.labs = 0.5)+
  scale_y_continuous(labels = scales::percent, name = "Rate")+theme_clean()

