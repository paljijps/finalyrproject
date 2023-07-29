rm(list = ls())

data = read.csv("StatesData.csv")
data$Year = as.character(data$Year)
library(ggplot2)
library(ggthemes)

states = ggplot(data, aes(State ,Cyber.Crimes ,fill= Year))+
  geom_bar(stat="identity",position=  'dodge')+geom_text(aes(label= Cyber.Crimes ), vjust = -0.5, position = position_dodge(.9), size=1.5, fontface = "bold")+
  theme_clean() +theme(axis.text.x = element_text(angle = 90, size = 8))+
  labs(title= "Cyber Crimes: State-Wise",
       cex.labs = 0.5)+theme(legend.position="right")

states.rates = ggplot(data, aes(State ,Rates ,fill= Year))+
  geom_bar(stat="identity",position=  'dodge')+geom_text(aes(label= round(Rates,2) ), vjust = -0.5, position = position_dodge(.9), size=1.5, fontface = "bold")+
  theme_clean() +theme(axis.text.x = element_text(angle = 90, size = 8))+
  labs(title= "Cyber Crime Rates: State-Wise",
       cex.labs = 0.5)+theme(legend.position="right")



data1 = read.csv("cybmot.csv")
data1$Year = as.character(data1$Year)

motives = ggplot(data1[55:90,], aes(Motive ,Percentage ,fill= Year))+
  geom_bar(stat="identity", position=  'dodge')+geom_text(aes(label= Percentage ), vjust = -0.5, position = position_dodge(.9), size=1.5, fontface = "bold")+
  theme_clean() +theme(axis.text.x = element_text(angle = 90, size = 8))+
  labs(title= "Cyber Crime Motives",
       cex.labs = 0.5)+theme(legend.position="right")

