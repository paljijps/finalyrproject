rm(list = ls())
library(ggplot2)
df20=read.csv("year2020.csv")


year2020


df20 = cbind(df20 , cases_pending_per = c(round(df20$Cases_Pending_20[1:9]/df20$Cases_Pending_20[10],5)*100,100 
                                          ,round(df20$Cases_Pending_20[11:29]/df20$Cases_Pending_20[30],5)*100 ,100
                                          ,round(df20$Cases_Pending_20[31:35]/df20$Cases_Pending_20[36],5)*100,100 ))

81.926