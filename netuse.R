rm(list = ls())
netusage = read.csv("internet_usage.csv")[1,]
nt = t(netusage)
d2 = read.csv("trenddata.csv")
data.frame(year = 2002:2020, users = as.numeric(nt[17:35,])*d2$midpopln[1:19]/100, users_per = as.numeric(nt[17:35,]) )

