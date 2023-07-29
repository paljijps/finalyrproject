rm(list = ls())
library(readxl)

child21 = read_excel("2021 childern.xlsx")
child20 = read_excel("2020 childern.xlsx")
child19 = read_excel("2019 childern.xlsx")
child18 = read_excel("2018 childern.xlsx")
child17 = read_excel("2017 childern.xlsx")

head(child21)
library(randtests)
runs.test(child21$totalcrimes[1:28])

cor.test(child21$fakeprofile[1:28], child21$cyberstalking[1:28], method = "kendall")
cor.test(child21$fakeprofile[1:28], child21$cyberstalking[1:28], method = "spearman")
cor.test(child21$cyberblackmailing[1:28], child21$cyberpornography[1:28], method = "kendall")
cor.test(child21$cyberblackmailing[1:28], child21$cyberpornography[1:28], method = "spearman")



tail(child20)
ks.test(child20$totalcrimes[1:28], child21$totalcrimes[1:28])
ks.test(child17$totalcrimes[1:28], child21$totalcrimes[1:28])
ks.test(child18$totalcrimes[1:28], child21$totalcrimes[1:28])
ks.test(child19$totalcrimes[1:28], child21$totalcrimes[1:28])

yearwise = unlist(c( child17[39,8], child18[39,8], child19[39,8], child20[39,8], child21[39,8] ) )
yearwise

childtrend = data.frame( year = 2017:2021, cases = yearwise )
childtrend

library(ggplot2)

plt = ggplot(data = childtrend, aes(x = year, y = cases))+geom_bar(stat = "identity", fill = "blue4")+theme_minimal()+
  xlab("Year")+ylab("No. of Cyber Crimes against Children")
plt

