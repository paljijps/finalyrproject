rm(list = ls())
stdata = read.csv("statewise.csv")
library(car)
head(stdata)
rownames(stdata) = stdata$States
head(stdata)
stdata = stdata[-1]
head(stdata)
dim(stdata)

names(stdata)

stcasesinvest = stdata[seq(1,17,4)]
head(stcasesinvest)
stinvest = unlist(stcasesinvest)
year = rep( c("y21","y20", "y19", "y18", "y17" ),each = 28 )

st_casesinvest = data.frame(stinvest, year)
st_casesinvest
leveneTest(stinvest~as.factor(year), st_casesinvest )
kruskal.test(stinvest~as.factor(year), st_casesinvest )

wilcox.test( stdata$Cases_Pending_17, stdata$Cases_Pending_21, paired = TRUE )
