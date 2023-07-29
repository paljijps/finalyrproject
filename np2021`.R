rm(list = ls())
data = read.csv("st2020_21.csv")
#data = data[-22,]
head(data)
names(data)
#data = scale(data, scale = TRUE)
xinv21 = data$Cases_for_Investigation/sqrt(var(data$Cases_for_Investigation))
xinv20 = data$Cases_for_Investigation_20/sqrt(var(data$Cases_for_Investigation_20))

wilcox.test(xinv20, xinv21)

xdesp21 = data$Cases_Disposed/sqrt(var(data$Cases_Disposed))
xdesp20 = data$Cases_Disposed.1/sqrt(var(data$Cases_Disposed.1))

wilcox.test(xdesp20, xdesp21)

xtrue21 = data$Cases_True_but_Insufficient_Evidence/sqrt(var(data$Cases_True_but_Insufficient_Evidence))
xtrue20 = data$Cases_True_but_Insufficient_Evidence_20/sqrt(var(data$Cases_True_but_Insufficient_Evidence_20))

wilcox.test(xtrue20, xtrue21)

xpend21 = data$Cases_Pending/sqrt(var(data$Cases_Pending))
xpend20 = data$Cases_Pending.1/sqrt(var(data$Cases_Pending.1))

wilcox.test(xpend20, xpend21)


