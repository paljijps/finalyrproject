library(readxl)
data=read_excel("computer related offense.xlsx")
data
x=data$year
y=data$CasesReported
plot(data,pch=20,col="blue4")
lines(data)

model1=lm(y~x)
summary(model1)

model2=lm(y~poly(x,2,raw = TRUE))
summary(model2)

model3 = lm(log(y) ~ x)
summary(model3)

x_axis=2014:2021
data.frame(year=data$year,actual = data$CasesReported,linear=predict(model1,data.frame(x=x_axis)),quad = predict(model2,data.frame(x=x_axis)),loge = exp(predict(model3,data.frame(x=x_axis))) )

plot(data,pch=20,col="blue4")
m3=function(x){
  exp(-524.60367+0.26470*x)
}
curve(m3,from = 2014 ,to = 2021,add = TRUE ,lwd = 1.5 , col = "blue")

##calculate percentage increase in x
rm(list = ls())
y1=2014:2021
x1=c(5548,6567,6818,10108,14141,23612,21926,19915)
df=data.frame(y1,x1)
df
pinc=function(x){
  per_inc = c()
  for(i in 2:length(x)){
    per_inc[i] = x[i]/x[i-1] - 1
  }
  return(100*per_inc)
}
cbind(df,per_inc = round(pinc(x1),4))
