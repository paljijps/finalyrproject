
rm(list = ls())
trend_data = read.csv("trenddata.csv")
trend_data = cbind(t = 1:20, trend_data, rate = trend_data$creg/trend_data$midpopln)
trend_data
attach(trend_data)
t_year = function(t){
  2001+t
}
year_t = function(yr){
 yr - 2001 
}

model1 = lm( log(rate) ~ t )
summary(model1)

model2 = lm( log(creg) ~ year )
summary(model2)

library(lmtest)
dwtest(model1)
dwtest(model2)

##prais - winston procedure

library(prais)
praismodel = prais_winsten(model1, data = trend_data[, c(1,5) ] , index = "t" )
praismodel2 = prais_winsten(model2, data = trend_data[, c(1,3) ] , index = "t" )
summary(praismodel)
summary(praismodel2)
bp = summary(praismodel)$coefficients[,1]
bp
rho = summary(praismodel)$rho[length( summary(praismodel)$rho )]
rho

bp2 = summary(praismodel2)$coefficients[,1]
bp2
rho2 = summary(praismodel2)$rho[length( summary(praismodel2)$rho )]
rho2

yt = function(t){
  exp(bp[1] + bp[2]*t)
}

yt2 = function(yr){
  exp(bp2[1] + bp2[2]*yr)
}


plot(trend_data[,c(1,5)], pch = 20, col = "blue4")
curve( yt, from = 1, to = 25, add = TRUE, lwd = 1.5, col = "red")
data.frame(rate, yt(1:20) )

plot(trend_data[,c(1,3)], pch = 20, col = "blue4")
curve( yt2, from = 2001, to = 2025, add = TRUE, lwd = 1.5, col = "red")
data.frame(creg, yt2(2002:2021) )


