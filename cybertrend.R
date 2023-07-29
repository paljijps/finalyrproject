rm(list = ls())
library(ggplot2)
library(ggthemes)
data = read.csv("trenddata.csv")
y = data$creg
x = data$year
rate = round(data$creg/data$midpopln, 4)


cyber_trend = ggplot(data=data, aes(x = year, y = creg, group=1)) +
  geom_line(color="red") + geom_point() + theme_clean() + geom_text(aes(label = creg ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crime Trend India 2002 - 2021") +
  xlab("Year") + ylab("No. of Cases Registered")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)

cyber_trendr = ggplot(data=data, aes(x = year, y = rate, group=1)) +
  geom_line(color="red") + geom_point() + theme_clean() + geom_text(aes(label = round(rate,2) ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crime Rates Trend India 2002 - 2021") +
  xlab("Year") + ylab("No. of Cases per 1 lakh Population")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)

source("project_funcs.R")
dfi = cbind(data, per_inc =  round(pinc(data$creg),4) )

cyber_trendp = ggplot(data=dfi, aes(x = year, y = per_inc, group=1)) +
  geom_line(color="red") + geom_point() + theme_clean() + geom_text(aes(label = round(per_inc,2) ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crime Percentage Increase Trend India 2002 - 2021") +
  xlab("Year") + ylab("Percentage Increse in Cases Registered")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)+
  geom_hline(yintercept = 0, col = "blue4")


netusage = read.csv("internet_usage.csv")[1,]
nt = t(netusage)
dfnet = data.frame(year = 2002:2020, users = as.numeric(nt[17:35,])*data$midpopln[1:19]/100, users_per = as.numeric(nt[17:35,]) )

net_usem = ggplot(data=dfnet, aes(x = year, y = users, group=1)) +
  geom_line(color="red") + geom_point() + theme_clean() + geom_text(aes(label = round(users,2) ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Internet users India 2002 - 2020") +
  xlab("Year") + ylab("Number of internet users in lakhs")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)+
  geom_hline(yintercept = 0, col = "blue4")

net_usep = ggplot(data=dfnet, aes(x = year, y = users_per, group=1)) +
  geom_line(color="red") + geom_point() + theme_clean() + geom_text(aes(label = round(users_per,2) ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Internet users Percentage India 2002 - 2020") +
  xlab("Year") + ylab("Number of internet Users Percentage")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)+
  geom_hline(yintercept = 0, col = "blue4")


t = 1:20
model4 = lm(log(rate)~ t )
b = summary(model4)$coefficients[,1]
m4 = function(t){
  exp(b[1] + b[2] * t)
}
dt = data.frame(t, rate)
# plot(dt, pch = 20, col = "blue4")
# curve( m4, from = 1, to = 25, add = TRUE, lwd = 1.5, col = "red")

fitplot = ggplot(data, aes(x= as.character(year), y=rate, group=1)) +
  geom_function(fun = m4, colour = "red")+
  geom_point()+theme_clean()+geom_text(aes(label = round(rate,2) ), vjust= -0.5, size=2.5)+
  labs(title= "Cyber Crimes Rate Prediction India",
       caption = "R.Square = 91.50 %",
       cex.labs = 0.5)+theme(axis.text.x = element_text(angle = 90, size = 8))+xlab("Year") + ylab("Cyber Crime Rates")

rate.prediction = data.frame(year = 2022:2030, Predicted_Rate = round(m4(21:29),3 ) )


t = 1:20
model5 = lm(log(data$creg)~ t )
b1 = summary(model5)$coefficients[,1]
m5 = function(t){
  exp(b1[1] + b1[2] * t)
}
dt2 = data.frame(t, data$creg)
# plot(dt, pch = 20, col = "blue4")
# curve( m4, from = 1, to = 25, add = TRUE, lwd = 1.5, col = "red")

fitplot2 = ggplot(data, aes(x= as.character(year), y=creg, group=1)) +
 geom_function(fun = m5, colour = "red")+
  geom_point()+theme_clean()+geom_text(aes(label = round(creg,2) ), vjust= -0.5, size=2.5)+
  labs(title= "Cyber Crimes Prediction India",
                                  caption = "R.Square = 92.29 %",
                                  cex.labs = 0.5)+theme(axis.text.x = element_text(angle = 90, size = 8))+xlab("Year") + ylab("Cyber Crimes")

creg.prediction = data.frame(year = 2022:2030, Predicted_Cyber_Crimes = round(m5(21:29),3 ) )


ress = as.data.frame( cbind(crimes  = resid(model5), rate = resid(model4)) )


crimeres = ggplot(data = ress, aes(sample = crimes) )+stat_qq()+stat_qq_line(color = "#0b1d78")+ggtitle("QQ Plot of Residuals") +theme_clean()
rateres = ggplot(data = ress, aes(sample = rate) )+stat_qq()+stat_qq_line(color = "red3")+ggtitle("QQ Plot of Residuals") +theme_clean()



