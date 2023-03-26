rm(list = ls())
data = read.csv("trenddata.csv")
y = data$creg
x = data$year
rate = round(data$creg/data$midpopln, 4)


cyber_trend = ggplot(data=data, aes(x = year, y = creg, group=1)) +
  geom_line(color="red") + geom_point() + theme_minimal() + geom_text(aes(label = creg ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crime Trend India 2002 - 2021") +
  xlab("Year") + ylab("No. of Cases Registered")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)

cyber_trendr = ggplot(data=data, aes(x = year, y = rate, group=1)) +
  geom_line(color="red") + geom_point() + theme_minimal() + geom_text(aes(label = round(rate,2) ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crime Rates Trend India 2002 - 2021") +
  xlab("Year") + ylab("No. of Cases per 1 lakh Population")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)

source("project_funcs.R")
dfi = cbind(data, per_inc =  round(pinc(data$creg),4) )

cyber_trendp = ggplot(data=dfi, aes(x = year, y = per_inc, group=1)) +
  geom_line(color="red") + geom_point() + theme_minimal() + geom_text(aes(label = round(per_inc,2) ), vjust= -0.5, size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crime Percentage Increase Trend India 2002 - 2021") +
  xlab("Year") + ylab("Percentage Increse in Cases Registered")+scale_x_continuous(labels = as.character(data$year),breaks = data$year)+
  geom_hline(yintercept = 0, col = "blue4")

