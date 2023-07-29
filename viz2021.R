rm(list = ls())
library(readxl)
library(ggplot2)
library(ggthemes)

df = read_excel("TABLE 9A.1.xlsx")
df$cases_reg2019 = as.numeric(df$cases_reg2019)
df = cbind(df, rate2019 = df$cases_reg2019/ df$proj_popln2019  ,rate2020 = df$cases_reg2020/ df$proj_popln2020 , rate2021 = df$cases_reg2021/ df$proj_popln2021 )

bardiag = function(df, ax, ay, cl, tit, val_lab, x_lab, y_lab ){
  bard = ggplot(data=df, aes(x = ax, y = ay ) ) +
    geom_bar(stat="identity", fill = cl)+theme_clean()+ geom_text(aes(label= val_lab ), vjust= -0.3, size=2.5)+
    theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle(tit) +
    xlab(x_lab) + ylab(y_lab)
  return(bard)
}

#2021
barst2021 = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$cases_reg2021[1:28], "#00c698", 
        "Cyber Crimes State Wise - 2021", df$cases_reg2021[1:28], 
        "States", "No. of Cases registered" )

barst2021r = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$rate2021[1:28], "#0069c0", 
                     "Cyber Crimes Rates State Wise - 2021", round(df$rate2021[1:28],2) , 
                     "States", "No. of Cases per 1 lakh Population" )
#2020
barst2020 = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$cases_reg2020[1:28], "#0b1d78", 
        "Cyber Crimes State Wise - 2020", df$cases_reg2020[1:28], 
        "States", "No. of Cases registered" )

barst2020r = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$rate2020[1:28], "#00a9b5", 
                     "Cyber Crimes Rates State Wise - 2020", round(df$rate2020[1:28],2) , 
                     "States", "No. of Cases per 1 lakh Population" )

# barut2021 = bardiag(df[31:38,], ax = df$state_ut[31:38], ay = df$cases_reg2021[31:38], "steelblue4", 
#                     "Cyber Crimes U.T. Wise - 2021", df$cases_reg2021[31:38] , 
#                     "U.T.", "No. of cases registered" )
# 
# barut2021r = bardiag(df[31:38,], ax = df$state_ut[31:38], ay = df$rate2021[31:38], "blue4", 
#                      "Cyber Crimes Rates U.T. Wise - 2021", round(df$rate2021[31:38],2) , 
#                      "U.T.", "No. of Cases per 1 lakh Population" )




moddf = function(df, fact , val){
  df2 <- tidyr::pivot_longer(df, cols = fact, names_to = 'Year',  values_to = val )
  for (i in 1:92) {
    if (df2$Year[i] == fact[1]) {
      df2$Year[i] = "2020"
    } else{
      df2$Year[i] = "2021"
    }
  }
  return(df2)
}

dfc = moddf(df, c("cases_reg2020", "cases_reg2021"), "cases" )
dfr = moddf(df, c("rate2020", "rate2021"), "rates" )


barut2020_21 =  ggplot(dfc[61:76,], aes(x= state_ut, y = cases, fill = Year)) +
  geom_bar(stat='identity', position='dodge') + theme_clean() + geom_text(aes(label = cases ),vjust = -0.5, position = position_dodge(.9), size=1.5, fontface = "bold")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crimes U.T. Wise") +
  xlab("U.T.") + ylab("No. of Cases Registered")

barut2020_21r  = ggplot(dfr[61:76,], aes(x= state_ut, y = rates, fill = Year)) +
  geom_bar(stat='identity', position='dodge') + theme_clean() + geom_text(aes(label = round(rates,2) ),vjust = -0.5, position = position_dodge(.9), size=1.5, fontface = "bold")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crime Rates U.T. Wise") +
  xlab("U.T.") + ylab("No. of Cases per 1 lakh population")


state5yr = function(state){
  dfmajor = read.csv("majorst.csv")
  
  yr = c("2017", "2018","2019","2020","2021")
  st = dfmajor[which(dfmajor$state_ut == state),]
  
  st = data.frame(year = yr, cases = as.numeric(st[,2:6]), rates = as.numeric(st[,2:6])/as.numeric(st[,7:11])  )
  st
}
telangana = state5yr("Telangana")
karnataka = state5yr("Karnataka")
maharashtra = state5yr("Maharashtra")
assam = state5yr("Assam")
uttar_pradesh = state5yr("Uttar Pradesh")

statelist = list(telangana, karnataka, maharashtra, assam, uttar_pradesh)
nam = c("Telangana", "Karnataka", "Maharashtra","Assam", "Uttar Pradesh")
names(statelist) = nam

trend_plot = function(state, tit, cs, ylabel){
  trend_state = ggplot(data = state, aes(x = year, y = cs, group=1)) +
    geom_line(color="red") + geom_point() + theme_clean() + geom_text(aes(label = round(cs,2) ), vjust= -0.5, size=2.5)+
    theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle(tit) +
    xlab("Year") + ylab(ylabel)+scale_x_discrete(labels = as.character(state$year), breaks = state$year)
  
  return(trend_state)
}



uts = read_excel("TABLE 9A.1.xlsx", sheet = "Sheet2")
uttrend = ggplot(data = uts, aes(x = ut, group = 1))+geom_line(aes(y = y2017), color = "black")+
  geom_line(aes(y = y2018), color = "red")+
  geom_line(aes(y = y2019), color = "blue")+
  geom_line(aes(y = y2020), color = "orange2")+
  geom_line(aes(y = y2021), color = "purple")+theme_clean()+theme(axis.text.x = element_text(angle = 90, size = 8))+
  ylab("Cyber Crimes Registered")+xlab("Union Territories")+scale_color_manual(values=c("black","red","blue","orange2", "purple"))

cols = c("black", "red", "blue", "orange2", "purple")
uttrend2 = ggplot(data = uts, aes(x = ut, group = 1))+geom_line(aes(y = y2017), color = "black")+
  geom_line(aes(y = y2018), color = "red")+
  geom_line(aes(y = y2019), color = "blue")+
  geom_line(aes(y = y2020), color = "orange2")+
  geom_line(aes(y = y2021), color = "purple")+theme_clean()+theme(axis.text.x = element_text(angle = 90, size = 8))+
  ylab("Cyber Crimes Registered")+xlab("Union Territories")


chandigarh = read_excel("chddata.xlsx") 

trendchd = ggplot(chandigarh, aes(x=Year, y=Cyber_Crimes, group=1)) +
  geom_line(color = "#0b1d78")+
  geom_point()+theme_clean()+labs(title= "Cyber Crimes: Chandigarh",
                                  caption = "2016 - 2021",
                                  cex.labs = 0.5)+geom_text(aes(label= Cyber_Crimes ), vjust= -0.3, size=2.5)

trendchdrate = ggplot(chandigarh, aes(x=Year, y=Rate, group=1)) +
  geom_line(color = "#0069c0")+
  geom_point()+theme_clean()+labs(title= "Cyber Crime Rates: Chandigarh",
                                  caption = "2016 - 2021",
                                  cex.labs = 0.5)+ylab("No. of Cyber Crimes per 1 lakh population")+geom_text(aes(label= Rate ), vjust= -0.3, size=2.5)




  