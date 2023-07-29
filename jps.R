rm(list = ls())
library(readxl)
library(ggplot2)

df = read_excel("TABLE 9A.1.xlsx")
df
df$cases_reg2019 = as.numeric(df$cases_reg2019)
df = cbind(df, rate2019 = df$cases_reg2019/ df$proj_popln2019  ,rate2020 = df$cases_reg2020/ df$proj_popln2020 , rate2021 = df$cases_reg2021/ df$proj_popln2021 )

bardiag = function(df, ax, ay, cl, tit, val_lab, x_lab, y_lab ){
  bard = ggplot(data=df, aes(x = ax, y = ay ) ) +
    geom_bar(stat="identity", fill = cl)+theme_minimal()+ geom_text(aes(label= val_lab ), vjust= -0.3, size=2.5)+
    theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle(tit) +
    xlab(x_lab) + ylab(y_lab)
  return(bard)
}


#2021
barst2021 = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$cases_reg2021[1:28], "steelblue4", 
                    "Cyber Crimes State Wise - 2021", df$cases_reg2021[1:28], 
                    "States", "No. of Cases registered" )

barst2021r = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$rate2021[1:28], "blue4", 
                     "Cyber Crimes Rates State Wise - 2021", round(df$rate2021[1:28],2) , 
                     "States", "No. of Cases per 1 lakh Population" )
#2020
barst2020 = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$cases_reg2020[1:28], "steelblue4", 
                    "Cyber Crimes State Wise - 2020", df$cases_reg2020[1:28], 
                    "States", "No. of Cases registered" )

barst2020r = bardiag(df[1:28,], ax = df$state_ut[1:28], ay = df$rate2020[1:28], "blue4", 
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
  geom_bar(stat='identity', position='dodge') + theme_minimal() + geom_text(aes(label = cases ),vjust = -0.5, position = position_dodge(.9), size=2.5)+
  theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle("Cyber Crimes U.T. Wise") +
  xlab("U.T.") + ylab("No. of Cases Registered")

barut2020_21r  = ggplot(dfr[61:76,], aes(x= state_ut, y = rates, fill = Year)) +
  geom_bar(stat='identity', position='dodge') + theme_minimal() + geom_text(aes(label = round(rates,2) ),vjust = -0.5, position = position_dodge(.9), size=2.5)+
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

trend_plot = function(state, tit, cs){
  trend_state = ggplot(data = state, aes(x = year, y = cs, group=1)) +
    geom_line(color="red") + geom_point() + theme_minimal() + geom_text(aes(label = round(cs,2) ), vjust= -0.5, size=2.5)+
    theme(axis.text.x = element_text(angle = 90, size = 8))+ ggtitle(tit) +
    xlab("Year") + ylab("No. of Cases registered")+scale_x_discrete(labels = as.character(state$year), breaks = state$year)
  
  return(trend_state)
}
library(ggplot2)
bardiag = function(df, ax, ay, cl, tit, val_lab, x_lab, y_lab ){
  bard = ggplot(data=df, aes(x = ax, y = ay ) ) +
    geom_bar(stat="identity", fill = cl,width =0.4)+ theme_classic()+
    theme(axis.text.x = element_text(angle = 90, size = 7))+ ggtitle(tit) +
    xlab(x_lab) + ylab(y_lab)
  return(bard)
}

Plot = bardiag(data1, ax = data1$States, ay = data1$Cases_Investigated_21, "pink", 
               "Police cases investigated  2021", data1$Cases_Investigated_21, 
               "STATES", " CASES " )
Plot
data1 = read.csv("stplt2021.csv")
DF <- rbind(data.frame(fill="blue", obs=data1$Cases_Investigated_21),
            data.frame(fill="green", obs=data1$Cases_Investigated_20),
            data.frame(fill="red", obs=data1$Cases_Investigated_19),
            data.frame(fill="orange", obs=data1$Cases_Investigated_18))
ggplot(DF, aes(x=obs, fill=fill)) +
  geom_histogram(binwidth=1, colour="black", position="dodge") +
  scale_fill_identity()
data = read.csv("stplot.csv") 
attach(data)
plot2021 = bardiag(data, ax = data$States, ay = data$investigated_21, "#00a9b5", 
                    "Crimes Investigated State Wise - 2021", data$investigated_21, 
                    "States", "No. of Cases Investigated " )
plot2021
df20 = read.csv("year2020.csv")
df20 = cbind(df20 , cases_disposed_per = c(round(df20$Cases_Disposed_20[1:9]/df21$Cases_Disposed_20[10],5)*100,100 
                                           ,round(df20$Cases_Disposed_20[11:29]/df21$Cases_Disposed_20[30],5)*100 ,100
                                           ,round(df20$Cases_Disposed_20[31:35]/df21$Cases_Disposed_20[36],5)*100,100 ))

Plot_case_disposed_IT_20 = bardiag(df20[1:9,], ax = df20$Crime_Head[1:9], ay = df20$Cases_Disposed_20[1:9], "#00a9b5", 
                                   "Police cases Disposed  - 2020", df20$cases_disposed_per[1:9], 
                                   "crime head", "Disposed Percentage under I.T Act" )
Plot_case_disposed_IT_20
