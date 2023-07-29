#![]("myplot1.png")
rm(list = ls())
library(readxl)
library(ggplot2)
df = read_excel("2021start.xlsx")
names(df)
attach(df)
# df$Cases_Invest=as.numeric(df$Cases_Invest)
cases_invest_per = (df$Cases_Invest[1:9]/df$Cases_Invest[10])*100
df = cbind(df ,cases_invest_percent = c(round(cases_invest_per,2),100))
# df$True_cases=as.numeric(df$True_cases)
cases_true_per = (df$True_cases[1:9]/df$True_cases[10])*100
df = cbind(df , cases_true_per = c(round(cases_true_per,2),100))


# df$Cases_Disposed = as.numeric(df$Cases_Disposed)
cases_disposed_per = (df$Cases_Disposed[1:9]/df$Cases_Disposed[10])*100
df = cbind(df , Cases_Disposed_per = c(round(cases_disposed_per,2),100))
df
# df$Cases_Pending = as.numeric(df$Cases_Pending)
cases_pending_per = (df$Cases_Pending[1:9]/df$Cases_Pending[10])*100
df = cbind(df , Cases_pending_per = c(round(cases_pending_per,2),100))
df

bardiag = function(df, ax, ay, cl, tit, val_lab, x_lab, y_lab ){
  bard = ggplot(data=df, aes(x = ax, y = ay ) ) +
    geom_bar(stat="identity", fill = cl,color = "black",width = 0.40)+ theme_classic()+geom_text(aes(label= val_lab ), vjust= -0.4, size=2.5)+
    theme(axis.text.x = element_text(angle = 90, size = 7))+ ggtitle(tit) +
    xlab(x_lab) + ylab(y_lab)
  return(bard)
}
#2021
plot2021 = bardiag(df[1:9,], ax = df$Crime_Head[1:9], ay = df$cases_invest_percent[1:9], "blue4", 
                    "Police cases investigated - 2021", df$cases_invest_percent[1:9], 
                    "States", "No. of Cases investigated" )
plot2021





 
#, Cases_disposed_per = c(df20$Cases_Disposed[1:9]/df20$Cases_Disposed[10],100)
rm(list = ls())
df21 = read_excel("comp.2021.xlsx")
attach(df21)


df21 = cbind(df21 ,cases_invest_per = c(round(df21$Cases_Invest[1:3]/df21$Cases_Invest[4],5)*100,100))

