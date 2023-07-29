bardiag = function(df, ax, ay, cl, tit, val_lab, x_lab, y_lab ){
  bard = ggplot(data=df, aes(x = ax, y = ay ) ) +
    geom_bar(stat="identity", fill = cl,width = 0.40)+ theme_classic()+geom_text(aes(label= val_lab ), vjust= -0.4, size=2.5)+
    theme(axis.text.x = element_text(angle = 90, size = 7))+ ggtitle(tit) +
    xlab(x_lab) + ylab(y_lab)
  return(bard)
}
library("ggplot2")
df21 = read.csv("year2021.csv")
head(df21)
names(df21)
# attach(df21)
plt = ggplot(data = df21 , aes(x =  Crime_Head, group = 1)) + 
  geom_line(aes(y = Cases_Disposed_21),color = "black")+
  geom_line(aes(y = Cases_Invest_21),color = "blue")
plt
