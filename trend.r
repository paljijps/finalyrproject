library(ggplot2)
library(ggthemes)
bardiag = function(df, ax, ay, cl, tit, val_lab, x_lab, y_lab ){
  bard = ggplot(data=df, aes(x = ax, y = ay ) ) +
    geom_bar(stat="identity", fill = cl,width =0.4)+geom_text(aes(label= val_lab ), vjust= -0.3, size=2.5)+ theme_clean()+
    theme(axis.text.x = element_text(angle = 90, size = 7), axis.title = element_text(size = 10))+ ggtitle(tit) +
    xlab(x_lab) + ylab(y_lab)
  return(bard)
}
