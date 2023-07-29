
rm(list = ls())
source("viz2021.R")

data2 = read.csv("tpmotives.csv")

for (i in 2:6) {
  data2[[i]][1:18] = round( 100*(data2[[i]][1:18] / data2[[i]][19] ), 2 )
}

data2 = data2[1:18,]

motives2020 = bardiag(data2, ax = data2$Motive, ay = data2$X2020, "#0069c0", 
        "Cyber Crime Motives India - 2020", data2$X2020, 
        "Motive", "Percentage of Cases registered" )

motives2021 = bardiag(data2, ax = data2$Motive, ay = data2$X2021, "#00a9b5", 
        "Cyber Crime Motives India - 2021", data2$X2021, 
        "Motive", "Percentage of Cases registered" )
