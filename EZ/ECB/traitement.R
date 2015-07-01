dataMagreg <- read.csv("~/Documents/Stage VU/Todo/ECB/dataMagreg.csv", sep=";")
dataMagreg$X<-NULL

monthly <- ts(dataMagreg,start=c(1970,1),frequency=12)
quarterly <- aggregate(monthly, nfrequency=4)/3

logquarterly <- 4*log(quarterly)

dataMagregate<-data.frame(logquarterly)




