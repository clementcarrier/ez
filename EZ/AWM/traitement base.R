require(ggplot2)
require(reshape)


data <- read.csv("~/Documents/Stage VU/Todo/AWM/AWM18UP14.csv")


#creation of the variable "compensation per employee" & "real consumption" & "real investment"
data$CPE=data$WIN/data$LEN
data$RCO=data$ITR/data$ITD
data$RIN=data$ITR/data$ITD


data<-data[,order(names(data))]
data$date<-data$X
data$X<-NULL


#data<-data.frame(time=1:176,data)
#datats<-ts(data,frequency=4, start=1970)






#subset VAR variables

var<-subset(data, select=c("time","EEN", "LTN", "POILU", "STN", "URX", "YER", "YWR"))

varm <- melt(var[4:179,],  id = 'time', variable_name = 'series')

ggplot(varm, aes(time,value)) + geom_line() + facet_grid(series ~ .)




# lasso estimation 
require(lassovar)
res<-lassovar(var[4:179,2:8],lags=10, ic="BIC")
res
nnzero(res$coefficients)
summary(res)

