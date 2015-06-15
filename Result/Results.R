require(lassovar)
require(ggplot2)
require(reshape)

load("~/Documents/Stage VU/Todo/vardata.R")

complete<-subset(vardataframe, select=c("YWR", "YER", "RCO", "GCR", "RIN", "XTR", "MTR", "LNN", "URX", "POILU", "PCOMU", "HICP", "YED", "MTD", "CPE", "STN", "LTN", "EEN", "M1" , "M3", "LIB"))


completets<-ts(complete,start=c(1970,1),frequency=4)

id<-seq(1970, 2013.75, 0.25 )
time<-data.frame(id)

varm <- melt(completets,  id = 'time', variable_name = 'series')

ggplot(completets, aes(time,value)) + geom_line() + facet_grid(series ~ .)



ggplot(complete$YWR, aes(time,value)) + geom_line() + facet_grid(series ~ .)


lv<-lassovar(complete, lags=2)

Lasso estimation :

# lasso estimation 
require(lassovar)
res<-lassovar(var[4:179,2:8],lags=10, ic="AIC", adaptive="lasso")
res$coefficients
nnzero(res$coefficients)
summary(res)


