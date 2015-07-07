require(lassovar)
require(ggplot2)
require(reshape2)
require(urca)
require(MSBVAR)


#load("Result/vardata3.Rdata")

#data<-subset(vardataframe[113:176,])
#datats<-ts(data,frequency=4, start=1998)

load("Result/vardata2")
data<-subset(vardataframe[117:180,])
datats<-ts(data,frequency=4, start=1998)

#difdata <- tail(data,-1) - head(data,-1)


# exogenous shock
forecast<-function(data,lag,horizon,choc){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon)
  lv<-lassovar(dat=data,lags=lag, ic="BIC")
  coeff<-lv$coefficients[-1,]
  for (i in 1:horizon){
    fore[,i]<-coeff^i%*%choc
  }
  return(t(fore))
}




forecast<-function(data,lag,horizon){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+1)
  fore[,1]<-t(data[dim(data)[1],])
  lv<-lassovar(dat=data,lags=lag)
  coeff<-as.matrix(t(lv$coefficients[-1,]),26,26)
  intercept<-as.matrix(lv$coefficients[1,],26,1)
  for (i in 2:(horizon+1)){
    fore[,i]<-intercept+coeff%*%fore[,i-1]
  }
  rownames(fore)<-names(data)
  return(t(fore))
}

IRF<-forecast(data,1,12)
colnames(IRF)<-names(data)
IRF<-data.frame(IRF)

var1<-IRF[,1:5]
var2<-IRF[,6:10]
var3<-IRF[,11:15]
var4<-IRF[,16:20]
var5<-IRF[,21:26]

var1$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var2$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var3$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var4$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var5$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")

mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
mvar2 <- melt(var2,  id = 'time', variable.name = 'series')
mvar3 <- melt(var3,  id = 'time', variable.name = 'series')
mvar4 <- melt(var4,  id = 'time', variable.name = 'series')
mvar5 <- melt(var5,  id = 'time', variable.name = 'series')

ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar2, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar3, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar4, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar5, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")



##############################

# generate RW exogonous variables

#variance du bruit ?

rw<-function(t,x){
  y<-matrix(0,dim(x)[2],t)
  y[,1]<-t(x)
  for(i in 2:t){
    y[,i] <- y[,i-1] + matrix(rnorm(dim(x)[2],0,0.4942424),dim(x)[2],1)
  }
  return(y)
}





conditional<-function(exogen,data,lag,horizon){
  all=data.frame(exogen,data)
  fore<-matrix(0,nrow=dim(data)[2]+dim(exogen)[2],ncol=horizon+1)
  fore[,1]<-t(all[dim(all)[1],])
  fore[1:dim(exogen)[2],-1]<-rw(horizon,as.matrix(exogen[dim(exogen)[1],]))
  lv<-lassovar(dat=all,lags=lag, ic="BIC")
  coeff<-as.matrix(t(lv$coefficients[-1,]),26,26)
  intercept<-as.matrix(lv$coefficients[1,],26,1)
  for (i in 2:(horizon+1)){
    fore[-dim(exogen)[2],i]<-intercept[-dim(exogen)[2],]+(coeff%*%fore[,i-1])[-dim(exogen)[2]]
  }
  rownames(fore)<-names(all)
  return(t(fore))
}

exo<-data.frame(data$POILU)
colnames(exo)<-'POILU'
end<-subset(data[,-which(names(data) %in% c("POILU"))])


IRF<-conditional(exo,end,1,12)
IRF<-data.frame(IRF)

var1<-IRF[,1:5]
var2<-IRF[,6:10]
var3<-IRF[,11:15]
var4<-IRF[,16:20]
var5<-IRF[,21:26]

var1$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var2$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var3$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var4$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")
var5$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")

mvar1 <- melt(var1,  id = 'time', variable.name = 'series')
mvar2 <- melt(var2,  id = 'time', variable.name = 'series')
mvar3 <- melt(var3,  id = 'time', variable.name = 'series')
mvar4 <- melt(var4,  id = 'time', variable.name = 'series')
mvar5 <- melt(var5,  id = 'time', variable.name = 'series')

ggplot(mvar1, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar2, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar3, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar4, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")
ggplot(mvar5, aes(time,value)) + geom_line() + facet_grid(series ~ . ,scales="free")




### inflation forecast with replication of rw

iter<-100
HICPpred<-matrix(0,13,iter)
for (i in 1:iter){
  HICPpred[,i]<-matrix(conditional(exo,end,1,12)[,"HICP"])
}
HICPpred
HICPpred<-data.frame(HICPpred)
HICPpred$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")

var <- melt(HICPpred,  id = 'time', variable.name = 'series')


ggplot(var, aes(time,value, col=series)) + geom_point() + stat_smooth() 







# inflation rate
HICP<-vardataframe[113:180,6]

inflation<-NULL
for (i in seq(from=5,to=length(HICP),by=1)){
  inflation[i]=-1+exp(HICP[i])/exp(HICP[i-4])
}
inflation
inflation<-inflation[5:68]*100

data$HICP<-NULL
data$inflation<-inflation

exo<-data.frame(data$POILU)
colnames(exo)<-'POILU'
end<-subset(data[,-which(names(data) %in% c("POILU"))])


iter<-100
HICPpred<-matrix(0,13,iter)
for (i in 1:iter){
  HICPpred[,i]<-matrix(conditional(exo,end,1,12)[,"inflation"])
}
HICPpred
HICPpred<-data.frame(HICPpred)
HICPpred$time<-seq(as.Date("2014/10/01"), as.Date("2017/12/31"), by = "quarter")

var <- melt(HICPpred,  id = 'time', variable.name = 'series')


ggplot(var, aes(time,value, col=series)) + geom_point() + stat_smooth() 



