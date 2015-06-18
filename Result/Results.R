require(lassovar)
require(ggplot2)
require(reshape2)
require(urca)
require(MSBVAR)

#load("~/Documents/Stage VU/Todo/Result/vardata.R")
load("vardata.R")
#complete<-subset(vardataframe, select=c("YWR", "YER"))
#complete<-subset(vardataframe[116:180,], select=c("YWR", "YER", "RCO", "GCR", "RIN", "XTR", "MTR", "URX", "POILU", "PCOMU", "HICP", "YED", "CPE", "STN", "LTN", "EEN", "M1" , "M3", "LIB", "LHO"))
#complete<-subset(vardataframe[116:180,], select=c("YWR", "YER", "RCO", "GCR", "RIN"))

# I keep variables from Q4 1997
subset<-subset(vardataframe[116:180,])

# Plot

var1<-subset[,1:5]
var2<-subset[,6:10]
var3<-subset[,11:15]
var4<-subset[,16:20]
var5<-subset[,21:26]

var1$time<-seq(as.Date("1997/10/01"), as.Date("2013/12/31"), by = "quarter")
var2$time<-seq(as.Date("1997/10/01"), as.Date("2013/12/31"), by = "quarter")
var3$time<-seq(as.Date("1997/10/01"), as.Date("2013/12/31"), by = "quarter")
var4$time<-seq(as.Date("1997/10/01"), as.Date("2013/12/31"), by = "quarter")
var5$time<-seq(as.Date("1997/10/01"), as.Date("2013/12/31"), by = "quarter")

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



#Test de Racine unité ERS H0 : pas stationnaire
# "YES" : there is a unit root

erstest<-function(data){
  result<-NULL
  for (i in seq(1,dim(data)[2],1)){
    ers<-ur.ers(data[,i], type = "P-test", model="trend", lag.max = 10)
    stat<-ers@teststat
    value5pc<-ers@cval[2]
    result[i]<-stat>value5pc
  }
  return(rbind(names(data),result))
}


dfglstest<-function(data){
  result<-NULL
  for (i in seq(1,dim(data)[2],1)){
    ers<-ur.ers(data[,i], type = "DF-GLS", model="trend", lag.max = 2)
    stat<-ers@teststat
    value5pc<-ers@cval[2]
    result[i]<-stat>value5pc
  }
  return(rbind(names(data),result))
}


test<-dfglstest(subset)
dim(test)



# First difference of all series
FDall<-function(data){
dif<-matrix(0,nrow=dim(data)[1]-1,ncol=dim(data)[2])
for (i in seq(1,dim(data)[2],1)){
  dif[,i]<-diff(data[,i],lag=1)
  colnames(dif)<-names(data)
}
return(dif)
}


# First difference of non stationary series only
FDnonstationary<-function(data){
  dif<-matrix(0,nrow=dim(data)[1]-1,ncol=dim(data)[2])
  for (i in seq(1,dim(data)[2],1)){
    if (dfglstest(data)[2,i]==TRUE) {
      dif[,i]<-diff(data[,i],lag=1)
    }
    else {
      dif[,i]<-data[2:dim(data)[1],i]
    }
    colnames(dif)<-names(data)
  }
  return(dif)
}

partialDF<-FDnonstationary(subset)
dsubset<-FDall(subset)
dsubset<-data.frame(dsubset)


#Plot of FD

var1<-dsubset[,1:5]
var2<-dsubset[,6:10]
var3<-dsubset[,11:15]
var4<-dsubset[,16:20]
var5<-dsubset[,21:26]

var1$time<-seq(as.Date("1998/01/01"), as.Date("2013/12/31"), by = "quarter")
var2$time<-seq(as.Date("1998/01/01"), as.Date("2013/12/31"), by = "quarter")
var3$time<-seq(as.Date("1998/01/01"), as.Date("2013/12/31"), by = "quarter")
var4$time<-seq(as.Date("1998/01/01"), as.Date("2013/12/31"), by = "quarter")
var5$time<-seq(as.Date("1998/01/01"), as.Date("2013/12/31"), by = "quarter")

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




# Test racine unité sur dif mais sans trend 
dfglstest2<-function(data){
  result<-NULL
  for (i in seq(1,dim(data)[2],1)){
    ers<-ur.ers(data[,i], type = "DF-GLS", model="constant", lag.max = 2)
    stat<-ers@teststat
    value5pc<-ers@cval[2]
    result[i]<-stat>value5pc
  }
  return(rbind(names(data),result))
}


erstest2<-function(data){
  result<-NULL
  for (i in seq(1,dim(data)[2],1)){
    ers<-ur.ers(data[,i], type = "P-test", model="constant", lag.max = 2)
    stat<-ers@teststat
    value5pc<-ers@cval[2]
    result[i]<-stat>value5pc
  }
  return(rbind(names(data),result))
}


erstest2(dsubset)



#Choix du nombre de lag

lagchoice<-function(data,lagmax){
  AIC<-NULL
  HQ<-NULL
  SC<-NULL
  for (i in 1:lagmax){
    lv<-lassovar(data, lags=i)
    cons<-lv$coefficients[1,]
    coef<-lv$coefficients[-1,]
    res<-lv$y-matrix(rep(1,dim(lv$y)[1]))%*%t(cons)-lv$x%*%coef
    Sigma<-((dim(lv$y)[1])^-1)*(t(as.matrix(res))%*%as.matrix(res))
    det<-det(Sigma)
    AIC[i]<-log(det)+2*i*dim(data)[2]^2/dim(subset)[1]
    HQ[i]<-log(det)+2*log(log(dim(data)[1]))*i*dim(data)[2]^2/dim(data)[1]
    SC[i]<-log(det)+log(dim(data)[1])*i*dim(data)[2]^2/dim(data)[1]
  }
  return(rbind(AIC,HQ,SC))
}

lagchoice(subset,lagmax=10)

lagchoice(dsubset,lagmax=10)

subset<-subset(vardataframe[116:180,])
subsettrend<-subset(vardataframe[116:180,])
trend<-seq(1,dim(subset)[1],1)
trend<-data.frame(seq(1,dim(subset)[1],1))
lv2<-lassovar(subset,lags=1, ic="AIC", exo=trend)





# Estimation of the VAR 
help(lassovar)
lv<-lassovar(dsubset,lags=1)
coef<-lv$coefficients[-1,]
coef
dim(subset)

forecast<-function(data,lag,horizon,choc){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon)
  lv<-lassovar(dat=data,lags=lag, ic="BIC")
  coef<-lv$coefficients[-1,]
  for (i in 1:horizon){
    fore[,i]<-coef^i%*%choc
  }
return(fore)
}

oilprice<-matrix(c(rep(0,16),1,rep(0,9)))
IRF<-forecast(dsubset,1,16,oilprice)

plot(IRF[17,])



