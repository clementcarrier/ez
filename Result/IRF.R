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




##############################

# generate RW exogonous variables

library(MASS)

#la matrice d'entrée est une matrice avec en ligne les périodes et en colonnes les variables

rw<-function(t,x){
  y<-matrix(0,dim(x)[2],t)
  y[,1]<-as.matrix(exo[dim(x)[1],])
  if(dim(exo)[2]==1) {
    for(i in 2:t){
      y[,i] <- y[,i-1] + matrix(mvrnorm(n = 1, mu=rep(0,dim(x)[2]) , Sigma=var(x)),dim(x)[2],1)
    }
  }
  else {
    for(i in 2:t){
      y[,i] <- y[,i-1] + matrix(mvrnorm(n = 1, mu=rep(0,dim(x)[2]) , Sigma=diag(diag(var(x)))),dim(x)[2],1)
    }
  }
  return(y[,-1])
}


# Only lag=1
conditional<-function(exogen,data,lag,horizon,preforecast){
  all=data.frame(exogen,data)
  fore<-matrix(0,nrow=dim(data)[2]+dim(exogen)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(all[(dim(all)[1]-preforecast+1):dim(all)[1],])
  fore[1:dim(exogen)[2],-c(1:(preforecast))]<-rw(horizon+1,exogen)
  lv<-lassovar(dat=all,lags=lag, ic="BIC")
  coeff<-as.matrix(t(lv$coefficients[-1,]),dim(all)[2],dim(all)[2])
  intercept<-as.matrix(lv$coefficients[1,],dim(all)[2],1)
  for (i in (preforecast+1):(horizon+preforecast)){
    fore[-dim(exogen)[2],i]<-intercept[-dim(exogen)[2],]+(coeff%*%fore[,i-1])[-dim(exogen)[2]]
  }
  rownames(fore)<-names(all)
  return(t(fore))
}


# With lag allowed
conditional2<-function(exogen,data,lag,horizon,preforecast){
  all=data.frame(exogen,data)
  fore<-matrix(0,nrow=dim(data)[2]+dim(exogen)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(all[(dim(all)[1]-preforecast+1):dim(all)[1],])
  fore[1:dim(exogen)[2],-c(1:(preforecast))]<-rw(horizon+1,exogen)
  lv<-lassovar(dat=all,lags=lag, ic="BIC")
  intercept<-as.matrix(lv$coefficients[1,],dim(all)[2],1)
  
  if(lag==1){
    coeff<-as.matrix(t(lv$coefficients[-1,]),dim(all)[2],dim(all)[2])
    for (i in (preforecast+1):(horizon+preforecast)){
      fore[-dim(exogen)[2],i]<-intercept[-dim(exogen)[2],]+(coeff%*%fore[,i-1])[-dim(exogen)[2]]
    }
  } else {
    if(lag==2){
      coeff1<-as.matrix(t(lv$coefficients[2:(dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
      coeff2<-as.matrix(t(lv$coefficients[(dim(all)[2]+2):(2*dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
      for (i in (preforecast+1):(horizon+preforecast)){
        fore[-dim(exogen)[2],i]<-intercept[-dim(exogen)[2],]+(coeff1%*%fore[,i-1])[-dim(exogen)[2]]+(coeff2%*%fore[,i-2])[-dim(exogen)[2]]
      }
    } else {
      if(lag==3){
        coeff1<-as.matrix(t(lv$coefficients[2:(dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
        coeff2<-as.matrix(t(lv$coefficients[(dim(all)[2]+2):(2*dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
        coeff3<-as.matrix(t(lv$coefficients[(2*dim(all)[2]+2):(3*dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
        for (i in (preforecast+1):(horizon+preforecast)){
          fore[-dim(exogen)[2],i]<-intercept[-dim(exogen)[2],]+(coeff1%*%fore[,i-1])[-dim(exogen)[2]]+(coeff2%*%fore[,i-2])[-dim(exogen)[2]]+(coeff3%*%fore[,i-3])[-dim(exogen)[2]]
        }
      }
      else {
        coeff1<-as.matrix(t(lv$coefficients[2:(dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
        coeff2<-as.matrix(t(lv$coefficients[(dim(all)[2]+2):(2*dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
        coeff3<-as.matrix(t(lv$coefficients[(2*dim(all)[2]+2):(3*dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
        coeff4<-as.matrix(t(lv$coefficients[(3*dim(all)[2]+2):(4*dim(all)[2]+1),]),dim(all)[2],dim(all)[2])
        for (i in (preforecast+1):(horizon+preforecast)){
          fore[-dim(exogen)[2],i]<-intercept[-dim(exogen)[2],]+(coeff%*%fore[,i-1])[-dim(exogen)[2]]+(coeff2%*%fore[,i-2])[-dim(exogen)[2]]+(coeff3%*%fore[,i-3])[-dim(exogen)[2]]+(coeff4%*%fore[,i-4])[-dim(exogen)[2]]
        }
      }
    }
  }
  rownames(fore)<-names(all)
  return(t(fore))
}






###### inflation rate ######
HICP<-vardataframe[113:180,6]

inflation<-NULL
for (i in seq(from=5,to=length(HICP),by=1)){
  inflation[i]=-1+exp(HICP[i])/exp(HICP[i-4])
}
inflation<-inflation[5:68]*100

data$HICP<-NULL
data$inflation<-inflation

exo<-as.matrix(data[,c("POILU")])
colnames(exo)<-c('POILU')
end<-subset(data[,-which(names(data) %in% c("POILU"))])


iter<-50
preforecast<-16
horizon<-28

HICPpred<-matrix(0,horizon+preforecast,iter)
for (i in 1:iter){
  HICPpred[,i]<-matrix(conditional2(exo,end,1,horizon,preforecast)[,"inflation"])
}
HICPpred
HICPpred<-data.frame(HICPpred)
HICPpred$time<-seq(as.Date("2010/01/01"), as.Date("2020/12/31"), by = "quarter")

var <- melt(HICPpred,  id = 'time', variable.name = 'series')


ggplot(var, aes(time,value, col=series)) + geom_point() 
plot(inflation)




#################  Test du modèle sur 2010-2014 ############

data<-subset(vardataframe[117:164,])
datats<-ts(data,frequency=4, start=1998)

HICP<-vardataframe[113:164,6]

inflation2<-NULL
for (i in seq(from=5,to=length(HICP),by=1)){
  inflation2[i]=-1+exp(HICP[i])/exp(HICP[i-4])
}
inflation2<-inflation2[5:52]*100

data$HICP<-NULL
data$inflation2<-inflation2

exo<-as.matrix(data[,c("POILU")])
colnames(exo)<-c('POILU')
end<-subset(data[,-which(names(data) %in% c("POILU"))])

exo<-as.matrix(data)
end=NULL

iter<-20
preforecast<-16
horizon<-16



HICPpred<-matrix(0,horizon+preforecast,iter)
for (i in 1:iter){
  HICPpred[,i]<-matrix(conditional(exo,end,1,horizon,preforecast)[,"inflation2"])
}

HICPpre<-data.frame(HICPpred)
HICPpre$time<-seq(as.Date("2006/01/01"), as.Date("2013/12/31"), by = "quarter")
var2 <- melt(HICPpre,  id = 'time', variable.name = 'series')


inflationtrue<-data.frame(inflation[33:64])
names(inflationtrue)<-'inflationtrue'
inflationtrue$time<-seq(as.Date("2006/01/01"), as.Date("2013/12/31"), by = "quarter")
var3 <- melt(inflationtrue,  id = 'time', variable.name = 'series')

ggplot(var2, aes(time,value, col=series)) + geom_point() + geom_line(linetype="solid")
ggplot(var3, aes(time,value, col=series)) + geom_point() + geom_line(linetype="solid")

# Calcul RMSE
HICPpred

library(base)

estimateur<-as.matrix(.rowMeans(HICPpred, m=32,n=20))[17:32]
true<-as.matrix(inflation[49:64])
ecart<-true-estimateur
sumecart<-t(ecart)%*%ecart
RMSE<-sumecart/length(ecart)



######################################################
exo<-as.matrix(data[,c("POILU")])
colnames(exo)<-'POILU'
end<-subset(data[,-which(names(data) %in% c("POILU"))])


IRF<-conditional(exo,end,1,12,4)
IRF<-data.frame(IRF)

var1<-IRF[,1:5]
var2<-IRF[,6:10]
var3<-IRF[,11:15]
var4<-IRF[,16:20]
var5<-IRF[,21:26]

time<-seq(as.Date("2010/01/01"), as.Date("2016/09/31"), by = "quarter")

var1$time<-time
var2$time<-time
var3$time<-time
var4$time<-time
var5$time<-time

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
#

