require(lassovar)
require(ggplot2)
require(reshape2)
require(urca)
require(MSBVAR)


#load("Result/vardata2")
#data<-subset(vardataframe[117:180,])
#datats<-ts(data,frequency=4, start=1998)


#####
#forecast<-function(data,lag,horizon){
#  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+1)
#  fore[,1]<-t(data[dim(data)[1],])
#  lv<-lassovar(dat=data,lags=lag)
#  coeff<-as.matrix(t(lv$coefficients[-1,]),26,26)
#  intercept<-as.matrix(lv$coefficients[1,],26,1)
#  for (i in 2:(horizon+1)){
#    fore[,i]<-intercept+coeff%*%fore[,i-1]
#  }
#  rownames(fore)<-names(data)
#  return(t(fore))
#}

#forecastadaptive<-function(data,lag,horizon){
#  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
#  fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
#  lv<-lassovar(dat=data,lags=lag,adaptive='lasso')
#  coeff<-as.matrix(t(lv$coefficients[-1,]),26,26)
#  intercept<-as.matrix(lv$coefficients[1,],26,1)
#  for (i in (preforecast+1):(horizon+preforecast)){
#    fore[,i]<-intercept+coeff%*%fore[,i-1]
#  }
#  rownames(fore)<-names(data)
#  return(t(fore))
#}


forecast2<-function(data,lag,horizon,preforecast,adap){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
  lv<-lassovar(dat=data,lags=lag,adaptive=adap,trend=TRUE)
  intercept<-as.matrix(lv$coefficients[1,],dim(data)[2],1)
  trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],dim(data)[2],1)
  if(lag==1){
    coeff<-as.matrix(t(lv$coefficients[2:(dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
    for (i in (preforecast+1):(horizon+preforecast)){
      fore[,i]<-intercept+trend*(i+dim(data)[1]-(preforecast))+coeff%*%fore[,i-1]
    }
  } else {
    if(lag==2){
      coeff1<-as.matrix(t(lv$coefficients[2:(dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
      coeff2<-as.matrix(t(lv$coefficients[(dim(data)[2]+2):(2*dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
      for (i in (preforecast+1):(horizon+preforecast)){
        fore[,i]<-intercept+trend*(i+dim(data)[1]-(preforecast))+coeff1%*%fore[,i-1]+coeff2%*%fore[,i-2]
      }
    } else {
      if(lag==3){
        coeff1<-as.matrix(t(lv$coefficients[2:(dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
        coeff2<-as.matrix(t(lv$coefficients[(dim(data)[2]+2):(2*dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
        coeff3<-as.matrix(t(lv$coefficients[(2*dim(data)[2]+2):(3*dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
        for (i in (preforecast+1):(horizon+preforecast)){
          fore[,i]<-intercept+trend*(i+dim(data)[1]-(preforecast))+coeff1%*%fore[,i-1]+coeff2%*%fore[,i-2]+coeff3%*%fore[,i-3]
        }
      }
      else {
        coeff1<-as.matrix(t(lv$coefficients[2:(dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
        coeff2<-as.matrix(t(lv$coefficients[(dim(data)[2]+2):(2*dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
        coeff3<-as.matrix(t(lv$coefficients[(2*dim(data)[2]+2):(3*dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
        coeff4<-as.matrix(t(lv$coefficients[(3*dim(data)[2]+2):(4*dim(data)[2]+1),]),dim(data)[2],dim(data)[2])
        for (i in (preforecast+1):(horizon+preforecast)){
          fore[,i]<-intercept+trend*(i+dim(data)[1]-(preforecast))+coeff1%*%fore[,i-1]+coeff2%*%fore[,i-2]+coeff3%*%fore[,i-3]+coeff4%*%fore[,i-4]
        }
      }
    }
  }
  rownames(fore)<-names(data)
  return(t(fore))
}




load("Result/vardata2")
#data until Q4 2009
data<-subset(vardataframe[117:164,])
#data<-subset(vardataframe[149:164,])
#datats<-ts(data,frequency=4, start=2006)
#datats



HICPtrue<-subset(vardataframe[149:180,])["HICP"]

IRFlassolag1<-forecast2(data,1,16,16,"none")[,"HICP"]
IRFlassolag2<-forecast2(data,2,16,16,"none")[,"HICP"]
IRFlassolag3<-forecast2(data,3,16,16,"none")[,"HICP"]
IRFlassoadaptlassolag1<-forecast2(data,1,16,16,"lasso")[,"HICP"]
IRFlassoadaptlassolag2<-forecast2(data,2,16,16,"lasso")[,"HICP"]
IRFlassoadaptlassolag3<-forecast2(data,3,16,16,"lasso")[,"HICP"]
IRFlassoadaptridgelag1<-forecast2(data,1,16,16,"ridge")[,"HICP"]
IRFlassoadaptridgelag2<-forecast2(data,2,16,16,"ridge")[,"HICP"]
IRFlassoadaptridgelag3<-forecast2(data,3,16,16,"ridge")[,"HICP"]
df<-data.frame(HICPtrue,IRFlassolag1,IRFlassolag2,IRFlassolag3,IRFlassoadaptlassolag1,IRFlassoadaptlassolag2,IRFlassoadaptlassolag3,IRFlassoadaptridgelag1,IRFlassoadaptridgelag2,IRFlassoadaptridgelag3)

time<-seq(as.Date("2006/01/01"), as.Date("2013/10/01"), by = "quarter")
df$time<-time
mvar1 <- melt(df,  id = 'time', variable.name = 'series')
ggplot(mvar1, aes(time, value, col=series)) + geom_line() 




## comparison with forecast.lassovar
help(lassovar)
lv<-lassovar(dat=data,lags=1,ic="BIC",horizon=1,trend=TRUE)
summary(lv)
forecast.lassovar()



help(forecast.lassovar)
help(lassovar)


# RMSE 
df2<-df[17:32,]
RMSE<-NULL
for (i in 2:(length(df)-1)){
  RMSE[i]<-as.matrix(t(df2[,1]-df2[,i])%*%(df2[,1]-df2[,i]))/16
}
RMSEmodel<-RMSE[-1]

names(RMSEmodel)<-names(df[2:10])








help(lassovar)
lv<-lassovar(data,lags=2,trend=TRUE)
co<-lv$coeff
dim(co)

horizon=16
preforecast=16
lag=2
adap="none"

fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])


lv<-lassovar(dat=data,lags=lag,adaptive=adap,trend=TRUE)
intercept<-as.matrix(lv$coefficients[1,],dim(data)[2],1)
coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],dim(data)[2],1)



dim(t(data[,1]))
M<-rep(0,lag*dim(data)[2])


y<-NULL
M<-matrix(0,horizon,lag*dim(data)[2])
for (i in (preforecast+1):(horizon+preforecast)){

  y<-fore[,(i-lag):(i-1)]
  
  if(lag==1){ 
    M[i,1:(lag*dim(data)[2])]<-t(rev(y))
    } else {
            for (j in 2:lag){
              M[i,1:(dim(data)[2])]<-t(rev(y[,1]))
              M[i,((j-1)*dim(data)[2]+1):(j*dim(data)[2])]<-t(rev(y[,j]))
            }           
        }
}


fore[,i]<-intercept+trend*(i+dim(data)[1]-(preforecast))+coeff%*%fore[,i-1]





new<-function(data,lag,horizon,preforecast,adap){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
  lv<-lassovar(dat=data,lags=lag,adaptive=adap,trend=TRUE)
  intercept<-as.matrix(lv$coefficients[1,],dim(data)[2],1)
  coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
  trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],dim(data)[2],1)
  
for (i in (preforecast+1):(horizon+preforecast)){
M<-NULL
for (l in 1:lag){
  M<-c(rev(fore[,(i-l)]),M)
}
  fore[,i]<-t(M%*%coef)+intercept+trend*(i+dim(data)[1]-(preforecast))
}
  
rownames(fore)<-names(data)
return(t(fore))
}
new(data,1,16,16,"none")


help(lassovar)





forecast2<-function(data,lag,horizon,preforecast,adap){
  fore<-matrix(0,nrow=dim(data)[2],ncol=horizon+preforecast)
  fore[,1:(preforecast)]<-t(data[(dim(data)[1]-preforecast+1):dim(data)[1],])
  lv<-lassovar(dat=data,lags=lag,adaptive=adap,trend=TRUE)
  intercept<-as.matrix(lv$coefficients[1,],dim(data)[2],1)
  coef<-as.matrix(lv$coefficients[2:(lag*dim(data)[2]+1),],lag*dim(data)[2],dim(data)[2])
  trend<-as.matrix(lv$coefficients[lag*dim(data)[2]+2,],dim(data)[2],1)
 
  for (i in (preforecast+1):(horizon+preforecast)){
      fore[,i]<-intercept+trend*(i+dim(data)[1]-(preforecast))+coeff%*%fore[,i-1]
    }
  rownames(fore)<-names(data)
  return(t(fore))
}