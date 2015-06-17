Database for LASSOVAR ESTIMATION
========================================================

We use 5 type of databse : 
- AWM
- IMF (IFS)
- ECB
- EUROSTAT
- DATASTREAM



#### AWM Database :

Open the AWM data : 

```r
data<-read.csv("AWM18UP14.csv")
```


Creation of the variable "compensation per employee" & "real consumption" & "real investment" : 

```r
data$CPE=data$WIN/data$LEN
data$RCO=data$ITR/data$ITD
data$RIN=data$ITR/data$ITD
```

Extracting subset of AWM data and creating a ts object

```r
subset<-subset(data, select=c("YWR", "YER", "RCO","GCR", "RIN","XTR", "MTR","LNN", "URX","POILU", "PCOMU","HICP", "YED","MTD", "CPE","STN", "LTN","EEN"))
subsets<-ts(subset,frequency=4, start=1970)
```


Making transformation required :

```r
YWR<-4*log(subsets[,"YWR"])
YER<-4*log(subsets[,"YER"])
RCO<-4*log(subsets[,"RCO"])
RIN<-4*log(subsets[,"RIN"])
GCR<-4*log(subsets[,"GCR"])
XTR<-4*log(subsets[,"XTR"])
MTR<-4*log(subsets[,"MTR"])
LNN<-4*log(subsets[,"LNN"])
POILU<-4*log(subsets[,"POILU"])
PCOMU<-4*log(subsets[,"PCOMU"])
HICP<-4*log(subsets[,"HICP"])
YED<-4*log(subsets[,"YED"])
MTD<-4*log(subsets[,"MTD"])
CPE<-4*log(subsets[,"CPE"])
EEN<-4*log(subsets[,"EEN"])
URX<-subsets[,"URX"]
STN<-subsets[,"STN"]
LTN<-subsets[,"LTN"]
```



#### ECB Database :


***Monetary Agregate :***

```r
dataMagreg <- read.csv("dataMagreg.csv", sep=";")
dataMagreg$X<-NULL

monthly <- ts(dataMagreg,start=c(1970,1),frequency=12)
quarterly <- aggregate(monthly, nfrequency=4)/3
logquarterly <- 4*log(quarterly)

M1<-logquarterly[,"M1"]
M3<-logquarterly[,"M3"]
```

***Loans :***

```r
loans <- read.csv("loans.csv", sep=";")
loans$date<-NULL

monthly2 <- ts(loans[2:196,],start=c(1997,10),frequency=12)
quarterly2 <- aggregate(monthly2, nfrequency=4)/3
logquarterly2 <- 4*log(quarterly2)
LFI<-logquarterly2[,"LFI"]
LHO<-logquarterly2[,"LHO"]
```


***Price Producer Index :***

```r
PPI <- read.csv("PPI.csv")

monthly3 <- ts(rev(PPI[,2]),start=c(1981,1),frequency=12)
quarterly3 <- aggregate(monthly3, nfrequency=4)/3
PPI <- 4*log(quarterly3)
```




#### IMF Database :

Libor 3 month (United States) :

```r
LIB3M <- read.csv("LIB3M.csv")
LIB<-ts(LIB3M,start=c(1970,1),frequency=4)
```


#### EUROSTAT Database :

Economic sentiment indicator : 

```r
ESIdata <- read.csv("ESIdata.csv")
indiceco<-ESIdata$Value
ESIts<-ts(rev(indiceco),start=c(1985,1),frequency=12)
ESIq<-aggregate(ESIts, nfrequency=4)/3

ESI <- ESIq/100
```

#### DATASTREAM Database :

```r
dowEuroStoxx <- read.csv("dowEuroStoxx.csv", sep=";", dec=",")
DJES<-ts(dowEuroStoxx[1:108,2],start=c(1987,1),frequency=4)
```

### Merge of the different variable into a dataframe


```r
vardata<-as.ts(cbind(YWR, YER, RCO,GCR, RIN,XTR, MTR,LNN, URX, POILU, PCOMU, HICP, YED, MTD, CPE, STN, LTN, EEN, LHO, LFI , M1 , M3 ,ESI, LIB, PPI,DJES ))
vardataframe<-data.frame(vardata[1:176,])
vardataframe<-vardataframe[,order(names(vardataframe))]

save(vardataframe,file="vardata.R")
```


Creation of a set of indicators for:


```r
# 0:nominal  1:real
nature<-c(rep(0,3),1,rep(0,4),1,rep(0,4),1,rep(0,3),rep(1,2),0,rep(1,2),0,rep(1,3))

# 1:Euro Area  0:ROW
country<-c(rep(0,7),1,rep(0,7),1,rep(0,8),rep(1,2))

# 0:NSA  1:SA
SeaAjus<-c(rep(0,2),1,rep(0,7),rep(1,2),rep(0,14))

# 0:macro  1:fin  2:mon  3:credit
def<-c(rep(0,5),rep(3,2),1,0,1,rep(2,2),rep(0,7),1,rep(0,5),1)

vardataframe<-rbind(nature,country,SeaAjus,def,vardataframe)
```


Save new database :

```r
save(vardataframe,file="vardata.R")
```


