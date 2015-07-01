#### AWM DATABASE ####

data <- read.csv("~/Documents/Stage VU/Todo/AWM/AWM18UP14.csv")

#creation of the variable "compensation per employee" & "real consumption" & "real investment"
data$CPE=data$WIN/data$LEN
data$RCO=data$ITR/data$ITD
data$RIN=data$ITR/data$ITD


data<-data[,order(names(data))]
data$date<-data$X
data$X<-NULL

# extracting subset of AWM data
#subset<-subset(data, select=c("YWR", "YER", "RCO","GCR", "RIN","XTR", "MTR","LNN", "URX","POILU", "PCOMU","HICP", "YED","MTD", "CPE","STN", "LTN","EEN"))

# making transformation required
YWR<-4*log(data$YWR)
YER<-4*log(data$YER)
RCO<-4*log(data$RCO)
RIN<-4*log(data$RIN)
GCR<-4*log(data$GCR)
XTR<-4*log(data$XTR)
MTR<-4*log(data$MTR)
LNN<-4*log(data$LNN)
POILU<-4*log(data$POILU)
PCOMU<-4*log(data$PCOMU)
HICP<-4*log(data$HICP)
YED<-4*log(data$YED)
MTD<-4*log(data$MTD)
CPE<-4*log(data$CPE)
EEN<-4*log(data$EEN)


#data<-data.frame(time=1:176,data)

subsets<-ts(subset,frequency=4, start=1970)


#### ECB DATABASE ####

#Monetary Agregate
dataMagreg <- read.csv("~/Documents/Stage VU/Todo/ECB/dataMagreg.csv", sep=";")
dataMagreg$X<-NULL

M1ts <- ts(dataMagreg$M1,start=c(1970,1),frequency=12)
M1q <- aggregate(M1ts, nfrequency=4)/3
M1 <- 4*log(M1q)


M3ts <- ts(dataMagreg$M3,start=c(1970,1),frequency=12)
M3q <- aggregate(M3ts, nfrequency=4)/3
M3 <- 4*log(M3q)



#Loans
loans <- read.csv("~/Documents/Stage VU/Todo/ECB/loans.csv", sep=";")
loans$date<-NULL

LHOts <- ts(loans[2:196,1],start=c(1997,10),frequency=12)
LHOq <- aggregate(LHOts, nfrequency=4)/3
LHO <- 4*log(LHOq)

LFIts <- ts(loans[2:196,2],start=c(1997,10),frequency=12)
LFIq <- aggregate(LFIts, nfrequency=4)/3
LFI <- 4*log(LFIq)


#### IMF ####



#### EUROSTAT ####

# Economic sentiment indicator
ESIdata <- read.csv("~/Documents/Stage VU/Todo/EUROSTAT/ESIdata.csv")
indiceco<-ESIdata$Value
ESIts<-ts(rev(indiceco),start=c(1985,1),frequency=12)

ESIq<-aggregate(ESIts, nfrequency=4)/3

ESI <- ESIq/100


#### FUSION BASE ####



vardata<-as.ts(cbind(YWR, YER, RCO,GCR, RIN,XTR, MTR,LNN, data$URX, POILU, PCOMU, HICP, YED, MTD, CPE, data$STN, data$LTN, EEN, LHO, LFI , M1 , M3 ))
vardataframe<-data.frame(vardata)

for (i in length(vardataframe)){
  vardataframe<-
}


#Indication on variables

# 1:nominal  0:real
nature<-c(rep(1,15),rep(0,4),rep(1,2),0,rep(1,9),rep(0,2),rep(1,3),0,1,0,rep(1,7))

# 1:Euro Area  0:ROW
country<-c(rep(1,42),rep(0,4))

# 0:Level  1:rates  2:indexes
type<-c(1,rep(2,4),rep(0,2),rep(2,7),rep(0,4),1,1,2,0,1,2,rep(0,3),1,1,0,1,0,1,0,0,2,0,2,0,2,0,0,2,2,0,2)

# 1:Seasonally adjusted
transf<-c()

data<-rbind(nature,country,type,data)




