#### AWM DATABASE ####

data <- read.csv("~/Documents/Stage VU/Todo/AWM/AWM18UP14.csv")

#creation of the variable "compensation per employee" & "real consumption" & "real investment"
data$CPE=data$WIN/data$LEN



# extracting subset of AWM data
subset<-subset(data, select=c("COMPR", "PCOMU", "POILU", "HEG", "HEXSA", "HICPSA", "HICP","EEN", "EXR", "STN", "LTN" , "URX", "YER", "XTR", "MTR", "CPE", "YWRX"))
subsets<-ts(subset,frequency=4, start=1970)

# making transformation required
COMPR<-log(subsets[,"COMPR"])
POILU<-log(subsets[,"POILU"])
PCOMU<-log(subsets[,"PCOMU"])
HEG<-log(subsets[,"HEG"])
HEXSA<-log(subsets[,"HEXSA"])
HICPSA<-log(subsets[,"HICPSA"])
HICP<-log(subsets[,"HICP"])

EEN<-subsets[,"EEN"]
EXR<-subsets[,"EXR"]
URX<-subsets[,"URX"]
STN<-subsets[,"STN"]/100
LTN<-subsets[,"LTN"]/100

YWRX<-log(subsets[,"YWRX"])
YER<-log(subsets[,"YER"])
CPE<-log(subsets[,"CPE"])
MTR<-log(subsets[,"MTR"])
XTR<-log(subsets[,"XTR"])




#### ECB DATABASE ####

#Monetary Agregate
dataMagreg <- read.csv("~/Documents/Stage VU/Todo/ECB/dataMagreg.csv", sep=";")
dataMagreg$X<-NULL

monthly <- ts(dataMagreg,start=c(1970,1),frequency=12)
quarterly <- aggregate(monthly, nfrequency=4)/3
logquarterly <- log(quarterly)

M1<-logquarterly[,"M1"]
M3<-logquarterly[,"M3"]


#Loans
loans <- read.csv("~/Documents/Stage VU/Todo/ECB/loans.csv", sep=";")
loans$date<-NULL

monthly2 <- ts(loans[2:196,],start=c(1997,10),frequency=12)
quarterly2 <- aggregate(monthly2, nfrequency=4)/3
logquarterly2 <- log(quarterly2)
LFI<-logquarterly2[,"LFI"]
LHO<-logquarterly2[,"LHO"]


#Price Producer Index
PPI <- read.csv("~/Documents/Stage VU/Todo/ECB/PPI.csv")

monthly3 <- ts(rev(PPI[,2]),start=c(1981,1),frequency=12)
quarterly3 <- aggregate(monthly3, nfrequency=4)/3
PPI <- log(quarterly3)




#### IMF ####
LIB3M <- read.csv("~/Documents/Stage VU/Todo/IMF/LIB3M.csv")
LIB<-ts(LIB3M,start=c(1970,1),frequency=4)/100



#### EUROSTAT ####

# Economic sentiment indicator
ESIdata <- read.csv("~/Documents/Stage VU/Todo/EUROSTAT/ESIdata.csv")
indiceco<-ESIdata$Value
ESIts<-ts(rev(indiceco),start=c(1985,1),frequency=12)

ESIq<-aggregate(ESIts, nfrequency=4)/3

ESI <- ESIq/100


#### DATASTREAM ####
dowEuroStoxx <- read.csv("~/Documents/Stage VU/Todo/Datastream/dowEuroStoxx.csv", sep=";", dec=",")
DJES<-ts(dowEuroStoxx[1:108,2],start=c(1987,1),frequency=4)
DJES<-log(DJES)


#### FUSION BASE ####

vardata<-as.ts(cbind(COMPR, PCOMU, POILU, HEG, HEXSA, HICPSA, HICP,EEN, EXR, STN, LTN , URX, YER, XTR, MTR, CPE, YWRX, LHO, LFI , M1 , M3 ,ESI, LIB, PPI, DJES ))
vardataframe<-data.frame(vardata[1:176,])



setwd("~/Documents/Stage VU/Todo/Result")
save(vardataframe,file="vardata3.Rdata")


