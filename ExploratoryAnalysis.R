rm(list=ls())

library(xgboost)
library(data.table)
library(plyr)
library(vegan)
set.seed(13)
#if all returns - 999 triptype
#length(unique(FinelineNumber)) per departmentdescription
#look for associations between departmentdescription / weekday and triptypes
#ratio of returns and purchase counts
startTime <- Sys.time()
factorsAsIntegers <- F
scriptName <- "ExploratoryAnalysis"
set.seed(13)
os <- Sys.info()[["sysname"]]
nodename <- Sys.info()[["nodename"]]
trainFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/walmart/data/train.csv"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/walmart/data/train.csv"),
                           ("/media/3TB/kag/walmart/data/train.csv")))
train <- read.csv(trainFile)
trainFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/walmart/data/trainWide.rdata"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/walmart/data/trainWide.rdata"),
                           ("/media/3TB/kag/walmart/data/trainWideWithFinelineCounts.rdata")))
load(trainFile)
print("Training data set loaded...")
testFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/walmart/data/test.csv"),
                   ifelse(os=="Darwin",
                          ("/Users/sthiagar/Kaggle/walmart/data/test.csv"),
                          ("/media/3TB/kag/walmart/data/test.csv")))
test <- read.csv(testFile)
testFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/walmart/data/testWide.rdata"),
                   ifelse(os=="Darwin",
                          ("/Users/sthiagar/Kaggle/walmart/data/testWide.rdata"),
                          ("/media/3TB/kag/walmart/data/testWideWithFinelineCounts.rdata")))
load(testFile)
print("Test data set loaded")
outputFolder <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/walmart/result/"),
                       ifelse(os=="Darwin",
                              ("/Users/sthiagar/Kaggle/walmart/result/"),
                              ("/media/3TB/kag/walmart/result/")))
#find the probability distribution per trip type of each departmentdescription
trainWidePurchase <- trainWide[,c(1:3,grep("PurchaseCount",colnames(trainWide)))]
trainWidePurchase$TotalPurchase <- rowSums(trainWidePurchase[,4:ncol(trainWidePurchase)])
calculateProportion <- function(num,den){
  return(num/den)
}
normalizedPurchaseCounts <- apply((trainWidePurchase[,grep("PurchaseCount",colnames(trainWidePurchase))]),2,calculateProportion,trainWidePurchase$TotalPurchase)
hist(normalizedPurchaseCounts)
trainWidePurchase <- cbind(trainWidePurchase[,c(1:3)],normalizedPurchaseCounts)
meanProbOfDeptPerTripType <- ddply(trainWidePurchase[,c(1,4:ncol(trainWidePurchase))],.(TripType),colwise(mean,na.rm=T))
library(reshape2)
meltMeanProbabilities <- melt(meanProbOfDeptPerTripType,id.vars="TripType")
library(ggplot2)
p <- ggplot(meltMeanProbabilities,aes(x=variable,y=value,group=TripType))+geom_line()+facet_wrap(~TripType,ncol=4)
perTripTypeMaxProb <- apply(meanProbOfDeptPerTripType[,2:ncol(meanProbOfDeptPerTripType)],1,
                           function(x){(Max=max(x))})
classFrequencies <- ddply(train,.(TripType),nrow)
save(list=ls(),file="/media/3TB/kag/walmart/result/Explorator.rdata")