rm(list=ls())

library(FNN)
library(plyr)
set.seed(13)
#if all returns - 999 triptype
#length(unique(FinelineNumber)) per departmentdescription
#look for associations between departmentdescription / weekday and triptypes
#ratio of returns and purchase counts
startTime <- Sys.time()
factorsAsIntegers <- F
scriptName <- "KNN"
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
#trainWidePurchase[is.nan(trainWidePurchase$TotalPurchase),]$TotalPurchase <- -1
trainWidePurchase[trainWidePurchase$TotalPurchase==0,]$TotalPurchase <- -1
calculateProportion <- function(num,den){
  return(num/den)
}
normalizedPurchaseCounts <- apply((trainWidePurchase[,grep("PurchaseCount",colnames(trainWidePurchase))]),2,calculateProportion,trainWidePurchase$TotalPurchase)
hist(normalizedPurchaseCounts)
trainWidePurchase <- cbind(trainWidePurchase[,c(1:3)],normalizedPurchaseCounts)
meanProbOfDeptPerTripType <- ddply(trainWidePurchase[,c(1,4:ncol(trainWidePurchase))],.(TripType),colwise(mean,na.rm=T))
testWidePurchase <- testWide[,c(1,2,grep("PurchaseCount",colnames(testWide)))]
testWidePurchase$TotalPurchase <- rowSums(testWidePurchase[,3:ncol(testWidePurchase)])
#testWidePurchase[is.nan(testWidePurchase$TotalPurchase),]$TotalPurchase <- -1
testWidePurchase[(testWidePurchase$TotalPurchase)==0,]$TotalPurchase <- -1
normalizedPurchaseCountsTest <- apply((testWidePurchase[,grep("PurchaseCount",colnames(testWidePurchase))]),2,calculateProportion,testWidePurchase$TotalPurchase)
testWidePurchase <- cbind(testWidePurchase[,c(1:2)],normalizedPurchaseCountsTest)

ncolsTestBeforeAddition <- ncol(testWidePurchase)
missTestDeptNames <- setdiff(colnames(trainWidePurchase),colnames(testWidePurchase))
for(x in grep("Count",missTestDeptNames)){
  testWidePurchase <- cbind(testWidePurchase,0)
}
colnames(testWidePurchase)[(ncolsTestBeforeAddition+1):ncol(testWidePurchase)] <- missTestDeptNames[grep("PurchaseCount",missTestDeptNames)]
testWidePurchase <- testWidePurchase[,colnames(trainWidePurchase)[-1]]
# nearestNeighbors <- get.knnx(testWidePurchase[,3:71],
#                          query = trainWidePurchase[,4:72],k=10,algorithm = "cover_tree")
# neighborIndices <- nearestNeighbors$nn.index
# neighborDistances <- nearestNeighbors$nn.dist
# neighborClasses <- apply(neighborIndices,c(1,2),function(x){trainWidePurchase[x,]$TripType})
# #modeClassPerRow <- apply(neighborClasses,1,function(x){which.max(table(x))})
# classProbPerRow <- apply(neighborClasses,1,function(x){unlist(lapply(x,function(y){length(x[x==y])/length(x)}))})
# sortedTripTypes <- sort(unique(trainWidePurchase$TripType))
# submission <- data.frame(VisitNumber=testWidePurchase$VisitNumber)
# neighborClassDF <- data.frame(cbind(VisitNumber=testWidePurchase$VisitNumber,neighborClasses))
# # for(c in sortedTripTypes){
# #   neighborClassDF <- cbind(neighborClassDF,c=0)
# # }
# #colnames(neighborClassDF)[2:ncol(neighborClassDF)] <- sortedTripTypes
# #split each row on itself - t<- (split(neighborClasses[1,],neighborClasses[1,]))
# #t1 <-lapply(t,length)
# #x<- data.frame(unlist(t1))
# #x$names <- rownames(x)
# classVotes <- function(x){
#   t<- split(x[-1],x[-1])
#   t1 <- lapply(t,length)
#   df <- data.frame(unlist(t1))
#   df$Class <- rownames(df)
#   df$VisitNumber <- x[1]
#   return(df)
# }
# te <- neighborClassDF[1:2,]
# tedf <- do.call(rbind,apply(te,1,classVotes))
# save(list=ls(),file="/media/3TB/kag/walmart/result/knn.rdata")
#predColNames <- do.call(paste,list("TripType",sortedTripTypes,sep="_"))
nearestNeighbors <- get.knnx(data=testWidePurchase[,3:71],
                         query = meanProbOfDeptPerTripType[,2:70],k=5)
neighborIndices <- nearestNeighbors$nn.index
neighborDistances <- nearestNeighbors$nn.dist
neighborClasses <- apply(neighborIndices,c(1,2),function(x){meanProbOfDeptPerTripType[x,]$TripType})