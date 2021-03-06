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
scriptName <- "PurchaseReturnPurchaseRatioForEachDept"
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
#train <- train[train$ScanCount>0,]
#trainCounts <- ddply(train,.(TripType,VisitNumber,Weekday,DepartmentDescription),function(x){data.frame(PurchaseCount=sum(x[x$ScanCount>0,]$ScanCount),
#                                                                                                        PurchaseUniqueFL=length(unique(x[x$ScanCount>0,]$FinelineNumber)),
#                                                                                                        ReturnCount=sum(x[x$ScanCount<0,]$ScanCount),
#                                                                                                        ReturnUniqueFL=length(unique(x[x$ScanCount<0,]$FinelineNumber)))})
# trainWide <- reshape(trainCounts, idvar=c("TripType","VisitNumber","Weekday"), timevar="DepartmentDescription", direction="wide")
# trainWide[is.na(trainWide)] <-0
TotalPurchaseProducts <- rowSums((trainWide[,grep("PurchaseCount",colnames(trainWide))]))
TotalPurchaseProducts[TotalPurchaseProducts==0] <- -1
TotalReturnProducts <- rowSums((trainWide[,grep("ReturnCount",colnames(trainWide))]))
TotalReturnProducts[TotalReturnProducts==0] <- -2
TotalPurchaseUniqueProducts <- rowSums((trainWide[,grep("PurchaseUnique",colnames(trainWide))]))
TotalPurchaseUniqueProducts[TotalPurchaseUniqueProducts==0] <- -1
TotalReturnUniqueProducts <- rowSums((trainWide[,grep("ReturnUnique",colnames(trainWide))]))
TotalReturnUniqueProducts[TotalReturnUniqueProducts==0] <- -2
calculateProportion <- function(num,den){
  return(num/den)
}
normalizedPurchaseCounts <- apply((trainWide[,grep("PurchaseCount",colnames(trainWide))]),2,calculateProportion,TotalPurchaseProducts)
normalizedReturnCounts <- apply((trainWide[,grep("ReturnCount",colnames(trainWide))]),2,calculateProportion,TotalReturnProducts)
returnToPurchaseRatio <- abs(TotalReturnProducts)/TotalPurchaseProducts
normalizedUniquePurchaseCounts <- apply((trainWide[,grep("PurchaseUnique",colnames(trainWide))]),2,calculateProportion,TotalPurchaseUniqueProducts)
normalizedUniqueReturnCounts <- apply((trainWide[,grep("ReturnUnique",colnames(trainWide))]),2,calculateProportion,TotalReturnUniqueProducts)
returnToPurchaseUniqueRatio <- abs(TotalReturnUniqueProducts)/TotalPurchaseUniqueProducts

normalizedReturnToPurchaseRatio <- normalizedReturnCounts/ normalizedPurchaseCounts
normalizedUniqueReturnToPurchaseRatio <- normalizedUniqueReturnCounts/ normalizedUniquePurchaseCounts
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}
is.infinite.data.frame <- function(x){
  do.call(cbind, lapply(x, is.infinite))
}
normalizedReturnToPurchaseRatio[is.nan.data.frame(normalizedReturnToPurchaseRatio)] <- -9999999
normalizedReturnToPurchaseRatio[is.infinite.data.frame(normalizedReturnToPurchaseRatio)] <- 9999999
normalizedUniqueReturnToPurchaseRatio[is.nan.data.frame(normalizedUniqueReturnToPurchaseRatio)] <- -9999999
normalizedUniqueReturnToPurchaseRatio[is.infinite.data.frame(normalizedUniqueReturnToPurchaseRatio)] <- 9999999
uniqueDeptForCols <- colnames(trainWide)[grep("PurchaseCount",colnames(trainWide))]
uniqueDeptForCols<- unlist(lapply(uniqueDeptForCols, function(x){unlist(strsplit(x,"PurchaseCount."))[2]}))
colnames(normalizedReturnToPurchaseRatio) <- paste("Ratio",uniqueDeptForCols,sep="_")
colnames(normalizedUniqueReturnToPurchaseRatio) <- paste("Ratio",uniqueDeptForCols,sep="_U")
trainWideNormalized <- cbind(trainWide[,1:3],normalizedPurchaseCounts,normalizedUniquePurchaseCounts,normalizedReturnCounts,normalizedUniqueReturnCounts,
                             ReturnPurchaseRatio=returnToPurchaseRatio,ReturnPurchaseUniqueRatio=returnToPurchaseUniqueRatio,
                             normalizedReturnToPurchaseRatio,normalizedUniqueReturnToPurchaseRatio)

# trainWideNormalized[is.nan(trainWideNormalized)] <-0
# trainWideNormalized[is.infinite(trainWideNormalized)] <-0
trainWideNormalized[is.na(trainWideNormalized)] <-0
deptNames <- colnames(trainWideNormalized)

trainWideNormalized$Weekday <- as.character(trainWideNormalized$Weekday )
trainWideNormalized$Weekday<- gsub("Sunday",1,trainWideNormalized$Weekday)
trainWideNormalized$Weekday<- gsub("Monday",2,trainWideNormalized$Weekday)
trainWideNormalized$Weekday<- gsub("Tuesday",3,trainWideNormalized$Weekday)
trainWideNormalized$Weekday<- gsub("Wednesday",4,trainWideNormalized$Weekday)
trainWideNormalized$Weekday<- gsub("Thursday",5,trainWideNormalized$Weekday)
trainWideNormalized$Weekday<- gsub("Friday",6,trainWideNormalized$Weekday)
trainWideNormalized$Weekday<- gsub("Saturday",7,trainWideNormalized$Weekday)
trainWideNormalized$Weekday <- as.integer(trainWideNormalized$Weekday )
sortedTripTypes <- sort(unique(trainWideNormalized$TripType))
for(i in 1:length(sortedTripTypes)){
  #trainWideNormalized$TripType<- gsub(sortedTripTypes[i],i-1,trainWideNormalized$TripType)
  trainWideNormalized[trainWideNormalized$TripType==sortedTripTypes[i],]$TripType <- (i-1)
}
tripTypeCounts <- ddply(trainWideNormalized,.(TripType),nrow)
sampleVNs <- list()
trainSamples <- data.frame()
evalSamples <- data.frame()
ctr <-1
# samplePerTripType <- function(x){
#   tt <- trainWideNormalized[trainWideNormalized$TripType==x,]
#   sampleVN <- sample(unique(tt$VisitNumber),0.10*length(unique(tt$VisitNumber)))
#   #print(paste(x,sampleVN))
#   sampleVNs[[ctr]] <<- sampleVN
#   ctr <- ctr+1
#   evalSamples <<- rbind(evalSamples,tt[tt$VisitNumber %in% sampleVN,])
#   ts <- tt[!tt$VisitNumber %in% sampleVN,]
# }
tripTypes <- unique(trainWideNormalized$TripType)
for(tt in tripTypes){
  ss <- trainWideNormalized[trainWideNormalized$TripType==tt,]
  sampleVN <- sample(unique(ss$VisitNumber),0.10*length(unique(ss$VisitNumber)))
  #print(paste(x,sampleVN))
  sampleVNs[[ctr]]<- sampleVN
  ctr <- ctr+1
  evalSamples <- rbind(evalSamples,ss[ss$VisitNumber %in% sampleVN,])
  trainSamples <-rbind(trainSamples, ss[!ss$VisitNumber %in% sampleVN,])
}
#trianPerTripType <- lapply(tripTypes, function(x){trainWideNormalized[trainWideNormalized$TripType==x,]})
#names(trianPerTripType) <- tripTypes
#trainSamples <- do.call(rbind,lapply(tripTypes,function(x){samplePerTripType(x)}))
set.seed(13)

dtrain <- xgb.DMatrix(data.matrix(trainSamples[,3:(ncol(trainSamples))]), label=trainSamples$TripType)




dval <- xgb.DMatrix(data.matrix(evalSamples[,3:(ncol(evalSamples))]), label=evalSamples$TripType)

watchlist <- list(eval = dval, train = dtrain)
set.seed(13)
param <- list(  num_class =38,
                objective           = "multi:softprob", 
                # booster = "gblinear",
                eta                 = 0.02,
                max_depth           = 6, # 6 has been optimal  
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                eval_metric         = "mlogloss"
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 5000, # changed from 300
                    verbose             = 1, 
                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = F)
#prepare test data
# testCounts <- ddply(test,.(VisitNumber,Weekday,DepartmentDescription),function(x){data.frame(Count=sum(x$ScanCount))})
# testWide <- reshape(testCounts, idvar=c("VisitNumber","Weekday"), timevar="DepartmentDescription", direction="wide")
# testWide[is.na(testWide)] <-0
# testCounts <- ddply(test,.(VisitNumber,Weekday,DepartmentDescription),function(x){data.frame(PurchaseCount=sum(x[x$ScanCount>0,]$ScanCount),
#                                                                                                        PurchaseUniqueFL=length(unique(x[x$ScanCount>0,]$FinelineNumber)),
#                                                                                                        ReturnCount=sum(x[x$ScanCount<0,]$ScanCount),
#                                                                                                        ReturnUniqueFL=length(unique(x[x$ScanCount<0,]$FinelineNumber)))})
# testWide <- reshape(testCounts, idvar=c("VisitNumber","Weekday"), timevar="DepartmentDescription", direction="wide")
# testWide[is.na(testWide)] <-0
TotalPurchaseProductsTest <- rowSums((testWide[,grep("PurchaseCount",colnames(testWide))]))
TotalPurchaseProductsTest[TotalPurchaseProductsTest==0] <- -1
TotalReturnProductsTest <- rowSums((testWide[,grep("ReturnCount",colnames(testWide))]))
TotalReturnProductsTest[TotalReturnProductsTest==0] <- -2
TotalPurchaseUniqueProductsTest <- rowSums((testWide[,grep("PurchaseUnique",colnames(testWide))]))
TotalPurchaseUniqueProductsTest[TotalPurchaseUniqueProductsTest==0] <- -1
TotalReturnUniqueProductsTest <- rowSums((testWide[,grep("ReturnUnique",colnames(testWide))]))
TotalReturnUniqueProductsTest[TotalReturnUniqueProductsTest==0] <- -2

normalizedPurchaseCountsTest <- apply((testWide[,grep("PurchaseCount",colnames(testWide))]),2,calculateProportion,TotalPurchaseProductsTest)
normalizedReturnCountsTest <- apply((testWide[,grep("ReturnCount",colnames(testWide))]),2,calculateProportion,TotalReturnProductsTest)
returnToPurchaseRatioTest <- abs(TotalReturnProductsTest)/TotalPurchaseProductsTest
normalizedUniquePurchaseCountsTest <- apply((testWide[,grep("PurchaseUnique",colnames(testWide))]),2,calculateProportion,TotalPurchaseUniqueProductsTest)
normalizedUniqueReturnCountsTest <- apply((testWide[,grep("ReturnUnique",colnames(testWide))]),2,calculateProportion,TotalReturnUniqueProductsTest)
returnToPurchaseRatioUniqueTest <- abs(TotalReturnUniqueProductsTest)/TotalPurchaseUniqueProductsTest

normalizedReturnToPurchaseRatioTest <- normalizedReturnCountsTest/ normalizedPurchaseCountsTest
normalizedUniqueReturnToPurchaseRatioTest <- normalizedUniqueReturnCountsTest/ normalizedUniquePurchaseCountsTest
normalizedReturnToPurchaseRatioTest[is.nan.data.frame(normalizedReturnToPurchaseRatioTest)] <- -9999999
normalizedReturnToPurchaseRatioTest[is.infinite.data.frame(normalizedReturnToPurchaseRatioTest)] <- 9999999
normalizedUniqueReturnToPurchaseRatioTest[is.nan.data.frame(normalizedUniqueReturnToPurchaseRatioTest)] <- -9999999
normalizedUniqueReturnToPurchaseRatioTest[is.infinite.data.frame(normalizedUniqueReturnToPurchaseRatioTest)] <- 9999999
uniqueDeptForColsTest <- colnames(testWide)[grep("PurchaseCount",colnames(testWide))]
uniqueDeptForColsTest<- unlist(lapply(uniqueDeptForColsTest, function(x){unlist(strsplit(x,"PurchaseCount."))[2]}))
colnames(normalizedReturnToPurchaseRatioTest) <- paste("Ratio",uniqueDeptForColsTest,sep="_")
colnames(normalizedUniqueReturnToPurchaseRatioTest) <- paste("Ratio",uniqueDeptForColsTest,sep="_U")

testWideNormalized <- cbind(testWide[,1:2],normalizedPurchaseCountsTest,normalizedUniquePurchaseCountsTest,
                            normalizedReturnCountsTest,normalizedUniqueReturnCountsTest,
                            ReturnPurchaseRatio=returnToPurchaseRatioTest,ReturnPurchaseUniqueRatio=returnToPurchaseRatioUniqueTest,
                            normalizedReturnToPurchaseRatioTest,normalizedUniqueReturnToPurchaseRatioTest)
testWideNormalized[is.na(testWideNormalized)] <-0
#testWideNormalized[is.na(testWideNormalized)] <-0
# testWideNormalized[is.nan(testWideNormalized)] <-0
# testWideNormalized[is.infinite(testWideNormalized)] <-0
ncolsTestBeforeAddition <- ncol(testWideNormalized)
missTestDeptNames <- setdiff(deptNames,colnames(testWideNormalized))
for(x in grep("Count|Unique|Ratio",missTestDeptNames)){
  testWideNormalized <- cbind(testWideNormalized,0)
}
colnames(testWideNormalized)[(ncolsTestBeforeAddition+1):ncol(testWideNormalized)] <- missTestDeptNames[grep("Count|Unique|Ratio",missTestDeptNames)]
testWideNormalized <- testWideNormalized[,deptNames[-1]]
#testWideNormalized <- cbind(testWide[,1:2],normalizedCountsTest)
testWideNormalized$Weekday <- as.character(testWideNormalized$Weekday )
testWideNormalized$Weekday<- gsub("Sunday",1,testWideNormalized$Weekday)
testWideNormalized$Weekday<- gsub("Monday",2,testWideNormalized$Weekday)
testWideNormalized$Weekday<- gsub("Tuesday",3,testWideNormalized$Weekday)
testWideNormalized$Weekday<- gsub("Wednesday",4,testWideNormalized$Weekday)
testWideNormalized$Weekday<- gsub("Thursday",5,testWideNormalized$Weekday)
testWideNormalized$Weekday<- gsub("Friday",6,testWideNormalized$Weekday)
testWideNormalized$Weekday<- gsub("Saturday",7,testWideNormalized$Weekday)
testWideNormalized$Weekday <- as.integer(testWideNormalized$Weekday )
testPredictions <- (data=predict(clf,data.matrix(testWideNormalized[,2:ncol(testWideNormalized)])))
predMatrix <- matrix(testPredictions,ncol=38,byrow=T)
colnames(predMatrix) <- do.call(paste,list("TripType",sortedTripTypes,sep="_"))
submission <- data.frame(VisitNumber=testWideNormalized$VisitNumber)
submission <- cbind(submission,predMatrix)
totalTime <- as.numeric(Sys.time()-startTime,units="mins")
prefix <- paste(outputFolder,scriptName,"_",nodename,"_",gsub(" ","",gsub(":","_",startTime)),sep="")
write.csv(submission, paste(prefix,".csv",sep=""),row.names = F)
save(list=ls(),file=paste(prefix,".rdata",sep=""))
# colors = rainbow(length(unique(trainWide$TripType)))
# names(colors) = unique(trainWide$TripType)
# ecb = function(x,y){ plot(x,t='n'); text(x,labels=trainWide$TripType, col=colors[trainWide$TripType]) }
#tsne_iris = tsne(trainWide[,4:72], epoch_callback = ecb, perplexity=50)
# clusters <- kmeans(trainWideNormalized[,3:72],centers = 38,iter.max = 100,nstart = 10)
# t <- data.frame(trainWideNormalized$TripType,clusters$cluster)
# tripTypeClusters <- (table(t$trainWideNormalized,t$clusters.cluster))
# pc<-prcomp(trainWideNormalized[,3:72])
# plot(pc$x[,1], pc$x[,2],col=trainWideNormalized$TripType,pch=16)
# # plot spiderweb and connect outliners with dotted line
# pc<-cbind(pc$x[,1], pc$x[,2])
# # ordispider(pc, factor(km$cluster), label = TRUE)
# # ordihull(pc, factor(km$cluster), lty = "dotted")
# ordispider(pc, trainWideNormalized$TripType, label = TRUE)
# ordihull(pc, factor(km$cluster), lty = "dotted")
# tsne_data <- tsne(trainWideNormalized[,3:72], k=2, max_iter=500, epoch=500)
# plot(tsne_data[,1], tsne_data[,2], col=km$cluster, pch=16)
# ordispider(tsne_data, factor(km$cluster), label = TRUE)
# ordihull(tsne_data, factor(km$cluster), lty = "dotted")
