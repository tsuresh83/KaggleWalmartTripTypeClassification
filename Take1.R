rm(list=ls())
library(readr)
library(dummy)
library(data.table)
set.seed(13)

startTime <- Sys.time()
scriptName<-"XGBoostAllNumericAndFactorsEQThirdAndUnifCombined"
set.seed(13)
os <- Sys.info()[["sysname"]]
nodename <- Sys.info()[["nodename"]]
trainFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/walmart/data/train.csv"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/walmart/data/train.csv"),
                           ("/media/3TB/kag/walmart/data/train.csv")))
train <- read_csv(trainFile)
print("Training data set loaded...")
testFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/walmart/data/test.csv"),
                   ifelse(os=="Darwin",
                          ("/Users/sthiagar/Kaggle/walmart/data/test.csv"),
                          ("/media/3TB/kag/walmart/data/test.csv")))
test <- read_csv(testFile)
trainDT <- data.table(train)
trainDT <- subset(trainDT,trainDT$ScanCount!=-1)
test <- data.table(test)
# setkey(trainDT,TripType,DepartmentDescription)
# counts <- trainDT[,list(Counts=sum(ScanCount)),by=key(trainDT)]
# setkey(counts,TripType)
# maxCounts <- counts[,list(Max=max(Counts),Dept=DepartmentDescription[Counts==max(Counts)]),by=key(counts)]
# library(tsne)
# colors = rainbow(length(unique(5)))
# names(colors) = 1:5
# ecb = function(x,y){ plot(x,t='n'); text(x,labels=1:5, col=colors[1:5]) }
# countsDF <- as.data.frame(counts)
# tsne_wt = tsne(countsDF[,1:2], epoch_callback = ecb, perplexity=50)
uniqueDepartments <- unique(trainDT$DepartmentDescription)
trainDTWithDeptCols <- data.frame()

