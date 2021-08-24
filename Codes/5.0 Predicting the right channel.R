rm(list = ls())

setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment/Outputs")
library(randomForest)
library(ggplot2)
library(caret)
library(e1071)
library(nnet)
library(naivebayes)
library(class)
library(car)
library(Boruta)
library(party)
library(ROCR)
library(pROC)
library(dplyr)
library(yardstick)

clusteredData <- read.csv('trainData_clustered.csv',stringsAsFactors = T)
str(clusteredData)
scoringData_master <- read.csv('scoringData_clustered.csv',stringsAsFactors = T)


doNotInclude <- c('CustomerID','Occupation','Title') #75 and 65
cleanedData <- clusteredData[,!names(clusteredData) %in% doNotInclude]
cleanedData$MotorValue[is.na(cleanedData$MotorValue)] <- 0


### finding the varibale importance
bor <- Boruta(PrefChannel~.,data = cleanedData, doTrace=2, maxRuns = 50)
setwd(paste0(getwd(),'/Plots/'))
pdf('ChannelPrediction - variable importance.pdf',width=12,height = 12)
plot(bor,las=2,cex.axis=0.8)
dev.off()


rocPlot <- function(trainData,df,fileName){
  jpeg(paste0(fileName,'_ROCPlot.jpeg'))
  par(pty='s')
  multiclass.roc(trainData$PrefChannel,df,plot = T,percent = T,xlab ='False Positive Percentage',ylab='True Positive Percentage', legacy.axes=T, print.auc=T) 
  dev.off()
}


##### splitting the data
set.seed(321)
rowInd <- sample(2,nrow(cleanedData),replace = T,prob = c(0.8,0.2))
sum(is.na(cleanedData))

trainData <- cleanedData[rowInd==1,]
testData <- cleanedData[rowInd==2,]


############ Random Forest ############ 
model_RF <- randomForest(getNonRejectedFormula(bor),data = trainData,importance=T,proximity=T)
print(model_RF)

p1_RF <- predict(model_RF,trainData)
confusionMatrix(p1_RF,trainData$PrefChannel)


p2_RF <- predict(model_RF,testData)
confusionMatrix(p2_RF,testData$PrefChannel)

confMatrix_1_RF <- table(Predicted=p1_RF,Actual=trainData$PrefChannel)
w_RF1 = sum(diag(confMatrix_1_RF ))/nrow(trainData)

confMatrix_2_RF <- table(Predicted=p2_RF,Actual=testData$PrefChannel)
w_RF2 = sum(diag(confMatrix_2_RF ))/nrow(testData)


rocPlot(trainData,as.numeric(p1_RF),'RF')



############ SVM ############ 
model_SVM <- svm(getNonRejectedFormula(bor),data = trainData,kernel='radial')
summary(model_SVM)

p1_SVM <- predict(model_SVM,trainData)
confMatrix_1_SVM <- table(Predicted=p1_SVM,Actual=trainData$PrefChannel)
w_SVM1 = sum(diag(confMatrix_1_SVM ))/nrow(trainData)

p2_SVM <- predict(model_SVM,testData)
confMatrix_2_SVM  <- table(Predicted=p2_SVM,Actual=testData$PrefChannel)
w_SVM2 = sum(diag(confMatrix_2_SVM ))/nrow(testData)

rocPlot(trainData,as.numeric(p1_SVM),'SVM')



############ Logistic Regression ############ 
model_MLR <- multinom(getNonRejectedFormula(bor),data = trainData)
summary(model_MLR)

p1_MLR <- predict(model_MLR,trainData)
confMatrix_1_MLR <- table(p1_MLR,trainData$PrefChannel)
w_MLR1 = sum(diag(confMatrix_1_MLR))/nrow(trainData)

p2_MLR <- predict(model_MLR,testData)
confMatrix_2_MLR <- table(p2_MLR,testData$PrefChannel)
w_MLR2 = sum(diag(confMatrix_2_MLR))/nrow(testData)

rocPlot(trainData,as.numeric(p1_MLR),'MLR')




############ Decision Tree ############ 
model_tree <- ctree(getNonRejectedFormula(bor),data = trainData, controls = ctree_control(mincriterion = 0.9,minsplit = 300))
plot(model_tree)

p1_Tree <- predict(model_tree,trainData,type = 'response')
confMatrix_1_Tree <- table(p1_Tree,trainData$PrefChannel)
sum(diag(confMatrix_1_Tree))/nrow(trainData)
w_Tree1 = sum(diag(confMatrix_1_Tree))/nrow(trainData)

p2_Tree <- predict(model_tree,testData,type = 'response')
confMatrix_2_Tree <- table(p2_Tree,testData$PrefChannel)
w_Tree2 = sum(diag(confMatrix_2_Tree))/nrow(testData)


rocPlot(trainData,as.numeric(p1_Tree),'Tree')


#### voting
findAccuracy <- function(cm){
  return(diag(cm/colSums(cm)))
}

weightList = t(data.frame('mlr'=mean(w_MLR1,w_MLR2),'svm'=mean(w_SVM1,w_SVM2),'RF'=mean(w_RF1,w_RF2),'Tree'=mean(w_Tree1,w_Tree2)))

trainResults <- data.frame('mlr'=p1_MLR,'svm'=p1_SVM,'RF'=p1_RF,'Tree'=p1_Tree,'actual'=trainData$PrefChannel)
testResults <- data.frame('mlr'=p2_MLR,'svm'=p2_SVM,'RF'=p2_RF,'Tree'=p2_Tree,'actual'=testData$PrefChannel)

row.names(trainResults) <- 1:nrow(trainResults)
row.names(testResults) <- 1:nrow(testResults)
getWeightedVote <- function(df){
  temp=df
  for (rowIter in 1:nrow(df)) {
    rowDF <- cbind.data.frame(t(df[rowIter,!names(df)%in%c('actual')]),weightList)
    colnames(rowDF) <- c('Model','Weight')
    rowDF$Weight <- rowDF$Weight/sum(rowDF$Weight)
    solutionDF <- rowDF %>% group_by(Model) %>% summarise_all(sum) %>% arrange(desc(Weight))
    temp$Predicted[rowIter] = as.character(solutionDF$Model[solutionDF$Weight==max(solutionDF$Weight)])
  }
  return(temp)
}



trainResults = getWeightedVote(trainResults)
confMatrix<- table(trainResults$Predicted,trainResults$actual)
confMatrix
sum(diag(confMatrix))/nrow(trainResults)

testResults = getWeightedVote(testResults)
confMatrix<- table(testResults$Predicted,testResults$actual)
confMatrix
sum(diag(confMatrix))/nrow(testResults)


######### predicting channel for scoring dataset
scoringData <- scoringData_master[,!names(scoringData_master) %in% doNotInclude]
scoringData$MotorValue[is.na(scoringData$MotorValue)] <- 0

score_Tree <- predict(model_tree,scoringData,type = 'response')
score_RF <- predict(model_RF,scoringData)
score_SVM <- predict(model_SVM,scoringData)
score_MLR <- predict(model_MLR,scoringData)

scoreResults <- data.frame('mlr'=score_MLR,'svm'=score_SVM,'RF'=score_RF,'Tree'=score_Tree)

scoreResults <- getWeightedVote(scoreResults)
scoringData_master$PrefChannel <- scoreResults$Predicted

testGroup <- scoringData_master %>% group_by(cluster) %>% sample_frac(0.95)
controlGroup <- scoringData_master[!scoringData_master$CustomerID %in% testGroup$CustomerID,]

testGroup$Group <- 'test'
controlGroup$Group <- 'control'

scoringData_grouped <- rbind.data.frame(testGroup,controlGroup)

scoringData_grouped$cluster <- as.character(factor(scoringData_grouped$cluster ,c(1,2,3),c('Ready Rurals','Millineum Potentials','Titan Customers')))


setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment/Outputs")
write.csv(scoringData_grouped,'scoringData_predictedChannels.csv',row.names = F)

