rm(list = ls())
setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment/Outputs/")

clusteredData <- read.csv('trainData_clustered.csv',stringsAsFactors = T)
scoringData <- read.csv('scoringData_withIndicators.csv',stringsAsFactors = T)


doNotInclude <- c('CustomerID','Occupation','MotorValue','Title','PrefChannel') #75 and 65

cleanedData <- clusteredData[,!names(clusteredData) %in% doNotInclude]

cleanedData$cluster <- as.factor(cleanedData$cluster)

### finding the varibale importance
bor <- Boruta(cluster~.,data = cleanedData, doTrace=2,maxRuns=50)
setwd(paste0(getwd(),'/Plots/'))
pdf('ClusterAssignment - variable importance.pdf',width=12,height = 12)
plot(bor,las=2,cex.axis=0.6)
dev.off()
# attStats(bor)


##### splitting the data
set.seed(213)
rowInd <- sample(2,nrow(cleanedData),replace = T,prob = c(0.8,0.2))
sum(is.na(cleanedData))

trainData <- cleanedData[rowInd==1,]
testData <- cleanedData[rowInd==2,]



### decision tree to find clusters
model_tree <- ctree(getNonRejectedFormula(bor),data = trainData, controls = ctree_control(mincriterion = 0.9,minsplit = 500))
pdf('ClusterAssigment - Decision Tree.pdf',width=12,height = 12)
plot(model_tree)
dev.off()

p1_Tree <- predict(model_tree,trainData,type = 'response')
confMatrix_1_Tree <- table(p1_Tree,trainData$cluster)
sum(diag(confMatrix_1_Tree))/nrow(trainData)

p2_Tree <- predict(model_tree,testData,type = 'response')
confMatrix_2_Tree <- table(p2_Tree,testData$cluster)
sum(diag(confMatrix_2_Tree))/nrow(testData)

scoringData$MotorType <- as.character(scoringData$MotorType)
scoringData$MotorType[is.na(scoringData$MotorType)] <- 'No MotorInsurance'
scoringData$MotorType <- as.factor(scoringData$MotorType)
scoringData$HealthDependentsAdults[is.na(scoringData$HealthDependentsAdults)] <- 'No HI'
scoringData$HealthDependentsKids[is.na(scoringData$HealthDependentsKids)] <- 'No HI'
scoringData$HealthDependentsAdults <- as.factor(scoringData$HealthDependentsAdults)
scoringData$HealthDependentsKids <- as.factor(scoringData$HealthDependentsKids)
scoringData$cluster <- predict(model_tree,scoringData[,!names(scoringData) %in% doNotInclude],type='response')

setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment/Outputs/")
write.csv(scoringData,'scoringData_clustered.csv',row.names = F)







