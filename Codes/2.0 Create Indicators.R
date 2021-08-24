rm(list = ls())

setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment/Outputs/")

toFix <- c('trainData','scoringData')

for (iterCol in toFix) {
  cleanedData <- read.csv(paste0(iterCol,'_cleaned.csv'),stringsAsFactors = F)
  
  cleanedData$MotorFlag <- 0
  cleanedData$MotorFlag[!is.na(cleanedData$MotorValue)] <- 1
  cleanedData$MotorValueBuckets[is.na(cleanedData$MotorValueBuckets)] <- 0
  cleanedData$MotorLevel <-  as.integer(as.character(factor(cleanedData$MotorValueBuckets,c(0,'low','medium','high'),c(0:3))))
  
  
  cleanedData$HealthFlag <- 0
  cleanedData$HealthFlag[!is.na(cleanedData$HealthType)] <- 1
  cleanedData$HealthType[is.na(cleanedData$HealthType)] <- 0
  cleanedData$HealthLevel <- as.integer(as.character(factor(cleanedData$HealthType,c(0,'Level1','Level2','Level3'),c(0:3))))
  
  
  cleanedData$TravelFlag <- 0
  cleanedData$TravelFlag[!is.na(cleanedData$TravelType)] <- 1
  cleanedData$TravelType[is.na(cleanedData$TravelType)] <- 0 
  cleanedData$TravelLevel <- as.integer(as.character(factor(cleanedData$TravelType,c(0,'Backpacker','Standard','Senior','Premium','Business'),c(0,1,2,2,3,3))))
  
  
  cleanedData$Instances <- cleanedData$HealthFlag+cleanedData$TravelFlag+cleanedData$MotorFlag
  cleanedData$customerValue <- cleanedData$HealthLevel + cleanedData$TravelLevel + cleanedData$MotorLevel
  
  cleanedData$SalaryIndicator <- as.character(factor(cleanedData$customerValue,c(0:9),c('noClue','low','low','low','med','med','med','high','high','high')))
  
  cleanedData$FamilyIndicator <- cleanedData$HealthDependentsAdults + cleanedData$HealthDependentsKids
  
  cleanedData$FamilyIndicator[cleanedData$FamilyIndicator > 2 ] <- 'Big family'
  cleanedData$FamilyIndicator[is.na(cleanedData$FamilyIndicator)] <- 'No Health Insurance'
  cleanedData$FamilyIndicator[cleanedData$FamilyIndicator == 0] <- 'single'
  cleanedData$FamilyIndicator[cleanedData$FamilyIndicator %in% c(1,2) ] <- 'nuclear family'
  
  write.csv(cleanedData,paste0(iterCol,'_withIndicators.csv'),row.names = F) 
}

