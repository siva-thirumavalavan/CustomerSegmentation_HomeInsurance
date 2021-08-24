rm(list = ls())

setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment")
masterWD <- getwd()

### required libraries
library(scales)


### reading the raw data
trainDataMaster<- read.csv('rawData/QUB_Insurance_Data_Assignment_Training.csv',stringsAsFactors = F,na.strings = "")
scoringDataMaster <- read.csv('rawData/QUB_Insurance_Data_Assignment_Scoring.csv',stringsAsFactors = F,na.strings = "")


uselessColumns <- c('GivenName','MiddleInitial','Surname') #have no use in models
trainData <- trainDataMaster[,!names(trainDataMaster) %in% uselessColumns]
scoringData <- scoringDataMaster[,!names(scoringDataMaster) %in% uselessColumns]

dir.create('Outputs')
dir.create('Outputs/Plots')
####################### EDA ###########################
toFix <- c('trainData','scoringData') # pass the df name to be cleaned

###cleaning both the dataframes in a loop
for (iterDF in toFix) {
  DF <- get(iterDF)
  #checking for duplicates in customer id
  nrow(DF)-length(unique(DF$CustomerID))
  
  #checking NA % in each column
  na_count <-sapply(DF, function(y) sum(length(which(is.na(y))))/length(y))
  summary(DF) ## age and motorValue has outliers
  str(DF)
  #fixing NA in CreditCardType
  unique(DF$CreditCardType)
  DF$CreditCardType[is.na(DF$CreditCardType)] <- 'No Card'
  
  #fixing Age column

  setwd(paste0(getwd(),'/Outputs/Plots/'))
  png(paste0("Age_",iterDF,".png"))
  boxplot(DF$Age)
  dev.off()
  DF$Age <- abs(DF$Age)
  DF$Age[DF$Age>100]=DF$Age[DF$Age>100]/10
  # DF$Age <- squish(DF$Age,quantile(DF$Age, c(0.05, 0.95),names = F))
  
  #checking unique occupations
  length(unique(DF$Occupation)) ## occupation has 1592 levels and 1556 NAs (38%)
  
  #fixing levels in gender column
  unique(DF$Gender)
  DF$Gender[DF$Gender=='f']='female'
  DF$Gender[DF$Gender=='m']='male'
  
  #fixing motorInsurance
  DF$MotorValue <- abs(DF$MotorValue)
  png(paste0("MotorValue_",iterDF,".png"))
  boxplot(DF$MotorValue)
  dev.off()
  DF$MotorValue<- squish(DF$MotorValue,quantile(DF$MotorValue, c(0.05, 0.95),names = F,na.rm = T))
  
  sum(is.na(DF[DF$MotorInsurance=='Yes','MotorValue']))
  sum(is.na(DF[DF$MotorInsurance=='Yes','MotorType']))
  ## MotorValue and MotorType are NA when the the customer hasn't opted for motor insurance.
  
  
  #fixing helthInsurance
  unique(DF$HealthInsurance)
  unique(DF$HealthDependentsKids)
  unique(DF$HealthDependentsAdults)
  unique(DF$HealthType)
  
  sum(is.na(DF[DF$HealthInsurance=='Yes','HealthDependentsAdults']))
  sum(is.na(DF[DF$HealthInsurance=='Yes','HealthDependentsKids']))
  sum(is.na(DF[DF$HealthInsurance=='Yes','HealthType']))
  
  
  #fixing travelInsurance
  unique(DF$TravelInsurance)
  unique(DF$TravelType)
  sum(is.na(DF[DF$TravelInsurance=='Yes','TravelType']))
  
  
  #fixing Preffered Channel
  if (iterDF=='trainData') {
    unique(DF$PrefChannel)
    DF$PrefChannel[DF$PrefChannel=='E']='Email'
    DF$PrefChannel[DF$PrefChannel=='P']='Phone'
    DF$PrefChannel[DF$PrefChannel=='S']='SMS'
  }

  
  sum(is.na(DF$HealthDependentsKids))
  DF$HealthDependentsKids==0
  
  
  ### derived columns
  #card holder
  DF$CardHolder <- 'Yes'
  DF$CardHolder[DF$CreditCardType=='No Card'] <- 'No'
  
  #married column
  DF$Married <- 'No'
  DF$Married[DF$Title=='Mrs.'] <- 'Yes'
  DF$Married[DF$Title!='Ms.' & DF$HealthDependentsKids!=0 & !is.na(DF$HealthDependentsKids)] <- 'Yes'
  
  
  #age buckets
  DF$AgeBuckets <- cut(DF$Age,breaks = c(17,35,50,100),labels = c('<=35','36-50','>50'))
  
  
  quantile(DF$MotorValue,na.rm = T)
  quantile(DF$MotorValue,c(0,0.2,0.7,1),na.rm = T,names = F)
  
  #motorValue buckets
  DF$MotorValueBuckets <- cut(DF$MotorValue,breaks = quantile(DF$MotorValue,c(0,0.25,0.75,1),na.rm = T,names = F),labels = c('low','medium','high'),include.lowest = T)
  
  
  ### exporting cleaned data
  setwd(masterWD)
  write.csv(DF,paste0('Outputs/',iterDF,'_cleaned.csv'),row.names = F)
  
}


