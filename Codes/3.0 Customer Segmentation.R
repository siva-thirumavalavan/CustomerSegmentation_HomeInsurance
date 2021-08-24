rm(list = ls())

setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment/Outputs/")
  

################## function definitions #####################

minMaxScale <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}


##################### k means ########################
cleanedData_master <- read.csv('trainData_withIndicators.csv',stringsAsFactors = F)
usedColumns <- c('Age','MotorValue')
clusterData <- cleanedData_master[,names(cleanedData_master) %in% usedColumns]


clusterData[is.na(clusterData)] <- 0
setwd(paste0(getwd(),'/Plots/'))
png("ScreePlot.png")
wssplot(clusterData)
dev.off()




clusterData_normalized <-  data.frame(apply(clusterData,2,minMaxScale))
clusterData <- data.frame(scale(clusterData))
set.seed(0000)
cluster <- kmeans(clusterData,3)
cluster
png("ClusterPlot.png")
plot(cleanedData_master$Age,cleanedData_master$MotorValue,col=cluster$cluster)
legend('bottomright',legend=c(1,2,3),col=1:3,pch=2)
dev.off()
cleanedData_master$cluster <- cluster$cluster
cleanedData_master$MotorType[is.na(cleanedData_master$MotorType)] <- 'No MotorInsurance'
cleanedData_master$HealthDependentsAdults[is.na(cleanedData_master$HealthDependentsAdults)] <- 'No HI'
cleanedData_master$HealthDependentsKids[is.na(cleanedData_master$HealthDependentsKids)] <- 'No HI'
cleanedData_master$HealthDependentsAdults <- as.character(cleanedData_master$HealthDependentsAdults)
cleanedData_master$HealthDependentsKids <- as.character(cleanedData_master$HealthDependentsKids)
setwd("D:/MSBA/Trimester 2/Data Management and mining/Team assignment/Outputs/")
write.csv(cleanedData_master,'trainData_clustered.csv',row.names = F)
