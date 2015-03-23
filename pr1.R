library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(lattice)
library(ggplot2)
library(caret)

filesDirectory = "./data"
trainFile <- "pml-training.csv"
testFile <- "pml-testing.csv"
trainFilePath <- paste(filesDirectory, trainFile, sep = "/")
testFilePath <- paste(filesDirectory, testFile, sep = "/")
training <- read.csv(trainFilePath, na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(testFilePath, na.strings=c("NA","#DIV/0!",""))
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]
dim(myTraining)
## [1] 11776   160
dim(myTesting)
## [1] 7846  160
myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)

myNZVvars <- names(myTraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")

myTraining <- myTraining[!myNZVvars]

dim(myTraining)
myTraining <- myTraining[c(-1)]
trainingV3 <- myTraining 
for(i in 1:length(myTraining)) { 
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { 
    for(j in 1:length(trainingV3)) {
      if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  {
        trainingV3 <- trainingV3[ , -j]
      }   
    } 
  }
}

dim(trainingV3)
myTraining <- trainingV3
rm(trainingV3)
clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -58]) #already with classe column removed
myTesting <- myTesting[clean1]
testing <- testing[clean2]

dim(myTesting)## [1] 7846   58
dim(testing)
## [1] 20 57
for (i in 1:length(testing) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(myTraining[i])
    }      
  }      
}
testing <- rbind(myTraining[2, -58] , testing)
testing <- testing[-1,]
library(rpart)
modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")
#install.packages("rattle", repos="http://rattle.togaware.com", type="source") 
#$ wget http://togaware.com/access/rattle_3.4.2.tar.gz
library(rpart)
fancyRpartPlot(modFitA1)
prp(modFitA1)
#library(randomForest)
#exSet1Modelrf <- randomForest(classe~.,data=trainEx1)
#exSet1Modelrf
#modelRf <- randomForest(classe~.,data=trainSet)
#modelRf
#pred <- predict(modelRf,testSet)
#testSet$predRight <- pred == testSet$classe
#table(pred,testSet$classe)
#testData <- read.csv(testFile.dpath)
#submitPredictionsModelRf <- predict(modelRf,testData)
#submitPredictionsModelRf
#answersDir <- "./answers"
#if(!file.exists(answersDir)) {dir.create(answersDir)}

#pml_write_files = function(x,y){
#  n = length(x)
#  for(i in 1:n){
#    filename = paste(y,"/problem_id_",i,".txt",sep="")
#    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#  }
#}

#pml_write_files(submitPredictionsModelRf,answersDir)
library(randomForest)
modFitB1 <- randomForest(classe ~. , data=myTraining)
predictionsB1 <- predict(modFitB1, myTesting, type = "class")
confusionMatrix(predictionsB1, myTesting$classe)
predictionsB2 <- predict(modFitB1, testing, type = "class")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)

