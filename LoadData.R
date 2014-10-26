setwd("~/GitHub/PracticalMachineLearning")

if (!file.exists("pml-training.csv"))
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
if (!file.exists("pml-testing.csv"))
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")

fullTraining <- read.csv("pml-training.csv", stringsAsFactors=F)

set.seed(123)
myTest <- createDataPartition(y=fullTraining$classe, p=0.1, list=F)

classification <- data.frame(classe=as.factor(fullTraining$classe[-myTest]))
for(classe in levels(classification$classe))
  classification[,classe] = classification$classe == classe

myTestClassification <- data.frame(classe=as.factor(fullTraining$classe[myTest]))
for(classe in levels(myTestClassification$classe))
  myTestClassification[,classe] = myTestClassification$classe == classe

training <- fullTraining[-myTest,8:(ncol(fullTraining)-1)]
myTestSet <- fullTraining[myTest,8:(ncol(fullTraining)-1)]
for (i in 1:ncol(training)) {
  training[, i] <- as.numeric(training[, i])
  myTestSet[, i] <- as.numeric(myTestSet[, i])
}

testing <- read.csv("pml-testing.csv", stringsAsFactors=F)
testing <- testing[,8:(ncol(testing)-1)]
for (i in 1:ncol(testing))
  testing[, i] <- as.numeric(testing[, i])
