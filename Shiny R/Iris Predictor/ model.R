library(RCurl)
library(randomForest)
library(caret)

#importing iris dataset
iris <- read.csv(text=getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv"))

iris$Species <- as.factor(iris$Species)

#performing stratified random split of the data set
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,]
TestingSet <- iris[-TrainingIndex,]

write.csv(TrainingSet,"training.csv")
write.csv(TestingSet,"testing.csv")

TrainSet <- read.csv("training.csv", header= TRUE)
TrainSet <- TrainSet[,-1]

model <- randomForest(Species ~ . , data=TrainingSet, ntree=500, mtry=4, importance= TRUE)

saveRDS(model,"model.RDS")
