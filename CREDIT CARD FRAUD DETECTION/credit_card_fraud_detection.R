#Importing required libraries
library(ggplot2)
library(dplyr)
library(tidymodels)
#For logistic regression
library(caTools)
#For ROC curve
library(ROCR)
library(pROC)
library(starter)
#for confusionMatrix
library(caret)
#For decision tree
library(rpart)
library(rpart.plot)
#for randomForest
library(randomForest)
#for xGboost algorithm
library(xgboost)

#importing the file
card_data <- read.csv('creditcard.csv')

#Checking he file
head(card_data)
max(card_data$Time)
#Summary of the dataframe
summary(card_data)

#Checking whether any columns have missing values
colSums(is.na(card_data))

#Observing the imbalance in the dataset
table(card_data$Class)

card_data$Class <- factor(card_data$Class, ordered=TRUE)
#It is observed that the data-set is hugely imbalanced with 284315 no. of non-frauds indicating with '0' and only 492 no. of frauds indicating with '1'.
#Visualizing the imbalance in dataset

ggplot(data=card_data, aes(x= factor(Class),y = prop.table(stat(count)),
                         fill=factor(Class), 
                         label = scales::percent(prop.table(stat(count)))))+
 stat_count(geom="bar")+
 geom_text(stat='count', size=3, vjust=-0.5)+
 scale_x_discrete(labels = c("no fraud", "fraud"))+
 scale_y_continuous(labels=scales::percent)+
 labs(x='Class', y='Percentage')+
 ggtitle("Distribution of class labels")+
 theme_minimal()

#Let's see distribution of class based on "Time" and "Amount"
#Subsetting the classes
card_fraud <- card_data[card_data$Class==1,]
card_nonfraud<- card_data[card_data$Class==0,]

#Based on time
ggplot()+
  geom_density(data= card_fraud, aes(x=Time),color="Red",fill="red",alpha=0.12)+
  geom_density(data= card_nonfraud, aes(x=Time), color="blue", fill="blue",alpha=0.12)

#Based on amount
ggplot()+
  geom_density(data=card_fraud, aes(x=Amount), color="Red", fill="red", alpha=0.12)+
  geom_density(data=card_nonfraud, aes(x=Amount), color="Blue", fill="Blue", alpha=0.12)


#splitting the data
set.seed(1234)
card_data_split <- initial_split(card_data, prop=0.8)
card_data_train <- training(card_data_split)
card_data_test <- testing(card_data_split)

##Fitting logistic regression model 
logistic_model <- glm(Class~ ., data=card_data_train, family='binomial')

logistic_model

#predicting class value as per the built model
predict_logistic_model <- predict(logistic_model, newdata = card_data_test, type='response')

#Assigning the predicted value to the card_data_test where any predicted value greater than 0.5 is substituted as logical 1
#else, logical 0

card_data_test$pred <- 0L
card_data_test$pred[predict_logistic_model > 0.5] <- 1L
card_data_test$pred <- factor(card_data_test$pred, ordered=TRUE)

confusionMatrix(card_data_test$pred,
                card_data_test$Class)

#Hence, the logistic model provides us accuracy of 0.9992

#calculating and plotting a ROC curve for logistic regression 
roc_score <- roc(card_data_test$Class, card_data_test$pred)
plot(roc_score, print.auc= TRUE, col="blue", main="ROC CURVE FOR LOGISTIC REGRESSION")
#AUC FOR THIS MODEL WAS FOUND TO BE 0.768

#Testing decision tree algorithm
#rpart 

decision_tree <- rpart(Class~., data=card_data_train, method="class")
decision_tree_pred <- predict(decision_tree, newdata=card_data_test)

card_data_test$pred <- 0L
card_data_test$pred[decision_tree_pred[,2]>0.5] <- 1L
card_data_test$pred <- factor(card_data_test$pred, ordered=TRUE)

confusionMatrix(card_data_test$Class, card_data_test$pred)

#The decision tree provides the accuracy of 0.9994 which is slightly better than logistic_model created before

rpart.plot(decision_tree, cex=0.72, type=5, extra=0, box.palette="RdGy")

roc_score <- roc(card_data_test$Class, card_data_test$pred)
plot(roc_score, print.auc=TRUE, col="blue", main="ROC CURVE FOR DECISION TREE")
#AUC FOR THIS MODEL WAS FOUND TO BE 0.847 which proves it to be a better model than previous one


#Testing random Forest algorithm
random_forest <- randomForest(Class~., data=card_data_train, ntree=39)
random_forest_pred <- predict(random_forest, newdata=card_data_test)
random_forest_pred
card_data_test$pred <- random_forest_pred
card_data_test$pred <- factor(card_data_test$pred, ordered=TRUE)
confusionMatrix(card_data_test$Class, card_data_test$pred)
#The random forest provides the accuracy of 0.9996 which is slightly better than decision tree model

roc_score <- roc(card_data_test$Class, card_data_test$pred)
plot(roc_score, print.auc= TRUE, col="blue", main="ROC CURVE FOR RANDOM FOREST")
#AUC FOR THIS MODEL WAS FOUND TO BE 0.872 WHICH PROVES IT TO BE A BETTER MODEL THAN PREVIOUS MODELS

#xGBoost Model
label <- card_data_train$Class 
#replacing the value of '0' factor as numeric 0 and similar for '1' to 1
y <- recode(label, '0'=0,'1'=1) 

params <- list(
  objective= "binary:logistic",
  eta= 0.1,
  gamma=0.1,
  max_depth= 10
)



xgb_model <- xgboost(data= data.matrix(card_data_train),
                     label=y,
                     params= params,
                     verbose=1,
                     nrounds=300,
                     nthread= 7,
                     set_seed= 0311
                     )

predict_xgb <- predict(xgb_model, newdata=data.matrix(card_data_test))

card_data_test$pred <- 0L
card_data_test$pred[predict_xgb >0.5] <- 1L
card_data_test$pred <- factor(card_data_test$pred, ordered=TRUE)

confusionMatrix(card_data_test$Class, card_data_test$pred)
#The XGBOOST provides accuracy of 1
roc_score <- roc(card_data_test$Class, card_data_test$pred)
plot(roc_score, print.auc=TRUE, col="blue", main="ROC CURVE FOR XGBOOST")
#The AUC of the model is 1.000 which proves it to be the best model among all
