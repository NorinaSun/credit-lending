library(tidyverse)
library(caret)
library(xgboost)
library(dplyr)
library(gbm)

#load the training data
train = read.csv("/Users/NorinaSun/Downloads/MATH60603/CREDIT_RISK/CreditGameData/CreditGame_TRAIN.csv")
#load the prediction data
to_predict = read.csv("/Users/NorinaSun/Downloads/MATH60603/CREDIT_RISK/CreditGameData/CreditGame_Applications.csv")

#see the data
#sample_n(train,10)

# #delete any rows with nas
# train <- na.omit(train)

#checking the data
# unique(train$target_0)
# str(train)

#creating a preprocessing function
process <- function(df,df_type) {
  #replacing the empty string value in ST_EMPL
  df$ST_EMPL[df$ST_EMPL==""]<-NA
  
  #changing char variables to a factor
  df$TYP_RES <- as.factor(df$TYP_RES)
  df$ST_EMPL <- as.factor(df$ST_EMPL)
  
  #dropping type since its all auto
  df <- subset(df, select = -c(TYP_FIN))
  
  if (df_type == "train"){
    #dropping the id column
    df <- subset(df, select= -c(ID_TRAIN))
    #changing target variable to a char
    df$target_0 <- ifelse(df$target_0==1, 'defaulted','no_default')
    df$target_0 <- as.factor(df$target_0)
  } else if (df_type == "test"){
    #dropping the id column
    df <- subset(df, select= -c(ID_TEST))
  } else {
    print("ok")
  }
  
  return(df)
}

#applying the processing
train_processed <- process(train,"train")

#split the data
training.sample <- train_processed$target_0 %>% createDataPartition(p=0.8, list=FALSE)
train_processed.data <- train_processed[training.sample,]
train_processed.data <- train_processed[-training.sample,]

train_X <- select(train_processed.data, -c(target_0))
test_X <- select(train_processed.data, -c(target_0))

train_Y <- train_processed.data$target_0
test_Y <- train_processed.data$target_0

#specifying model parameters
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE, sampling="up")

#fitting the model
model <- train(train_X, train_Y, method ="gbm", trControl=objControl, metric="ROC")

#using the test set
test_predictions <- predict(object=model, test_X, type='raw')
postResample(pred=test_predictions, obs=as.factor(test_Y))

#prediction with the final dataset
to_predict_processed <- process(to_predict,"test")
predictions_final <- predict(object=model, to_predict_processed, type='raw')

#adding the predictions to the dataframe
to_predict$prediction <- predictions_final

#taking just the yesses
good_loans = to_predict[to_predict$prediction == "no_default",]

#saving the table
write.csv(good_loans$ID_TEST,"/Users/NorinaSun/Downloads/MATH60603/CREDIT_RISK/CreditGameData/good_loans.csv")
