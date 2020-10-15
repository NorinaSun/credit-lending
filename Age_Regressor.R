#loading library
library(tidyverse)

#creating a function to impute missing age values with a regressor prediction
impute_na <- function(df,df_type) {
  
  #drop any na values for 
  df_na = df %>% drop_na()
  
  #undergoing some basic processing
  df_train <- preprocess_help(df_na,df_type)
  
  #training a random forest to predict age
  age.mlr <- lm(AGE_D ~., data=df_train)
  summary(age.mlr)
  
  #making the table for the pred 
  df_pred <- preprocess_help(df,df_type)
  
  #make predictions for training set 
  df$predicted_age <- predict(object=age.mlr, df_pred)
  
  #replace na values in the full df with the predicted ones
  df$AGE_D <- ifelse(is.na(df$AGE_D),df$predicted_age, df$AGE_D)
  
  #drop the predicted age column
  df <- subset(df, select = -c(predicted_age))
  
  return(df)

}

#creating a helper function to do some preprocessing
preprocess_help <- function(df,df_type){
  
  if (df_type == "train"){
    #drop the id and the target variable
    df <- subset(df, select= -c(ID_TRAIN, target_0, TYP_FIN))
  }else if (df_type == "test"){
    #drop the id and the target variable
    df <- subset(df, select= -c(ID_TEST, TYP_FIN))
  }
  #filling missing ST_EMPL
  df$ST_EMPL[df$ST_EMPL==""]<-"U"
  
  return(df)
}