library(stats)
library(lattice)
library(caret)
library(e1071)
library(carData)
library(car)
library(pROC)
library(dplyr)
library(Hmisc)
library(tidyr)
library(tidyverse)

dfSTR <- read.csv("listingsExitedMarket.csv", na.strings = c("NA", ""))
str(dfSTR)

dfSTR$host_is_superhost <- factor(dfSTR$host_is_superhost)
dfSTR$neighbourhood_cleansed <- factor(dfSTR$neighbourhood_cleansed)
dfSTR$month <- factor(dfSTR$month)
dfSTR$book_month <- factor(dfSTR$book_month)
dfSTR$book_year <- factor(dfSTR$book_year)
dfSTR$arch_year <- factor(dfSTR$arch_year)
dfSTR$arch_month <- factor(dfSTR$arch_month)
dfSTR$exitMarket <- factor(dfSTR$exitMarket)

drop <- c('host_id','latitude','longitude','reviewer_id','superHostInd','book_year',
          'review_scores_accuracy','review_scores_cleanliness','review_scores_checkin',
          'review_scores_communication', 'review_scores_location', 'review_scores_value',
          'arch_year','arch_month')
  
dfSTR <- dfSTR[, !names(dfSTR) %in% drop]

vif(glm(formula=exitMarket~host_is_superhost + neighbourhood_cleansed + month
         + price + host_listings_count + bedrooms + minimum_nights + maximum_nights 
        + number_of_reviews + review_scores_rating + reviews_per_month + review_scores_cleanliness
          + review_scores_location + arch_year  ,  family = binomial(link='logit'),data = dfSTR, singular.ok = TRUE
        ))



trainIndex <- createDataPartition(dfSTR$exitMarket,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

#Create Training Data
dfSTR.train <- dfSTR[trainIndex,]

#Create Validation Data
dfSTR.valid <-dfSTR[-trainIndex,]

#Run a very simple baseline model with the training dataset
regressionSTR.model <- train(exitMarket~host_is_superhost + neighbourhood_cleansed + month
                             + price + host_listings_count + bedrooms + minimum_nights + maximum_nights 
                             + number_of_reviews + review_scores_rating + reviews_per_month + review_scores_cleanliness
                             + review_scores_location + arch_year,
                             data=dfSTR.train,
                             method='glm',
                             family='binomial',
                             na.action=na.pass)

#View the model results
summary(regressionSTR.model)

#Evaluation model performance using the validation dataset
prediction <- predict(regressionSTR.model,newdata=dfSTR.valid)

#Confusion Matrix
confusionMatrix(prediction,dfSTR.valid$Enroll)


#Predicting probabilities from the logistic regression model
pred.probabilities <- predict(regressionSTR.model,newdata=dfSTR.valid,type='prob')

#Creating ROC curve from the logistic regression model
regressionSTR.ROC <- roc(predictor=pred.probabilities$`1`,
                         response=dfSTR.valid$Enroll,
                         levels=levels(dfSTR.valid$Enroll))
#Plotting the ROC
plot(regressionSTR.ROC)

#Area under the Curve for Tree Model
regressionSTR.ROC$auc















