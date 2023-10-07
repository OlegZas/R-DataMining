### -------------------------------------------------------------------------------
# You have been given a data file by the San Francisco Bay Area Rapid Transit (BART), 
# which identifies a set of demographics for residents in a local area. We will use 
# this file to determine residents segmentations so that we can use it to develop marketing
# plans accordingly.

# VARIABLE DESCRIPTIONS:
# Age:  1. 14 thru 17; 
#       2. 18 thru 24 
#       3. 25 thru 34
#       4. 35 thru 44
#       5. 45 thru 54
#       6. 55 thru 64
#       7. 65 and Over
# DistToWork: Distance to work in miles
# DualInc: Is dual income household or not
# Education:  1. Grade 8 or less
#             2. Grades 9 to 11
#             3. Graduated high school
#             4. 1 to 3 years of college
#             5. College graduate
#             6. Grad Study
# Gender:	M or F
# Income: 1. Less than $10,000
#         2. $10,000 to $14,999
#         3. $15,000 to $19,999
#         4. $20,000 to $24,999
#         5. $25,000 to $29,999
#         6. $30,000 to $39,999
#         7. $40,000 to $49,999
#         8. $50,000 to $74,999
#         9. $75,000 or more
# Language:	Language spoken at home
# NbrInHouseHold:	Number in household
# NbrInHouseholdUnder18:	Number in household under 18 years old
# OwnRent:	Own, rent, or live with parents
# YrsInArea:	Years lived in bay area
# Rider:	No, Non-rider; Yes, Rider
### ------------------------------------------------------------------------------


# 1. Import the dataset
BartRider <- read.csv(file = "BartRider.csv", stringsAsFactors = FALSE)

# 2.	Show the overall structure and summary of the input data.
str(BartRider)
summary(BartRider)

# 3.	Transform DualInc, Gender, Language, OwnRent, and Rider to factor variables. Show the overall structure and summary of the input data again.
BartRider$DualInc <- factor(BartRider$DualInc)
BartRider$Gender <- factor(BartRider$Gender)
BartRider$Language <- factor(BartRider$Language)
BartRider$OwnRent <- factor(BartRider$OwnRent)
BartRider$Rider <- factor(BartRider$Rider)
str(BartRider)
summary(BartRider)

# 5. Partition the dataset: 70% for training, 30% for testing   
library(caret)
set.seed(1)
train_index <- createDataPartition(BartRider$Rider, p=0.7, list=FALSE)
datTrain <- BartRider[train_index,]
datTest <- BartRider[-train_index,]

# 6. Check the rows and proportion of target variable for both training and testing datasets
nrow(datTrain)
nrow(datTest)
prop.table(table(datTrain$Rider))
prop.table(table(datTest$Rider))

# 7.Build decision tree model with rpart model, set cp = 0.005
library(rpart)
library(rminer)
library(rpart.plot)
rpart_model <- rpart(Rider~., data = datTrain,control = rpart.control(cp = 0.005))
rpart.plot(rpart_model)

predict_train <- predict(rpart_model,datTrain)
predict_test <- predict(rpart_model,datTest)
mmetric(datTrain$Rider,predict_train, metric = c("ACC","TPR","PRECISION","F1")) 
mmetric(datTest$Rider,predict_test, metric = c("ACC","TPR","PRECISION","F1")) 

# 8. Bagging based on decision tree model, set cp = 0.005
library(adabag)
set.seed(1)
bagging_model <- bagging(Rider~ ., data = datTrain, control = rpart.control(cp = 0.005), nbagg = 25)

bagging_predict_train <- predict(bagging_model,datTrain)
bagging_predict_test <- predict(bagging_model,datTest)
mmetric(datTrain$Rider,factor(bagging_predict_train$class), metric = c("ACC","TPR","PRECISION","F1")) 
mmetric(datTest$Rider,factor(bagging_predict_test$class), metric = c("ACC","TPR","PRECISION","F1")) 

# 9. AdaBoost based on decision tree model, set cp = 0.005
M1_model <- boosting(Rider~., data = datTrain, control = rpart.control(cp = 0.005), mfinal=50)

AdaBoost_predict_train <- predict(M1_model,datTrain)
AdaBoost_predict_test <- predict(M1_model,datTest)
mmetric(datTrain$Rider,factor(AdaBoost_predict_train$class), metric = c("ACC","TPR","PRECISION","F1")) 
mmetric(datTest$Rider,factor(AdaBoost_predict_test$class), metric = c("ACC","TPR","PRECISION","F1")) 


# 10. Build decision tree model, Bagging, and AdaBoost with cp = 0.02 (the higher cp value will generate simpler decision tree model)
rpart_model <- rpart(Rider~., data = datTrain,control = rpart.control(cp = 0.02))
rpart.plot(rpart_model)
predict_train <- predict(rpart_model,datTrain)
predict_test <- predict(rpart_model,datTest)
mmetric(datTrain$Rider,predict_train, metric = c("ACC","TPR","PRECISION","F1")) 
mmetric(datTest$Rider,predict_test, metric = c("ACC","TPR","PRECISION","F1")) 

set.seed(1)
bagging_model <- bagging(Rider~ ., data = datTrain, control = rpart.control(cp = 0.02), nbagg = 25)
bagging_predict_train <- predict(bagging_model,datTrain)
bagging_predict_test <- predict(bagging_model,datTest)
mmetric(datTrain$Rider,factor(bagging_predict_train$class), metric = c("ACC","TPR","PRECISION","F1")) 
mmetric(datTest$Rider,factor(bagging_predict_test$class), metric = c("ACC","TPR","PRECISION","F1")) 

M1_model <- boosting(Rider~., data = datTrain, control = rpart.control(cp = 0.02), mfinal=50)
AdaBoost_predict_train <- predict(M1_model,datTrain)
AdaBoost_predict_test <- predict(M1_model,datTest)
mmetric(datTrain$Rider,factor(AdaBoost_predict_train$class), metric = c("ACC","TPR","PRECISION","F1")) 
mmetric(datTest$Rider,factor(AdaBoost_predict_test$class), metric = c("ACC","TPR","PRECISION","F1")) 


# 11. Random forests
library(randomForest)
set.seed(1)
rf_model <- randomForest(Rider~., data = datTrain, ntree = 500, maxnodes=200)
rf_model
rf_model$importance

rf_predict_train <- predict(rf_model,datTrain)
rf_predict_test <- predict(rf_model,datTest)
mmetric(datTrain$Rider,rf_predict_train, metric = c("ACC","TPR","PRECISION","F1")) 
mmetric(datTest$Rider,rf_predict_test, metric = c("ACC","TPR","PRECISION","F1")) 



 



