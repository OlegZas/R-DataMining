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

#Goal: to improve model performance with samples. 
#we will use thre different sample methods: bagging, boosting , and random forests.  

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
rpart_model=rpart(Rider~., data = datTrain,control = rpart.control(cp=0.005))
rpart.plot(rpart_model)
 #RIder is a target variable;~. means we will use all the predictors to 
#predict the target variable. 
#We will use training dataset to beuild this decison tree model.
#cp is the complexity parameter. If its lower, the decision tree will be more complex 
#with more decsion and leaf nodes. If higher, than the model will be simpler. 
#We will use this decision tree model to build three sample models and then we will compare 
#the model performance with or without samples. 

#We make prediction on both training and testing data so we can look at model's performance.
predict_train=predict(rpart_model,datTrain)
predict_test=predict(rpart_model,datTest)

#Now we can evaluate the model performance on the testing and training data. 
mmetric(datTrain$Rider,predict_train,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,predict_test,metric = c("ACC","PRECISION","TPR","F1"))


# 8. Bagging based on decision tree model, set cp = 0.005
library(adabag)#first import adabag package that contains BAGGING function. 
set.seed(1)
#next, build the bagging model. 
bagging_model = bagging(Rider~.,data = datTrain, control = rpart.control(cp = 0.005), nbagg = 25) 
#building 25 decision tree models with each decision tree model cp value set to 0.005
#but each of these decision tree models will be different from each other because the input of the traiining dataset
#will be slightly different. So bagging will apply sampling to sample 80% of the datTrain set
#so it will repeat sampling 25 times. These 25 datasets will be slightly different from each other because each 
#tranining dataset is randomly sampled from original training dataset. 
#so at the end we will be using 25 of these decison tree models instad of 1 like we did in task 7. 
predict_train_bagging=predict(bagging_model,datTrain)
predict_test_bagging=predict(bagging_model,datTest)
#For bagging, the prediction result is stored in predict_train_bagging (and test).
mmetric(datTrain$Rider,factor(predict_train_bagging$class),metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,factor(predict_test_bagging$class),metric = c("ACC","PRECISION","TPR","F1"))

# 9. AdaBoost based on decision tree model, set cp = 0.005
#In this case boosting model will build 50 decison tree (different from bagging).
#each decion model will be trained to overcome the weakness of the proceeding decison tree model. 
adaboost_model = boosting(Rider~., data = datTrain, control = rpart.control(cp = 0.005),mfinal = 50)
predict_train_boosting=predict(adaboost_model,datTrain)
predict_test_boosting=predict(adaboost_model,datTest)
mmetric(datTrain$Rider,factor(predict_train_boosting$class),metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,factor(predict_test_boosting$class),metric = c("ACC","PRECISION","TPR","F1"))
#Accuracy for training is very high it implies that the model is somewhat overfit

# 10. Build decision tree model, Bagging, and AdaBoost with cp = 0.02 (the higher cp value will generate simpler decision tree model)
rpart_model=rpart(Rider~., data = datTrain,control = rpart.control(cp=0.002))
rpart.plot(rpart_model)
predict_train=predict(rpart_model,datTrain)
predict_test=predict(rpart_model,datTest)
mmetric(datTrain$Rider,predict_train,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,predict_test,metric = c("ACC","PRECISION","TPR","F1"))
#With the simpler one accurcay on training and testing got lower (than the one in step 7), f measures are also lower.Prediction performance is lower on this one. 

set.seed(1)
bagging_model = bagging(Rider~.,data = datTrain, control = rpart.control(cp = 0.002), nbagg = 25) 
predict_train_bagging=predict(bagging_model,datTrain)
predict_test_bagging=predict(bagging_model,datTest)
mmetric(datTrain$Rider,factor(predict_train_bagging$class),metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,factor(predict_test_bagging$class),metric = c("ACC","PRECISION","TPR","F1"))
#begging model acheived exactly the same performance as the decision tree rpart (with one decion tree). So its not sensative to slightly different training datasets. 
#so it does not provide any advatage over the previous model. 

adaboost_model = boosting(Rider~., data = datTrain, control = rpart.control(cp = 0.002),mfinal = 50)
predict_train_boosting=predict(adaboost_model,datTrain)
predict_test_boosting=predict(adaboost_model,datTest)
mmetric(datTrain$Rider,factor(predict_train_boosting$class),metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,factor(predict_test_boosting$class),metric = c("ACC","PRECISION","TPR","F1"))
#unlike adaboost with 0.005, this one (simpler)does not cause overfit on training.
#This results in better accuracy on testing.
#

# 11. Random forests
#Here we have 500 classifiers. 
library(randomForest)
set.seed(1)
randomforest_model = randomForest(Rider~., data = datTrain, ntree = 500, maxnodes = 200)
randomforest_model
randomforest_model$importance
predict_train_rf=predict(randomforest_model,datTrain)
predict_test_rf=predict(randomforest_model,datTest)
mmetric(datTrain$Rider,predict_train_rf,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,predict_test_rf,metric = c("ACC","PRECISION","TPR","F1"))
#Better performance compared to bagging model. 