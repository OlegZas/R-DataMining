##Data description

'In order for a health insurance company to make money, it needs to collect more
in yearly premiums than it spends on medical care to its beneficiaries. As a result, 
insurers invest a great deal of time and money in developing models that accurately 
forecast medical expenses for the insured population.

Medical expenses are difficult to estimate because the most costly conditions are 
rare and seemingly random. Still, some conditions are more prevalent for certain 
segments of the population. For instance, lung cancer is more likely among smokers 
than non-smokers, and heart disease may be more likely among the obese.

The goal of this analysis is to use patient data to estimate the average medical
care expenses for such population segments. These estimates can be used to create 
actuarial tables that set the price of yearly premiums higher or lower, 
depending on the expected treatment costs.'

### ---------------------------------------------------------------------------
# The insurance data set has 1338 observations of 7 variables.
# We will use this file to predict the medical expenses.

# VARIABLE DESCRIPTIONS:
#age:	      age in years
#sex:	      gender
#bmi:	      body mass index
#children:	how many children do they have?
#smoker:	  do they smoke?
#region:	  geographic region
#expenses:	yearly medical expenses
### ---------------------------------------------------------------------------

# 1. Import the datadet
insurance <- read.csv(file = "insurance.csv", stringsAsFactors = FALSE)

# 2. str() shows the structure of data
str(insurance)

# 3. summary() shows the mean and the five-number statistics indicating the spread of each column's values
summary(insurance)

# 4. Change all categorical variables to factors
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)
str(insurance)
summary(insurance)

#5 Data exploration: some examples
# histogram of insurance expenses
#we want to take a look at the distribution of the dependent variable, 
# Distribution of the expenses variable. 
hist(insurance$expenses)#insurance is the name of the table; Expenses is a dependent variable. 

# exploring relationships among all numeric variables: correlation matrix is used to provide correlation coefficients among different numeric variables.  
# comma means we want all the rows of this dataset 
#on the right side we specify the column numbers that we want. 
#since we need to mention more than one numeric variable, we use combine function. 
cor(insurance[,c(1,3,4,7)])
#for the expenses, the age had the highest correlation, which indicates 
#that age could be more useful when we wnat to predict the patient's expenses.  

#---------

#NExt, to build the prediction model, we first need to split data into 
#training and testing data set. 
# 6. Partition the dataset: 70% for training, 30% for testing   
#first, we load the caret library and set the seed equal to one. 
#then we need to use the partitioning function, to split insurance data as 
#70% training and 30% testing. 
library(caret)
set.seed(1)
train_index <- createDataPartition(insurance$expenses, p=0.7, list=FALSE)
datTrain <- insurance[train_index,]
datTest <- insurance[-train_index,]

# 7. Build a single linear regression model with only bmi as predictor
#Here we need to load the rminer package to provide us the evaluation matrix. 
library(rminer)
#First, we build linear regression model with lm function.
#The input for lm function: 1st specify target variable (expenses) and predictors (in this model we only use bmi value to predict the patient's expenses)
#This model is built on the training dataset. 
lm_model <- lm(expenses~bmi, data = datTrain)
summary(lm_model)

# Make predictions on both training and testing sets
prediction_on_train <- predict(lm_model, datTrain)
prediction_on_test <- predict(lm_model, datTest)

# Generating evaluation metrics on both training and testing data 
#Since these are numeric predictions, we dont look at the accuraccy, precision etc.,(those are for classification problems)
#Instead we use eval metrics for numeric problems. 
mmetric(datTrain$expenses,prediction_on_train,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest$expenses,prediction_on_test,metric=c("MAE","RMSE","MAPE","RAE"))

# 8. Build multiple linear regression model by using all predictors
#Isntead of specifying bmi as a predictor, we place a small dot. 
#The dot indicates that we will use all the predictors to predict customers' expenses. 
#also we change the name for this model. 
lm_model2 <- lm(expenses~., data = datTrain)
summary(lm_model2)

# Make predictions on both training and testing sets
prediction_on_train <- predict(lm_model, datTrain)
prediction_on_test <- predict(lm_model, datTest)

# Generating evaluation metrics on both training and testing data  
mmetric(datTrain$expenses,prediction_on_train,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest$expenses,prediction_on_test,metric=c("MAE","RMSE","MAPE","RAE"))

# 9. Improving Model Performance:  Adding non-linear relationships

# Approach 1: add a higher-order "age" term
#by running this line of code, we add a new variable "age2" into the insurance dataset. 
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
#Bmi30 is a bianry indicator creatd based on BMI value. 
#This funciton has 3 inputs: 1st: condition IF. If bmi greater or equal to 30
#then bmi30 equals to 1, otherwise its zero. helps to see whether the patient is obese or not. 
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

#Partition the dataset again
#we added two new variables, so now there are 9 variables in this dataset.
set.seed(1)
train_index <- createDataPartition(insurance$expenses, p=0.7, list=FALSE)
datTrain2 <- insurance[train_index,]
datTest2 <- insurance[-train_index,]

#Build multiple linear regression model and add interaction effects bmi30*smoker
#change model name and datTest2. Add formula 
lm_model3 <- lm(formula = expenses~. + bmi30 * smoker, data = datTrain2)

# Make predictions on both training and testing sets
prediction_on_train3 <- predict(lm_model3, datTrain2)
prediction_on_test3 <- predict(lm_model3, datTest2)


# Generating evaluation metrics on both training and testing data  
mmetric(datTrain2$expenses,prediction_on_train3,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest2$expenses,prediction_on_test3,metric=c("MAE","RMSE","MAPE","RAE"))


# 10. Regression Tree

# Build a regression tree with cp = 0.0001, maxdepth = 3
library(rpart)
library(rpart.plot)
rpart_model <- rpart(expenses~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 3))
rpart.plot(rpart_model)
rpart_model

# Make predictions on both training and tessting sets
prediction_on_train_rpart <- predict(rpart_model, datTrain)
prediction_on_test_rpart <- predict(rpart_model, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$expenses,prediction_on_train_rpart,metric = c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest$expenses,prediction_on_test_rpart,metric = c("MAE","RMSE","MAPE","RAE"))


# 11. Build a Model Tree with all predictors
#Its a more comlex mode. Model tree incorporates regression model into the decision tree.
#Unlike the regression model, which in this case would be based on the average medical expense value,
#The Model tree will build regression model for each of the leave notes (check above), since there 
#are 8 leaf nodes we will have 8 regression models. So this model more complex. 
library(RWeka)
M5P_model <- M5P(expenses~.,data = datTrain)
M5P_model

# Make predictions on both training and tessting sets
prediction_on_train_M5P <- predict(M5P_model, datTrain)
prediction_on_test_M5P <- predict(M5P_model, datTest)

# Generating evaluation metrics on both training and testing data  
mmetric(datTrain$expenses,prediction_on_train_M5P,metric = c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest$expenses,prediction_on_test_M5P,metric = c("MAE","RMSE","MAPE","RAE"))


# 12. Cross validation (cv) for numeric predictions

# Set up cv parameters
# df: identifies the whole data set by its name
# target: identifies the target variable by its column index in df
# nFolds: indicates the number of folds for cv
# seedVal: carries the seed value for random sampling of instances when creating folds
# prediction_method: indicates the prediction method - e.g., lm
# metric_list: is a list of evaluation metrics that mmetric should generate

library(matrixStats)
library(knitr)
# Training performance for cross validation
cv_function_train <- function(df, target, nFolds, seedVal, prediction_method, metrics_list)
{
  # create folds
  set.seed(seedVal)
  folds = createFolds(df[,target],nFolds) 
  # perform cross validation
  cv_results <- lapply(folds, function(x)
  { 
    test_target <- df[x,target]
    test_input  <- df[x,-target]
    
    train_target <- df[-x,target]
    train_input <- df[-x,-target]
    
    prediction_model <- prediction_method(train_target~.,train_input) 
    pred<- predict(prediction_model,train_input)
    return(mmetric(train_target,pred,metrics_list))
  })
  # generate means and sds and show cv results, means and sds using kable
  cv_results_m <- as.matrix(as.data.frame(cv_results))
  cv_mean<- as.matrix(rowMeans(cv_results_m))
  cv_sd <- as.matrix(rowSds(cv_results_m))
  colnames(cv_mean) <- "Mean"
  colnames(cv_sd) <- "Sd"
  cv_all <- cbind(cv_results_m, cv_mean, cv_sd)
  kable(t(cv_all),digits=2)
}

# Testing performance for cross validation
cv_function_test <- function(df, target, nFolds, seedVal, prediction_method, metrics_list)
{
  # create folds
  set.seed(seedVal)
  folds = createFolds(df[,target],nFolds) 
  # perform cross validation
  cv_results <- lapply(folds, function(x)
  { 
    test_target <- df[x,target]
    test_input  <- df[x,-target]
    
    train_target <- df[-x,target]
    train_input <- df[-x,-target]
    
    prediction_model <- prediction_method(train_target~.,train_input) 
    pred<- predict(prediction_model,test_input)
    return(mmetric(test_target,pred,metrics_list))
  })
  # generate means and sds and show cv results, means and sds using kable
  cv_results_m <- as.matrix(as.data.frame(cv_results))
  cv_mean<- as.matrix(rowMeans(cv_results_m))
  cv_sd <- as.matrix(rowSds(cv_results_m))
  colnames(cv_mean) <- "Mean"
  colnames(cv_sd) <- "Sd"
  cv_all <- cbind(cv_results_m, cv_mean, cv_sd)
  kable(t(cv_all),digits=2)
}

# 3-fold cv with lm prediction method
df <- insurance
target <- 7
nFolds <- 3
seedVal <- 1
prediction_method <- lm
metrics_list <- c("MAE","RMSE","MAPE","RAE")
cv_function_train(df, target, nFolds, seedVal, prediction_method, metrics_list)
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)

# 10-fold cv with M5P prediction method
df <- insurance
target <- 7
nFolds <- 10
seedVal <- 1
prediction_method <- M5P
metrics_list <- c("MAE","RMSE","MAPE","RAE")
cv_function_train(df, target, nFolds, seedVal, prediction_method, metrics_list)
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)


