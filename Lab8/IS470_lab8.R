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


# 1. Import the datadet
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

# 4. Create dummy variables for factors
BartRider$DualInc <- ifelse(BartRider$DualInc == 'Y', 1, 0)
BartRider$Gender <- ifelse(BartRider$Gender == 'F', 1, 0)
BartRider$Language_English <- ifelse(BartRider$Language == 'English', 1, 0)
BartRider$Language_Spanish <- ifelse(BartRider$Language == 'Spanish', 1, 0)
BartRider$Language <- NULL
BartRider$OwnRent_own <- ifelse(BartRider$OwnRent == 'Own', 1, 0)
BartRider$OwnRent_Parent <- ifelse(BartRider$OwnRent == 'Parent', 1, 0)
BartRider$OwnRent <- NULL
BartRider$Rider <- ifelse(BartRider$Rider == 'Yes', 1, 0)
str(BartRider)
summary(BartRider)

# 5. k-mean clustering with number of cluster k = 2
library(RWeka)
BartRider_clustering <- SimpleKMeans(BartRider, Weka_control(N=2))

# show the results of 2 clusters
BartRider_clustering

# What is the size and centroids of each cluster?


# What is the percentage of BART riders for cluster 0 and 1?


# Use the attributes information of cluster centers to understand the profile of residents. Interpret each cluster based on your understanding.



# 6. k-mean clustering with number of cluster k = 3


# show the results of 3 clusters


# What is the size and centroids of each cluster?



# Assign a meaningful name to each cluster based on the representative profile of residents in each cluster. Explain the reasons for the name you choose.



# Based on the cluster size and centroids, which k (k=2 or k=3) you will use, and why?



# If we segment residents into 3 clusters, what marketing plans you can use to target each cluster?



# 7. elbow test/scree plot to determine optimal number of clusters.
# Step 1: initial a vector contains a list of k values from 1 to 10.
a_list_of_k = c(1:10)
# Step 2: initial a vector that contains errors
errors = vector()
# Step 3: create a for loop: for each k in the list of k (from 1 to 10)
# We run a simpleKMeans model based on that k and store the value in errors vector.
for (k in a_list_of_k) {
  model <- SimpleKMeans(BartRider, Weka_control(N=k))
  errors[k] = model$clusterer$getSquaredError()
  print(errors[k])
}
#Step 4: Plot the elbow plot: x axis = k and y axis = errors
plot(x = a_list_of_k, y = errors, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares Error", type="b")


# What is the optimal cluster number?



# 8. Classification with clustering:

# Classification without clustering
BartRider$Rider <- as.factor(BartRider$Rider)
str(BartRider)

# Testing performance for cross validation
library(matrixStats)
library(knitr)
library(C50)
library(rminer)
library(caret)
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

df <- BartRider
target <- 10
nFolds <- 10
seedVal <- 1
prediction_method <- C5.0
metrics_list <- c("ACC","PRECISION","TPR","F1")
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)


# Classification with Clustering
BartRider_clustering3 <- SimpleKMeans(BartRider[,-10], Weka_control(N=3))
BartRider_clustering3
BartRider$class_ids = BartRider_clustering3$class_ids
BartRider1 = BartRider[which(BartRider$class_ids==0),]
BartRider2 = BartRider[which(BartRider$class_ids==1),]
BartRider3 = BartRider[which(BartRider$class_ids==2),]
str(BartRider1)
str(BartRider2)
str(BartRider3)

df <- BartRider1
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)
df <- BartRider2
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)
df <- BartRider3
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)

# Does the overall classification performance get better by applying clustering, why?




