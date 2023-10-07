### -------------------------------------------------------------------------------
# You have been given a data file by the San Francisco Bay Area Rapid Transit (BART), 
# which identifies a set of demographics for residents in a local area. We will use 
# this file to determine residents segmentation so that we can use it to develop marketing
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
#Note: kmeans function will not create dummy variables automatically. 
#
BartRider$DualInc <- ifelse(BartRider$DualInc == 'Y', 1, 0)
#yes = 1, otherwise = 0 
BartRider$Gender <- ifelse(BartRider$Gender == 'F', 1, 0)
#female = 1, otherwise = 0 
#for language there are 3 different variables: 
  #English, Spanish, Other 
  #So we need to create 2 dummy variables. 
BartRider$Language_English <- ifelse(BartRider$Language == 'English', 1, 0)
#^ means if language is English then value = 1, otherwise zero
BartRider$Language_Spanish <- ifelse(BartRider$Language == 'Spanish', 1, 0)
#^ means if language is Spanish then value = 1, otherwise zero
BartRider$Language <- NULL
#^We remove the orginal language variable by setting it to NULL
#Below: for own rent variable also 3 vlaues: owns the house, with parent, rents 
BartRider$OwnRent_own <- ifelse(BartRider$OwnRent == 'Own', 1, 0)
BartRider$OwnRent_Parent <- ifelse(BartRider$OwnRent == 'Parent', 1, 0)
BartRider$OwnRent <- NULL
BartRider$Rider <- ifelse(BartRider$Rider == 'Yes', 1, 0)
str(BartRider)
summary(BartRider)

# 5. k-mean clustering with number of cluster k = 2
#1. k-mean is in the rweka package
library(RWeka)
#N=2, means we are creating 2 clusters using the BartRider data set. 
#this is unsupervised learning, so there is not testing and training 
# AND there is NO Target Variable. In contrast, in supervised learning where you 
#first identify whats the target variable, then split the data into trainig and testing 
#data set, and then do predictions. 
BartRider_clustering <- SimpleKMeans(BartRider, Weka_control(N=2))

# show the results of 2 clusters
BartRider_clustering
#^will show three columns: 
#1.first columns shows average value of every variable on the whole dataset,
#2. Mean of average value for each variable for Cluster 0
#3. Mean for Cluster 1. 

# What is the size and centroids of each cluster?
#CLuster 0: size is 3437
#CLuster 1: size is 2056
      #Centroids: 
                          #Cluster0       Cluster 1          
     #                   (3437.0)       (2056.0)
#========================================================
#Age                       4.5732     2.4472
#DistToWork               11.5362     11.432
#DualInc                    0.4671     0.0316
#Education                  4.4145     3.3567
#Gender                     0.6247     0.4565
#Income                     6.9712      3.438
#NbrInHouseHold             2.7054     3.0956
#NbrInHouseholdUnder18     0.5963     0.8128
#YrsInArea                 4.5063     4.0885
#Rider                      0.1617     0.6824
#Language_English           0.9563     0.8753
#Language_Spanish          0.0295     0.0799
#OwnRent_own               0.8794     0.0128
#OwnRent_Parent              0     0.4984

#
# What is the percentage of BART riders for cluster 0 and 1?
# 16.17% are riders in cluster 0 
#68.24% arer riders in cluster 1

# Use the attributes information of cluster centers to understand the profile of residents. Interpret each cluster based on your understanding.
#Cluster 0: 
    #They are working professionals who have a higher level of education, and higher income 
#when compared to cluster 1; most of them own their own house. They are also older
#Cluster 1: Students; live with their parnets, lower income, most of them are riders. 

# 6. k-mean clustering with number of cluster k = 3
BartRider_clustering2 <- SimpleKMeans(BartRider, Weka_control(N=3))


# show the results of 3 clusters
BartRider_clustering2

# What is the size and centroids of each cluster?
#Cluster                        0          1          2
#       Size                (2368.0)   (1210.0)   (1915.0)
#===================================================================
#  Age                      4.6917     3.2603     2.1311
#DistToWork                 11.5051    11.5231    11.4298
#DualInc                    0.4472     0.1694     0.0397
#Education                   4.4054     4.1347     3.0475
#Gender                      0.5904     0.4893     0.5055
#Income                      6.9949     5.6868     2.5598
#NbrInHouseHold              2.7758     2.2653       3.47
#NbrInHouseholdUnder18       0.6326     0.3702     1.0125
#YrsInArea                   4.5743     3.9256     4.1749
#Rider                       0.1858     0.0149     0.9901
#Language_English            0.951      0.9612     0.8407
#Language_Spanish            0.0329     0.0256     0.1018
#OwnRent_own                 0.9996          0     0.0125
#OwnRent_Parent              0          0.1157     0.6595


# Assign a meaningful name to each cluster based on the representative profile of residents in each cluster. Explain the reasons for the name you choose.
#CLuster 0: Older professional, rich, own housees, some riders 
#Cluster 1:Mid age, trade school, midlle class, no time to ride
#Cluster 2:Students, younger, poor, rent, a lot of riders   


# Based on the cluster size and centroids, which k (k=2 or k=3) you will use, and why?
#I would definately go with k=3. It lets us clearly identify the segmetn that almost entirely consists 
#of riders. That is very important because this is clearly a primary target segment. I 


# If we segment residents into 3 clusters, what marketing plans you can use to target each cluster?
#Cluster 0:more sophisticated, exclusive services; they have money and would be intrested in better quality of service
#Cluster 1:advertise that they could drink when they go out, since company would get them home safely
#Cluster 2: a lot of promotions, like coupons or discounts, they have lower inocmes and would be 
#attracted by sales. 


# 7. elbow test/scree plot to determine optimal number of clusters.
# Step 1: initial a vector contains a list of k values from 1 to 10.
a_list_of_k = c(1:10)
#^here we determine the number of k not based on results, but rather on 
#within cluster's squared error. 

# Step 2: initial a vector that contains errors
errors = vector()
#^we define an error vector; this error will save within cluster's squared error
# Step 3: create a for loop: for each k in the list of k (from 1 to 10)
# We run a simpleKMeans model based on that k and store the value in errors vector.
for (k in a_list_of_k) {
  model <- SimpleKMeans(BartRider, Weka_control(N=k))
  errors[k] = model$clusterer$getSquaredError()
  print(errors[k])
}#^ we do a loop to generate within clusters squared error. 

#Step 4: Plot the elbow plot: x axis = k and y axis = errors
plot(x = a_list_of_k, y = errors, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares Error", type="b")


# What is the optimal cluster number?
#k=3, the benefits of increased number of k become smaller and smaller after k=3

# 8. Classification with clustering:
#Doing classification on the whole dataset without clustering:
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
#We just directly did crossvalidaiton, thats why we did not need to split the dat into trainig and testing. 
df <- BartRider
target <- 10
nFolds <- 10
seedVal <- 1
prediction_method <- C5.0
metrics_list <- c("ACC","PRECISION","TPR","F1")
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)
#Here wedid 10 folds crossvalidation to predict whether or not a rider is a rider. 
#     |   ACC|    PRECISION1|  PRECISION2|  TPR1|  TPR2|   F11|   F12|
#|Mean   | 89.20|      90.61|      87.53|   90.60| 87.34| 90.57| 87.36|

#Clustering will segment the riders into different groups. Those riders within each 
#group are very similar to each other, while the riders between different groups will not be 
#similar to each other. 
#If we can build the classification model for each cluster, means we first segment the data set into differetnt 
#subsets; if we set k=3, we would have 3 subsets. 
#this way, with classification model, we wold probably do better, because in each subset those riders would be most similar to each otoher. 

#If the riders in groups are similar to each to each other (with clustering), it would be easier for classificatino model to 
#do the prediction. 

# Classification with Clustering
#   first we do the clustering with k=3
BartRider_clustering3 <- SimpleKMeans(BartRider[,-10], Weka_control(N=3))
BartRider_clustering3
BartRider$class_ids = BartRider_clustering3$class_ids
#then we generate 3 subsets; the number of riders in each subset will be shown on the right in the Environment.
BartRider1 = BartRider[which(BartRider$class_ids==0),]
BartRider2 = BartRider[which(BartRider$class_ids==1),]
BartRider3 = BartRider[which(BartRider$class_ids==2),]
str(BartRider1)
str(BartRider2)
str(BartRider3)

#For each subset, we build a classifier 
#we use the same classification function, but the dtaframe will differ, 
#for first one we use first subset Bartrider 1, then 2nd, then 3d.
df <- BartRider1
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)
df <- BartRider2
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)
df <- BartRider3
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)

# Does the overall classification performance get better by applying clustering, why?

#for overall performance we check accuracy; 
#First check accuracy for crossvalidaiton without clustering: 89 (mean).
#then cluster 1: 92.5; 
#clusteter 2: 90.8;
#clsuter 3: 82.77

#This means, for customers in cluster 1 and 2, we can use classifiers that we built on those small subsets; 
#becasue they provide better performance than classifier on the whole dataset;
#BUT for the riders in 3d clsuter, its better to use classifier build on the whole dataset, because 
#it provides better accuracy


