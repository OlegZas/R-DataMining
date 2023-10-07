### -------------------------------------------------------------------------------
#Market basket analysis is used behind the scenes for the recommendation systems 
#used in many brick-and-mortar and online retailers. The learned association rules 
#indicate the combinations of items that are often purchased together. Knowledge 
#of these patterns provides insight into new ways a grocery chain might optimize 
#the inventory, advertise promotions,or organize the physical layout of the store. 
#For instance, if shoppers frequently purchase coffee or orange juice with a breakfast 
#pastry, it may be possible to increase profit by relocating pastries closer to coffee 
#and juice.

#In this lab, we will perform a market basket analysis of transactional data 
#from a grocery store.Our market basket analysis will utilize the purchase data 
#collected from one month of operation at a real-world grocery store. The data contains 
#9,835 transactions.
### ------------------------------------------------------------------------------

# 1. Import groceries.csv file
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")

# 2. Understanding of your data.
# Summary of dataset
summary(groceries)

# Inspect the first 5 transactions
inspect(groceries[1:5])

# How many transactions and items in this data?
#9835 transactions 
#169 items 
#also summary shows most frequent items. 
#Also 2159 transactions include only 1 item. 
#1643 transactions include 2 items. Transaction with the largest number of items includes 32 items.

# 3. Data exploration
# Examine the relative frequency of items in descending order
sort(itemFrequency(groceries, type="relative"), decreasing = TRUE)

# What are the top 3 most frequent items?
#Top three most frequent: Whole milk, other vegetables, and rolls/bunns. 

# What are the support values for top 3 most frequent items?

# whole milk          other vegetables 
#0.2555160142              0.1934926284 
#rolls/buns                      
#0.1839349263

#The relative frequency shows the percentage of transactions which include the corresponding item.
# Examine the absolute frequency of items in descending order
sort(itemFrequency(groceries, type="absolute"), decreasing = TRUE)
#Absolute frequency shows the number of transactions which include the corresponding item.
#For example, 2513 transactions include whole milk. 

# Plot the most frequent 8 items in the descending order of transaction frequency in percentage
itemFrequencyPlot(groceries, type="relative", topN = 8)

# 4. Use the apriori command to generate rules with minimal support = 0.01 and minimal confidence = 0.3 and max length = 2.
groceries_rules <- apriori(groceries, parameter = list(support = 0.01, confidence = 0.3, maxlen=2))
summary(groceries_rules)
#for unsupervised leanring we dont make preditioncs, we use the whole data set. 
# Display all rules sorted by confidence levels.
inspect(sort(groceries_rules, by = "confidence"))

# Display top 5 rules
inspect(sort(groceries_rules, by = "confidence")[1:5])

# What is the probability of buying whole milk when butter is purchased?
#probability that butter is purchased and then whole milk is 49.72%. We look at confidence value in this case. 

# What is the probability of buying butter and whole milk together?
####
#In this case we look at support value. The probability of buying these two together is 2.755%


# Interpret the first rule based on the values of the support, confidence, and lift.
###
#1. Support value: the probability of buying butter and whole milk together is 0.027. Or 2.755% of 
#transactions include butter AND whole milk. 
#Confidence: If the customer bought butter, there is 49.7% probability that he bought whole milk.Also, among all the 
#transaction WHICH INCLUDE butter, 49.7% also include whole milk. 
#Lift value: It is greater than 1 (1.94)which means that butter and whole milk positively affect each other.The probalbity of buying whole milk
#is increased by 1.94 times if customer bought butter (same for probability of butter).


# 5. Use the apriori command to  generate rules with minimal support = 0.02 and minimal confidence = 0.4 and max length = 3.
groceries_rules2 <- apriori(groceries, parameter = list(support = 0.02, confidence = 0.4, maxlen=3))
summary(groceries_rules2)

# Display top 10 rules for Task 2 sorted by lift.
inspect(sort(groceries_rules2, by = "lift")[1:10])

# Find and display rules containing "other vegetables"
vegetables_rules <- subset(groceries_rules2,items %in% "other vegetables")
inspect(vegetables_rules)
# Find and display rules containing "other vegetables" on the left-hand side
vegetables_rules_l <- subset(groceries_rules2,lhs %in% "other vegetables")
inspect(vegetables_rules_l)

# Interpret the first rule (containing "other vegetables" on the left-hand side) based on support, confidence, and lift values.
#[1] {other vegetables,root vegetables} => {whole milk} 0.02318251 0.4892704 
#    coverage   lift     count
#[1] 0.04738180 1.914833 228 
#aNSwer###### 
#Support: The probablity of buying other vegetables, root vegetables, and whole milk TOGETHER is 2.31%. OR
#2.31% of all transactions include other vegetables, root vegetables, and whole milk. 
#Confidence: If the customer bought other vegetables AND root vegetables there is 48.9% probablity that he/she bought whole milk. 
####Alternative interpretations: Among all the transactions that include other veggies and yougurt, 48.9% also include whole milk. 
#lift: lift value is greater than 1, it means that there is a positive relationship between veggies and whole milk. The probability of buying 
#whole milk is increased by 1.91 times if customer bought veggies, and vice versa. 

# Find and display rules containing "other vegetables" on the right-hand side 
vegetables_rules_r <- subset(groceries_rules2,rhs %in% "other vegetables")
inspect(vegetables_rules_r)

# 6. Use the apriori command to generate about 30 to 50 association rules. Set your own minimum support and confidence threshold levels. 
# Remember if the thresholds are too low, you will get too many rules, or if you set them too high, you may not get any or enough rules.
groceries_rules3 <- apriori(groceries, parameter = list(support = 0.025, confidence = 0.21, maxlen=2))
summary(groceries_rules3)
#39 rules

# Inspect all of the rules in the descending lift values of the rules.
inspect(sort(groceries_rules3, by = "lift"))

# Select an interesting rule and explain how it can benefit the grocery store.

#[3]  {whipped/sour cream}    => {other vegetables} 0.02887646 0.4028369  0.07168277
#[3]  2.0819237  284 

#Alhtough rule one has the highest lift value, this rule (as well as rule 2)contains only vegetables.
#It is logical that when custoemr shop for vegetables they are more likely to buy different kinds, so there is no surprise.
#however, I think think rule 3 is much more interesting. On the left hands side it has whipped/sour cream and on the right other vegetables. 
#This ruel has a pretty high Lift Value which means that two of these items positively affect each other. In fact, 
#The probablity that the customer will buy one of them after buyig the other is increased by 2.08 times. 
#Moreover, this rule also has a relatively high confidence, 0.402. It means that if the customer buys whipped/sour cream there is 40.2% probablity that 
#he will buy other vegetables. The support value in this case shows that there is 2.88% probability of buying whipped/sour cream and other veggies together. 

#The store could use this information to strategically place items. These two items should probably not be placed together due to low support. However, I think other veggies should be 
#placed somewhere ahead of sour cream (on the customer's route). The customer would buy sour cream, then see other veggies and get some as well. 
#Also, retailers of other veggies could advertise their products after advertisements of sour cream. 

