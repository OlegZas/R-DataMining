#####IS 470 Data Exploration---------------------------------------------------------

# Part 1: Pokemon Data
### ---------------------------------------------------------------------------
# This dataset contains information on 800 Pokemon from six generations of Pokemon.
# VARIABLE DESCRIPTIONS:
#number: The entry number of the Pokemon
#name: The English name of the Pokemon
#type1: The Primary Type of the Pokemon
#type2: The Secondary Type of the Pokemon
#hp: The Base HP of the Pokemon
#attack: The Base Attack of the Pokemon
#defense: The Base Defense of the Pokemon
#sp.atk: The Base Special Attack of the Pokemon
#sp.def: The Base Special Defense of the Pokemon
#speed: The Base Speed of the Pokemon
#generation: The numbered generation which the Pokemon was first introduced
#legendary: Denotes if the Pokemon is legendary.
### ---------------------------------------------------------------------------

### 1. Import and clean data

# Import data from csv
pokemon <- read.csv(file = "pokemon.csv", stringsAsFactors = FALSE)

# str() shows the structure of data
str(pokemon)

# examine the number of rows and cols
nrow(pokemon)
ncol(pokemon)

# Show the head and tail rows of a data frame
head(pokemon)
pokemon[1:6,]
head(pokemon, n=1)
tail(pokemon)

# summary() shows the mean and the five-number statistics indicating the spread of each column's values
summary(pokemon)

# Remove unique identifiers (pokemon number and Name) from further analysis.
pokemon <- pokemon[,c(-1,-2)]

# Change categorical variables to factors
str(pokemon)
pokemon$Type.1 <- factor(pokemon$Type.1)
pokemon$Type.2 <- factor(pokemon$Type.2)
pokemon$Generation <- factor(pokemon$Generation)
pokemon$Legendary <- factor(pokemon$Legendary)
str(pokemon)
summary(pokemon)
summary(pokemon$Type.1)

# set missing values in Type.2 as none
levels(pokemon$Type.2)
levels(pokemon$Type.2)[1]
levels(pokemon$Type.2)[1] <- "none"
str(pokemon)
summary(pokemon)

### 2. understanding a single variable: numeric variables

# Show summary of one or more columns
summary(pokemon$Attack)
summary(pokemon[,c("Attack", "Defense")])
summary(pokemon$Sp..Atk + pokemon$Sp..Def)
which(pokemon$Sp..Atk + pokemon$Sp..Def == 340)

# obtain the mean, median, and range of a numeric variable
mean(pokemon$Attack)
median(pokemon$Attack)
range(pokemon$Attack)

# use quantile to calculate the five-number summary for Attack
quantile(pokemon$Attack)

# IQR
IQR(pokemon$Attack)

# boxplot of numeric variables
boxplot(pokemon$Attack, main="Boxplot of Attack in the pokemon data set", ylab="Attack")
boxplot(pokemon$Defense, main="Boxplot of Defense in the pokemon data set", ylab="Defense")
boxplot(pokemon[which(pokemon$Generation==1),4], main="Boxplot of Attack of the 1st generation pokemon", ylab="Attack")
boxplot(pokemon[which(pokemon$Generation==1),5], main="Boxplot of Defense of the 1st generation pokemon", ylab="Defense")

# histograms of a numeric variable
hist(pokemon$Attack, main = "Histogram of Attack in the pokemon data set", xlab = "Attack")
hist(pokemon$Defense, main = "Histogram of Defense in the pokemon data set", xlab = "Defense")
hist(pokemon$HP, main = "Histogram of HP in the pokemon data set", xlab = "HP")

# variance and standard deviation of a numeric varaible
var(pokemon$Attack)
sd(pokemon$Attack)


### 3. Exploring categorical variables

# Summary of categorical variable
summary(pokemon$Type.1)
nlevels(pokemon$Type.1)

# Plot categorical variable
plot(pokemon$Type.1, main = "Plot of Type.1 in the pokemon data set", xlab = "Type.1")
table(pokemon$Type.1)
sort(table(pokemon$Type.1))

# Run prop.table
Type_table = table(pokemon$Type.1)
prop.table(Type_table)


### 4. Understand relationships of multiple variables

# scatter plot: two numeric variables
plot(pokemon$Attack, pokemon$Defense)

# Generate correlation coefficients of two numeric variables in a 2x2 matrix
# cor(X,Y) lies between -1 and 1. zero means no correlation. 1 or -1 indicates full correlation
# positive value means positive correlation and negative values mean negative relationships
cor(pokemon[,c("Attack", "Defense")])
cor(pokemon[,c(4,5)])

# Generate the correlation matrix of all numeric variables
cor(pokemon[,3:8])

# Generate 2D scatter plots
pairs(pokemon[,3:8])

## Examine relationships between numeric variables and factors
# boxplot groups values of a numeric variable based on the values of a factor
boxplot(Attack~Type.1, data = pokemon)
boxplot(Attack~Type.1, data = pokemon[which(pokemon$Legendary=='True'),])
boxplot(Attack~Type.1, data = pokemon[which(pokemon$Legendary=='False'),])
boxplot(HP~Legendary, data = pokemon)
boxplot(HP~Type.1, data = pokemon)


# Part 2: CarAuction Data
### ---------------------------------------------------------------------------
# This dataset contains information of cars purchased at the Auction.
# VARIABLE DESCRIPTIONS:
#Auction: Auction provider at which the  vehicle was purchased
#Color: Vehicle Color
#IsBadBuy: Identifies if the kicked vehicle was an avoidable purchase
#MMRCurrentAuctionAveragePrice: Acquisition price for this vehicle in average condition as of current day
#Size: The size category of the vehicle (Compact, SUV, etc.)
#TopThreeAmericanName:Identifies if the manufacturer is one of the top three American manufacturers
#VehBCost: Acquisition cost paid for the vehicle at time of purchase
#VehicleAge: The Years elapsed since the manufacturer's year
#VehOdo: The vehicles odometer reading
#WarrantyCost: Warranty price (term=36month  and millage=36K)
#WheelType: The vehicle wheel type description (Alloy, Covers)
### ---------------------------------------------------------------------------

### 1. Import and clean data

# Import data from csv
carAuction <- read.csv(file = "carAuction.csv", stringsAsFactors = FALSE)

# str() shows the structure of data


# summary() shows the mean and the five-number statistics indicating the spread of each column's values


# Change all categorical variables to factors



### 2. understanding a single variable: numerical variables

# Show summary of VehOdo


# obtain the mean, median, and range of WarrantyCost


# use quantile to calculate the five-number summary for WarrantyCost


# display the IQR of WarrantyCost


# boxplot of numeric variables: VehBCost and VehicleAge


# histograms of VehOdo


### 3. Exploring categorical variables

# Show the number of cars in different WheelType


# Show the proportion of cars in different WheelType


### 4. Understand relationships of multiple variables

# scatter plot: VehBCost and MMRCurrentAuctionAveragePrice


# Generate correlation coefficients of VehBCost and MMRCurrentAuctionAveragePrice


# Generate the correlation matrix of all numeric variables (you can specify the numeric variable names or their column numbers in a c() function)


## Examine relationships between numeric variables and factors
# boxplot VehBCost based on IsBadBuy


# Question: list one thing you learned from the carAuction data exploration.


