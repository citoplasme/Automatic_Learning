# Matrix multiplication
A <- matrix(c(1,2,3,4),2,2)
B <- matrix(c(5,6,7,8),2,2)
B %*% A

Adv = read.csv(file="dados/Advertising.csv", header=T)
head(Adv)
dim(Adv)

# Ex. 1
# This exercise involves the Auto data set. Make sure that the missing values have been removed from the data.. 

Auto = read.csv(file="dados/Auto.csv", header=T)
head(Auto)
Auto <- subset(Auto, horsepower!='?')

# Which of the predictors are quantitative, and which are qualitative?  
# Interest variable (Y): MPG
# Qualitative: origin (Name)
# Quantitative: everything else

# What is the range of each quantitative predictor? You can answer this using the range() function. 

range(Auto$mpg) # Min & Max
range(as.numeric(Auto$horsepower))
summary(Auto)

# What is the mean and standard deviation of each quantitative predictor? 

sd(Auto$cylinders)
mean(Auto$cylinders)

# Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains? 

test <- Auto[-c(10:85),] 
range(test$mpg); sd(test$mpg); mean(test$mpg)
range(test$cylinders); sd(test$cylinders); mean(test$cylinders)
range(test$displacement); sd(test$displacement); mean(test$displacement)
range(as.numeric(test$horsepower)); sd(as.numeric(test$horsepower)); mean(as.numeric(test$horsepower))
range(test$weight); sd(test$weight); mean(test$weight)
range(test$acceleration); sd(test$acceleration); mean(test$acceleration)
range(test$year); sd(test$year); mean(test$year)

# Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings. 

par(mfrow=c(3,2))
plot(Auto$cylinders, Auto$mpg, pch=20)
plot(Auto$displacement, Auto$mpg, pch=20)
plot(as.numeric(Auto$horsepower), Auto$mpg, pch=20) # Categorizado
plot(Auto$weight, Auto$mpg, pch=20)
plot(Auto$acceleration, Auto$mpg, pch=20)
plot(Auto$year, Auto$mpg, pch=20)

# Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the other variables might be useful in predicting mpg? Justify your answer. 

# Usefull: Displacement, Weight, horsepower
# Not so usefull: Cylinders, Acceleration, Year

# Ex. 2
# This exercise involves the Boston housing data set. Load in the Boston data set. The Boston data set is part of the MASS library in R. 
library(MASS)
data(Boston)

# How many rows are in this data set? How many columns? What do the rows and columns represent? 
head(Boston)
nrow(Boston)
ncol(Boston)
colnames(Boston)

# Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.  # Medv is the variable of interest 
par(mfrow=c(5,3))
plot(Boston$crim, Boston$medv, pch=20)
plot(Boston$zn, Boston$medv, pch=20)
plot(Boston$indus, Boston$medv, pch=20)
plot(Boston$chas, Boston$medv, pch=20)
plot(Boston$nox, Boston$medv, pch=20)
plot(Boston$rm, Boston$medv, pch=20)
plot(Boston$age, Boston$medv, pch=20)
plot(Boston$dis, Boston$medv, pch=20)
plot(Boston$rad, Boston$medv, pch=20)
plot(Boston$tax, Boston$medv, pch=20)
plot(Boston$ptratio, Boston$medv, pch=20)
plot(Boston$black, Boston$medv, pch=20)
plot(Boston$lstat, Boston$medv, pch=20)

# Are any of the predictors associated with per capita crime rate? If so, explain the relationship. 
par(mfrow=c(4,3))
plot(Boston$zn, Boston$crim, pch=20)
plot(Boston$indus, Boston$crim, pch=20)
plot(Boston$chas, Boston$crim, pch=20)
plot(Boston$nox, Boston$crim, pch=20)
plot(Boston$rm, Boston$crim, pch=20)
plot(Boston$age, Boston$crim, pch=20)
plot(Boston$dis, Boston$crim, pch=20)
plot(Boston$rad, Boston$crim, pch=20)
plot(Boston$tax, Boston$crim, pch=20)
plot(Boston$ptratio, Boston$crim, pch=20)
plot(Boston$black, Boston$crim, pch=20)
plot(Boston$lstat, Boston$crim, pch=20)

# Dis (Distance weighted to empleyment center) & age (Age of the building)

# Do any of the suburbs of Boston appear to have particularly high crime rates? 
plot(Boston$crim, pch=20)
Boston[with(Boston,order(-Boston$crim)),]
head(Boston[with(Boston,order(-Boston$crim)),])
# Tax rates? 
plot(Boston$tax, pch=20)
Boston[with(Boston,order(-Boston$tax)),]
# Pupil-teacher ratios? Comment on the range of each predictor. 
plot(Boston$ptratio, pch=20)
Boston[with(Boston,order(-Boston$ptratio)),]

# How many of the suburbs in this data set bound the Charles river?  # CHAS - 1 if land tract bounds Charles River; 0 otherwise
length(which(Boston$chas == 1))

# What is the median pupil-teacher ratio among the towns in this data set?  # PRATIO - Pupil-teacher ratio by town district
median(Boston$ptratio)

# Which suburb of Boston has lowest median value of owner-occupied homes? 
# What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.  
head(Boston[with(Boston,order(Boston$medv)),])

# In this data set, how many of the suburbs average more than seven rooms per dwelling? 
# RM - Average number of rooms in owner units
length(which(Boston$rm >= 7))
# More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling. 
length(which(Boston$rm >= 8))
which(Boston$rm >= 8)

bigger8 <- subset(Boston, Boston$rm >= 8)
plot(bigger8$rm, bigger8$medv, pch=20)
