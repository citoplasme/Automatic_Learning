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
