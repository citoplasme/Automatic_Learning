# Library used to plot 
library(ggplot2)
# Library used to handle discrepancy in result variable 
library(ROSE)
# Library used to sample the dataset
library(caTools)
# Library used to evaluate the prediction - Confusion Matrix
library(caret)

# Dataset
BankMarketing <- read.csv("bank-marketing/bank-additional-full.csv", sep=";",header = TRUE)

# --------------------------------------------------------------------------------------------
# Exploratory Analysis
# --------------------------------------------------------------------------------------------
dim(BankMarketing)
names(BankMarketing)
head(BankMarketing)

# Discrepancy in the results -> model that always responds NO (Overfitting)
plot(BankMarketing$y)
dim(BankMarketing[BankMarketing$y == "no",])
dim(BankMarketing[BankMarketing$y == "yes",]) 

# Unknown values
par(mfrow=c(2,3))
plot(BankMarketing$housing, xlab = "Housing")
plot(BankMarketing$job, xlab = "Job")
plot(BankMarketing$loan, xlab = "Loan")
plot(BankMarketing$default, xlab = "Default")
plot(BankMarketing$education, xlab = "Education")

# Outliers
par(mfrow=c(2,2))
boxplot(BankMarketing$pdays, xlab = "Days since call")
boxplot(BankMarketing$duration, xlab = "Duration")
boxplot(BankMarketing$campaign, xlab = "campaign")
boxplot(BankMarketing$age, xlab = "Age")

# --------------------------------------------------------------------------------------------
# Dataset Problem's
# --------------------------------------------------------------------------------------------

# Existance of NA's
BankMarketingNA <- BankMarketing
BankMarketingNA[BankMarketingNA == "unknown"] <- NA
sapply(BankMarketingNA, function(x) sum(is.na(x)))

# Dataset is big, so we can omit NA's
BankMarketingComplete <- BankMarketingNA
BankMarketingComplete  <- BankMarketingComplete[complete.cases(BankMarketingComplete), ]
sapply(BankMarketingComplete, function(x) sum(is.na(x)))

# -------------------------------------------------
# pdays with big distances beetween them (999) - Weekly analysis
BM_WeeksCathegorical <- BankMarketingComplete
plot(table(BM_WeeksCathegorical$pdays))

BM_WeeksCathegorical$pdays <- cut(BankMarketingComplete$pdays, c(-1,7,14,998,999))
levels(BM_WeeksCathegorical$pdays) <- c('1W', '2W', '+2W', 'N')
plot(BM_WeeksCathegorical$pdays)

BankMarketingComplete <- BM_WeeksCathegorical

# Modelo = Treino , Previsao = Teste 
set.seed(7)

# 80% treino, 20% teste
sample <- sample.split(BankMarketingComplete,SplitRatio = 0.80) 
train <- subset(BankMarketingComplete, sample == TRUE)
test <-  subset(BankMarketingComplete, sample == FALSE)

model <- glm(y~., data = train, family = binomial)
summary(model)

# Prediction
test_result <- predict(model,test,type = "response")
test_result <- ifelse(test_result > 0.5,1,0)

test_result <- as.factor(test_result)
levels(test_result) <- c("no","yes")

# Actual Values
realValues <- test$y

# Comparassion 
confusion_matrix <- confusionMatrix(realValues,test_result,positive = "yes")
confusion_matrix

# p-values - H0: probability of the influence on the result being null
pvalues <- 1-summary(model)$coefficients[,4]
pvalues <- pvalues[-1] # Removes Intercept
line <- as.list(rep(0.99,length(pvalues)))
bp <- barplot(pvalues, main="Probability of H1 according to p-values",col=sample(colours(), 200), las=2, cex.names=0.6, cex.axis = 0.7, mgp = c(-1, -0, -1))
lines(x=bp,y=line,col="red") 

# --------------------------------------------------------------------
# Model using only significative variables

model.sig <- glm(y ~ contact + month + day_of_week + duration + campaign + pdays + poutcome + emp.var.rate + cons.price.idx, data = train,family = binomial)
summary(model.sig)

pvalues2 <- 1-summary(model.sig)$coefficients[,4]
pvalues2 <- pvalues2[-1] # Removes Intercept
line2 <- as.list(rep(0.99,length(pvalues2)))
bp2 <- barplot(pvalues2, main="Probability of H1 according to p-values",col=sample(colours(), 200), las=2, cex.names=0.6, cex.axis = 0.7, mgp = c(-1, -0, -1))
lines(x=bp2,y=line2,col="red") 

# Prediction
test_result_sig <- predict(model.sig,test,type = "response")
test_result_sig <- ifelse(test_result_sig > 0.5,1,0)

test_result_sig <- round(test_result_sig,0)
test_result_sig <- as.factor(test_result_sig)
levels(test_result_sig) <- c("no","yes")

# Actual Values
realValues <- test$y

# Comparassion 
confusion_matrix_sig <- confusionMatrix(realValues,test_result_sig,positive = "yes")
confusion_matrix_sig


# --------------------------------------------------------------------
# Model not using pdays

model.nopdays <- glm(y ~ contact + month + day_of_week + duration + campaign + poutcome + emp.var.rate + cons.price.idx, data = train,family = binomial)
summary(model.nopdays)

pvalues3 <- 1-summary(model.nopdays)$coefficients[,4]
pvalues3 <- pvalues3[-1] # Removes Intercept
line3 <- as.list(rep(0.99,length(pvalues3)))
bp3 <- barplot(pvalues3, main="Probability of H1 according to p-values",col=sample(colours(), 200), las=2, cex.names=0.6, cex.axis = 0.7, mgp = c(-1, -0, -1))
lines(x=bp3,y=line3,col="red") 

# Prediction
test_result_nopdays <- predict(model.nopdays,test,type = "response")
test_result_nopdays <- ifelse(test_result_nopdays > 0.5,1,0)

test_result_nopdays <- round(test_result_nopdays,0)
test_result_nopdays <- as.factor(test_result_nopdays)
levels(test_result_nopdays) <- c("no","yes")

# Actual Values
realValues <- test$y

# Comparassion 
confusion_matrix_nopdays <- confusionMatrix(realValues,test_result_nopdays,positive = "yes")
confusion_matrix_nopdays


# ------------------------------ K FOLD ------------------------------------
model.kf <- glm(y~., data = BankMarketingComplete, family = binomial)
summary(model.kf)

library(boot)
cv.err <- cv.glm(data = BankMarketingComplete,model.kf, K = 10)
cv.err$delta[1]


model.kf2 <- glm(y ~ contact + month + day_of_week + duration + campaign + pdays + poutcome + emp.var.rate + cons.price.idx, data = BankMarketingComplete,family = binomial)
summary(model.kf2)
cv.err2 <- cv.glm(data = BankMarketingComplete,model.kf2, K = 10)
cv.err2$delta[1]

model.kf3 <- glm(y ~ contact + month + day_of_week + duration + campaign + poutcome + emp.var.rate + cons.price.idx, data = BankMarketingComplete,family = binomial)
summary(model.kf3)
cv.err3 <- cv.glm(data = BankMarketingComplete,model.kf3, K = 10)
cv.err3$delta[1]
