
mydata <- read.csv(file.choose(), header = T)

# EDA

summary(mydata)
str(mydata)

mydata$Churn <- as.factor(mydata$Churn)
mydata$ContractRenewal <- as.factor(mydata$ContractRenewal)
mydata$DataPlan <- as.factor(mydata$DataPlan)


# Univariate Analysis

dev.off()
par(mfrow=c(1,2))

hist(mydata$AccountWeeks, xlab = "Account weeks", main = "Account weeks")
boxplot(mydata$AccountWeeks, horizontal = T, xlab = "Account weeks", main = "Account weeks")

hist(mydata$DataUsage, xlab = "Data Usage", main = "Data Usage")
boxplot(mydata$DataUsage, horizontal = T, xlab = "Data Usage", main = "Data Usage")

hist(mydata$CustServCalls, xlab = "No. of calls", main = "No. of calls")
boxplot(mydata$CustServCalls, horizontal = T, xlab = "Data Usage", main = "Data Usage")

hist(mydata$DayMins, xlab = "Daytime Minutes", main = "Daytime Minutes")
boxplot(mydata$DayMins, horizontal = T, xlab = "Daytime Minutes", main = "Daytime Minutes")

hist(mydata$DayCalls, xlab = "Daytime Calls", main = "Daytime Calls")
boxplot(mydata$DayCalls, horizontal = T, xlab = "Daytime Calls", main = "Daytime Calls")

hist(mydata$MonthlyCharge, xlab = "Monthly Bill", main = "Monthly Bill")
boxplot(mydata$MonthlyCharge, horizontal = T, xlab = "Monthly Bill", main = "Monthly Bill")

hist(mydata$OverageFee, xlab = "Overage Fee", main = "Overage Fee")
boxplot(mydata$OverageFee, horizontal = T, xlab = "Overage Fee", main = "Overage Fee")

hist(mydata$RoamMins, xlab = "Roaming Minutes", main = "Roaming Minutes")
boxplot(mydata$RoamMins, horizontal = T, xlab = "Roaming Minutes", main = "Roaming Minutes")

dev.off()
par(mfrow=c(2,2))

barplot(table(mydata$Churn), main = "Churn",
        xlab = "Values", ylab = "Count")

barplot(table(mydata$ContractRenewal), main = "Contract Renewal",
        xlab = "Values", ylab = "Count")

barplot(table(mydata$DataPlan), main = "Data plan",
        xlab = "Values", ylab = "Count")


# Lot of customers have renewed contracts and chrun rate is low

# Bivariate analysis

library(corrplot)
data.cor = cor(mydata)
corrplot(data.cor, method = "number")

# Multicollinearity

library(car)
attach(mydata)

model = lm(Churn~ AccountWeeks + ContractRenewal + DataPlan + DataUsage +
             CustServCalls + DayMins + DayCalls + MonthlyCharge + OverageFee +
             RoamMins, data = mydata)
vif(model)

model = lm(Churn~ AccountWeeks + ContractRenewal + DataPlan + DataUsage +
             CustServCalls + DayMins + DayCalls + OverageFee +
             RoamMins, data = mydata)
vif(model)

model = lm(Churn~ AccountWeeks + ContractRenewal + DataPlan + 
             CustServCalls + DayMins + DayCalls + OverageFee +
             RoamMins, data = mydata)
vif(model)

# Monthly Usage and data usage columns are removed to avoid multicollinearity



# Chart for multicollinearity

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mydata, histogram = TRUE, pch="+") 


by(mydata, INDICES = mydata$Churn, FUN = summary) 

# Logistic Regression

indices = sample(1:nrow(mydata), 0.7*nrow(mydata))

train = mydata[indices,-c(5,9)]
test = mydata[-indices,-c(5,9)]

model <- glm(Churn ~., data = train)
summary(model)

model1 <- glm(Churn ~ ContractRenewal + DataPlan + CustServCalls + DayMins +
               OverageFee + RoamMins, data = train)
summary(model1)

# Preductions and evaluation for train data

train$predict.class <- predict(model1, train, type="response")

cutoff_train <- floor(train$predict.class+0.8) 
confmat.train = table (Predicted = cutoff_train, Actual = train$Churn)
confusionMatrix(confmat.train, positive = '1', mode = 'everything')

# Preductions and evaluation for test data

library(caret)

test$predict.class <- predict(model1, test, type="response")

cutoff_test <- floor(test$predict.class+0.8) 
confmat.test = table (Predicted =cutoff_test, Actual = test$Churn)
confusionMatrix(confmat.test, positive = '1', mode = 'everything')


# KS Stat
library(ROCR)
pred <- prediction(test$predict.class, test$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

install.packages("InformationValue")
library(InformationValue)
ks_stat(test_data$Personal.Loan, test_data$predict.class, returnKSTable = T)

ks_plot(test_data$Personal.Loan, test_data$predict.class)

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(test$predict.class, type="Gini")
gini

