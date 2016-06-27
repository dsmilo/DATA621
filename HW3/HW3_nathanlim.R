library(ggplot2)
library(dplyr)
library(pROC)
library(leaps)


train_df <- read.csv(url("https://raw.githubusercontent.com/dsmilo/DATA621/master/HW3/Data/crime-training-data.csv"))


train_df$target <- factor(train_df$target) # to factor
summary(train_df)
# no missing values


pairs(train_df, col= train_df$target)




# Full model
glm.fit_full= glm(target ~ ., data=train_df, family=binomial)
summary(glm.fit_full)


#  BIC

regfit.full=regsubsets(factor(target) ~., data=train_df)
summary(regfit.full)

par(mfrow=c(1,2))
plot(regfit.full, scale="bic", main="Predictor Variables vs. BIC")
reg.summary <- summary(regfit.full)
reg.summary$bic
plot(reg.summary$bic, xlab="Number of Predictors", ylab="BIC", type="l", main="Best subset Selection using BIC")
which.min(reg.summary$bic) 
points(4, reg.summary$bic[4], col="red", cex=2, pch=20)


# Cp
plot(regfit.full, scale="Cp", main="Predictor Variables vs. Cp")
plot(reg.summary$cp, xlab="Number of Predictors", ylab="Cp", type="l", main="Best subset Selection using Cp" )
reg.summary$cp
points(5, reg.summary$cp[5],col="red", cex=2, pch=20)



# build model
model1 <- glm(target ~ nox + age + rad + medv, family=binomial, data = train_df)
summary(model1)

train_df$predicted_model1 <- predict(model1, train_df, type='response')

roc_model1 <- roc(factor(target) ~ predicted_model1, data=train_df)

plot(roc_model1, col="red")
# AUC is  0.957


model2 <- glm(target ~ nox + age + rad + ptratio + medv, family=binomial, data = train_df)
summary(model2)
train_df$predicted_model2 <- predict(model2, train_df, type='response')

roc_model2 <- roc(factor(target) ~ predicted_model2, data=train_df)

plot(roc_model2, col="red")
#AUC is 0.9605


