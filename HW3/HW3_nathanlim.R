library(ggplot2)
library(dplyr)
library(pROC)
library(leaps)
library(caret)
library(bestglm)

train_df <- read.csv(url("https://raw.githubusercontent.com/dsmilo/DATA621/master/HW3/Data/crime-training-data.csv"))
test_df <- read.csv(url("https://raw.githubusercontent.com/dsmilo/DATA621/master/HW3/Data/crime-evaluation-data.csv"))

hist(train_df$nox)
hist(log(train_df$nox))
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
train_df$target_model1 <- ifelse(train_df$predicted_model1>0.5, 1, 0)
roc_model1 <- roc(factor(target) ~ predicted_model1, data=train_df)

plot(roc_model1, col="red")

confusionMatrix(train_df$target, train_df$target_model2, positive = "1")

# AUC is  0.957 Accuracy : 0.8691 


model2 <- glm(target ~ nox + age + rad + ptratio + medv, family=binomial, data = train_df)
summary(model2)

train_df$predicted_model2 <- predict(model2, train_df, type='response')
train_df$target_model2 <- ifelse(train_df$predicted_model2>0.5, 1, 0)

roc_model2 <- roc(factor(target) ~ predicted_model2, data=train_df)

plot(roc_model2, col="red")

confusionMatrix(train_df$target, train_df$target_model2, positive = "1")
#AUC is 0.9605 Accuracy : 0.8691


model3 <- glm(target ~ log(nox) + age + log(rad) + medv, family=binomial, data = train_df)
summary(model3)

train_df$predicted_model3 <- predict(model3, train_df, type='response')
train_df$target_model3 <- ifelse(train_df$predicted_model3>0.5, 1, 0)
roc_model3 <- roc(factor(target) ~ predicted_model3, data=train_df)

plot(roc_model3, col="red")
confusionMatrix(train_df$target, train_df$target_model3, positive = "1")

#AUC is 0.9584, Accuracy : 0.8691 


model4 <- glm(target ~ log(nox) +  log(rad) + tax, family=binomial, data = train_df)
summary(model4)
train_df$predicted_model4 <- predict(model4, train_df, type='response')
train_df$target_model4 <- ifelse(train_df$predicted_model4>0.5, 1, 0)
roc_model4 <- roc(factor(target) ~ predicted_model4, data=train_df)

plot(roc_model4, col="red")
confusionMatrix(train_df$target, train_df$target_model4, positive = "1")

#AUC is 0.961,  Accuracy : 0.8648




bestglm_bic<- bestglm(train_df, IC= "BIC", family = binomial)
bestglm_bic

bestglm_model <- glm(target ~ nox +  rad + tax, family=binomial, data = train_df)
summary(bestglm_model)
train_df$predicted_bestglm <- predict(bestglm_model, train_df, type='response')
train_df$target_bestglm <- ifelse(train_df$predicted_bestglm>0.5, 1, 0)
roc_bestglm <- roc(factor(target) ~ predicted_bestglm, data=train_df)

plot(roc_bestglm, col="red")
confusionMatrix(train_df$target, train_df$target_bestglm, positive = "1")



