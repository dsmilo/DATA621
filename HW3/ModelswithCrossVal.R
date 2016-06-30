library(ggplot2)
library(dplyr)
library(pROC)
library(leaps)
library(caret)


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


#################CV Starts here

k = 10
set.seed(1306)
folds = sample(1:k, nrow(train_df), replace = TRUE)
cv.errors1 = matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))
cv.errors2 = matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))
cv.errors3 = matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))
cv.errors4 = matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))

train_df$target = as.numeric(as.character(train_df$target))

for (j in 1:k) {
  
  model1 <- glm(target ~ nox + age + rad + medv, family = binomial, data = train_df[folds != j, ])
  model2 <- glm(target ~ nox + age + rad + ptratio + medv, family = binomial, data = train_df[folds != j, ])
  model3 <- glm(target ~ log(nox) + age + log(rad) + medv, family = binomial, data = train_df[folds != j, ])
  model4 <- glm(target ~ log(nox) +  log(rad) + tax, family = binomial, data = train_df[folds != j, ])
  
    #best.fit = regsubsets(y ~ ., data = train_df[folds != j, ], nvmax = 10)
    for (i in 1:10) {
      pred1 = predict(model1, train_df[folds == j, ], id = i)
      cv.errors1[j, i] = mean((train_df$target[folds == j] - pred1) ^ 2)
    
      pred2 = predict(model2, train_df[folds == j, ], id = i)
      cv.errors2[j, i] = mean((train_df$target[folds == j] - pred2) ^ 2)
  
      pred3 = predict(model3, train_df[folds == j, ], id = i)
      cv.errors3[j, i] = mean((train_df$target[folds == j] - pred3) ^ 2)
      
      pred4 = predict(model4, train_df[folds == j, ], id = i)
      cv.errors4[j, i] = mean((train_df$target[folds == j] - pred4) ^ 2)
  }
  
}


cv.errors1
mean.cv.errors1 <- apply(cv.errors1, 2, mean)
mean.cv.errors1 = apply(cv.errors1, 2, mean)

# which.min(mean.cv.errors1)
# mean.cv.errors1[6]
cv.errors2
mean.cv.errors2 <- apply(cv.errors2, 2, mean)
mean.cv.errors2
# which.min(mean.cv.errors2)
# mean.cv.errors2[6]


cv.errors3
mean.cv.errors3 <- apply(cv.errors3, 2, mean)
mean.cv.errors3
# which.min(mean.cv.errors3)
# mean.cv.errors3[6]


cv.errors4
mean.cv.errors4 <- apply(cv.errors4, 2, mean)
mean.cv.errors4
# which.min(mean.cv.errors4)
# mean.cv.errors4[6]

all.cv.error = data.frame(
mean(mean.cv.errors1),
mean(mean.cv.errors2),
mean(mean.cv.errors3),
mean(mean.cv.errors4) )
names(all.cv.error) = c("Model1", "Model2", "Model3", "Model4")