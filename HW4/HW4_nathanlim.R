library(stringr)
library(dplyr)
library(reshape)
library(glmnet)
library(leaps)
library(pROC)
library(MASS)
library(ggplot2)
library(vcd)


train <- read.csv("https://raw.githubusercontent.com/dsmilo/DATA621/master/HW4/Data/insurance_training_data.csv")
train$EDUCATION
head(train)
summary(train)


# AGE, YOJ,  INCOME, HOME_VAL, CAR_AGE have NAs

#clean money fields.
train$INCOME <- as.numeric(str_replace_all(train$INCOME, "[[:punct:]\\$]",""))
train$HOME_VAL <- as.numeric(str_replace_all(train$HOME_VAL, "[[:punct:]\\$]",""))
train$BLUEBOOK <- as.numeric(str_replace_all(train$BLUEBOOK, "[[:punct:]\\$]",""))
train$OLDCLAIM <- as.numeric(str_replace_all(train$OLDCLAIM, "[[:punct:]\\$]",""))

# Cast TARGET_FLAG as factor
train$TARGET_FLAG <- factor(train$TARGET_FLAG)

head(train)

numeric_variables <- train %>%
  dplyr::select(-c(PARENT1, MSTATUS, SEX, EDUCATION, JOB, 
            CAR_USE, CAR_TYPE, RED_CAR, REVOKED, URBANICITY))

melted_numeric <- melt(numeric_variables, id=c('TARGET_FLAG', 'TARGET_AMT'))


ggplot(melted_numeric, aes(x=TARGET_FLAG, y=value)) + geom_boxplot() + facet_wrap( ~ variable, scales = 'free')



#Convert indicator variables to 0s and 1s; 1 = Yes, Male for Sex, Commercial for Car Use, Red for RED_CAR, and Highly Urban for URBANICITY
train$PARENT1 <- ifelse(train$PARENT1=="Yes", 1, 0)
train$MSTATUS <- ifelse(train$MSTATUS=="Yes", 1, 0)
train$SEX <- ifelse(train$SEX=="M", 1, 0)
train$CAR_USE <- ifelse(train$CAR_USE=="Commercial", 1, 0)
train$RED_CAR <- ifelse(train$RED_CAR=="Yes", 1, 0)
train$REVOKED <- ifelse(train$REVOKED=="Yes", 1, 0)
train$URBANICITY <- ifelse(train$URBANICITY == "Highly Urban/ Urban", 1, 0)

#Convert categorical predictor values to indicator variables - EDUCATION, CAR_TYPE, JOB

#EDUCATION, High school graduate is base case

train$EDUCATION <- as.numeric(factor(train$EDUCATION, levels = c("<High School", "z_High School", "Bachelors", "Masters", "PhD")))-1


#CAR_TYPE, base case is minivan
train$Panel_Truck <- ifelse(train$CAR_TYPE=="Panel Truck", 1, 0)
train$Pickup <- ifelse(train$CAR_TYPE=="Pickup", 1, 0)
train$Sports_Car <- ifelse(train$CAR_TYPE=="Sports Car", 1, 0)
train$Van <- ifelse(train$CAR_TYPE=="Van", 1, 0)
train$SUV <- ifelse(train$CAR_TYPE=="z_SUV", 1, 0)

#JOB, base case is ""
train$Professional <- ifelse(train$JOB == "Professional", 1, 0)
train$Blue_Collar <- ifelse(train$JOB == "Professional", 1, 0)
train$Clerical <- ifelse(train$JOB == "Clerical", 1, 0)
train$Doctor <- ifelse(train$JOB == "Doctor", 1, 0)
train$Lawyer <- ifelse(train$JOB == "Lawyer", 1, 0)
train$Manager <- ifelse(train$JOB == "Manager", 1, 0)
train$Home_Maker <- ifelse(train$JOB == "Home Maker", 1, 0)
train$Student <- ifelse(train$JOB == "Student", 1, 0)


df <- train %>%
      dplyr::select(-c(INDEX,CAR_TYPE,JOB))

str(df) # 8161

new_df=df[complete.cases(df),]

str(new_df) # 6448

#I choose to delete NA including records


crash_data = new_df[which(new_df$TARGET_FLAG==1),]

crash_df <- crash_data %>%
  dplyr::select(-TARGET_FLAG)



#FULL model
fullmodel <- glm(TARGET_AMT ~., data = crash_df)
summary(fullmodel)
# AIC: 35300

fullmodel_log <- glm(log(TARGET_AMT) ~., data = crash_df)
summary(fullmodel_log)
#AIC: 4108.2

#BIC
regfit.full=regsubsets(log(TARGET_AMT) ~., data=crash_df)
reg.summary <- summary(regfit.full)
plot(regfit.full, scale = "bic", main = "Predictor Variables vs. BIC")
plot(reg.summary$bic, xlab="Number of Predictors", ylab="BIC", type="l", main="Best subset Selection using BIC")
which.min(reg.summary$bic) 
points(1, reg.summary$bic[1], col="red", cex=2, pch=20)

glm_model_bic <- glm(log(TARGET_AMT) ~ BLUEBOOK, data = crash_df)
summary(glm_model_bic)
#AIC: 4073.1

glm_model_bic_log <- glm(log(TARGET_AMT) ~ log(BLUEBOOK), data = crash_df)
summary(glm_model_bic_log)
#AIC: 4065

#Cp
plot(regfit.full, scale="Cp", main="Predictor Variables vs. Cp")
plot(reg.summary$cp, xlab="Number of Predictors", ylab="Cp", type="l", main="Best subset Selection using Cp" )
reg.summary$cp
points(4, reg.summary$cp[4],col="red", cex=2, pch=20)


model_cp <- glm(log(TARGET_AMT) ~ MSTATUS + BLUEBOOK + MVR_PTS + Professional, data = crash_df)
summary(model_cp)
# AIC: 4069.9


model_cp2 <- glm(log(TARGET_AMT) ~ MSTATUS + log(BLUEBOOK) + MVR_PTS + Professional, data = crash_df)
summary(model_cp2)
# AIC: 4061.8