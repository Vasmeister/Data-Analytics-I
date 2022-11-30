# ----------------------------------------------------------------------------
#                                                                                                                      
#
#                           88
#                           88
#                           88             
#                           88                                   
#                           88                                    
#                           8b,dPPPPYba, ,dPPPYba,   ,adYPPPYba88
#                           88P'    `"8a a8P   `"8a  a8P       88
#                           88        88 8PP         8PP       88
#                           88        88 `"PPYYba"   8O        88
#"                          88        88         8P   8b,     ,88
#                           88        88 "8a,   "a8   `"Ybbdd888"
#                           88        88 `"YbbdPP"             88
#                                                              88
#                                                     "8b      88
#                                                     `"Ybbdd888"  
#
#     Jose de Mattamouros, Linda Odermatt, Elena Trevisani, Vasily Zhuravlev
#     Master in Economics
#     University of St. Gallen
#     9000 St. Gallen
#
# ------------------------------------------------------------------------------
# 
# Filename:  Data Analytics 1: Predictive Econometrics - PC 3
# Topic:     Topic: Penalized Regression
# Date of submission: 06.12.2022
#
# ------------------------------------------------------------------------------
#Step 1: Set the working directory:
#Find which directory you are currently working in
ddpath 
getwd()
# "~/Desktop/HSG/MEcon Herbst Semester/2nd half of the semester/Data Analytics I - Predictive Econometrics/Lecture 4"
#Set the directory you would like to work in
setwd("~/Desktop/HSG/MEcon Herbst Semester/2nd half of the semester/Data Analytics I - Predictive Econometrics/Lecture 4")
load("student-mat-train.Rdata") # Load data
load("student-mat-test.Rdata")
#
# ------------------------------------------------------------------------------
#Step 2: Install the relevant packages for the problem

# install.packages("glmnet")
library(glmnet)

# Other packages
# install.packages("corrplot")
library(corrplot)
library(ggplot2)

# ------------------------------------------------------------------------------
#Exercise 1: How many observations are in the both datasets? 
nrow(test) #There are 143 observations in the test set
nrow(train) #There are 214 observations in the train set

nrow(test)/(nrow(test)+nrow(train)) 
#the ratio is roughly 40:60% between the test and the train data sets.

# ------------------------------------------------------------------------------
#Exercise 2: What is the average, minimum, and maximum grade in the training data? 

summary(train$G3) # very concise

#alternative solution but more precise to the problem
grade_summary=matrix(data=data.frame(col1=mean(train$G3),
                                     col2=min(train$G3),
                                     col3=max(train$G3)))
colnames(grade_summary)=c("G3")
rownames(grade_summary)=c("Mean","Min","Max")

grade_summary # very precise code with the matrix

#Therefore, we find the following in the training data:
#the average grade is 11.64, the minimum is 4 and the maximum is 19.00.

# ------------------------------------------------------------------------------
#Exercise 3: Plot the histogram of the final math grades in the training data. 

ggplot(data=train, aes(x=train$G3)) + 
  geom_histogram(binwidth =1 , color = "darkblue", fill = "lightblue") +
  ggtitle("Histogram for Student Grades in Maths, rounded to the closest figure")+
  ylab("Count")+
  xlab("Final Math Grade (0-20)")


#useful code for the problem set. Keep it for now for aid
# ------------------------------------------------------------------------------
################################
# #Class PC Session #
################################

#############################
# #Exercise 1 #
#############################
#1. Install package caret
library(caret)

#2. Histogram for the charges variable. 
#The bandwidth of the histogram bins is set to 1000.

ggplot(data=data, aes(x=data$charges)) + 
  geom_histogram(binwidth =1000 , color = "darkblue", fill = "lightblue") +
  ggtitle("Histogram for Charges")+
  xlab("Medical bill charges")

# 3.Histogram of the variable logcharges. 
# The bandwidth of the histogram bins is set to 0.1.
data <- mutate(data, logcharges=log(data$charges))
data <- select(data, -c("charges")) # Delete charges

ggplot(data=data, aes(x=logcharges)) + 
  geom_histogram(binwidth =0.1 , color = "darkblue", fill = "lightblue") +
  ggtitle("Histogram for Logarithmic Charges")+
  xlab("Logarithm of Medical bill charges")



##############################
# Task 4 - Train-Test Split #
##############################

## Creating train and test data sets
set.seed(28112022)
#Set 80% of the sample as train data and 20% as test data
idtrain <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))

train <- data[idtrain,]
test <- data[!idtrain,]

summary(train)
summary(test)

nrow(train)
nrow(test)

# ------------------------------------------------------------------------------

##############################

# Exercise 2: In-Sample MSE #

##############################
# Task 1 - Log Models #
########################

# Model 1
model1log <- lm(logcharges ~ smoker + age + smoker*age, data=train)
summary(model1log)
MSE1.log.in <- mean(model1log$residuals^2) # calculate mean squared error
print(paste("MSE Log Model 1:", round(MSE1.log.in,4)))
anova(model1log)
# Model 2
model2log <- lm(logcharges ~ .*smoker,
                 data=train)
summary(model2log)
anova(model2log)

MSE2.log.in <- mean(model2log$residuals^2) # calculate mean squared error

print(paste("MSE Log Model 2:", round(MSE2.log.in,4)))

# Model 3
#too long to do it manually...
model3log <- lm(logcharges ~ smoker + age + bmi + sex + region + children + 
                smoker*age + smoker*bmi + smoker*sex + smoker*region + smoker*children 
                + bmi*age  + bmi*sex + bmi*region + bmi*children 
                + sex*age  + sex*region + sex*children,
                 data=train)
anova(model3log)
#Alternative approach... faster but maybe quite sensitive
model3log <- lm(logcharges ~ .^2 +.^3 +.^4,
                data=train)
summary(model3log)

MSE3.log.in <- mean(model3log$residuals^2) # calculate mean squared error
print(paste("MSE Log Model 3:", round(MSE3.log.in,4)))


#Model 3 has the lowest MSE. As we include more variables, MSE decreases.

# Plot - In-sample MSE:
# (1) Data Frame for the plot
MSElog.in <- data.frame(model = c("Model1", "Model2", "Model3"), 
                        MSE = c(MSE1.log.in, MSE2.log.in, MSE3.log.in))
# (2) Plot
ggplot(data=MSElog.in, aes(y=MSE, x=model)) +
  geom_col(color="black", fill="darkcyan") +
  ggtitle("Barplot for In-Sample MSEs of different models")

##########################################
# Task 2 - Fit Plots and Residual Plots #
##########################################

## FIT PLOTS
# Plot - Model 1 - Logs

ggplot(data=train, aes(y=logcharges, x=fitted(model1log))) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)  +
  ggtitle("Fit Plot - Model 1 - in Logs")+
  labs(y="Logarithm of Charges", x="OLS Fitted values of model 1")


# Plot - Model 2
ggplot(data=train, aes(y=logcharges, x=fitted(model2log))) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)  +
  ggtitle("Fit Plot - Model 2 - in Logs") +
  labs(y="Logarithm of Charges", x="OLS Fitted values of model 2")

# Plot - Model 3
ggplot(data=train, aes(y=logcharges, x=model3log$fitted.values)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)  +
  ggtitle("Fit Plot - Model 3 - in Logs") +
  labs(y="Logarithm of Charges", x="OLS Fitted values of model 3")


## RESIDUAL PLOTS
# Plot - Log Model 1
ggplot(data=train, aes(y=model1log$residuals, x=model1log$fitted.values)) +
  geom_point() + 
  geom_hline(yintercept = 0)  +
  ggtitle("Residual Plot - Model 1 - in Logs") +
  labs(y="Logarithm of Charges", x="OLS residual values of model 1")


# Plot - Log Model 2
ggplot(data=train, aes(y=model2log$residuals, x=model2log$fitted.values)) +
  geom_point() + 
  geom_hline(yintercept = 0)  +
  ggtitle("Residual Plot - Model 2 - in Logs") +
  labs(y="Logarithm of Charges", x="OLS residual values of model 2")

# Plot - Log Model 3
ggplot(data=train, aes(y=model3log$residuals, x=model3log$fitted.values)) +
  geom_point() + 
  geom_hline(yintercept = 0)  +
  ggtitle("Residual Plot - Model 3 - in Logs") +
  labs(y="Logarithm of Charges", x="OLS residual values of model 3")

# ------------------------------------------------------------------------------

############################################

# Exercise 3: Training and Test Data Sets #

###############################
# Task 2 - Out of sample MSE #
###############################

# Model 1
fit1 <- predict(model1log, newdata=test) #predicted values using model 1 coefficients
MSE1.log.out <- mean((test$logcharges-fit1)^2) # calculate mean squared error 
#(actual data - predicted data)^2
print(paste("MSE Log Model 1:", round(MSE1.log.out,4)))

# Model 2
fit2 <- predict(model2log, newdata=test) # predicted values using model 2 coefficients
MSE2.log.out <- mean((test$logcharges-fit2)^2) # calculate mean squared error 
print(paste("MSE Log Model 2:", round(MSE2.log.out,4)))

# Model 3
fit3 <- predict(model3log, newdata=test) # predicted values using model 2 coefficients
MSE3.log.out <- mean((test$logcharges-fit3)^2) # calculate mean squared error 
print(paste("MSE Model 3:", round(MSE3.log.out,4)))

# Plot - Out-of-sample MSE
# (1) Data Frame for the plot
MSEoutlog <- data.frame(model = c("Model1", "Model2", "Model3"), 
                        MSE = c(MSE1.log.out, MSE2.log.out, MSE3.log.out))
# (2) Plot
ggplot(data=MSEoutlog, aes(y=MSE, x=model)) +
  geom_col(color="black", fill ="steelblue1")+
  ggtitle("Barplot for Out-of-Sample MSE - Log Models") +
  xlab("Log model")+
  theme_classic()

### Alternatives 
## MAE
MAE1.log.out <- mean(abs(test$logcharges-fit1))
print(paste("MAE Log Model 1:", round(MAE1.log.out,4)))
MAE2.log.out <-  mean(abs(test$logcharges-fit2))
print(paste("MAE Log Model 2:", round(MAE2.log.out,4)))
MAE3.log.out <-  mean(abs(test$logcharges-fit3))
print(paste("MAE Log Model 3:", round(MAE3.log.out,4)))

# Plot - MAE
# (1) Data Frame for the plot
MAEoutlog <- data.frame(model = c("Model1", "Model2", "Model3"), 
                     loss = c(MAE1.log.out, MAE2.log.out, MAE3.log.out))
# (2) Plot
ggplot(data=MAEoutlog, aes(model, loss)) +
  geom_col(color="black", fill = "plum2") +
  ggtitle("Out-of-Sample MAE - Log Models") +
  xlab("Mean absolute error") + 
  theme_classic()


## Out-of-sample R^2 
Rsq1.log.out <- 1 - (sum((test$logcharges - fit1)^2)/sum((test$logcharges - mean(test$logcharges))^2))
Rsq2.log.out <- 1 - (sum((test$logcharges - fit2)^2)/sum((test$logcharges - mean(test$logcharges))^2))
Rsq3.log.out <- 1 - (sum((test$logcharges - fit3)^2)/sum((test$logcharges - mean(test$logcharges))^2))

# Plot - Out-of-sample R^2
# (1) Data Frame for the plot
Rsqlog <- data.frame(model = c("Model1", "Model2", "Model3"), loss = c(Rsq1.log.out, Rsq2.log.out, Rsq3.log.out))
# (2) Plot
ggplot(Rsqlog, aes(model, loss)) +
  geom_col(color="black", fill = "firebrick1") +
  ggtitle("Out-of-Sample Rsq - Log Models") +
  xlab("") + 
  theme_classic()
#the higher the R^2 the better 

###############################################################################

# Exercise 4: Cross-Validation #

##############################
# Task 2 - Cross-Validation #
##############################

### K = 5
# Settings for 5-fold CV
train.control <- trainControl(method ="CV", number = 5, p=0.8) # Define training control

## Model 1
set.seed(123) # Fix the seed for replicability of the results
model1log.5kcv <- train(logcharges ~ age + smoker +age*smoker, 
                        data=train,
                        method="lm", 
                        trControl = train.control)
MSE1.log.5kcv <-(model1log.5kcv$result$RMSE)^2
print(paste("5K-CV MSE Model 1:", round(MSE1.log.5kcv,4))) # Print the CV MSE


## Model 2
set.seed(123) # Fix the seed for replicability of the results
model2log.5kcv <- train(logcharges ~ .*smoker, 
                        data=train,
                        method="lm", 
                        trControl = train.control)
MSE2.log.5kcv <- (model2log.5kcv$result$RMSE)^2
print(paste("5K-CV MSE Model 2:", round(MSE2.log.5kcv,4))) # Print the CV MSE

## Model 3
set.seed(123) # Fix the seed for replicability of the results
model3log.5kcv <- train(logcharges ~ .^2 + .^3 +.^4, 
                        data=train,
                        method="lm", 
                        trControl = train.control)
MSE3.log.5kcv <- (model3log.5kcv$result$RMSE)^2
print(paste("5K-CV MSE Model 3:", round(MSE3.log.5kcv,4))) # Print the CV MSE

#Average K=5 MSE increases as we use more variables on the test set

### K = 10
# Settings for 10-fold CV
train.control <- trainControl(method ="CV", number = 10, p=0.8) # Define training control

## Model 1
set.seed(123) # Fix the seed for replicability of the results
model1log.10kcv <- train(logcharges ~ smoker + age + smoker*age, data = data, method = "lm", trControl = train.control)
MSE1.log.10kcv <-model1log.10kcv$results$RMSE^2
print(paste("10K-CV MSE Model 1:", round(MSE1.log.10kcv,4))) # Print the CV MSE


## Model 2
set.seed(123) # Fix the seed for replicability of the results
model2log.10kcv <- train(logcharges ~ .*smoker, data = data, method = "lm", trControl = train.control)
MSE2.log.10kcv <-model2log.10kcv$results$RMSE^2
print(paste("10K-CV MSE Model 2:", round(MSE2.log.10kcv,4))) # Print the CV MSE


## Model 3
set.seed(123) # Fix the seed for replicability of the results
model3log.10kcv <- train(logcharges ~ . + .^2 + .^3 + .^4, data = data, method = "lm", trControl = train.control)
MSE3.log.10kcv <-model3log.10kcv$results$RMSE^2
print(paste("10K-CV MSE Model 3:", round(MSE3.log.10kcv,4))) # Print the CV MSE

#similar result as previously 

### K = N-1 => LOOCV
# Settings for LOOCV
train.control <- trainControl(method ="LOOCV") # Define training control # Define training control


## Model 1
set.seed(123) # Fix the seed for replicability of the results
model1log.loocv <- train(logcharges ~ smoker + age + smoker*age, data = data, method = "lm", trControl = train.control)
MSE1.log.loocv <-model1log.loocv$results$RMSE^2
print(paste("LOOCV MSE Model 1:", round(MSE1.log.loocv,4))) # Print the CV MSE


## Model 2
set.seed(123) # Fix the seed for replicability of the results
model2log.loocv <- train(logcharges ~ .*smoker, data = data, method = "lm", trControl = train.control)
MSE2.log.loocv <-model2log.loocv$results$RMSE^2
print(paste("LOOCV MSE Model 2:", round(MSE2.log.loocv,4))) # Print the CV MSE


## Model 3
set.seed(123) # Fix the seed for replicability of the results
model3log.loocv <- train(logcharges ~ . + .^2 + .^3 + .^4, data = data, method = "lm", trControl = train.control)
MSE3.log.loocv <-model3log.loocv$results$RMSE^2
print(paste("LOOCV MSE Model 3:", round(MSE3.log.loocv,4))) # Print the CV MSE

#LOOCV MSE increases at some point, we can observe overfitting

# Plot
# (1) Data Frame for the plot
MSEoutlog <- data.frame(model = c(rep("Model1",4), rep("Model2",4), rep("Model3",4)), 
                        MSE = c(MSE1.log.out, MSE1.log.5kcv, MSE1.log.10kcv, MSE1.log.loocv,
                                MSE2.log.out, MSE2.log.5kcv, MSE2.log.10kcv, MSE2.log.loocv,
                                MSE3.log.out, MSE3.log.5kcv, MSE3.log.10kcv, MSE3.log.loocv),
                                type = factor(rep(c("Test", "5K-CV", "10K-CV", "LOOCV"),3), 
                                      levels = c("Test", "5K-CV", "10K-CV", "LOOCV")))
# (2) Plot

ggplot(MSEoutlog, aes(y=MSE, x=model, fill=type)) +
  geom_col(position=position_dodge(), color="black") +
  ggtitle("Out-of-Sample MSE - Test + CV - Log Models") +
  xlab("")+
  theme_minimal()+
  scale_fill_brewer(palette="Pastel1")

# Check the values for 5K and 10K-fold CV
model1log.5kcv$resample$RMSE^2
model1log.10kcv$resample$RMSE^2

model2log.5kcv$resample$RMSE^2
model2log.10kcv$resample$RMSE^2

