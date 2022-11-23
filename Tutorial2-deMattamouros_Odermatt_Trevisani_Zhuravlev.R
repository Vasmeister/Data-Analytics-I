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
# Filename:  Data Analytics 1: Predictive Econometrics - PC 2
# Topic:     Introduction to R and Simulations
# Date: 23.11.2022

## Topic: Model Evaluation
###############################################################################

load("insurance-all.Rdata") # Load data

#############################

# Exercise 1: Prepare Data

###########################
# Task 1 - Caret package #
###########################

# install.packages("caret")
library(caret)

# Other packages
# install.packages("dplyr")
library(dplyr)
library(ggplot2)

##############################################
# Task 2 - Analysing the dependent variable #
##############################################
ggplot() + 



################################
# Task 3 - Log Transformation #
################################

data <- mutate() # Generate log(charges)
data <- select(data, -c("charges")) # Delete charges

ggplot() + 
  geom_histogram(binwidth = , color = "darkblue", fill = "lightblue")


##############################
# Task 4 - Train-Test Split #
##############################

## Creating train and test data sets
set.seed(25112020)
idtrain <- sample()

train <- data[idtrain,]
test <- data[-idtrain,]

summary(train)
summary(test)


##############################

# Exercise 2: In-Sample MSE #

##############################
# Task 1 - Log Models #
########################

# Model 1
model1log <- lm()
MSE1.log.in <- 
print(paste("MSE Log Model 1:", round(MSE1.log.in,4)))

# Model 2
model2log <- lm()
MSE2.log.in <- 
print(paste("MSE Log Model 2:", round(MSE2.log.in,4)))

# Model 3
model3log <- lm()
MSE3.log.in <- 
print(paste("MSE Log Model 3:", round(MSE3.log.in,4)))

# Plot - In-sample MSE:
# (1) Data Frame for the plot
MSElog.in <- data.frame(model = c("Model1", "Model2", "Model3"), 
                        MSE = c(MSE1.log.in, MSE2.log.in, MSE3.log.in))
# (2) Plot
ggplot() +


##########################################
# Task 2 - Fit Plots and Residual Plots #
##########################################

## FIT PLOTS
# Plot - Model 1 - Logs
ggplot() +
  
# Plot - Model 2
ggplot() +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)  +
  ggtitle("Fit Plot - Model 2 - in Logs")

# Plot - Model 3
ggplot() +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)  +
  ggtitle("Fit Plot - Model 3 - in Logs")


## RESIDUAL PLOTS
# Plot - Log Model 1
ggplot() +

# Plot - Log Model 2
ggplot() +
  geom_point() + 
  geom_hline(yintercept = 0)  +
  ggtitle("Residual Plot - Model 2 - in Logs")

# Plot - Log Model 3
ggplot() +
  geom_point() + 
  geom_hline(yintercept = 0)  +
  ggtitle("Residual Plot - Model 3 - in Logs")


############################################

# Exercise 3: Training and Test Data Sets #

###############################
# Task 2 - Out of sample MSE #
###############################

# Model 1
fit1 <- predict()
MSE1.log.out <-
print(paste("MSE Log Model 1:", round(MSE1.log.out,4)))

# Model 2
fit2 <- predict()
MSE2.log.out <- 
print(paste("MSE Log Model 2:", round(MSE2.log.out,4)))

# Model 3
fit3 <- predict()
MSE3.log.out <- 
print(paste("MSE Model 3:", round(MSE3.log.out,4)))

# Plot - Out-of-sample MSE
# (1) Data Frame for the plot
MSEoutlog <- data.frame(model = c("Model1", "Model2", "Model3"), MSE = c(MSE1.log.out, MSE2.log.out, MSE3.log.out))
# (2) Plot
ggplot() +
  geom_col(color="black", fill ="steelblue1") +
  ggtitle("Out-of-Sample MSE - Log Models") +
  xlab("") + 
  theme_classic()


### Alternatives 
## MAE
MAE1.log.out <- 
MAE2.log.out <- 
MAE3.log.out <- 

# Plot - MAE
# (1) Data Frame for the plot
MAElog <- data.frame(model = c("Model1", "Model2", "Model3"), loss = c(MAE1.log.out, MAE2.log.out, MAE3.log.out))
# (2) Plot
ggplot() +
  geom_col(color="black", fill = "plum2") +
  ggtitle("Out-of-Sample MAE - Log Models") +
  xlab("") + 
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



###############################################################################

# Exercise 4: Cross-Validation #

##############################
# Task 2 - Cross-Validation #
##############################

### K = 5
# Settings for 5-fold CV
train.control <- trainControl(method = , number = ) # Define training control

## Model 1
set.seed(123) # Fix the seed for replicability of the results
model1log.5kcv <- 
MSE1.log.5kcv <-
print(paste("5K-CV MSE Model 1:", round(MSE1.log.5kcv,4))) # Print the CV MSE


## Model 2
set.seed(123) # Fix the seed for replicability of the results
model2log.5kcv <-
MSE2.log.5kcv <-
print(paste("5K-CV MSE Model 2:", round(MSE2.log.5kcv,4))) # Print the CV MSE

## Model 3
set.seed(123) # Fix the seed for replicability of the results
model3log.5kcv <- 
MSE3.log.5kcv <-
print(paste("5K-CV MSE Model 3:", round(MSE3.log.5kcv,4))) # Print the CV MSE

### K = 10
# Settings for 10-fold CV
train.control <- trainControl(method = , number = ) # Define training control

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

### K = N-1 => LOOCV
# Settings for LOOCV
train.control <- trainControl(method = ) # Define training control


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



# Plot
# (1) Data Frame for the plot
MSEoutlog <- data.frame(model = c(rep("Model1",4), rep("Model2",4), rep("Model3",4)), 
                        MSE = c(MSE1.log.out, MSE1.log.5kcv, MSE1.log.10kcv, MSE1.log.loocv,
                                MSE2.log.out, MSE2.log.5kcv, MSE2.log.10kcv, MSE2.log.loocv,
                                MSE3.log.out, MSE3.log.5kcv, MSE3.log.10kcv, MSE3.log.loocv),
                        type = factor(rep(c("Test", "5K-CV", "10K-CV", "LOOCV"),3), 
                                      levels = c("Test", "5K-CV", "10K-CV", "LOOCV")))
# (2) Plot
ggplot() +

# Check the values for 5K and 10K-fold CV
model1log.5kcv$resample$RMSE^2
model1log.10kcv$resample$RMSE^2

model2log.5kcv$resample$RMSE^2
model2log.10kcv$resample$RMSE^2