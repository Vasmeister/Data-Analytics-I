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

# ------------------------------------------------------------------------------
#Exercise 4: The difference between causal and predictive modelling.

# In causal modelling we are interested in proving the relationship between variables.
# Or more explicitly how does one variable (e.g., study time for maths) 
# influence another variable (e.g., final maths grade). 
# Explicitly we study the causation of one or multiple variables on another.
#
# In predictive modelling we are interested in building a model 
# to predict the dependent variable.
# The focus shifts to predicting the outcome with a set of variables we observe.
# The input variables do not need to necessarily have a causation relationship with
# the output variable, therefore we assess the strength of an association. 
# An example of predictive modelling would be a model for predicting ice-cream sales. 
# One could assess how temperatures can help predict the sales of ice-creams. 
# However note that ice-cream sales do not directly occur because of rising temperatures. 
# One would first need an ice-cream truck and a human to sell the ice-cream.

# ------------------------------------------------------------------------------
#Exercise 5: Five individually picked most relevant variables to predict 
# the final grade
# I decide to implement ML through the use of Akaike information criterion (AIC)
G3 = train$G3
f1 = formula(G3 ~ .)
lm.fit1 = lm(f1, data=test, na.action = na.exclude)
stepreg.fit1 = step(lm.fit1, scope =~., direction="both" , trace=T) 

# Best model selected by AIC 
f_AIC_best= formula(G3 ~ sex + age + address + Fedu + 
                      failures + schoolsup + goout + health)
extractAIC(stepreg.fit1, scale = 0)
summary(lm(f_AIC_best, data=train, na.action = na.exclude))

# We select the variables for the exercise
# Simple model
f2 = formula(G3 ~ sex + failures + schoolsup + goout + health)
lm_fit2=lm(f2, data=train, na.action = na.exclude)
summary(lm_fit2)
MSE2.in = mean(lm_fit2$residuals^2)

print(paste("In-sample MSE Model with 5 covariates:", round(MSE2.in,4)))

# Model with first-order interaction terms
f3=formula(G3 ~   sex + failures + schoolsup + goout + health 
           + (sex + failures + schoolsup + goout + health)^2)
lm_fit3=lm(f3, data=train, na.action = na.exclude)
summary(lm_fit3)
MSE3.in=mean(lm_fit3$residuals^2)

print(paste("In-sample MSE Model with interaction terms:", round(MSE3.in,4)))

# To be honest, the model with 5 covariates performs rather poorly in prediction 
# if we rely solely on the in-sample fit.
# The second model performs slightly better if we solely focus on R-squared and disregard
# the level of significance of the prediction variables. As we add interaction terms
# the R-squared increases. Also the mean squared residuals as expected
# decrease as we add more covariates. 
# Nonetheless it must be said that both models score poorly with R-squared and hence
# it should impact negatively the ability to predict grades while
# using out-of-sample data. 

# ------------------------------------------------------------------------------
#Exercise 6: Add another five individually picked most relevant variables 
# to predict the final grade
# I take the same approach in selection by using the Akaike information criterion

stepreg.fit1 = step(lm.fit1, scope =~., direction="both" , trace=T) 

#I hand-pick a model with 10 covariates with the lowest AIC score

f4 = formula(G3 ~  sex + failures + schoolsup + goout + health +
               age + address + Fedu + paid + Dalc)
lm_fit4=lm(f4, data=train, na.action = na.exclude)
summary(lm_fit4)
MSE4.in=mean(lm_fit4$residuals^2)
print(paste("In-sample MSE Model with 10 covariates, AIC-picked:", round(MSE4.in,4)))

# Model with first-order interaction terms

f5 = formula(G3 ~ sex + failures + schoolsup + goout + health +
               age + address + Fedu + paid + Dalc +
               (sex + failures + schoolsup + goout + health +
                  age + address + Fedu + paid + Dalc)^2)
lm_fit5=lm(f5, data=train, na.action = na.exclude)
summary(lm_fit5)
MSE5.in=mean(lm_fit5$residuals^2)
print(paste("In-sample MSE Model with interaction terms for 10 covariates:", round(MSE5.in,4)))


#Estimation for out-of-sample fit
# Model with 5 variables
fit2 <- predict(lm_fit2, newdata=test) 
# calculate mean squared error 
MSE2.out <- mean((test$G3-fit2)^2)
#(actual data - predicted data)^2
print(paste("Out-of-sample MSE Model 2:", round(MSE2.out,4)))

# Model with 5 variables and all interaction terms
fit3 <- predict(lm_fit3, newdata=test) 
# calculate mean squared error 
MSE3.out <- mean((test$G3-fit3)^2) 
print(paste("Out-of-sample MSE Model 3:", round(MSE3.out,4)))

# Model with 10 variables
fit4 <- predict(lm_fit4, newdata=test) 
MSE4.out <- mean((test$G3-fit4)^2) 
# calculate mean squared error 
print(paste("Out-of-sample MSE Model 4:", round(MSE4.out,4)))

# Model with 10 variables and all first order interaction terms
fit5 <- predict(lm_fit5, newdata=test) 
MSE5.out <- mean((test$G3-fit5)^2) 
# calculate mean squared error 
print(paste("Out-of-sample MSE Model 5:", round(MSE5.out,4)))

#-----------------------------------simple idea, but if we want to impress, we
# may need to consider something more sophisticated #btw this is not a fit plot
#MSE method
# (1) Data Frame for the plot
MSEplot <- data.frame(model = c(rep("5var",2), rep("5var+interaction",2),
                                rep("10var",2), rep("10 var+interaction",2)), 
                        MSE = c(MSE2.in, MSE2.out, MSE3.in, MSE3.out,
                                MSE4.in, MSE4.out, MSE5.in, MSE5.out),
                        type = factor(rep(c("In-sample MSE", "Out-of-sample MSE"),2), 
                                      levels = c("In-sample MSE", "Out-of-sample MSE")))

# (2) Plot

ggplot(MSEplot, aes(y=MSE, x=model, fill=type)) +
  geom_col(position=position_dodge(), color="black") +
  ggtitle("Out-of-sample and in-sample MSE using different OLS models") +
  ylab("Mean Squared Error")
  xlab("Model")+
  theme_minimal()+
  scale_fill_brewer(palette="Pastel1")

#---------------
  ## FIT PLOTS
  # Plot - Model with 5 variables
ggplot(data=train, aes(y=train$G3, x=fitted(lm_fit2))) +
    geom_point() + 
    geom_abline(intercept = 0, slope = 1)  +
    ggtitle("Fit Plot - Model with 5 variables")+
    labs(y="Grades", x="OLS Fitted values with the model")
  
ggplot(data=train, aes(y=train$G3, x=fitted(lm_fit2))) +
    geom_point() + 
    geom_abline(intercept = 0, slope = 1)  +
    ggtitle("Fit Plot - Model with 5 variables")+
    labs(y="Grades", x="OLS Fitted values with the model")
  
  # Plot - Model with 5 variables and interaction terms
ggplot(data=train, aes(y=train$G3, x=fitted(lm_fit3))) +
    geom_point() + 
    geom_abline(intercept = 0, slope = 1)  +
    ggtitle("Fit Plot - Model with 5 variables and interaction variables")+
    labs(y="Grades", x="OLS Fitted values with the model")

#---------------------------------------------------------------------------------
### Fit Plot - Train Sample , In-sample fit plot###

# Specify Outcome Variable
G3_train <- as.matrix(train$G3)
G3_test <- as.matrix(test$G3)

# Create data frame for plotting predictions from models
model_var <- c(rep("5 variables",length(G3_train)), rep("5 variables and interaction terms",length(G3_train)),
               rep("10 variables",length(G3_train)),rep("10 variables and interaction terms",length(G3_train)))
observed_G3 <- c(rep(G3_train, 4))
predicted_G3 <- c(fitted(lm_fit2), fitted(lm_fit3), fitted(lm_fit4),fitted(lm_fit5))

fit.plot <- data.frame("observed grades" = observed_G3, "predicted grades" = predicted_G3, "models" = model_var)

# Plot
ggplot(fit.plot, aes(x=observed_G3, y = predicted_G3, col = model_var)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) +
  xlab("Predicted Grades") + 
  ylab("Observed Grades") + ylim(0,20) + xlim(0,20)

# Here we observe that model with 10 variables and interaction terms performs 
# best in comparison to other models.

### Fit Plot - Test Sample , Out-of-sample fit plot###

# Create data frame for plotting predictions from models
test_model_var <- c(rep("5 variables",length(G3_test)), rep("5 variables and interaction terms",length(G3_test)),
               rep("10 variables",length(G3_test)),rep("10 variables and interaction terms",length(G3_test)))
test_observed_G3 <- c(rep(G3_test, 4))
test_predicted_G3 <- c(fit2, fit3, fit4, fit5)

test_fit.plot <- data.frame("observed grades" = test_observed_G3, "predicted grades" = test_predicted_G3, "models" = test_model_var)


# Plot
ggplot(test_fit.plot, aes(x=test_observed_G3, y = test_predicted_G3, col = test_model_var)) +
  geom_point(size = 1.75) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) +
  xlab("Predicted Grades") + 
  ylab("Observed Grades") + ylim(0,20) + xlim(0,20)

#On an out-of-sample fit we can clearly see that model with 10 variables and interaction terms
# doesn't make the best predictions, quite on the contrary. 
# It is rather hard to observe which model performs the best, but
# from the look of it one could consider either the model with 5 variables or the model with 
# 5 variables and interaction terms. However based on out-of-sample MSE we know that 
# model 10 performs the best with the test data.