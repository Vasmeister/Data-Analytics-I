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

##### alternative
dim(test) # There are 143 observations in the test data.
dim(train) # There are 214 observations in the train data.$

percent.test <- round(nrow(test)/(nrow(test)+nrow(train)), 3) # ca. 40 per cent of total data sample
percent.train <- round(1-percent.test, 3) # ca. 60 per cent of data

# The sizes of the two data sets are within the recommended sizes 20-40% for test set and 60-80% for the training set.


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

# Notice the data is fairly normally distributed, there are no large outliers for which we need to tranfor the data.

# Exercise 4

# Causal and predictive modelling have in common that they both recur to statistical learning
# to define a function $\hat f$ so that $Y \approx \hatf(X)$, given any true set (X,Y). The core difference 
# lies in their task and their use for the functional form $\hat f(X)$. Causal models aim at finding causal
# relations between covariates and dependent variable; in this view it is concerned with the exact form 
# of the function considered. It requires an interpretable form of $\hat f(X)$. Predictive modelling is about 
# finding a functional form that best predicts/estimates an unobserved dependent variable. The specific form of 
# the function therefore plays lesser of a big role. The important is that the value predicted is
# reliable/close to the true one.

# Exercise 5

# Chosen covariates and why:
# absences: presence durgin classes has been shown to positively impact a student's engagement with the subject studied.
# schoolsup: extra educational school support indeed may affect the knowledge and engagement of a student with his education.
# studytime: hours spent although may vary depending on background of esch individual may highly impact the final grade.
# higher: a motivated person may very likely achieve higher grades as math is a core subject in many advanced higher education. levels.
# internet: access to it means access to information.

lm.5vars <- lm(data = train, G3 ~ absences + internet +  schoolsup + higher + studytime)
summary(lm.5vars)

lm.fo.5.interact <- lm(data = train, G3 ~ absences + internet +  schoolsup + higher + studytime + (absences + internet +  schoolsup + higher + studytime )^2)
summary(lm.fo.5.interact)

MSE.5vars.in.sample <- mean((train$G3 - lm.5vars$fitted.values)^2)
print(paste("MSE Model With 5 Variables:", round(MSE.5vars.in.sample, 4))) 

MSE.fo.5.in.sample <- mean((train$G3 - lm.fo.5.interact$fitted.values)^2)
print(paste("MSE Model With Interaction Variables:", round(MSE.fo.5.in.sample, 4))) 

# Apparently the MSE is lower with more covariates. 
# This is due to the fact that we train our model in-sample and we test it in-sample. 
# The model logically will do a better job at predicting the test score the more 
# covariates we add, as each will add a little more information to what is in the error term.

# Exercise 6

# Fedu
# failures
# famsup
# paid
# freetime ### add why we chose them

lm.10vars <- lm(data = train, G3 ~ absences + internet +  schoolsup + 
                  higher + studytime + Fedu + failures + famsup + 
                  paid + freetime)
summary(lm.10vars)

lm.fo.10.interact <- lm(data = train, G3 ~ absences + internet +  schoolsup + 
                          higher + studytime + Fedu + failures + famsup + 
                          paid + freetime + (absences + internet +  schoolsup + 
                                               higher + studytime + Fedu + failures + famsup + 
                                               paid + freetime)^2)
summary(lm.fo.10.interact)

MSE.10vars.in.sample <- mean((train$G3 - lm.10vars$fitted.values)^2)
print(paste("MSE Model With 10 Variables:", round(MSE.10vars.in.sample, 4))) 

MSE.fo.10.in.sample <- mean((train$G3 - lm.fo.10.interact$fitted.values)^2)
print(paste("MSE Model With 10 and Interaction Variables:", round(MSE.fo.10.in.sample, 4)))
 
# We specify new data
fitted.1 <- predict(lm.5vars, newdata = test) # We obtain a new object in the environment with exactly 143 entries.

MSE.5vars.out.sample <- mean((test$G3 - fitted.1)^2)
print(paste("MSE Model With 5 Variables:", round(MSE.5vars.out.sample, 4))) 

fitted.2 <- predict(lm.fo.5.interact, newdata = test)

MSE.fo.5.out.sample <- mean((test$G3 - fitted.2)^2)
print(paste("MSE Model 5 Variables With Interaction Variables:", round(MSE.fo.5.out.sample, 4))) 

fitted.3 <- predict(lm.10vars, newdata = test)

MSE.10vars.out.sample <- mean((test$G3 - fitted.3)^2)
print(paste("MSE Model With 10 Variables:", round(MSE.10vars.out.sample, 4))) 

fitted.4 <- predict(lm.fo.10.interact, newdata = test)

MSE.fo.10.out.sample <- mean((test$G3 - fitted.4)^2)
print(paste("MSE Model 10 Variables With Interaction Variables:", round(MSE.fo.10.out.sample, 4))) 

# Apparently, the functional form with 10 variables fits the best (model 3).

# We plot the results

MSEout <- data.frame(model = c("Model.5.vars", "Model.5.inter", "Model.10.vars", "Model.10.inter"), MSE.out = c(MSE.5vars.out.sample, MSE.fo.5.out.sample , MSE.10vars.out.sample , MSE.fo.10.out.sample))
MSEin <- data.frame(model = c("Model.5.vars", "Model.5.inter", "Model.10.vars", "Model.10.inter"), MSE.in = c(MSE.5vars.in.sample, MSE.fo.5.in.sample, MSE.10vars.in.sample, MSE.fo.10.in.sample))

combined.data<- cbind(MSEin, MSEout)

ggplot() +
  geom_col(data = MSEout, aes(model, MSE.out), fill="blue", alpha = 0.2) +
  geom_col(data = MSEin, aes(model, MSE.in), fill="orange2", alpha = 0.6) +
  ylab("MSE")+ 
  annotate("text", x=4, y=12, label= "In blue: MSEs out sample") +
  annotate("text", x=4, y=11, label= "In orange: MSEs in sample")

























###################################################          RMARKDOWN DRAFT       ######################################







---
title: "PS2_Markdown"
author: 
- José Maria de Mattamouros Resende Fonseca de Oliveira (22-602-783)
- Linda Fiorina Odermatt (17-946-310)
- Elena Trevisani (22-620-603) 
- Vasily Zhuravlev (18-502-401) 
date: "2022-12-02"
output: pdf_document
---

## Exercise 1

```{r, results=FALSE}
load("/Users/nenetrevisani/Desktop/data an/DA2/student-mat-test.Rdata")
load("/Users/nenetrevisani/Desktop/data an/DA2/student-mat-train.Rdata")
```
```{r}
nrow(data)
ncol(data)
```

There are 143 observations in the test set, and 214 observations in the train set.

```{r}
nrow(test)/(nrow(test)+nrow(train)) 
```

The ratio is roughly 40:60% between the test and the train data sets.
The sizes of the two data sets are within the recommended sizes 20-40% for test set and 60-80% for the training set.

## Exercise 2

```{r}
grade_summary=matrix(data=data.frame(col1=mean(train$G3),
                                     col2=min(train$G3),
                                     col3=max(train$G3)))
colnames(grade_summary)=c("G3")
rownames(grade_summary)=c("Mean","Min","Max")

grade_summary
```

Therefore, we find the following in the training data:the average grade is 11.64, the minimum is 4 and the maximum is 19.00.

## Exercise 3
```{r, results=FALSE, include=FALSE}
library(ggplot2)
```
```{r}
ggplot(data=train, aes(x=train$G3)) + 
  geom_histogram(binwidth =1 , color = "darkblue", fill = "lightblue") +
  ggtitle("Histogram for Student Grades in Maths, rounded to the closest figure")+
  ylab("Count")+
  xlab("Final Math Grade (0-20)")
```

Notice the data is fairly normally distributed, there are no large outliers for which we need to tranfor the data.

## Exercise 4

add text

## Exercise 5

Chosen covariates and why:absences: presence durgin classes has been shown to positively impact a student's engagement with the subject studied.schoolsup: extra educational school support indeed may affect the knowledge and engagement of a student with his education. studytime: hours spent although may vary depending on background of esch individual may highly impact the final grade.
higher: a motivated person may very likely achieve higher grades as math is a core subject in many advanced higher education. levels. internet: access to it means access to information.


```{r}
lm.5vars <- lm(data = train, G3 ~ absences + internet +  schoolsup + higher + studytime)
summary(lm.5vars)

lm.fo.5.interact <- lm(data = train, G3 ~ absences + internet +  schoolsup + higher + studytime + (absences + internet +  schoolsup + higher + studytime )^2)
summary(lm.fo.5.interact)

MSE.5vars.in.sample <- mean((train$G3 - lm.5vars$fitted.values)^2)
print(paste("MSE Model With 5 Variables:", round(MSE.5vars.in.sample, 4))) 

MSE.fo.5.in.sample <- mean((train$G3 - lm.fo.5.interact$fitted.values)^2)
print(paste("MSE Model With Interaction Variables:", round(MSE.fo.5.in.sample, 4))) 
```

Apparently the MSE is lower with more covariates. This is due to the fact that we train our model in-sample and we test it in-sample. The model logically will do a better job at predicting the test score the more covariates we add, as each will add a little more information to what is in the error term.

## Exercise 6

Variables chosen: Fedu, failures, famsup, paid, freetime. Bc ...

```{r}
lm.10vars <- lm(data = train, G3 ~ absences + internet +  schoolsup + 
                  higher + studytime + Fedu + failures + famsup + 
                  paid + freetime)
summary(lm.10vars)

lm.fo.10.interact <- lm(data = train, G3 ~ absences + internet +  schoolsup + 
                          higher + studytime + Fedu + failures + famsup + 
                          paid + freetime + (absences + internet +  schoolsup + 
                                               higher + studytime + Fedu + failures + famsup + 
                                               paid + freetime)^2)
summary(lm.fo.10.interact)

MSE.10vars.in.sample <- mean((train$G3 - lm.10vars$fitted.values)^2)
print(paste("MSE Model With 10 Variables:", round(MSE.10vars.in.sample, 4))) 

MSE.fo.10.in.sample <- mean((train$G3 - lm.fo.10.interact$fitted.values)^2)
print(paste("MSE Model With 10 and Interaction Variables:", round(MSE.fo.10.in.sample, 4)))

# We specify new data
fitted.1 <- predict(lm.5vars, newdata = test) # We obtain a new object in the environment with exactly 143 entries.

MSE.5vars.out.sample <- mean((test$G3 - fitted.1)^2)
print(paste("MSE Model With 5 Variables:", round(MSE.5vars.out.sample, 4))) 

fitted.2 <- predict(lm.fo.5.interact, newdata = test)

MSE.fo.5.out.sample <- mean((test$G3 - fitted.2)^2)
print(paste("MSE Model 5 Variables With Interaction Variables:", round(MSE.fo.5.out.sample, 4))) 

fitted.3 <- predict(lm.10vars, newdata = test)

MSE.10vars.out.sample <- mean((test$G3 - fitted.3)^2)
print(paste("MSE Model With 10 Variables:", round(MSE.10vars.out.sample, 4))) 

fitted.4 <- predict(lm.fo.10.interact, newdata = test)

MSE.fo.10.out.sample <- mean((test$G3 - fitted.4)^2)
print(paste("MSE Model 10 Variables With Interaction Variables:", round(MSE.fo.10.out.sample, 4))) 
```

Apparently, the functional form with 10 variables fits the best (model 3).

#```{r}
MSEout <- data.frame(model = c("Model.5.vars", "Model.5.inter", "Model.10.vars", "Model.10.inter"), MSE.out = c(MSE.5vars.out.sample, MSE.fo.5.out.sample , MSE.10vars.out.sample , MSE.fo.10.out.sample))
MSEin <- data.frame(model = c("Model.5.vars", "Model.5.inter", "Model.10.vars", "Model.10.inter"), MSE.in = c(MSE.5vars.in.sample, MSE.fo.5.in.sample, MSE.10vars.in.sample, MSE.fo.10.in.sample))

combined.data<- cbind(MSEin, MSEout)

ggplot() +
  geom_col(data = MSEout, aes(model, MSE.out), fill="blue", alpha = 0.2) +
  geom_col(data = MSEin, aes(model, MSE.in), fill="orange2", alpha = 0.6) +
  ylab("MSE")+ 
  annotate("text", x=4, y=12, label= "In blue: MSEs out sample") +
  annotate("text", x=4, y=11, label= "In orange: MSEs in sample")
Footer
#```

### Fit Plot - Train Sample , In-sample fit plot


Specify Outcome Variable

#```{r}
G3_train <- as.matrix(train$G3)
G3_test <- as.matrix(test$G3)
#```

#```{r}

Create data frame for plotting predictions from models

model_var <- c(rep("5 variables",length(G3_train)), rep("5 variables and interaction terms",length(G3_train)),
               rep("10 variables",length(G3_train)),rep("10 variables and interaction terms",length(G3_train)))
observed_G3 <- c(rep(G3_train, 4))
predicted_G3 <- c(fitted(lm.fo.5.interact), fitted(lm.fo.5.interact), fitted(lm.10vars),fitted(lm.fo.10.interact))

#``` 

Plot

#```{r}
ggplot(fit.plot, aes(x=observed_G3, y = predicted_G3, col = model_var)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) +
  xlab("Predicted Grades") + 
  ylab("Observed Grades") + ylim(0,20) + xlim(0,20)

#``` 

Here we observe that model with 10 variables and interaction terms performs best in comparison to other models.

### Fit Plot - Test Sample , Out-of-sample fit plot

Create data frame for plotting predictions from models

#```{r}

test_model_var <- c(rep("5 variables",length(G3_test)), rep("5 variables and interaction terms",length(G3_test)),
               rep("10 variables",length(G3_test)),rep("10 variables and interaction terms",length(G3_test)))
test_observed_G3 <- c(rep(G3_test, 4))
test_predicted_G3 <- c(fitted.1, fitted.2, fitted.3, fitted.4)

test_fit.plot <- data.frame("observed grades" = test_observed_G3, "predicted grades" = test_predicted_G3, "models" = test_model_var)

#``` 

Plot

#```{r}

ggplot(test_fit.plot, aes(x=test_observed_G3, y = test_predicted_G3, col = test_model_var)) +
  geom_point(size = 1.75) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) +
  xlab("Predicted Grades") + 
  ylab("Observed Grades") + ylim(0,20) + xlim(0,20)

#``` 

On an out-of-sample fit we can clearly see that model with 10 variables and interaction terms doesn't make the best predictions, quite on the contrary. 
It is rather hard to observe which model performs the best, but from the look of it one could consider either the model with 5 variables or the model with 5 variables and interaction terms. 
However based on out-of-sample MSE we know that model 10 performs the best with the test data.



