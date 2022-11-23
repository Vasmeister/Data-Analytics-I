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









####  1  ####
# 7 x 1204
nrow(data) # observations =1204
ncol(data) # covariates   =7    (assuming all of the columns are covariates, and there is no dependent variable)


####  2  ####
# What is the highest number of children who are covered by AT LEAST one health insurance?
data %>% count(children) # the highest number is 296 (1)
# BUT could be    max(data$children)   =5    from last year

####  3  ####

a=levels(data$region)
b=length(a)
s.m=c()
for (i in 1:b){
  c=length(which(data$region==a[i]&data$smoker=="yes"))/length(which(data$region==a[i]))
  
  sss=paste("The share of smokers in", a[i],"is",as.character(c))
  
  print(sss)
  s.m=c(s.m, c)
}
names(s.m)=a

names(s.m)[s.m==min(s.m)]

#### 4 ####
ggplot(data=data)+
  geom_point(aes(x=age,y=charges,colour=smoker))+
  theme_bw()
# We can find the following patterns:
# We can see from the scatter plot that the charges for medical bills for smokers are
# on average much higher than the charges for people who don't smoke. Having the age
# variable on the x-axis, we can oberve that the charges are correlated with the age of the person.


#### 5 ####
plot2 <-function(data_inp,x.variable,y.variable,color.variable){
  ggplot(data=data_inp)+
    geom_point(aes(x =get(x.variable),y=get(y.variable),colour=get(color.variable)))+
    labs(x=x.variable,y=y.variable,colour=color.variable)+
    theme_bw()}
plot2(data,"bmi","charges","sex")


#### 6 ####
boxplot<-function(data_inp, split_var){
  ggplot(data=data_inp,aes(x =get(split_var),y=bmi))+
    geom_boxplot()+
    labs(x=split_var)+theme_bw()}
boxplot(data,"region")
# The other variable that is passed as the argument is the region.
# There we can see a difference amongst the BMIs in the different region: 
# the BMI seems to be higher in the Southeast region.

