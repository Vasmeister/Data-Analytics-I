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
# Date: 24.11.2022
## Topic: Model Evaluation
###############################################################################
 ------------------------------------------------------------------------------
# Step 1: Set the working directory:
# Find which directory you are currently working in
ddpath 
getwd()
# Set the directory you would like to work in
setwd() #in our case we insert "~/Data Analytics I - Predictive Econometrics/Lecture 2"

# Download the data
load("insurance-all.Rdata") # Load data

#############################

# ------------------------------------------------------------------------------
# Step 2: Install the relevant packages for solving the problem

###########################
# Caret package #
###########################

# install.packages("caret")
library(caret)

# Other packages
# install.packages("dplyr")
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
####  1  ####
# 7 x 1204
nrow(data) # observations =1204
ncol(data) # covariates   =7    (assuming all of the columns are covariates, and there is no dependent variable)


####  2  ####
# What is the highest number of children who are covered by AT LEAST one health insurance?
data %>% count(children) # the highest number is 296 (1)

# However, if the question adresses the highest number of children per single household who is covered by health insurance we find to folowing answer:  
max(data$children)  # the highest n. of children is 5 per 14 households

# ------------------------------------------------------------------------------
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
# -------------------------------
# Alternative with matrix table
cond_matrix = matrix(NA, nrow=length(levels(data$smoker)), ncol=length(levels(data$region)))

cond_matrix # the matrix is indeed empty and full of NA

# Proceed with a for loop
a=levels(data$region)
region_matrix=matrix(a,ncol=length(a))
region_matrix
colnames(cond_matrix)=region_matrix
rownames(cond_matrix)=levels(data$smoker)

for (a in c(1:4)) {
  cond_matrix[1,a] <- sum(data$smoker=="no" & data$region==region_matrix[,a])/sum(data$region==region_matrix[,a])
  cond_matrix[2,a] <- sum(data$smoker=="yes" & data$region==region_matrix[,a])/sum(data$region==region_matrix[,a])
  
  }

print(cond_matrix) # the matrix has the full answer summed in a table for each region

# region with the lowest share of smokers: northwest, the share is
min(cond_matrix[2,]) # 0.1689655

# ------------------------------------------------------------------------------
#### 4 ####
ggplot(data, aes(x=age, y=charges, color=smoker)) +
  geom_point(na.rm=TRUE) +
  ggtitle("Scatter Plot of Age vs. Charges Given Smoking Status") + ylab("Charges") + xlab("Age") +
  geom_smooth(method = "lm", se = FALSE)

# From the scatter plot we find the following patterns:
# We see that the charges for medical bills for smokers are on average
# much higher than the charges for people who don't smoke. Having the age
# variable on the x-axis, we oberve that the charges correlate with age. 
# As people age, they should expect their medical charges to increase.

#------------------------------------------------------------------------------
# We observe that there is an overall positive correlation between the level of charge and the age 
# for both non- and smokers. However, we can clearly distinguish how smokers pay greater charges 
# on average than non-smokers. Moreover, the data for smokers is more dispersed around the scatter plot.
# While controlling for age, the variance in charges for smokers is greater than for non smokers.

# ------------------------------------------------------------------------------
#### 5 ####
# Create a function
compact_scatter_plot <-function(data_inp,x.variable,y.variable,color.variable){
    ggplot(data_inp, aes(x=get(x.variable), y=get(y.variable), color=get(color.variable)))+ 
    geom_point(na.rm=TRUE)+ ggtitle("Scatter Plot") +
    labs(x=x.variable,y=y.variable,colour=color.variable)+
    geom_smooth(method = "lm", se = FALSE)
}

# Execute the function
compact_scatter_plot(data,"bmi","charges","sex")

# We observe that as bmi increases, the variance of charges also increases, therefore
# the data has heteroskedasticity implied in it. Moreover, we observe in general
# a weakly positive correlation between the bmi and charges, more so for males. 
# A possible explanation could be that males are more likely to afford medical costs.
# A possible way to improve the analysis, is by also accounting for the wage differences of 
# between individuals.

# ------------------------------------------------------------------------------
#### 6 ####
# Create a function
split_boxplot_bmi<-function(data_inp, split_var){
  ggplot(data=data_inp,aes(x =data_inp$bmi, y=get(split_var)))+
    geom_boxplot(outlier.colour="red", outlier.shape=20, outlier.size=2)+
    labs(x="bmi", y=split_var)
}

# Execute the function
split_boxplot_bmi(data,"region")


# The other variable that is passed as the argument is the region.
# There we can see a difference amongst the BMIs in the different region: 
# the BMI seems to be higher in the Southeast region.
# ------------------------------------------------------------------------------
# We observe a difference in the distribution of bmi when we account for each region.
# For instance, the median in southeast is the highest and also has a wider 
# distribution of individuals with ranging bmi. On the contrary, both regions in the
# north have a smaller data distribution with the least narrow quartiles. 
# For instance, northwest has the most narrow quartiles. 
