# Code written by: Eric Haney
# For: Data 630 Fall 2021
# Assignment 4: Classification using neural networks
# Note: some of this code is based off of ungraded assignment in class module
# Install packages necessary for creating neural networks model
install.packages("neuralnet")     
library("neuralnet")               
install.packages("NeuralNetTools") 
library ("NeuralNetTools")

# Set the working directory where the heart attack dataset is on compute
setwd("/Users/ehane/OneDrive - UMGC/Desktop/Eric's Schoolwork/DATA630/Week 9")
# Read the heart attack dataset using the read command
attack<-read.csv(file="whas1.csv", head=TRUE, sep=",")

# Display summary statistics of the dataset
summary(attack)

# Confirm that there are no missing values in the dataset
apply(attack, MARGIN =2, anyNA)

# Remove the unique ID column which will not be used in analysis
attack$ID<-NULL

# Scale the first 12 variables so they all have same mean of 0
attack[1:12]<-scale(attack[1:12])

# Take a look at the internal structure and summary statistics to confirm that we have removed ID and scaled
str(attack)
summary(attack)

# Run the neural networks model

# Split the data into training and test data
set.seed(1234)
ind <- sample(2, nrow(attack), replace = TRUE, prob = c(0.7, 0.3))
train.data <- attack[ind == 1, ]  #70%
test.data <- attack [ind == 2, ]  #30%

# Build model with one hidden layer and the 12 nodes as inputs
nn <- neuralnet(FSTAT~AGE+SEX+CPK+SHO+CHF+MIORD+MITYPE+YEAR+YRGRP+LENSTAY+DSTAT+LENFOL, data=train.data, hidden=c(8), threshold = 0.2)

# Plot your neural networks in blue using the visualization package
plot(nn)
plotnet(nn)
plotnet(nn, circle_col="blue")

# Relative importance of independent variaables considering weights
garson(nn)  
# Relative importance of the independent variables considering the weights and signs
olden(nn)

# Create a confusion matrix for the training data
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)            # Round the predicted probabilities
table(mypredict, train.data$FSTAT, dnn =c("Predicted", "Actual"))
mean(mypredict==train.data$FSTAT)

# Create confusion matrix for the test data
testPred <- compute(nn, test.data[, 0:12])$net.result
testPred<-apply(testPred, c(1), round)
testPred
table(testPred, test.data$FSTAT, dnn =c("Predicted", "Actual"))
mean(testPred==test.data$FSTAT)

