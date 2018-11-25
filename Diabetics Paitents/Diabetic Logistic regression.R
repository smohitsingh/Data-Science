# Statement :


# Business objecive :

# Goal of analysis :


##------ Loading Packages ------##

# Including Required packages for Reading data, Manipulation of data, Visualization of data.

library(dplyr)    
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

##__________ SETTING-UP WORKING DIRECTORY _________

setwd("C:/Users/Mohit Singh/Desktop/Data Science Portfolio/Diabetic logistic")
getwd()        # To check whether the file is properly loaded or not
dir()          # To Check the Target file Name : 

# Clean the file for any Pre-Used valiables
remove(list = ls())


##_________ DATA LOADING _________

# Creating a New Data frame bike_data  
# Blank spaces if any will be replaced by NA using na.strings and we dont want strings to be converted to factors automatically while reading file 
diabetic <- read.csv("diabetes.csv",na.strings = c("",NA),header = TRUE, stringsAsFactors = FALSE)
View(diabetic)

str(diabetic)
summary(diabetic)
dim(diabetic)

# "diabetic" data set has 768 observations and 9 variables.
# All data type are numerical.

colSums(is.na(diabetic))
# Na values are not present here.
# Data cleaning not required here.

nrow(unique(diabetic))
# we have all unique observations.


#-------- EDA ----------#

#------- Univariate Analysis --------#
## ------- Quantitative Data

ggplot(diabetic, aes( x = diabetic$Pregnancies)) + geom_bar(fill = "aquamarine3")
table(diabetic$Pregnancies)

ggplot(diabetic, aes( x = diabetic$Glucose)) + geom_histogram()
ggplot(diabetic, aes( y = diabetic$Glucose)) + geom_boxplot()
# outlier
quantile(diabetic$Glucose,seq(0,1,0.01))
diabetic$Glucose[diabetic$Glucose < 57] <- 57


ggplot(diabetic, aes( x = diabetic$BloodPressure)) + geom_histogram()
ggplot(diabetic, aes( y = diabetic$BloodPressure)) + geom_boxplot()
quantile(diabetic$BloodPressure,seq(0,1,0.01))
# outlier
diabetic$BloodPressure[diabetic$BloodPressure < 38.7] <- 38.7
diabetic$BloodPressure[diabetic$BloodPressure > 94.99] <- 94.99


table(diabetic$SkinThickness)
ggplot(diabetic, aes( x = diabetic$SkinThickness)) + geom_histogram(binwidth = 5 ,fill = "aquamarine3")
ggplot(diabetic, aes( y = diabetic$SkinThickness)) + geom_boxplot()
# outlier
quantile(diabetic$SkinThickness,seq(0,1,0.01))
diabetic$SkinThickness[diabetic$SkinThickness > 51.33] <- 51.33


ggplot(diabetic, aes( x = diabetic$Insulin)) + geom_histogram()
table(diabetic$Insulin)
ggplot(diabetic, aes( y = diabetic$Insulin)) + geom_boxplot()
# outlier
quantile(diabetic$Insulin,seq(0,1,0.01))
diabetic$Insulin[diabetic$Insulin > 224.85] <- 224.85


table(diabetic$BMI)
ggplot(diabetic, aes( x = diabetic$BMI)) + geom_histogram(binwidth = 2)
ggplot(diabetic, aes( y = diabetic$BMI)) + geom_boxplot()
# outlier
quantile(diabetic$BMI,seq(0,1,0.01))
diabetic$BMI[diabetic$BMI < 19.168] <-  19.168
diabetic$BMI[diabetic$BMI > 47.526] <- 47.526



table(diabetic$DiabetesPedigreeFunction)
ggplot(diabetic, aes( x = diabetic$DiabetesPedigreeFunction)) + geom_histogram()
ggplot(diabetic, aes( y = diabetic$DiabetesPedigreeFunction)) + geom_boxplot()
# Outlier 
quantile(diabetic$DiabetesPedigreeFunction,seq(0,1,0.01))
diabetic$DiabetesPedigreeFunction[diabetic$DiabetesPedigreeFunction > 1.184240 ] <- 1.184240
  

table(diabetic$Age)
ggplot(diabetic, aes( x = diabetic$Age)) + geom_bar(fill = "aquamarine3")
ggplot(diabetic, aes( y = diabetic$Age)) + geom_boxplot()
# Outlier 
quantile(diabetic$Age,seq(0,1,0.01))
diabetic$Age[diabetic$Age > 64.66 ] <- 64.66


table(diabetic$Outcome)
ggplot(diabetic, aes( x = diabetic$Outcome)) + geom_bar(fill = "aquamarine3")


pairs(diabetic)

# Transformation for making variables normally distributed


diabetic[1] <- log(diabetic$Pregnancies+1)

min(diabetic[2])
diabetic[2] <- log(diabetic[2])

min(diabetic[3])
diabetic[3] <- log(diabetic[3])

min(diabetic[4])
diabetic[4] <- log(diabetic[4]+1)

min(diabetic[5])
diabetic[5] <- log(diabetic[5]+1)


min(diabetic[6])
diabetic[6] <- log(diabetic[6])
min(diabetic[7])
diabetic[7] <- log(diabetic[7]+1)
min(diabetic[8])
diabetic[8] <- log(diabetic[8])

# Model Building

library(caTools)
set.seed(123)
split = sample.split(diabetic, SplitRatio = 0.75)
training_set = subset(diabetic, split == TRUE)
test_set = subset(diabetic, split == FALSE)

# Feature Scaling
training_set[-9] = scale(training_set[-9])
test_set[-9] = scale(test_set[-9])

# Checking dimensions of train and test.
dim(training_set)
dim(test_set)
# Train data set: 513 Observations
# Test data set: 255 Observations

# Exporting train and test datasets for modelling in Python.
write.csv(training_set,"train.csv",row.names = FALSE)
write.csv(test_set,"test.csv",row.names = FALSE)


##------------ MODEL BUILDING --------------##

## ---- logistic regression model

model <- glm ( Outcome ~ ., data = training_set, family = binomial(link = 'logit'))
summary(model)

anova(model, test="Chisq")


predict <- predict(model,newdata = test_set[-9], type = 'response')

predict <- ifelse(predict > 0.5,1,0)

#confusion matrix
cm = table(test_set[,9], predict)
cm

# Checking accuracy
misClasificError <- mean(predict != test_set[9])
paste('Accuracy',1-misClasificError)
#  Accuracy od model on test dateset : 76.47%

# ROC Curve
library(ROCR)

pr <- prediction(predict, test_set$Outcome)

perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)

perf1 <- performance(pr, measure = "auc")
perf1@y.values[[1]]
# 73.07

perf2 <- performance(pr, measure='acc') #Simple accuracy, what % were right?

perf3 <- performance(pr, measure='prec') #What % of the elements I predicted to be in the class actually?
perf4 <- performance(pr, measure='recall') #What % of the elements that are in class, did I predict to be in this class?

perf5 <- performance(pr, measure='f') #F-measure a balance between them
perf6 <- performance(pr, measure='auc') #Area Under the Curve, another way to balance them

perf1@y.values[[1]]
# 73.07

## KNN classifier.
library(class)
y_pred = knn(train = training_set[,-9],
             test = test_set[,-9],
             cl = training_set[,9],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
cm = table(test_set[,9], y_pred)
cm

## Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Outcome ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred_svm = predict(classifier, newdata = test_set[-9]) 
table(y_pred_svm) 
table(test_set[9])
# Making the Confusion Matrix
cm = table(test_set[,9], y_pred_svm)
cm


## Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Outcome ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred_svmr = predict(classifier, newdata = test_set[-9])

# Making the Confusion Matrix
cm = table(test_set[,9], y_pred_svmr)
cm


## Fitting naiveBayes to the Training set
# install.packages('e1071')
library(e1071)
classifier = naiveBayes(x = training_set[-9],
                        y = training_set$Outcome)

# Predicting the Test set results
y_prednv = predict(classifier, newdata = test_set[-9])

# Making the Confusion Matrix
cm = table(test_set[, 9], y_pred)
cm

## Decision tree
library(rpart)
classifier = rpart(formula = Outcome ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[,-9], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-9],
                          y = training_set$Outcome,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-9])
y_pred
# Making the Confusion Matrix
cm = table(test_set[,9], y_pred)
cm





