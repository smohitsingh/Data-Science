
###--------- SVM - Handwritten Digit Recognition Assignment --------####

##--- Business Understanding ---##

# A classic problem in the field of pattern recognition is that of handwritten digit recognition.
# Suppose, that we have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices. 
# The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 
# The given digits are 28 X 28 pixels.

##--- Goal of Analysis ---##

# To develop a model using Support Vector Machine (SVM) which should correctly classify 
# the handwritten digits based on the pixel values given as features in the data-set. 


## ------- Installing & Loading Required Packages --------- ##

library(caret) 
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)

# Removing the used variables
remove(list = ls())
ls()      # Above command is executed sucessfully


## ------- LOADING DATA --------- ##

setwd("C:/Users/Mohit Singh/Desktop/SVM assignment")
getwd()
# Checked, Directory Added successfully

# Checking for the target file in the directory added
dir()   
# We are provided 2 Data File's. 

# For this problem, we use the MNIST data which is a large database 
# of handwritten digits where we have 28 X 28 pixel values of each digit along with its label. 


# Loading the given 2 data file's in "mnist_train" and "mnist_test" variables.

train_data <- read.csv("mnist_train.csv",header = FALSE , na.strings = c("",NA))
test_data <- read.csv("mnist_test.csv",header = FALSE , na.strings = c("",NA))

# As per instructions as the data size is very large which slows down processing
# We will be taking 15% of data from both Data-sets.

# "V1" is the column name with our target DIGITS in both given datasets.

#---------- DATA CLEANING -----------#

# Checking of any duplicated data 
table(duplicated(train_data))
table(duplicated(test_data))
# Hence, no duplicated data found in both the data frames.

# Check for "NA" values in data
table(is.na(train_data))
table(is.na(test_data))
# Hence, no Missing data found as "NA" in both the data frames.


##--------TAKING SAMPLE 15% OF DATA --------##

# Checking the actual distribution of DIGITS in the "train_data" and "test_data" data frames.
round(prop.table(table(train_data$V1)),2)
round(prop.table(table(test_data$V1)),2)
# The distribution of classes in our target variable is same in both "train_data" and "test_data".


##-- TRAIN DATA SAMPLE --##

# Calculation of number of rows for our new extracted data from "train_data".
nrow(train_data)
nrow(train_data)*0.15
# "train_data" data frame has 60,000 rows and
# Its 15% is 9,000 rows for our data to be extracted.

# In order to get the same sample data, we set seed = 100 here.
set.seed(100)
sample_indix <- sample(1: nrow(train_data), 0.15*nrow(train_data)) 
mnist_train <- train_data[sample_indix, ]
dim(mnist_train) 
# Now, we created new data frame "mnist_train" which has 9000 observations & 785 columns.


# Checking the actual distribution of DIGITS in the "train_data" and "mnist_train" data frames.
round(prop.table(table(train_data$V1)),2)
round(prop.table(table(mnist_train$V1)),2)
# The distribution of data is same in both "train_data" and "test_data", 
# So our extraction done right, to maintain the stratification of "V1" column digit's, 
# which we have specified while splitting earlier to maintain the "digit" distribution in "mnist_train" same as original data frame "train_data".
# and the digit distibution in "V1" column is found same.
# So, our final data frame "mnist_train" is ready for analysis. 


##----- TEST DATA SAMPLE ------##

# Calculation of number of rows for our new extracted data from "test_data".
nrow(test_data)
nrow(test_data)*0.15
# "test_data" data frame has 10,000 rows and
# Its 15% is 1,500 rows for our data to be extracted.

# In order to get the same sample data, we set seed = 112 here.
set.seed(112)
sample_indix <- sample(1:nrow(test_data), 0.15*nrow(test_data)) 
mnist_test <- test_data[sample_indix, ]
dim(mnist_test) 
# Now, we created new data frame "mnist_test" which has 1,500 observations & 785 columns.

# Checking the actual distribution of DIGITS in the "test_data" and "mnist_test" data frames.
round(prop.table(table(test_data$V1)),2)
round(prop.table(table(mnist_test$V1)),2)
# The distribution of data is same in both "mnist_test" and "test_data", 
# So our extraction done right, to maintain the stratification of "V1" column digit's, 
# which we have specified while splitting earlier to maintain the "digit" distribution in "mnist_test" same as original data frame "test_data".
# and the digit distibution in "V1" column is found same.
# So, our final data frame "mnist_test" is ready for analysis. 


##-------- DATA UNDERSTANDING --------##

#--- Checking "mnist_train" structure

class(mnist_train)
summary(mnist_train)
str(mnist_train)
# We have all columns of integer type
dim(mnist_train)
# "mnist_train" data frame contains 9,000 observations and 785 variables.
sort(unique(mnist_train$V1))
# 0 1 2 3 4 5 6 7 8 9 are digit's we have as Dependent Variable.  

# Lets check for pixel values given for any digit,
min(mnist_train[1, 2:ncol(mnist_train)])
max(mnist_train[1, 2:ncol(mnist_train)])
# minimum value is 0 & maximum value is 255
# So we have range of pixel values between [0 to 255] both inclusive.
pixel <- length(mnist_train[1, 2:ncol(mnist_train)])
pixel
# taking square root of pixel to find the frame dimensions,
sqrt(pixel)
# Total we have 784 values of pixels(also called as features) for all digit's, 
# which means the digits are on a 28 X 28 image.


#----- Checking "mnist_test" structure's

class(mnist_test)
summary(mnist_test)
str(mnist_test)
# We have all columns of integer type
dim(mnist_test)
# "mnist_test" data frame contains 10,000 observations and 785 variables.
sort(unique(mnist_test$V1))
# 0 1 2 3 4 5 6 7 8 9 are digit's we have as Dependent Variable.

# Lets check for pixel values given for any digit,
min(mnist_test[1, 2:ncol(mnist_test)])
max(mnist_test[1, 2:ncol(mnist_test)])
# minimum value is 0 & maximum value is 255
# So we have range of pixel values between [0 to 255] both inclusive.
pixel <- length(mnist_test[1, 2:ncol(mnist_test)])
pixel
# taking square root of pixel to find the frame dimensions,
sqrt(pixel)
# Total we have 784 values of pixels(also called as features) for all digit's, 
# which means the digits are on a 28 X 28 frame.

# Our First column in both the train and test datsets are our target variable,
# It contains the Digits which we are required to be classified.
# So, we are renaming the 1st column as "digit".
names(mnist_train)[1] <- "digit"
names(mnist_test)[1]  <- "digit"

# Making factor of target variables containing classes.
mnist_train$digit <- factor(mnist_train$digit)
mnist_test$digit <- factor(mnist_test$digit)
# Checking for changes made in classes
class(mnist_train$digit)
class(mnist_test$digit)


##-------- EXPLORATORY DATA ANALYSIS --------##

### Let's check the distribution of "digit" in "mnist_train"

##  "digit" 
ggplot(data = mnist_train) +
  aes(x = digit, fill = digit) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_viridis_d(option  = "viridis") +
  labs(title = "Percentage of Digit distribution ", y= "Percentage") +
  theme_light()

# The sampled data "mnist_train" has classes of "digits" in a fairly equal distribution.
# Such a Balanced data is always Good to have.


### Let's check the distribution of "digit" in "mnist_test".

##  "digit" 
ggplot(data = mnist_test) +
  aes(x = digit, fill = digit) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_viridis_d(option  = "viridis") +
  labs(title = "Percentage of Digit distribution ", y= "Percentage") +
  theme_light()

# The sampled data "mnist_test" has classes of "digits" in a fairly equal distribution.
# Such a Balanced data is always Good to have.


##----------- MODEL BUILDING & EVALUATION--------------##

# Not doing scaling of data here as all the features contain pixel values in the same range 0 to 255.
# Also, data has pixel values by scaling them means images intensities of images will get affected, to get the same results as in original data.

# Making copy for "mnist_train" and "mnist_test" data.
training_set <- mnist_train
test_set <- mnist_test
# Now our data is ready and will fulfill the assumptions made by model.
# Starting model building phase.


## 1. Simple linear kernel model with C=1 (default value).

linear_model <- ksvm(digit~., data = training_set, scaled = FALSE,kernel = "vanilladot", C=1)

print(linear_model) 
# Training error : 0 
# parameter : cost C = 1 
# Linear (vanilla) kernel function. 
# Number of Support Vectors : 2545 

# Predicting the model on "test_set"   
evaluate1 <- predict(linear_model,test_set,type = "response")

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate1, test_set$digit)

# Accuracy    : 91.4 %
# 95% CI : (0.8987, 0.9277)
# Range for Sensitivity : (max) 99.3 % - 79.6 % (min)         
# Range for Specificity : (max) 99.7 % - 98.4 % (min)
# Kappa : 0.9044  

# We get the 91.4 % accuracy for C = 1 (default).


#--- Now, Lets check Simple linear kernel model with "C=10".

linear_model.1 <- ksvm(digit~., data = training_set, scaled = FALSE,kernel = "vanilladot", C=10)

print(linear_model.1) 
# parameter : cost C = 10 
# Linear (vanilla) kernel function 
# Number of Support Vectors : 2545 
# Training error : 0 

# Predicting the model on "test_set"   
evaluate2 <- predict(linear_model.1,test_set,type = "response")

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate2, test_set$digit)

# Accuracy    : 91.4 %
# 95% CI      : (0.8987, 0.9277)
# Range for Sensitivity : (max) 99.3 % - 79.6 % (min)         
# Range for Specificity : (max) 99.6 % - 98.4 % (min)
# Kappa : 0.9044     

# With C = 10, the accuracy remained the same as with C=1 in our previous model, 
# So, next let's check for lower values.


##---- Hyperparameter tuning and Cross Validation : Linear-SVM -----##

# We will use the train function from caret package to perform crossvalidation

# Setting seed = 1111(any random number) to get same data split on cv, when run any number of times.  
set.seed(1111)
train.control <- trainControl(method="cv", number=5)
# number - Number of folds = 5
# method - cross validation = cv
metric <- "Accuracy"

grid <- expand.grid(C= c(0.001,0.01,0.1,1,0.5,5,10,50))

# Performing 5-fold cross validation
grid_search <- train(digit~., data=training_set, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl= train.control)

# Printing cross validation result
print(grid_search)

# Support Vector Machines with Linear Kernel 
# 9000 samples
# 784 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 7201, 7199, 7199, 7200, 7201 
# Resampling results across tuning parameters:
  
#  C      Accuracy   Kappa    
# 1e-03  0.9072203  0.8968849
# 1e-02  0.9072203  0.8968849
# 1e-01  0.9072203  0.8968849
# 5e-01  0.9072203  0.8968849
# 1e+00  0.9072203  0.8968849
# 5e+00  0.9072203  0.8968849
# 1e+01  0.9072203  0.8968849
# 5e+01  0.9072203  0.8968849

# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 0.001.

# Plotting "grid_search" results
plot(grid_search)
# model showed high accuracy at C = 0.001

# Evaluating the model on "test_set"
eval_cv <- predict(grid_search,test_set)
confusionMatrix(eval_cv,test_set$digit)

# Accuracy : 0.914           
# 95% CI : (0.8987, 0.9277)
# Range for Sensitivity :  (min) 79.6 % - (max) 99.3 %
# Range for Specificity :  (min) 98.4 % - (max) 99.6 %
# Kappa : 0.9044 

# NOTE ::
# Here we got the highest accuracy At C = 0.001 which as same as accuracy we got at C= 1 in previous model,
# Therefore, for our model we can keep the default value of C=1 here.
# Sometimes, default values also give's better results than tuned parameters.
# and it is always good to have less complex models. 


###--------- Non-Linear Kernels : "rbfdot" ------### 

## Radial kernel "rbfdot" using default parameters
rbf_model <- ksvm(digit~., data = training_set, scaled=FALSE, kernel = "rbfdot")

print(rbf_model) 
# Training error : 0.017889

# Validating our "rbf_model" on test data
eval_rbf <- predict(rbf_model,test_set, type = "response")
confusionMatrix(eval_rbf, test_set$digit) 

# Accuracy : 96.4 %            
# 95% CI   : (0.9533, 0.9728)
# Range for specificity : (min) 91.97% - (max) 99.4%
# Range for sensitivity : (min) 99.2 % - (max) 99.8%
# Kappa : 0.96

## NOTE :: 
# Here, our "Non-Linear rbfdot" kernel with default parameters 
# gave good accuracy = 96.4% as compared to "Linear" Kernels.
# Lets, try to tune its hyperparameters for more accuracy, if we can achieve.


#####----- Cross Validation for "Non-Linear" Kernel : "rbfdot" ------#####

# Tuning range of value of "C" & "sigma" hyperparameters.
# Performed  5 - fold cross-validation for this polynomial kernel (Computation time 13 hour's)

# Setting seed = 2222(any random number) to get same data split on cv, when run any number of times.  
set.seed(2222)
train.control <- trainControl(method="cv", number=5)
# number - Number of folds = 5
# method - cross validation = cv
metric <- "Accuracy"

# Making grid of "sigma" and "C" values. 
grid <- expand.grid(C= c(0.1,1,10), sigma = c(0.1,0.5,1))

# Performing 5-fold cross validation
model_svm.radial <- train(digit~., data=training_set, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl= train.control)

# Printing cross validation result
print(model_svm.radial)

# Support Vector Machines with Radial Basis Function Kernel 
# 9000 samples
# 784 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

# No pre-processing
# Resampling: Cross-Validated (5 fold) 

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 1 and C = 0.1.

# Plotting the model results 
plot(model_svm.radial)
# we can conclude that sigma = 1 and C = 0.1 are the best hyperparameter values.

# Validating the model results on test data
eval_radial <- predict(model_svm.radial, test_set)
confusionMatrix(eval_radial, test_set$digit)

# Accuracy : ~ 93 %        
# 95% CI : (0.0791, 0.1092)
# Range for Sensitivity : (min) 0.00 % - 100 % (max)
# Range for Specificity : (min) 0.00 % - 100 % (max)
   

# NOTE ::
# Here at sigma = 1 and C = 0.1, we get ~ 93% accuracy which is quite low 
# as compared to our previous model with default values of "C" and "sigma". 
# Lets now check for other kernels we have.


##--------- Polynomial Kernel : "polydot" --------##
# with default parameters
poly_model <- ksvm(digit~., data = training_set, kernel = "polydot", scaled = FALSE)

print(poly_model) 
# Support Vector Machine object of class "ksvm" 
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# Polynomial kernel function. 
# Hyperparameters : degree =  1  scale =  1  offset =  1 
# Number of Support Vectors : 2545 
# Training error : 0 

eval_poly <- predict(poly_model,test_set, type = "response")
confusionMatrix(eval_poly, test_set$digit) 

# Accuracy : 0.914           
# 95% CI : (0.8987, 0.9277)
# Range for Sensitivity : (min) 79.6% - 99.3% (max)  
# Range for Specificity : (min) 98.4% - 99.6% (max) 
# Kappa : 0.9044 

# Here, with cost C = 1, degree =1, scale =  1, we getting 91.4% accuracy with the default parameters.
# Lets tune their range of values to increase our accuracy.


####----- Cross-validation on Polynomial kernel -----####
# Performed  3 - fold cross-validation for this polynomial kernel, as 5-fold is taking much more computation time.

# Setting seed = 3333(any random number) to get same data split on cv, when run any number of times.  
set.seed(3333)
# Making grid of C, degree, scale values 
poly_eval <- expand.grid(C= c(0.01, 0.1, 1),degree=c(1,2,3),scale= c(-1,1,2))

model_svm.polyn <- train(digit~., data=training_set, method="svmPoly", metric="Accuracy",tuneGrid= poly_eval, 
                            trControl = trainControl(method = "cv", number = 3,verboseIter = TRUE), 
                            preProcess = NULL,Verbose=TRUE)

print(model_svm.polyn)

# Support Vector Machines with Polynomial Kernel 
# 9000 samples
# 784 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

# No pre-processing
# Resampling: Cross-Validated (3 fold) 
# Summary of sample sizes: 6000, 6002, 5998 
# Resampling results across tuning parameters:
  
#   C     degree  scale  Accuracy     Kappa     
# 0.01  1       -1     0.001333704  -0.1162343
# 0.01  1        1     0.908334565   0.8981237
# 0.01  1        2     0.908334565   0.8981237
# 0.01  2       -1     0.955443913   0.9504849
# 0.01  2        1     0.955443913   0.9504849
# 0.01  2        2     0.955443913   0.9504849
# 0.01  3       -1     0.001333630  -0.1152608
# 0.01  3        1     0.950444060   0.9449283
# 0.01  3        2     0.950444060   0.9449283
# 0.10  1       -1     0.001333704  -0.1162343
# 0.10  1        1     0.908334565   0.8981237
# 0.10  1        2     0.908334565   0.8981237
# 0.10  2       -1     0.955443913   0.9504849
# 0.10  2        1     0.955443913   0.9504849
# 0.10  2        2     0.955443913   0.9504849
# 0.10  3       -1     0.001333630  -0.1152608
# 0.10  3        1     0.950444060   0.9449283
# 0.10  3        2     0.950444060   0.9449283
# 1.00  1       -1     0.001333704  -0.1162343
# 1.00  1        1     0.908334565   0.8981237
# 1.00  1        2     0.908334565   0.8981237
# 1.00  2       -1     0.955443913   0.9504849
# 1.00  2        1     0.955443913   0.9504849
# 1.00  2        2     0.955443913   0.9504849
# 1.00  3       -1     0.001333630  -0.1152608
# 1.00  3        1     0.950444060   0.9449283
# 1.00  3        2     0.950444060   0.9449283

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were degree = 2, scale = -1 and C = 0.01.

# Plotting the "model_svm.polyn" results 
plot(model_svm.polyn)
# We can see higher accuracy for C = 0.01

# Valdiating the model on test data
eval_polyn <- predict(model_svm.polyn, test_set)
confusionMatrix(eval_polyn,test_set$digit)

# Accuracy : 96.73 %         
# 95% CI : (0.957, 0.9757)
# Range for Sensitivity : (min) 94.3 % - 99.3 % (max)
# Range for Specificity : (min) 99.48 % - 99.85 % (max)
# Kappa :  0.9637


### NOTE ::
# Here with "degree = 2", "scale = -1" and "C = 0.01",
# we got the highest accuracy 96.73 % as compared to all the models we have checked till now.
# So we can go with this model ahead.



##---------- FINAL MODEL ------------## 

## So, finally we got the following best model with highest accuracy.

# BEST MODEL NAME ::  "model_svm.polyn" SVM polynomial kernel with 3-folds cross-validation

# Accuracy : 96.73 %

# The final values used for the model were degree = 2, scale = -1 and C = 0.01.

