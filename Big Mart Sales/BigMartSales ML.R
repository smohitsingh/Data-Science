##------- Problem Statement ---------##
# The data scientists at BigMart have collected sales data for 1559 products across 10 stores in different cities for the year 2013. 
# Now each product has certain attributes that sets it apart from other products. 
# Same is the case with each store.

##------ Business Objective -----## 
# To identify the properties of products and stores affecting the sales of products, 
# So, that better decisions can be made later on to increase the sales.

##------ Goal of analysis -------##
# The aim is to build a predictive model to find out the sales of each product at a particular


# Including Required packages for Reading data, Manipulation of data, Visualization of data.

library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots 
library(corrplot)
library(car)
library(caTools)


##__________ SETTING-UP WORKING DIRECTORY _________

# setwd("C:/Users/Mohit Singh/Desktop/Data Science Portfolio/Big Mart Dataset  AV")
getwd()        # To check whether the file is properly loaded or not
dir()          # To Check the Target file Name : 

# Clean the file for any Pre-Used valiables
remove(list = ls())


##_________ DATA LOADING _________##

# Creating a New Data frame "train" and "test"   
# Blank spaces if any will be replaced by NA using na.strings and 
# we dont want strings to be converted to factors automatically while reading file 

train <- read.csv("Train.csv",na.strings = c("",NA),header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("Test.csv",na.strings = c("",NA),header = TRUE, stringsAsFactors = FALSE)


## ________ DATA UNDERSTADING ______ ##

View(train)   # To view data frame in table form in R for easy analysis.          
View(test)

# train data column names
names(train)

# test data column names
names(test)
# "Item_Outlet_Sales" varibale is not present in the test data frame.

# structure of train data
str(train)
summary(train)

# structure of test data
str(test)
summary(test)

dim(train)
# There are 8523 observations and 12 attributes 
dim(test)
# There are 5681 observations and 11 attributes 


## ---------- DATA CLEANING ---------- ##

# ----- Missing Value analysis -----

# to check for NA values in each column
colSums(is.na(train))
# Item_Weight : 1463 , Outlet_Size : 2410 missing data

colSums(is.na(test))
# Item_Weight : 976 , Outlet_Size : 1606 missing data

# train data frame
index <- which(train$Item_Weight != "NA")
med <- median(train$Item_Weight, na.rm = TRUE)
train$Item_Weight[-index] <- med
# Checking
colSums(is.na(train))
# No missing values in "Item_Weight" feature

# test data frame
index <- which(test$Item_Weight != "NA")
med <- median(test$Item_Weight, na.rm = TRUE)
test$Item_Weight[-index] <- med
# Checking
colSums(is.na(test))
# No missing values in "Item_Weight" feature

# train data
# replacing 0 in Item_Visibility with mean
zero_index = which(train$Item_Visibility == 0)
for(i in zero_index){
  item = train$Item_Identifier[i]
  train$Item_Visibility[i] = mean(train$Item_Visibility[train$Item_Identifier == item], na.rm = T)
}

# Test data
# replacing 0 in Item_Visibility with mean
zero_index1 = which(test$Item_Visibility == 0)
for(i in zero_index1){
  item = test$Item_Identifier[i]
  test$Item_Visibility[i] = mean(test$Item_Visibility[test$Item_Identifier == item], na.rm = T)
}


# Outlet Size - train data
table(train$Outlet_Size)
index_out <- which(train$Outlet_Size != "NA")
train$Outlet_Size[-index_out] <- "Medium"
# Checked
sum(is.na(train$Outlet_Size))

# Outlet Size - test data
table(test$Outlet_Size)
index_out <- which(test$Outlet_Size != "NA")
test$Outlet_Size[-index_out] <- "Medium"
# Checked
sum(is.na(test$Outlet_Size))

colSums(is.na(train))
colSums(is.na(test))
# Hence, Checked no Missing values present in both train and test data sets.


##-------- Feature Engineering --------##

# Train data set
# create a new feature 'Item_Type_new' 
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")


train[13] <- ifelse(train$Item_Type %in% perishable, "perishable",ifelse(train$Item_Type %in% non_perishable, "non_perishable", "not_sure"))
names(train)[13] <- c("Item_Type_new")

# We will also change the values of Item_Fat_Content wherever Item_category is 'NC' because non-consumable items cannot have any fat content. 
# We will also create a couple of more features - Outlet_Years (years of operation) and price_per_unit_wt (price per unit weight).

train$Item_Fat_Content[train$Item_category == "NC"] <- "Non-Edible"

# years of operation of outlets
train$Outlet_Establishment_Year <- as.numeric(train$Outlet_Establishment_Year)
train[14] <- (2018 - train$Outlet_Establishment_Year)
names(train)[14] <- ("Outlet_Years")

# Price per unit weight
train[15] <- train$Item_MRP/train$Item_Weight
names(train)[15] <- ("Price_per_unit")

# Dropping Columns from we have drived new metrics
train <- train[-c(1,2,5,6,8)]
View(train)
dim(train)


# test data set
# create a new feature 'Item_Type_new' 
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")


test[12] <- ifelse(test$Item_Type %in% perishable, "perishable",ifelse(test$Item_Type %in% non_perishable, "non_perishable", "not_sure"))
names(test)[12] <- c("Item_Type_new")

# We will also change the values of Item_Fat_Content wherever Item_category is 'NC' because non-consumable items cannot have any fat content. 
# We will also create a couple of more features - Outlet_Years (years of operation) and price_per_unit_wt (price per unit weight).

test$Item_Fat_Content[test$Item_category == "NC"] <- "Non-Edible"

# years of operation of outlets
test$Outlet_Establishment_Year <- as.numeric(test$Outlet_Establishment_Year)
test[13] <- (2018 - test$Outlet_Establishment_Year)
names(test)[13] <- ("Outlet_Years")

# Price per unit weight
test[14] <- test$Item_MRP/test$Item_Weight
names(test)[14] <- ("Price_per_unit")

# Dropping Columns from we have drived new metrics
test <- test[-c(1,2,5,6,8)]
View(test)
dim(test)


##---- Creating Dummy Variables -----##

# Categorical features are not accepted by the algorithms,
# So, converting them to numerical columns called as Dummy variables - containing only 0 and 1.  

# train
dummy_var <- data.frame(model.matrix( ~ Item_Fat_Content+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Item_Type_new
                                      ,data = train))
dim(dummy_var)
dummy_var <- dummy_var[-1]
train <- train[-c(1,3:6,8)]

train <- cbind(train,dummy_var) 
dim(train)

#test
dummy_var1 <- data.frame(model.matrix( ~ Item_Fat_Content+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type+Item_Type_new 
                                       ,data = test))
                                       
dim(dummy_var1)
dummy_var1 <- dummy_var1[-1]
test <- test[-c(1,3:7)]

test <- cbind(test,dummy_var1) 
dim(test)

#-------------------------------------------------------------------------------------------------------------------------


## EDA - Univariate

ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +
  xlab("Item_Outlet_Sales")


p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")

p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")

p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")

plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package
#-------------------------------------------------------------------------------------------------------------------------

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
#-------------------------------------------------------------------------------------------------------------------------

# plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1)

plot_grid(p4, second_row, ncol = 1)
#-------------------------------------------------------------------------------------------------------------------------

# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

# ploting both plots together
plot_grid(p7, p8, ncol = 2)
#-------------------------------------------------------------------------------------------------------------------------

## EDA - Bivariate

train = combi[1:nrow(train)]

# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
     theme(axis.title = element_text(size = 8.5))

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
      theme(axis.title = element_text(size = 8.5))

# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
      theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2)

plot_grid(p9, second_row_2, nrow = 2)
#-------------------------------------------------------------------------------------------------------------------------

# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13, p14, ncol = 2)

plot_grid(p12, second_row_3, ncol = 1)
#-------------------------------------------------------------------------------------------------------------------------

ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")

p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol = 1)


## Correlation Plot
cor_train = cor(train)

corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)
corrplot(cor_train, method = "shade")

# The correlation plot above shows correlation between all the possible pairs of variables in out data. The correlation between any two variables is represented by a pie. A blueish pie indicates positive correlation and reddish pie indicates negative correlation.
# The magnitude of the correlation is denoted by the area covered by the pie.

# Variables price_per_unit_wt and Item_Weight are highly correlated as the former one was created from the latter. 
# Similarly price_per_unit_wt and Item_MRP are highly correlated for the same reason.


## Remove skewness

# Skewness in variables is undesirable for predictive modeling. 
# Some machine learning methods assume normally distributed data and a skewed variable can be transformed by taking its log, square root, or cube root so as to make its distribution as close to normal distribution as possible. 
# In our data, variables Item_Visibility and price_per_unit_wt are highly skewed. 
# So, we will treat their skewness with the help of log transformation.

# train transformation 

ggplot(train, aes(x=train$Item_Visibility)) + geom_histogram()
ggplot(train, aes(x=train$Item_Outlet_Sales)) + geom_histogram(binwidth = 100)
ggplot(train, aes(x=train$Price_per_unit)) + geom_histogram(binwidth = 3)

train$Item_Visibility <- log(train$Item_Visibility+1)
train$Item_Outlet_Sales <-  log(train$Item_Outlet_Sales)
train$Price_per_unit <- log(train$Price_per_unit)

# test data set transformation

ggplot(test, aes(x=test$Item_Visibility)) + geom_histogram()
ggplot(test, aes(x=test$Price_per_unit)) + geom_histogram(binwidth = 1)

test$Item_Visibility  <- log(test$Item_Visibility+1)
test$Price_per_unit <- log(test$Price_per_unit)


# Copy of train data set 
train_copy <- train
 
## ----- MODEL BUILDING ------- ##

#-----To check the accuracy of model 
#  for Cross Validation set and seperating the training_set

set.seed(1200)
split = sample.split(train, SplitRatio = 0.8)   # Library caTools
training_set = subset(train, split == TRUE)
test_set = subset(train, split == FALSE)
dim(training_set)
# 6559 observations and 26 attributes
dim(test_set)
# 1964 observations and 26 attributes


# SCALE
training_set <- scale(training_set)
test_set <- scale(test_set)

training_set <- as.data.frame(training_set)
test_set <- as.data.frame(test_set)


##----- We will build the following models.

# Linear Regression
# Lasso Regression
# Ridge Regression
# RandomForest
# XGBoost


##-- We can check the model's performance by following methods --##

# Mean Absolute Error (MAE)
# Mean Square Error (MSE)
# Root Mean Square Error (RMSE)


##----- Linear Regression
# Going by validation set approach

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Item_Outlet_Sales ~ .,
               data = training_set)

summary(regressor)

pred_x <- predict(regressor,training_set)
RMSE(pred_x,training_set$Item_Outlet_Sales)
# 57.08 % RMSE for training_set .

r <- residuals(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

## RMSE Calculation for linear model
#Load Library
library(hydroGOF)

#Calculate RMSE 
RMSE=RMSE(y_pred,test_set$Item_Outlet_Sales)
RMSE
# RMSE is 59.74 % for test set 

# SVR (Support vector for regression) 
library(e1071)
regressor = svm(formula = Item_Outlet_Sales ~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting a new result
y_pred = predict(regressor,test_set )

rmse = RMSE(y_pred,test_set$Item_Outlet_Sales)
rmse
# 59.74 % for test data 

## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, Item_Outlet_Sales ~. , data=training_set,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

## Select the best model out of 1100 trained models and compute RMSE

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,data)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,data$Y)

##--- K-fold Cross - Validation

# Sum the MSE for each fold, divide by the number of observations,
# and take the square root to get the cross-validated standard error of estimate.


# two methods that slightly modify ordinary least
# squares (OLS) regression - ridge regression and the lasso.
#-------------------------------------------------------------------------------------------------------------------------
# install.packages("glmnet")
library(glmnet)

##---- Lasso Regression
set.seed(1200)
my_control = trainControl(method="cv", number= 10)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))

lasso_linear_reg_mod = train(x = training_set[-2], y = training_set$Item_Outlet_Sales,
                       method='glmnet', trControl= my_control, tuneGrid = Grid)

summary(lasso_linear_reg_mod)

# mean validation score
mean(lasso_linear_reg_mod$resample$RMSE)
# 59.2%
# Tried using 5-fold but 10 fold CV is little better

#-------------------------------------------------------------------------------------------------------------------------
##---- Ridge Regression
set.seed(1200)
my_control = trainControl(method="cv", number=10)
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(x = training_set[-2], y = training_set$Item_Outlet_Sales,
                       method='glmnet', trControl= my_control, tuneGrid = Grid)

# mean validation score
mean(ridge_linear_reg_mod$resample$RMSE)
# 59.57 %


#-------------------------------------------------------------------------------------------------------------------------
install.packages("ranger")
library(ranger)

## RandomForest Model
set.seed(1200)
my_control = trainControl(method="cv", number=5)

tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)

rf_mod = train(x = training_set[-2], 
               y = training_set$Item_Outlet_Sales,
               method='ranger', 
               trControl= my_control, 
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

# mean validation score
mean(rf_mod$resample$RMSE)
# RMSE : 0.5863684

## plot displaying RMSE scores for different tuning parameters
plot(rf_mod)

## plot variable importance
plot(varImp(rf_mod))

#-------------------------------------------------------------------------------------------------------------------------
## List of parameters for XGBoost modeling
param_list = list(
        
        objective = "reg:linear",
        eta=0.01,
        gamma = 1,
        max_depth=6,
        subsample=0.8,
        colsample_bytree=0.5
        )

## converting train and test into xgb.DMatrix format
dtrain = xgb.DMatrix(data = as.matrix(train[,-c(3)]), label= train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test))

## 5-fold cross-validation to find optimal value of nrounds
set.seed(112)
xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)

## training XGBoost model at nrounds = 430
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)

##----- Variable Importance Plot ------##

# To check which are the
var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model)

xgb.plot.importance(var_imp)

