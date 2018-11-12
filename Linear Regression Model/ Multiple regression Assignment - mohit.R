## ----- Business Objective ------- ## 
# To understand the factors affecting the pricing of cars in the American marketing, 
# Since, those may be very different from the Chinese market. 
# They can accordingly manipulate the design of the cars, the business strategy etc. to meet certain price levels.

## ------ Goals of Analysis ------## 
# 1.  Which Independent Variables are significant in predicting the price of a car?
# 2.  How well these variables describe the price of the car?

## ------- Installing & Loading Required Packages --------- ##

# install.packages("MASS")   for StepAIC
# install.packages("car")    for VIF
# install.packages("dplyr")  for data manipulation
# install.packages("stringr") for string operations
# install.packages("ggplot2") for visualization
# install.packages("tidyr") 

# Loading packages  
library("car")
library("MASS")
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Removing the used variables
remove(list = ls())
ls()      # Above command is executed sucessfully


## ------- LOADING DATA --------- ##

# setwd("C:/Users/Mohit Singh/Desktop/Linear regression assignment")
getwd()
# Checked, Directory Added successfully

# Checking for the target file in the directory added
dir()    # "CarPrice_Assignment.csv" file is present 
# "Data Dictionary - carprices.xlsx" file also provided. 

# Loading the data file and named as car_price
car_price <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("",NA))


## ____________ DATA UNDERSTADING _________ ##

# View the loaded frame in the table format in R. 
View(car_price)

# Structure & Summary of data frame car_price
str(car_price)
# The "car_price" data frame contains 26 Variables (Columns) and 205 observations (Rows).
dim(car_price)
summary(car_price)


# --------- DATA PREPARATION --------- #

# Checking NA values in the data frame car_price
table(is.na(car_price))
# Hence, "NA" values are not present in the data frame "car_price"

# Checking any Duplicate values 
nrow(unique(car_price))
# 205 unique observatins found so No duplicates found.

# In Column " CarName " The first is company name & the later part is model name, 
# Only company name is to be used, So removing the model name from this column.
carname_split <- str_split_fixed(car_price$CarName, pattern = " ", n=2)
car_price$CarName <- carname_split[,1]
rm(carname_split)

# Making the Character case same throughout the data frame to avoid case sensitive errors,
# Converting all to upper-case characters
case_change <- sapply(car_price[c(3:9,15,16,18)],toupper)
case_change <- unlist(case_change)
class(case_change)
# The structure of case_change is matrix, so converting it to data frame type
case_change <- as.data.frame(case_change)
car_price[c(3:9,15,16,18)] <- case_change
View(car_price)
# View again the changed data frame

# Checking the structure of "car_price" data frame for changes made, 
str(car_price)
# Changes done successfully

# Number of unique values in "car_ID" variable
length(unique(car_price$car_ID))
# Same as number of rows in the data, so they are not be useful for further analysis
# Hence removing "car_ID" variable
car_price <- car_price[-1]
dim(car_price)
# Now we have 25 variables in "car_price" data frame.

# Checking uniformity of data in column "CarName" 
car_price$CarName
# There are many misspelling values in "CarName" and hence are to be corrected

car_price$CarName <- str_replace_all(car_price$CarName, pattern = "MAXDA", replacement = "MAZDA")
car_price$CarName <- str_replace_all(car_price$CarName, pattern = "PORCSHCE", replacement = "PORSCHE")
car_price$CarName <- str_replace_all(car_price$CarName, pattern = "TOYOUTA", replacement = "TOYOTA")
car_price$CarName <- str_replace_all(car_price$CarName, pattern = "VOKSWAGEN", replacement = "VOLKSWAGEN")
car_price$CarName <- str_replace_all(car_price$CarName, pattern = "VW", replacement = "VOLKSWAGEN")

# Converting "CarName" variable to factor type 
car_price$CarName <- as.factor(car_price$CarName)
# Checking for changes made.
str(car_price)


## ----------------- Exploratory Data Analysis -------------- ##

## -------- UNIVARIATE ANALYSIS -------- ##

# ---------- Qualitative data -------- #

# "symboling" is insurance risk rating - Ordered variable
# A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.		

# To see the count of each insurance risk rating
table(car_price$symboling)

# From the summary it is clear that there are very few cars with very safe rating. Majority of the cars have a rating of either 0 or +1
ggplot(car_price, aes(x = as.factor(symboling))) + geom_bar(fill = "aquamarine4")
# Distribution is more on high risk ratings and also there are 67 cars having Zero risk, shown by peak in the plot.
# Very less for -2 risk ratings : count of only 3 present 

# "CarName" is name of car company - Nominal variable
table(car_price$CarName)
ggplot(car_price, aes(x = CarName)) + geom_bar(fill = "aquamarine4")
# There are more cars of "TOYOTA" company and less cars of "MERCURY" company.

# "fueltype" : Car fuel type i.e gas or diesel - Nominal variable
table(car_price$fueltype)
# GAS and DIESEL are fuel types and their counts are 185 and 20 respectively.
ggplot(car_price, aes(x = fueltype)) + geom_bar(fill = "aquamarine4")
# Distribution is more for GAS fuel type and the margin is very large.

# "aspiration" is Aspiration used in a car - Nominal variable
table(car_price$aspiration)
# STD & TURBO are Aspiration types and their counts are 168 and 37 respectively.
ggplot(car_price, aes(x = aspiration)) + geom_bar(fill = "aquamarine4")
# Distribution is more for STD fuel type and the margin is very large.

# "doornumber" is Number of doors in a car - Ordinal variable
table(car_price$doornumber)
# FOUR & TWO are doornumber types and their counts are 115 and 90 respectively.
ggplot(car_price, aes(x = doornumber)) + geom_bar(fill = "aquamarine4")
# Distribution is more for "FOUR" number of doors.

# "carbody" is body of car - Nominal variable
table(car_price$carbody)
# CONVERTIBLE :6, HARDTOP :8, HATCHBACK :70, SEDAN :96, WAGON :25 are counts of five different car body types. 
ggplot(car_price, aes(x = carbody)) + geom_bar(fill = "aquamarine4")
# Distribution is more for "SEDAN" car body type and very less for "CONVERTIBLE" type .

# "drivewheel" is type of drive wheel - Nominal variable
table(car_price$drivewheel)
# 4WD :9, FWD :120, RWD :76 are counts of three different drive wheel types. 
ggplot(car_price, aes(x = drivewheel)) + geom_bar(fill = "aquamarine4")
# Distribution is more for "FWD" wheel type and very less for "4WD" type.

# "enginelocation" is Location of car engine - Nominal variable
table(car_price$enginelocation)
# FRONT :202 and REAR :3 are counts of two different car engine locations. 
ggplot(car_price, aes(x = enginelocation)) + geom_bar(fill = "aquamarine4")
# Distribution is more for "FRONT" location of engine and the margin is very large.

# "enginetype" is type of car engine - Nominal variable
table(car_price$enginetype)
# "enginetype" has 7 different types present in data 
ggplot(car_price, aes(x = enginetype)) + geom_bar(fill = "aquamarine4")
# count is more for "OHC" location of engine :148 and very low for "DOHCV" : 1

# "cylindernumber" is number of cynlinders in car - Ordinal variable
table(car_price$cylindernumber)
# there are EIGHT , FIVE, FOUR, SIX, THREE, TWELVE, TWO number of cylinders in cars
ggplot(car_price, aes(x = cylindernumber)) + geom_bar(fill = "aquamarine4")
# Count is more for "FOUR" Number of cylinders :159 and 
# very less for Three, twelve Number of cylinders : 1 only

# "fuelsystem" is type of fuel system in cars - Nominal variable
table(car_price$fuelsystem)
# there are 8 different fuel types in cars
ggplot(car_price, aes(x = fuelsystem)) + geom_bar(fill = "aquamarine4")
# Count is more for "MPFI" Number of cylinders :94 
# and very less for "SPFI", "MFI" : 1 only 


## ---------- Quantitative Variables --------- ##

# Capping Variables, In case outliers are found, 
# they are required to be treated, wherever possible according to business understanding at hand,   
# We could cap it by replacing those observations outside the lower limit with the value of 5th %ile
# And those that lie above the upper limit, with the value of 95th %ile.

# "wheelbase" - continuous variable
summary(car_price$wheelbase)
ggplot(car_price,aes(y=wheelbase ))+geom_boxplot()
quantile(car_price$wheelbase,seq(0,1,0.01))
# there is sudden jump form  99 % - 100 %, so outliers are present here 
# So, Outlier treatment required , capping values with 115.544
car_price$wheelbase[which(car_price$wheelbase > 115.544)] <- 115.544

# "carlength" - continuous variable
summary(car_price$carlength)
ggplot(car_price,aes(y = carlength ))+geom_boxplot()
quantile(car_price$carlength,seq(0,1,0.01))
# Outlier Treatment is not required

# "carwidth" : Width of car - Continuous variable
summary(car_price$carwidth)
ggplot(car_price,aes(y = carwidth ))+geom_boxplot()
quantile(car_price$carwidth,seq(0,1,0.01))
# Outlier Treatment is not required

# "carheight" : Height of car - Continuous variable
summary(car_price$carheight)
ggplot(car_price,aes(y = carheight ))+geom_boxplot()
quantile(car_price$carheight,seq(0,1,0.01))
# Outlier Treatment is not required

# "curbweight" : The weight of a car without occupants or baggage	
summary(car_price$curbweight)
ggplot(car_price,aes(y = curbweight ))+geom_boxplot()
quantile(car_price$curbweight,seq(0,1,0.01))
# Outlier Treatment is not required

# "enginesize" is The size of engine 	
summary(car_price$enginesize)
ggplot(car_price,aes(y = enginesize ))+geom_boxplot()
quantile(car_price$enginesize,seq(0,1,0.01))
# Outlier Treatment is not required

# "boreratio" : The boreratio of car 	
summary(car_price$boreratio)
ggplot(car_price,aes(y = boreratio )) + geom_boxplot()
quantile(car_price$boreratio,seq(0,1,0.01))
# Outlier Treatment is not required

# "stroke" is the Stroke or volume inside the engine
summary(car_price$stroke)
ggplot(car_price,aes(y = stroke )) + geom_boxplot()
quantile(car_price$stroke,seq(0,1,0.01))
# There is a jump between 0% - 2%. So, cap all values below 2.64.
# So, Outlier Treatment is required
car_price$stroke[which(car_price$stroke < 2.64)] <- 2.64

# "compressionratio" is compression ratio of car  
summary(car_price$compressionratio)
ggplot(car_price,aes(y = compressionratio )) + geom_boxplot()
quantile(car_price$compressionratio,seq(0,1,0.01))
# There is a jump above 90%, So, cap all values above 10.9400 (90%) with 10.9400. 
car_price$compressionratio[which(car_price$compressionratio > 10.9400 )] <- 10.9400

# "horsepower" is the Stroke or volume inside the engine
summary(car_price$horsepower)
ggplot(car_price,aes(y = horsepower )) + geom_boxplot()
quantile(car_price$horsepower,seq(0,1,0.01))
# There is a jump between 97% - 98%, So, cap all values above 184.00  
# Outlier Treatment
car_price$horsepower[which(car_price$horsepower > 184.00 )] <- 184.00

# "peakrpm" 
summary(car_price$peakrpm)
ggplot(car_price,aes(y = peakrpm ))+geom_boxplot()
quantile(car_price$peakrpm,seq(0,1,0.01))
# There is a jump between 96% - 100%, So capping values with 6000  
car_price$peakrpm[which(car_price$peakrpm > 6000 )] <- 6000

# "citympg" is Mileage in city 
summary(car_price$citympg)
ggplot(car_price,aes(y = citympg )) + geom_boxplot()
quantile(car_price$citympg,seq(0,1,0.01))
# There is a jump between 98% - 99% , So capping values with 38.00  
# Outlier Treatment
car_price$citympg[which(car_price$citympg > 38.00 )] <- 38.00


# "highwaympg" is Mileage on highway 
summary(car_price$highwaympg)
ggplot(car_price,aes(y = highwaympg )) + geom_boxplot()
quantile(car_price$highwaympg, seq(0,1,0.01))
# Outlier Treatment required,
# There is a jump between 98% - 100%, So capping values with 46.92
car_price$highwaympg[car_price$highwaympg > 46.92] <- 46.92

# "price" is the Price of car.
summary(car_price$price)
ggplot(car_price,aes(y = price )) + geom_boxplot()
quantile(car_price$price, seq(0,1,0.01))
# Outlier Treatment
# There is a jump between 99% - 100%, So capping values with 36809.60 
car_price$price[car_price$price > 36809.60] <- 36809.60

#-------Bi- variate analysis

# symboling
ggplot( car_price, aes(x=as.factor(symboling), y = price))+ geom_boxplot() 
# prices are high for low risk rates

# fueltype
ggplot(car_price, aes(x=fueltype, y= price))+ geom_boxplot()
# prices are high for dielsel 

# aspiration
ggplot(car_price, aes(x=aspiration, y= price))+ geom_boxplot()
# prices are high for turbo type 

# doornumber
ggplot(car_price, aes(x=doornumber, y= price))+ geom_boxplot()
#  less variation is see in the box plots 

# carbody
ggplot(car_price, aes(x= carbody, y= price))+ geom_boxplot()
# prices are high for HARDTOP type 

# drivewheel
ggplot(data = car_price, aes(x= drivewheel, y= price))+ geom_boxplot()
# prices are very high for RWD type wheels

# enginelocation
ggplot(car_price, aes(x=enginelocation, y= price))+ geom_boxplot()
# REAR have very high price ranges

#wheelbase
ggplot(car_price, aes(x= wheelbase, y= price))+ geom_point()+geom_smooth()
# price shows trend of declining for 95 wheelbase value and after that linear rise is shown

# carlength
ggplot(car_price, aes(x=carlength, y= price))+ geom_point()+geom_smooth()
# rise in price shows linear trend with the increase in car length 
# which obvious fact that the cost of making will be more for long cars

# carwidth
ggplot(car_price, aes(x=carwidth, y= price))+ geom_point()+geom_smooth()
# rise in price shows linear trend with the increase in car width 
# which obvious fact that the cost of making will be more for wide and big cars
 

# carheight
ggplot( car_price, aes(x=carheight, y= price))+ geom_point()+geom_smooth()
# 

# curbweight
ggplot( car_price, aes(x=curbweight, y=price))+ geom_point()+geom_smooth()
# rise in price shows linear trend with the increase in curbweight 
# which obvious fact that the cost of making will be more heighvier cars

# enginetype
ggplot(car_price, aes(x= enginetype, y= price))+ geom_boxplot()
# OHCV engine types have more prices as compared to other engines

# cylindernumber
ggplot(car_price, aes(x= car_price$cylindernumber, y= price))+ geom_boxplot()
# eight and six number of cylinders have more prices

# enginesize
ggplot(car_price, aes(x= car_price$enginesize, y= price))+ geom_boxplot()

# fuelsystem
ggplot(car_price, aes(x= car_price$fuelsystem, y= price))+ geom_boxplot()
# MPFI have some exceptional prices and high rates as compared to others 

#boreratio
ggplot(car_price, aes(x= car_price$boreratio, y= price))+ geom_point()

# stroke
ggplot(car_price, aes(x= car_price$stroke, y= price))+ geom_point()




## ------ Creating Dummy variables ( Categorical Variables ) ------ ##

# Creating dummy_var for "CarName" input variable
# Following columns are character types and created into dummy from them
# CarName, fueltype, aspiration, doornumber, carbodydrivewheel, enginelocation, enginetype, cylindernumber, fuelsystem

dummy_var <- data.frame(model.matrix( ~ CarName+fueltype+aspiration+doornumber+carbody+drivewheel+enginelocation+enginetype+cylindernumber+fuelsystem, data = car_price))
dim(dummy_var)
# dummy_var contains 205 observations and 51 variables
# Excluding the first column which contains only number of rows
dummy_var <- dummy_var[-1]
View(dummy_var)
# Checking the class of "dummy_var" 
class(dummy_var)
##  Final, dummy_var contains 205 observations and 50 variables


# "car_price_final" is final data frame after adding dummy variable and removing character variables 
car_price_final <- car_price[-c(2:8,14,15,17)]
car_price_final <- cbind(car_price_final,dummy_var) 
dim(car_price_final)
# The car_price_final data frame has 205 observations and 65 varibales.
View(car_price_final)

# Changing to numeric type for data uniformity 
car_price_final$curbweight <- as.numeric(car_price_final$curbweight)
car_price_final$enginesize <- as.numeric(car_price_final$enginesize)
car_price_final$horsepower <- as.numeric(car_price_final$horsepower)
# Checking the structure of "car_price_final"
str(car_price_final)


## --------- Model Building --------- ##

# To generate random samples everytime 
set.seed(100)

# Dividing data randomly from "car_price_final" data frame into "train" and "test" data frames.

# train_index is randomly selected data for "train" data frame
# making seperate train and test data frames from  "car_price_final"
train_index = sample(1:nrow(car_price_final), 0.7*nrow(car_price_final))
train = car_price_final[train_index,]
test = car_price_final[-train_index,]
str(train)
str(test)
# "train" data frame contains 143 observations and "test" data frame contains 62 observations

# Execute the first model_1 multilinear model in the training set. 
model_1 <- lm(price~. ,data = train)

# Check the summary of model_1. 
summary(model_1)
# Getting NA values for the 9 variable's because it is linearly dependent on the other variables.

# Check if the correlation matrix give some insight.
corrs = cor(car_price_final)
View(corrs)

# Now, lets see how to use stepAIC
# In stepAIC function, we pass our first model i.e model_1 and 
# direction as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

step <- stepAIC(model_1, direction="both")
# So, many iterations have been done through the stepwise command. 
# Now we need to know our model further.

# the Step command here to see the final sorted model. 
step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignificant variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2


model_2 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameHONDA + CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                drivewheelFWD + enginelocationREAR + enginetypeOHC + enginetypeROTOR + 
                fuelsystem2BBL, data = train)


# Check the summary of model_2
summary(model_2)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant
vif(model_2)

# Checking for variables with high VIF (VIF > 2) and high p-values > 0.05
# Multiple R-squared:  0.9814,	Adjusted R-squared:  0.9756  

# "curbweight" :  25.011843 vif & "enginesize" : 19.313853 vif, "carbodyHATCHBACK" :13.535929 vif,
# "carbodySEDAN " : 13.403083 vif , carwidth" : 11.217662 vif, "carbodyWAGON" : 7.358246 vif
#  "drivewheelFWD" :5.971294, enginelocationREAR" : 5.891237 vif, "stroke" : 5.436056 vif,
# "CarNamePORSCHE ": 5.301549,
# have high vif but cannot be removed since these are highly significant (0.05>p).
# enginetypeOHC : 5.014885 : 0.071533 which is more than 0.05
# Hence, Removing  enginetypeOHC it in model_3 

model_3 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameHONDA + CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                drivewheelFWD + enginelocationREAR + enginetypeROTOR + 
                fuelsystem2BBL, data = train)


# Check the summary of model_3
summary(model_3)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_3)

#  Multiple R-squared:  0.9808,	Adjusted R-squared:  0.975
# "curbweight" :  25.010821 vif & "enginesize" : 13.866823 vif, "carbodyHATCHBACK" :13.532286 vif,
# "carbodySEDAN " : 13.374648 vif , carwidth" : 10.307428 vif, "carbodyWAGON" : 7.331659 vif
#  "drivewheelFWD" :5.931143, enginelocationREAR" : 5.706741 vif, "stroke" : 5.076289 vif,
# "CarNamePORSCHE ": 5.184217,"peakrpm" : 3.391261 vif, "boreratio " : 3.795329vif,
# "CarNameHONDA": 3.506098 vif, "CarNameTOYOTA" : 3.382254 vif, "carbodyHARDTOP" :3.473543 vif,
#  have high vif but cannot be removed since these are highly significant (0.05>p).
# fuelsystem2BBL : 3.359662 vif  :p-value 0.193156 which is more than 0.05
# Hence, Removing it in model_4 

model_4 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameHONDA + CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + CarNameSAAB + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                drivewheelFWD + enginelocationREAR + enginetypeROTOR 
                , data = train)

# Check the summary of model_4
summary(model_4)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_4)


#  Multiple R-squared:  0.9805,	Adjusted R-squared:  0.9749 
# "curbweight" :  25.007856 vif & "enginesize" : 13.339588 vif, "carbodyHATCHBACK" :13.264931 vif,
# "carbodySEDAN " : 13.056602  vif , carwidth" : 10.246595 vif, "carbodyWAGON" : 7.140196 vif
#  "drivewheelFWD" :5.828002 , enginelocationREAR" : 5.701550 vif, "stroke" : 4.687875 vif,
# "CarNamePORSCHE ": 5.156535,"peakrpm" : 3.186046 vif, "boreratio " : 3.742245 vif,
# "CarNameHONDA": 3.492677 vif, "CarNameTOYOTA" : 3.378584 vif, "carbodyHARDTOP" : 3.441960 vif,
# "CarNameBUICK" : 2.775097 vif
#  have high vif but cannot be removed since these are highly significant (0.05>p).
# While CarNameSAAB : 2.104063 vif : 0.107039 p-value  
# Hence, Removing it in model_5


model_5 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameHONDA + CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                drivewheelFWD + enginelocationREAR + enginetypeROTOR 
                , data = train)

# Check the summary of model_5
summary(model_5)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_5)

#  Multiple R-squared:  0.9801,	Adjusted R-squared:  0.9745
# "curbweight" :  20.515225 vif & "enginesize" : 13.223042 vif, "carbodyHATCHBACK" :12.948872 vif,
# "carbodySEDAN " : 12.876517  vif , carwidth" : 9.532643 vif, "carbodyWAGON" : 7.137777 vif
#  "drivewheelFWD" :3.988448 , enginelocationREAR" : 5.684613 vif, "stroke" : 4.137272 vif,
# "CarNamePORSCHE ": 5.103995,"peakrpm" : 3.164873 vif, "boreratio " : 3.705048 vif,
# "CarNameHONDA": 3.391614 vif, "CarNameTOYOTA" : 3.182784 vif, "carbodyHARDTOP" : 3.292067 vif,
# "CarNameBUICK" : 2.713833 vif
#  have high vif but cannot be removed since these are highly significant (0.05>p).
# drivewheelFWD : 3.988448 vif and  0.095965 p-value 
# Hence, Removing it in model_6

model_6 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + peakrpm + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameHONDA + CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                enginelocationREAR + enginetypeROTOR 
                , data = train)

# Check the summary of model_6
summary(model_6)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_6)


#  Multiple R-squared:  0.9796,	Adjusted R-squared:  0.9741
# peakrpm : 3.008246  vif and 0.027480 p value
#  hence can be removed in model_7.


model_7 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameHONDA + CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                enginelocationREAR + enginetypeROTOR 
              , data = train)
  

# Check the summary of model_7
summary(model_7)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_7)


# Multiple R-squared:  0.9787,	Adjusted R-squared:  0.9732  
# "CarNameHONDA" : 2.889151 vif low significane, p value = 0.028783 
#  hence can be removed in model_8.

model_8 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                enginelocationREAR + enginetypeROTOR 
              , data = train)


# Check the summary of model_8
summary(model_8)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_8)

#  Multiple R-squared:  0.9777,	Adjusted R-squared:  0.9723   
# boreratio  : 3.539052 vif and p-value 0.024614
# hence can be removed in model_9.

model_9 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + 
                CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + carbodyWAGON + 
                enginelocationREAR + enginetypeROTOR 
              , data = train)


# Check the summary of model_9
summary(model_9)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_9)

#  Multiple R-squared:  0.9767,	Adjusted R-squared:  0.9713    
# carbodySEDAN : 12.67019	vif and 0.000558 p-value
#  hence can be removed in model_10.

model_10 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + CarNamePORSCHE + CarNameRENAULT + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 carbodyHARDTOP + carbodyHATCHBACK + carbodyWAGON + 
                 enginelocationREAR + enginetypeROTOR 
               , data = train)


# Check the summary of model_10
summary(model_10)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_10)

# Multiple R-squared:  0.9742,	Adjusted R-squared:  0.9684
# CarNamePORSCHE : 4.914593 vif and p-value : 0.000374
# removing in model_11

model_11 <- lm(formula = price ~ carwidth + curbweight + enginesize + 
                             stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                             CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                             CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                             CarNamePLYMOUTH + CarNameRENAULT + 
                             CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                             carbodyHARDTOP + carbodyHATCHBACK + carbodyWAGON + 
                             enginelocationREAR + enginetypeROTOR 
                             , data = train)


# Check the summary of model_11
summary(model_11)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_11)

## Multiple R-squared:  0.9712,	Adjusted R-squared:  0.965
## curbweight : 15.774510 vif and 0.000666 p-value
#  hence can be removed in model_12


model_12 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + CarNameRENAULT + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 carbodyHARDTOP + carbodyHATCHBACK + carbodyWAGON + 
                 enginelocationREAR + enginetypeROTOR 
               , data = train)


# Check the summary of model_12
summary(model_12)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_12)

# As we have finished eliminating the high VIF and high p-values
# Next elimination of variable is based only on high p values
## Multiple R-squared:  0.9682,	Adjusted R-squared:  0.9617 
##  carbodyWAGON  0.919119 p-values
#  hence can be removed in model_13


model_13 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameISUZU + CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + CarNameRENAULT + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 carbodyHARDTOP + carbodyHATCHBACK + 
                 enginelocationREAR + enginetypeROTOR 
               , data = train)


# Check the summary of model_13
summary(model_13)

# Let us check for multicollinearity 
# Removing the variables if they are statistically insignificant.
vif(model_13)

## Multiple R-squared:  0.9682,	Adjusted R-squared:  0.962  
## CarNameISUZU :  : 0.307352 p- value
#  hence can be removed in model_14

model_14 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + CarNameRENAULT + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 carbodyHARDTOP + carbodyHATCHBACK + 
                 enginelocationREAR + enginetypeROTOR , data = train)


# Check the summary of model_14
summary(model_14)

# Removing the variables if they are statistically insignificant.
vif(model_14)

## Multiple R-squared :  0.9679,	Adjusted R-squared:  0.962  
## carbodyHATCHBACK : 0.336216 
#  hence can be removed in model_15

model_15 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + CarNameRENAULT + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 carbodyHARDTOP + enginelocationREAR + enginetypeROTOR , data = train)


# Check the summary of model_15
summary(model_15)

# Removing the variables if they are statistically insignificant.
vif(model_15)

##  Multiple R-squared:  0.9676,	Adjusted R-squared:  0.962   
## CarNameRENAULT  :  0.041035 p-value
#  hence can be removed in model_16

model_16 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 carbodyHARDTOP + 
                 enginelocationREAR + enginetypeROTOR , data = train)

  
# Check the summary of model_16
summary(model_16)

# Removing the variables if they are statistically insignificant.
vif(model_16)

## Multiple R-squared :  0.9665,	Adjusted R-squared:  0.961
## carbodyHARDTOP  : 0.052451 pp-value
#  hence can be removed in model_17

model_17 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMERCURY + CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)

  
# Check the summary of model_17
summary(model_17)

# Removing the variables if they are statistically insignificant.
vif(model_17)


## Multiple R-squared:  0.9654,	Adjusted R-squared:  0.9601
## CarNameMERCURY  :  0.02473 p-value
#  hence can be removed in model_18

model_18 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + 
                 CarNameSUBARU + CarNameTOYOTA + CarNameVOLKSWAGEN + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)


# Check the summary of model_18
summary(model_18)

# Removing the variables if they are statistically insignificant.
vif(model_18)

## Multiple R-squared:  0.964,	Adjusted R-squared:  0.9588 
## CarNameVOLKSWAGEN  : 0.02662 p-value
#  hence can be removed in model_19

model_19 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + CarNameDODGE + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + 
                 CarNameSUBARU + CarNameTOYOTA + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)


# Check the summary of model_19
summary(model_19)

# Removing the variables if they are statistically insignificant.
vif(model_19)

## Multiple R-squared :  0.9625,	Adjusted R-squared :  0.9574 
## CarNameDODGE : 0.015568 p-value
#  hence can be removed in model_20


model_20 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMITSUBISHI + CarNameNISSAN + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + 
                 CarNameSUBARU + CarNameTOYOTA + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)


# Check the summary of model_20
summary(model_20)

# Removing the variables if they are statistically insignificant.
vif(model_20)

## Multiple R-squared :  0.9607,	Adjusted R-squared :  0.9557
## CarNameNISSAN  : 0.024893 p- value
#  hence can be removed in model_21


model_21 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMITSUBISHI + CarNamePEUGEOT + 
                 CarNamePLYMOUTH + 
                 CarNameSUBARU + CarNameTOYOTA + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)


# Check the summary of model_21
summary(model_21)

# Removing the variables if they are statistically insignificant.
vif(model_21)

## Multiple R-squared:  0.9591,	Adjusted R-squared:  0.9543
## CarNamePLYMOUTH : 0.052428 p-value
#  hence can be removed in model_22

model_22 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameMAZDA + 
                 CarNameMITSUBISHI + CarNamePEUGEOT + 
                 CarNameSUBARU + CarNameTOYOTA + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)

# Check the summary of model_22
summary(model_22)

# Removing the variables if they are statistically insignificant.
vif(model_22)

## Multiple R-squared :  0.9579,	Adjusted R-squared :  0.9533 
## CarNameMITSUBISHI : 0.001452 p-value 
#  hence can be removed in model_23


model_23 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameMAZDA + CarNamePEUGEOT + 
                 CarNameSUBARU + CarNameTOYOTA + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)

# Check the summary of model_23
summary(model_23)

# Removing the variables if they are statistically insignificant.
vif(model_23)

## Multiple R-squared:  0.9544,	Adjusted R-squared:  0.9498 
## CarNameTOYOTA : 0.00370 p-value
#  hence can be removed in model_24


model_24 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameMAZDA + CarNamePEUGEOT + 
                 CarNameSUBARU + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)

# Check the summary of model_24
summary(model_24)

# Removing the variables if they are statistically insignificant.
vif(model_24)

## Multiple R-squared:  0.9513,	Adjusted R-squared:  0.9468
## CarNameMAZDA : 0.01112 p-value
#  hence can be removed in model_25


model_25 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNamePEUGEOT + 
                 CarNameSUBARU + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)

# Check the summary of model_25
summary(model_25)

# Removing the variables if they are statistically insignificant.
vif(model_25)

## Multiple R-squared:  0.9488,	Adjusted R-squared:  0.9445
## CarNamePEUGEOT :  0.00756 p- value
#  hence can be removed in model_26


model_26 <- lm(formula = price ~ carwidth + enginesize + 
                 stroke + CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameSUBARU + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)

# Check the summary of model_26
summary(model_26)

# Removing the variables if they are statistically insignificant.
vif(model_26)

## Multiple R-squared:  0.9459,	Adjusted R-squared:  0.9418 

## this is the stage where we have all three stars (***) highly significant variables,
# but also we have two variables "carwidth" & "enginesize" with high VIF and low p-values
# carwidth : 3.618967 vif, 2.13e-14 p-value & enginesize : 5.224752 vif, 3.16e-16 p-value

# Checking their corrleation
cor(train$carwidth,train$enginesize)
# 76.4 % correlation found between them 

# Making new models by dropping one of them in each model and 
# Checking their effect on predictive power and dropping one showing more affect
# creating two models  remove_carwidth & remove_enginesize

remove_carwidth <- lm(formula = price ~   enginesize + 
                        stroke + CarNameBMW + CarNameBUICK + 
                        CarNameJAGUAR + CarNameSUBARU + aspirationTURBO + 
                        enginelocationREAR + enginetypeROTOR , data = train)

remove_enginesize <- lm(formula = price ~   carwidth + 
                          stroke + CarNameBMW + CarNameBUICK + 
                          CarNameJAGUAR + CarNameSUBARU + aspirationTURBO + 
                          enginelocationREAR + enginetypeROTOR , data = train)


summary(remove_carwidth)
vif(remove_carwidth)
# Multiple R-squared:  0.9157,	Adjusted R-squared:   0.91
    
summary(remove_enginesize)
vif(remove_enginesize)
# Multiple R-squared:  0.9102,	Adjusted R-squared:  0.9041 
# impact on predictive power : 0.0357 , decrease in vif of carwidth : 1.958799

# When compared "remove_enginesize" & "remove_carwidth" we get following insight's :
# by removing the "enginesize" no high vif variables left
# Keep this model "remove_enginesize" and putting in model_27 for further analysis

# removing the stroke as it has less effect on R-squared

model_27 <- lm(formula = price ~   carwidth + 
                 CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameSUBARU + aspirationTURBO + 
                 enginelocationREAR + enginetypeROTOR , data = train)

  
summary(model_27)
vif(model_27)
# Multiple R-squared:  0.9002,	Adjusted R-squared:  0.8942 

# Removing enginetypeROTOR

model_28 <- lm(formula = price ~   carwidth + 
                 CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + CarNameSUBARU + aspirationTURBO + 
                 enginelocationREAR  , data = train)

summary(model_28)
vif(model_28)
# Multiple R-squared:  0.8999,	Adjusted R-squared:  0.8947 

#  CarNameSUBARU
model_29 <- lm(formula = price ~   carwidth + 
                 CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + aspirationTURBO + 
                 enginelocationREAR  , data = train)


summary(model_29)
vif(model_29)

# Multiple R-squared:  0.8987,	Adjusted R-squared:  0.8943 

# removing aspirationTURBO
model_30 <- lm(formula = price ~   carwidth + 
                 CarNameBMW + CarNameBUICK + 
                 CarNameJAGUAR + 
                 enginelocationREAR  , data = train)

summary(model_30)
vif(model_30)
# Multiple R-squared:  0.8971,	Adjusted R-squared:  0.8933

# making final model "final_model"
final_model <- model_30

summary(model_30)
vif(model_30)

# So, In "final_model" we have all the variables are significant having all three (***) star marks

# final_model to predict the price of cars in the test data frame,
# predict_price for predicted price value
predict_price <- predict(final_model, test[,-which(names(test)=="price")])

# find the correlation between predicted price & price in test data frame : "correlation_value"
correlation_value <- cor(test$price, predict_price)
correlation_value

# The correlation_value is equal to 0.9161259, Hence, our predicted price are very close to the test price value.
rsquare <- correlation_value^2
rsquare
# "rsquare" : R-squared for test is 0.8392867 

# notice that the r-squared value for the model is ~89% 
# and the r-squared value for the test dataset is ~84% .

# The deviation in predicton is (near) ~5% between the R-squared value of the model for the test and our final model

# plotting the Predicted & test price

actual_test_price <- test$price
predicted_value <- predict_price

plot(actual_test_price,predicted_value)
abline(lm(predicted_value ~ actual_test_price))


##-------- CONCLUSION of Analysis--------##

###--- Which Variables are significant in predicting the price of a car?

# final prediction model we have identified that carwidth, CarNameBMW, CarNameBUICK, CarNameJAGUAR, enginelocationREAR are significant variables in predicting the price of the car. 
# Our Final Regression model shows that the price of the vehicle is not entirely dependent on the parameters of the vehicle but rather the brand of the vehicles also impacts the price. 

###----- How well these variables describe the price of the car?

# We seen the  (near) ~5% variation on prediction and actual prices of test data set, this much is acceptable to make predictions. 


# our final model model_30 will help the "Geely Auto" company to precisely predict the prices in american market.
vif(model_30)
summary(model_30)

