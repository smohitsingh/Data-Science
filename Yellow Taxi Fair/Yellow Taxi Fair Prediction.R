##------ Data Understanding -----##

# key - Unique string identifying each row in both the training and test sets.
# Comprised of pickup_datetime plus a unique integer, but this doesn't matter, it should just be used as a unique ID field. Required in your submission CSV. Not necessarily needed in the training set, but could be useful to simulate a 'submission file' while doing cross-validation within the training set.

##----- Features :
# pickup_datetime - timestamp value indicating when the taxi ride started.
# pickup_longitude - float for longitude coordinate of where the taxi ride started.
# pickup_latitude - float for latitude coordinate of where the taxi ride started.
# dropoff_longitude - float for longitude coordinate of where the taxi ride ended.
# dropoff_latitude - float for latitude coordinate of where the taxi ride ended.
# passenger_count - integer indicating the number of passengers in the taxi ride.

#------- Target variable :
# fare_amount - float dollar amount of the cost of the taxi ride. This value is only in the training set; this is what you are predicting in the test set and it is required in your submission CSV.


# Including Required packages for Reading data, Manipulation of data, Visualization of data.

library(dplyr)    
library(tidyr)
library(lubridate)
library(ggplot2)
library(data.table)   # For fread() 

##__________ SETTING-UP WORKING DIRECTORY _________

setwd("C:/Users/Mohit Singh/Desktop/Data Science Portfolio/all/Yellow Taxi Fair")
getwd()        # To check whether the file is properly loaded or not
dir()          # To Check the Target file Name : 

# Clean the file for any Pre-Used valiables
remove(list = ls())

##_________ DATA LOADING _________##

# Original Data set contains 55 Million rows, But due to size restrictions and 
# as our test data is having 9914 rows, 
# So in this case 2 Millon rows are enough for a model to learn 
# and give good prediction on test datasets.

# Links:
# https://rpubs.com/msundar/large_data_analysis

##--- Checking for those functions which take less time loading big data sizes.
system.time(DT1 <- read.csv("train.csv", nrows = 2000000))
# user  system elapsed 
# 116.39    0.36  118.03 

system.time(DT1 <- fread("train.csv", nrows = 2000000))
# user  system elapsed 
# 2.75    0.70    3.55
# As compared to "read.csv()" format -  
# "fread()" is performing better, so going with this function ahead for loading data.


# Creating a New Data frame taxi_fair for train dataset 
# Blank spaces if any will be replaced by NA using na.strings and we dont want strings to be converted to factors automatically while reading file 
taxi_fair  <- fread("train.csv",na.strings = c("",NA),header = TRUE, stringsAsFactors = FALSE,nrows = 2000000)
View(taxi_fair)        # To view data frame uber_data in table form in R for easy analysis.          
dim(taxi_fair)
# 2,000,000 rows and 8 variables.
names(taxi_fair)

# Creating a New Data frame test_fair for test data set 
test_fair  <- fread("test.csv",na.strings = c("",NA),header = TRUE, stringsAsFactors = FALSE)
dim(test_fair)
# 9914 rows and 7 variables.
names(test_fair)

# Understanding structure of data
str(taxi_fair)
summary(taxi_fair)

str(test_fair)
summary(test_fair)

taxi_fair <- as.data.frame(taxi_fair)
test_fair <- as.data.frame(test_fair)
class(taxi_fair)

# From analysis of structure, There appears to be issues in each variable including the target fare amount. 
# it is found that :

# fare amount has negative values and a large max relative to the 99th quantile
# pickup longitude, pickup latitude, dropoff longitude, and dropoff latitude have weird values for min and max
# passenger count is sometimes 0 or really large

# Dropping "Key" column  
taxi_fair <- taxi_fair[-1]
test_fair <- test_fair[-1]

#----To fix this, I start by dropping the coordinates that outside a range. 
# The range was determined by roughly outlining New York City. Certainly not exact, but, gets rid of the really crazy values.
taxi_fair <- taxi_fair %>%  
  filter(pickup_longitude > -80 & pickup_longitude < -70) %>%
  filter(pickup_latitude > 35 & pickup_latitude < 45) %>%
  filter(dropoff_longitude > -80 & dropoff_longitude < -70) %>%
  filter(dropoff_latitude > 35 & dropoff_latitude < 45)

##----

# taxi_fair
taxi_fair <- taxi_fair %>%
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    year = as.factor(year(pickup_datetime)),
    month = as.factor(month(pickup_datetime)),
    day = as.numeric(day(pickup_datetime)),
    dayOfWeek = as.factor(wday(pickup_datetime)),
    hour = as.numeric(hour(pickup_datetime)),
    timeOfDay = as.factor(ifelse(hour >= 3 & hour < 9,
                                 "Morning", ifelse(hour >= 9 & hour < 14, "Mid-Day",
                                                   ifelse(hour >= 14 & hour < 18, "Evening", "Night"))))
  )%>%
  select(-pickup_datetime)

# test_fair
test_fair <- test_fair %>%
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    year = as.factor(year(pickup_datetime)),
    month = as.factor(month(pickup_datetime)),
    day = as.numeric(day(pickup_datetime)),
    dayOfWeek = as.factor(wday(pickup_datetime)),
    hour = as.numeric(hour(pickup_datetime)),
    timeOfDay = as.factor(ifelse(hour >= 3 & hour < 9,
                                 "Morning", ifelse(hour >= 9 & hour < 14, "Mid-Day",
                                                   ifelse(hour >= 14 & hour < 18, "Evening", "Night"))))
  )%>%
  select(-pickup_datetime)


dim(test_fair)
dim(taxi_fair)

##--- Fair amount n "taxi_fair" dataset
ggplot(taxi_fair, aes(x= taxi_fair$fare_amount))+ geom_histogram()
ggplot(taxi_fair, aes(y = taxi_fair$fare_amount))+ geom_boxplot()
quantile(taxi_fair$fare_amount, seq(0,1,0.01)) 

# Outlier Treatment: Capping done. 
# Lower percentile
taxi_fair$fare_amount[which(taxi_fair$fare_amount < 3.30)] <- 3.30 

# Upper percentile
quantile(taxi_fair$fare_amount, seq(0,1,0.01)) 
taxi_fair$fare_amount[which(taxi_fair$fare_amount > 30.27)] <- 30.27 


##  train
dummy_var <- data.frame(model.matrix( ~ timeOfDay, data = taxi_fair))
# Excluding the first column which contains only number of rows
dummy_var <- dummy_var[-1]
View(dummy_var)
# changing name 
names(dummy_var)[1] 
# Checking the class of "dummy_var" 
class(dummy_var)
dim(dummy_var)
##  Final, dummy_var contains 1958885 observations and 3 variables

## test
dummy_var <- data.frame(model.matrix( ~ timeOfDay, data = test_fair))
# Excluding the first column which contains only number of rows
dummy_var <- dummy_var[-1]
View(dummy_var)
# changing name 
names(dummy_var)[1] 
# "test_fair" is final data frame after adding dummy variable and removing character variables 
test_fair <- test_fair[-c(11)]
test_fair <- cbind(test_fair,dummy_var) 
dim(test_fair)
# The taxi_fair data frame has 9914 observations and 13 varibales.

# Checking structure of tes and train data.
str(taxi_fair)
str(test_fair)


test_fair$year <-  as.numeric(test_fair$year)
test_fair$month <- as.numeric(test_fair$month)
test_fair$dayOfWeek <-as.numeric(test_fair$dayOfWeek)  


taxi_fair$year <- as.numeric(taxi_fair$year)
taxi_fair$month <-as.numeric(taxi_fair$month)
taxi_fair$dayOfWeek <- as.numeric(taxi_fair$dayOfWeek)

# Data is ready for modelling,
# we will split data and then do scaling of it.

# write train test .
write.csv(taxi_fair,"taxi_fair.csv",row.names = FALSE)
write.csv(test_fair,"test_fair.csv",row.names = FALSE)













