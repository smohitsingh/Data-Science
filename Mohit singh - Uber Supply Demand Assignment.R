# ----- MOHIT SINGH ( APFE18803194 ) - PGDDS -  JUNE 2018----- #

# Including Required packages for Reading data, Manipulation of data, Visualization of data.

library(dplyr)    
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

##__________ SETTING-UP WORKING DIRECTORY _________

setwd("C:/Users/Mohit Singh/Desktop/Uber Assignment")
getwd()        # To check whether the file is properly loaded or not
dir()          # To Check the Target file Name : 

# Clean the file for any Pre-Used valiables
remove(list = ls())

##_________ DATA LOADING _________

# Creating a New Data frame uber_data  
# Blank spaces if any will be replaced by NA using na.strings and we dont want strings to be converted to factors automatically while reading file 
uber_data<-read.csv("Uber Request Data.csv",na.strings = c("",NA),header = TRUE, stringsAsFactors = FALSE)
View(uber_data)        # To view data frame uber_data in table form in R for easy analysis.          


##_____________ Exploratory Data Analysis _______________##
##__________________ DATA CLEANING ___________

#________ Missing Value analysis _______

colSums(is.na(uber_data))
# It is found that NA values are present in two columns : Driver.id & Drop.timestamp

navalue_id <- sum(is.na(uber_data$Driver.id))                 # navalue_id : NA values in Driver.id column of uber_data
navalue_time <- sum(is.na(uber_data$Drop.timestamp))          # navalue_time : NA values in Drop.timestamp column of uber_data
total_na_value <- sum(is.na(uber_data))                       # total_na_value : total NA values in uber_data
ifelse((navalue_id + navalue_time) == total_na_value, "Verified NA's","NA's not Verified")
# Verified NA's, there are 2650 NA's in Driver.id & 3914 NA's Drop.timestamp,
# adding to 6564 NA's in uber_data, shown by total_na_value

#------- Analysis Result ------

# There are NA's in Driver.id, which shows that 2650 drivers are not available at time of request. 
# There are NA's in Drop.timestamp, which shows that 3914 number of trips did not get completed.
status_na <- uber_data %>% filter(is.na(Driver.id)) %>%  select(Status)     # checking by status_na for status when there are NA's in Driver.id
table(status_na)
# hence, Driver.id == NA match with trip where Status == No Cars Available. 
# NA's in columns Drop.timestamp and Driver.id to be kept as it is, since it would assist in identification later

# Checking for any Duplicate values in data frame uber_data
if( nrow(unique(uber_data)) == nrow((uber_data))){
  print("No Duplicated Data")
}else{
  print("Duplicated Data Found")
}
# Hence, checked No Duplicated Data found


# _________ STANDARDIZING DATA _________

# Date in Request.timestamp & Drop.timestamp is in 2 different formats, having both "-"  &  "/" .
# We will make the separator's consistent throughout in date of Request.timestamp & Drop.timestamp columns &
# putting them in new variable's Request_dt & Drop_dt and adding them to uber_data data frame respectively.
uber_data$Request_dt <- str_replace_all(uber_data$Request.timestamp, "[/]",  "-")
uber_data$Drop_dt <- str_replace_all(uber_data$Drop.timestamp, "[/]",  "-")

# Convert Request_dt & Drop_dt to a common Date-Time format,which are present in %d-%m-%Y %H:%M
# To bring uniformity of data, seconds are ignored from time, as they are not present in some rows.  
uber_data$Request_dt <- as.POSIXlt(uber_data$Request_dt, format = "%d-%m-%Y %H:%M")
uber_data$Drop_dt <- as.POSIXlt(uber_data$Drop_dt, format = "%d-%m-%Y %H:%M")

# as.POSIXlt converts data in the list type of day, month, year, hour, minute, second, etc., which is not suitable  for further calculations
# So, Converting to as.POSIXct data type, which converts the data in the seconds format 
# Converting Request_dt & Drop_dt to POSIXct Date-time objects
uber_data$Request_dt <- as.POSIXct(uber_data$Request_dt)
uber_data$Drop_dt <- as.POSIXct(uber_data$Drop_dt)
# Checking their data type for confirmation
str(uber_data[7:8])
# Hence, all values are converted successfully.

# Lets check if all values have been converted or not for Request_dt & Drop_dt. 
length(which(is.na(uber_data$Request_dt)))  # NA values are 0 same as in Request.timestamp 
length(which(is.na(uber_data$Drop_dt)))     # NA values are 3914 same as in Drop.timestamp
# Hence, all values are converted successfully.


#___________ FIX INVALID VALUES ___________

# Converting Pickup.point and Status to factor data type, as they will be helpful while plotting graph and further analysis.
uber_data$Pickup.point <- as.factor(uber_data$Pickup.point)
uber_data$Status <- as.factor(uber_data$Status)
# Checking for the data type of changed Variables
str(uber_data)
# Hence, all values are converted successfully


##___________ DATA UNDERSTANDING - UNIVARIATE ANALYSIS ___________##

# Understanding characteristcs of data frame uber_data and its variable data type 
names(uber_data)     # Names of each columns in uber_data
# columns present in uber_data  :  "Request.id",  "Pickup.point",  "Driver.id",  "Status",  "Request.timestamp",  "Drop.timestamp"   
length(uber_data)    # Number of columns in uber_data  : 6
nrow(uber_data)      # Number of Rows in uber_data  : 6745
summary(uber_data)   
str(uber_data)
# Shows that data frame uber_data contains 6745 (Rows) observations and 6 (columns) variables
# TWO columns are of Integer type and FOUR columns are of Character type

total_request <- length(uber_data$Request.id)
# As there are no NA's in Request.id, total 6745 requests are made by customers in the given time period, denoted by total_request

# Count of each status in data frame uber_data 
table(uber_data$Status)
# Trip Completed : 2831, Cancelled : 1264, No Cars Available : 2650
# Total Incomplete trips when request is made = 1264 + 2650 = 3914, Same as the number of NA's in Drop.timestamp, NA shows no trip done

# Unique dates in Request_dt & Drop_dt for which data is given
unique(date(uber_data$Request_dt))
# There are 5 unique dates in columns : "2016-07-11",  "2016-07-12",  "2016-07-13",  "2016-07-14",  "2016-07-15" 
unique(date(uber_data$Drop_dt))
# There are 6 unique dates in columns : "2016-07-11",  "2016-07-12",  "2016-07-13",  "2016-07-14",  "2016-07-15", "2016-07-16"           

# Hours of the day for which data is given in Request_dt & Drop_dt
sort(unique(hour(uber_data$Request_dt)))
sort(unique(hour(uber_data$Drop_dt)))
# It can be seen time-span is across 24 hours pf the day
# 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23


## ___________ UNIVARIATE ANALYSIS - PLOTS ___________

# Pickup.point
ggplot(uber_data, aes(x = as.factor(uber_data$Pickup.point))) + geom_bar()
# NUmber of Requests made are more at city than airport.

# Status
ggplot(uber_data, aes(x = as.factor(uber_data$Status))) + geom_bar()


## ___________ DERIVED MATRIX ___________

# Adding new required columns to the data frame uber_data 
# by seperating date and hour from Request_dt to new columns Request_date, Request_hour &
# from Drop_dt to new columns Drop_date, Drop_hour.
# They will be helpful for date-wise and hour-wise analysis.
uber_data <- mutate(uber_data,Request_date = date(uber_data$Request_dt), 
                    Request_hour = hour(uber_data$Request_dt),
                    Drop_date = date(uber_data$Drop_dt),
                    Drop_hour = hour(uber_data$Drop_dt))

# Checking for the data type of new added columns
str(uber_data)
# The data types of new columns of date's is date type and of hours column is integer data type

# Calculating duration of time taken during Request made and drop time (Drop_dt and Request_dt) between City and Airport in minutes.
# Earlier, Drop_dt and Request_dt columns data type was converted into as.POSIXct() type which stores values in Seconds.
time_dif <- unclass(uber_data$Drop_dt) - unclass(uber_data$Request_dt)   # time_dif is difference of time taken, it's unit is seconds
class(time_dif)                         # Checking class of time_dif, which is of numeric data type
Duration_min <- time_dif/60             # Duration_min is the time taken to complete the trip in minutes, converted from seconds to minutes by diving time_dif by 60, i.e. 1 Minute = 60 Seconds 
class(Duration_min)                     # Checking class of Duration_min, which is of numeric data type

# Creating a new variable Supply for availability of Supply of cars at the time of request by customer.
# In Supply variable taking input as "No Supply" when Status is "No Cars Available" else "Supply Available"
Supply <- ifelse(uber_data$Status=="No Cars Available","No Supply","Supply Available")
class(Supply)                           # Checking class of Supply, which is of Character data type
# Converting into factor type, to be used while plotting graph and further analysis
Supply <- as.factor(Supply)

# Adding new columns Supply and Duration_min to the data frame uber_data
uber_data <- uber_data %>% mutate(Supply,Duration_min)
str(uber_data)                          
# Checking class of Supply, which is of factor data type
# Checking class of Duration_min, which is of Numeric data type

table(is.na(uber_data$Duration_min))
table(is.na(uber_data$Drop_dt))
# Checking for NA's present : 3914 , same as number of NA's in Drop_dt. Hence, it is added correctly.


## ___________ PROBLEM IDENTIFICATION ___________

# Stacked Bar chart Bplot1 to see frequency of Pickup.point by Request_hour of all days
Bplot1 <- ggplot(uber_data, aes(x = as.factor(uber_data$Request_hour),fill = uber_data$Pickup.point)) + geom_bar() +
labs(x = "Hour", y = "Number of Requests", fill = "Pickup Point" )
# It can be seen that demand is more in city in the Morning hours 
# and demand is more at airport in from Evening till night hours 


# Stacked Bar chart Bplot2 to see % frequency of Requests over Status by Request_hour of all days 
Bplot2 <- ggplot(uber_data, aes(x = as.factor(uber_data$Request_hour),fill = uber_data$Status)) + geom_bar(position = "fill")+
labs(x = "Hour", y = "% Requests", fill = "Status" )
# Fromt Plot, it is seen that there is more distribution of "No Cars Available" as compared to "Cancelled" cars
# More number of "Cancelled" cars are seen only in the early morning hours as compared to other hours and
# problem of "No Cars Available" is more in Morning hours & from Evening till late Night hours


# Stacked Bar chart Bplot3 to see % frequency of Requests over Status by Request_date of the given data   
Bplot3 <- ggplot(uber_data, aes(x = as.factor(uber_data$Request_date),fill = uber_data$Status)) + geom_bar(position = "fill") +
labs(x = "Date", y = "% Requests", fill = "Status" )
# Throughout the days their respective distribution remained almost the same 
# Problem of "No Cars Available" is more reflected in all 5 days as compared to "Cancelled" cars.
# But this days-wise plot is not giving any strong evidence of any raised problem to be taken out of further analysis.


# Stacked Bar chart Bplot4 to see percentage distribution of Pickup.point by Request_hour of all days
Bplot4 <- ggplot(uber_data, aes(x = as.factor(uber_data$Request_hour),fill = uber_data$Pickup.point)) + geom_bar(position = "fill") +
labs(x = "Hours", y = "% Requests", fill = "Status" )
# From Bplot4 chart 5 major time blocks can be noticed based on variations in requests made by customer in 24 hours, 
# and the distribution date-wise showed less significant variation on Bplot3 plot, so hours are used for final analysis
# So, We need to make segments out of the time slots by dividing the time slots into 5 groups and storing them in a new column time_slot
# Time slots of the day & Segment Name considered are as mentioned below :
# 0000 - 0359  : Early_Morning, 0400 - 0859  : Morning, 0900 - 1559  : Afternoon, 1600 - 2059  : Evening, 2100 - 2359  : Late_Night 
# Segmentation of hours is made on the basis of pattern noticed in frequency of requests made during 24 hours
uber_data$time_slot = ifelse((uber_data$Request_hour <= 4), "Early_Morning", ifelse(uber_data$Request_hour <= 9,"Morning",ifelse(uber_data$Request_hour <= 16,"Afternoon",ifelse(uber_data$Request_hour <=20 ,"Evening","Late_Night"))))
# Making column time_slot as factor, it will be useful in plotting graphs for further analysis
uber_data$time_slot <- as.factor(uber_data$time_slot)


# Now we will plot a bar chart Bplot5 for number of trips made during different time slots
Bplot5 <- ggplot(uber_data,aes( x= time_slot, fill = time_slot )) + geom_bar() 
# Lets see how many Requests are made in Different time slots
table(uber_data$time_slot)
# So, from respective counts of time slots, It is evident that more requests are made during Morning hours, then during Evening hours and lesser for other time slots 
# In Morning hours the frequency of requests made is 2103 
# In Evening hours the frequency of requests made is 1893
# In Afternoon hours the frequency of requests made is 1224
# In Late_Night hours the frequency of requests made is 947
# In Early_Morning hours the frequency of requests made is 578


# Stacked bar chart Bplot6 of distribution of status among different time slots
Bplot6 <- ggplot(uber_data,aes(x = time_slot, fill = Status)) + geom_bar()
# Among all time slots "Cancelled" issue is more in morning hours.
# Among all time slots "No cars available" issue is more in evening hours.

#  morning_df and evening_df data frame are created for further analysis of requests in morning and evening hours
morning_df<- subset(uber_data, uber_data$time_slot == "Morning") 
table(morning_df$Status)
# In Morning, Count of status is :-  Cancelled  : 843,  No Cars Available : 406,  Trip Completed : 854 
evening_df<- subset(uber_data, uber_data$time_slot == "Evening") 
table(evening_df$Status)


#---- Analysis of  status of requests made in the Morning hours & Evening hours respectively---

# Total Morning hour analysis for issue's in requests made
issue_mor <- nrow(morning_df)                                        # issue_mor is number of requests in morning 
issue_mor_tc <- length(which(morning_df$Status == "Trip Completed")) # issue_mor_tc is number of trips completed in morning
mor_tc_Per <-   (issue_mor_tc/issue_mor)*100                         # mor_tc_Per is percent of trips completed in morning
issue_mor_can <- length(which(morning_df$Status == "Cancelled"))     # issue_mor_can is number of trips cancelled in morning 
mor_can_Per <-   (issue_mor_can/issue_mor)*100                       # mor_can_Per is percent of trips cancelled in morning
mor_fail_cars <-  (issue_mor - issue_mor_tc)                         # mor_fail_cars is number of trips failed to complete in morning, considering cancelled and no cars available issues
mor_fail_per <-  (mor_fail_cars/issue_mor)*100                       # mor_fail_per is percent of trips failed to complete in morning
# out of 2103 requests, only 854 requests were completed, which are around 40 % of the total morning requests shown by mor_tc_Per
# out of 2103 requests, 1249 requests were not completed by the Drivers in the same time slot because of cancelled and no cars available issues shown by mor_fail_per
# 843 requests were cancelled by drivers, which are around 40 % of the total morning requests shown by mor_can_Per


# Total Evening hour analysis for issue's in requests made
issue_ev <- nrow(evening_df)                                         # issue_ev is number of requests in evening 
issue_ev_tc <- length(which(evening_df$Status == "Trip Completed"))  # issue_ev_tc is number of trips completed in evening
ev_tc_Per <-   (issue_ev_tc/issue_ev)*100                            # ev_tc_Per is percent of trips completed in evening
issue_ev_can <- length(which(evening_df$Status == "Cancelled"))      # issue_ev_can is number of cancelled in evening 
ev_can_Per <-   (issue_ev_can/issue_ev)*100                          # ev_can_Per is percent of trips cancelled
ev_fail_cars <-  (issue_ev - issue_ev_tc)                            # ev_fail_cars is number of trips failed to complete in evening, considering cancelled and no cars available issues
ev_fail_per <-  (ev_fail_cars/issue_ev)*100                          # ev_fail_per is percent of trips failed to complete in evening
# out of 1893 requests only 642 requests were completed, which are around 34 % of the total evening requests 
# out of 1893 requests, 1251 requests were not completed by the Drivers in the same time slot,which are around 66 % because of cancelled and no cars available issues.
# 124 requests were cancelled by drivers, which are only around 6% of the total evening requests, so less sgnificant problem in evening


##_____________ Based on the Above Analysis of Issues _____________##
#------- The 2 Most Pressing Problems for UBER are as mentioned below :

# Problem 1:  Most number of requests are made in morning hours from City to Airport and most issue's are found in Morning hours itself,
# out of 2103 total morning Requests only 854 were completed, which is around 40% of the total Morning requests
# 1249 requests were not completed by the Drivers, which are around 60 % of the total morning requests.
# Cancelled cars are high as 843, which are around 40 % of the total morning requests.

#	Problem 2:  Most number of requests are made in evening hours from Airport to City and most issue's are occuring in Evening hours itself, 
# out of 1893 total evening Requests only 642 were completed, which is only around 34% of the total Evening requests 
# 1251 requests were not completed by the Drivers, which are around 66 % of the total evening requests, dominated by "No Cars Available" issue.



##_____________ANALYSIS FOR THE PROBLEM's FROM CITY TO AIRPORT _____________##


# New Data frame city to analyse total requests made at City  
city <- subset(uber_data, uber_data$Pickup.point == "City")  
# distribution of the Respective status at City
table(city$Status)
# In City, Count of status is :-  Cancelled  : 1066,  No Cars Available : 937,  Trip Completed : 1504 

#------- Calculation's for Issues at CITY ---- :

# Here, issue is "cancelled" cars and non availability of cars at city
# Calculating the Overall problems at City due to both "Cancelled" or "No Car Available" issues
# Total Number of request's at city is city_x
city_x <- nrow(city)
# Total number of issue's at city is city_y
city_y <- length(which(city$Status != "Trip Completed"))
# Percentage of issue's at City is issue_city
issue_city <- (city_y/city_x)*100
# So,issues occuring are around 57 percent of total requests at city.
# Only around 43 percent Requests were actually completed at city pick-up point


# Bar Plot to check overall distribution of Request's over status at City pickup point  
ggplot(city, aes(x = city$Status , fill=city$Pickup.point )) + geom_bar() + 
labs( y = "Number of Requests", x = "Status", fill = "Pick-up Point") + theme_bw()
# Plot shows Both "Cancelled" and "No Car Available" problems of cars are significantly high at City 

# Stacked Bar Plot to see the Percentage distribution of status date-wise in city. 
ggplot(city,aes(x = city$Request_date, fill = city$Status)) + geom_bar(position = "fill") + 
labs( y = "% Count", x = "Date", fill = "Status at City")
# There is no significant variations in distribution amoung days, which cannot show evidence of problem when checked on each day

# Stacked Bar Plot to see the Time-slot wise distributions of Request's over status in city
ggplot(city,aes(x = city$time_slot,fill = city$Status)) + geom_bar() + 
labs( y = "Request's", x = "Time Slot", fill = "Status at City") 
# So, In the Morning hours "Cancelled" and "No Car Available" cars problems are very high as compared to other time slots 
# Hence, We can take Morning hours for further analysis

# Stacked Bar Plot to see the Hour-wise distributions of status, to confirm our morning hour analysis
ggplot(city,aes(x = city$Request_hour,fill = city$Status)) + geom_bar()+ 
labs( y = "Request's", x = "Hours", fill = "Status at City") 
# So, In the Morning hours from 5am to 9am,  "Cancelled" and "No Car Available" problems are very high as compared to other time of the day 

# Analysis reflected that Morning time slot gives the highest numbers of problems, So it is to be further explored 
# New Data frame city_morning for Morning time-slot 
city_morning <- subset(city, city$time_slot == "Morning")

#------- Calculation's for Issue's at CItY (Morning time - slot) ---- :

# Here issue is "cancelled" cars and non availability of cars in Morning time-slot. 
# Calculating the Overall problems at City due to both "Cancelled" or "No Car Available" issues.
# Total Number of request's at city_morning hours is city_xx.
city_xx <- nrow(city_morning)
# Total number of issue's at city_morning hours is city_yy 
city_yy <- length(which(city_morning$Status != "Trip Completed"))
# Percentage of issue's at City Morning hours is issue_city_mor
issue_city_mor <- (city_yy/city_xx)*100
# So,issues occuring are around 72 percent of total request's made at city in morning time-slot.
# Only around 28 percent Requests were actually completed at city pick-up point in morning time slot


# Morning time slot to be explored to get better insight 
# Bar Plot to see the distribution of Status in Morning time-slot in city
ggplot(city_morning, aes(x = Status , fill= Pickup.point )) + geom_bar() + 
labs( y = "Request's", x = "Status in Morning time-slot", fill = "Pick-up Point")
# So, In Morning time-slot "Cancelled" cars problems are very high 

# Stacked Bar Plot to show the day-wise % distribution of status in morning
ggplot(city_morning, aes(x = Request_date , fill= Status)) + geom_bar(position = "fill") + 
labs(title = "% Request's over status in Morning time in City", y = "% Request of Status ", x = "Date", fill = "Status")
# The bar graph shows only slight variation in day-wise
# because of less variation amoung days, we can't take it for further analysis on day basis

# Stacked Bar Plot to show the % distribution of status in Morning hours (5am - 9am)
ggplot(city_morning, aes(x = Request_hour,fill = Status)) + geom_bar(position = "fill")+
labs(title = "% Request's over status in Morning time in City",x = "Morning Hours",y = "% Request of Status",fill = "Status")
# The bar graph shows the peak of the problem is found around 5pm, which is of only slight variation than other morning hours
# because of less variation amoung hours, we cant take it for further analysis on hour basis



##_____________ ANALYSIS FOR THE PROBLEM FROM AIRPORT TO  CITY ____________## 


# New Data frame airp to analyse total requests made at Airport  
airp <- subset(uber_data, uber_data$Pickup.point == "Airport")  
# distribution of the Respective status at Airport
table(airp$Status)
# At Airport, Count of status is :-  Cancelled  : 198,  No Cars Available : 1713,  Trip Completed : 1327 

#------- Calculation's for Issues at AIRPORT ---- :

# Here, issue is "cancelled" cars and non availability of cars at Airport
# Calculating the Overall problems at Airport due to both "Cancelled" or "No Car Available" issues
# Total Number of request's at airport is airp_x
airp_x <- nrow(airp)
# Total number of issues at airport is airp_y 
airp_y <- length(which(airp$Status != "Trip Completed"))
# Percentage of issue at airport is issue_airp
issue_airp <- (airp_y/airp_x)*100
# So,issues occuring are around 59 percent of total requests at airport.   
# Only around 40 percent Requests were actually completed at airport pick-up point


# Bar Plot to check overall distribution of requests on status at Airport pickup point  
ggplot(airp, aes(x = airp$Status , fill = airp$Pickup.point )) + geom_bar() + 
labs( y = "Number of Requests in each status", x = "Status", fill = "Pick-up Point") + theme_bw()
# Plot shows "No Car Available" problem of cars are very high at Airport 

# Stacked Bar Plot to see the % distribution of status date-wise at Airport 
ggplot(airp,aes(x = airp$Request_date,fill = airp$Status)) + geom_bar(position = "fill") + 
labs( title = ("% Requests made over status at Airport each day") ,  y = "% Requests", x = "Date", fill = "Status")
# There is no significant variations in distribution amoung days, which cannot show evidence of problem when checked on each day
 
# Stacked Bar Plot to see the Time-slot wise distributions of status
ggplot(airp,aes(x = airp$time_slot,fill = airp$Status)) + geom_bar() + 
labs(title = ("% Requests made over status at Airport on each time-slot"), y = "Count of Requests", x = "Time Slot", fill = "Status") 
# So, In the Evening hours "No Cars Available" problems are very high as compared to other time slots 
# Hence, We can take Evening hours for further analysis

# Stacked Bar Plot to see the Hour-wise distributions of status, to confirm our Evening hour analysis
ggplot(airp,aes(x = airp$Request_hour,fill = airp$Status)) + geom_bar()+ 
labs(title = ("% Requests made over status at Airport on Hour-wise"), y = "Count of Requests", x = "Hours", fill = "Status") 
# So, In the Evening hours from 17 pm to 22 pm, "No Car Available" problems are very high as compared to other time of the day 

# Analysis reflected that Evening time slot gives the highest numbers of problems, So it is to be further explored 
# New Data frame ap_evening for Evening time-slot 
airp_evening <- subset(airp,airp$time_slot == "Evening")
View(airp_evening)

# Evening time slot to be explored to get better insight 
# Bar Plot to see the distribution of Status in Evening time-slot at Airport
ggplot(airp_evening, aes(x = Status , fill= Pickup.point )) + geom_bar() + 
labs( y = "Count", x = "Status in Evening time-slot", fill = "Pick-up Point")
# So, In Evening time-slot "No Car Available" problems are very high and "Cancelled" cars issue is very low 

# Stacked Bar Plot to show the day-wise % distribution of requests over status in Evening time-slot
ggplot(airp_evening, aes(x = Request_date , fill= Status)) + geom_bar(position = "fill") + 
labs(title = "% Request's on Time_Slot(Evening) at airport", y = "% Requests over Status ", x = "Date", fill = "Status")
# The bar graph shows only slight variation in day-wise analysis
# because of less variation amoung days, we can't take it for further analysis on day basis

# Stacked Bar Plot to show the % distribution of requests over status in Evening hours (17pm - 20pm)
ggplot(airp_evening, aes(x = Request_hour,fill = Status)) + geom_bar(position = "fill")+
labs(title = "% Request's on Time-slot(Evening) at airport",x = "Evening hours",y = "% Request over Status",fill = "Status")
# So, throughout Evening time-slot "No Car Available" problems are very high and "Cancelled" cars issue is very low 
# Drivers are cancelling very less number of requests


#------- Calculation's for Issues at AIRPORT (Evening time-slot) ---- :

# Here, issue is "cancelled" cars and fewer cases of non availability of cars at Airport in Evening time-slot.
# Calculating the Overall problems at Airport due to both "Cancelled" or "No Car Available" issues.
# Total Number of request's at airport Evening time-slot is airp_xx
airp_xx <- nrow(airp)
# Total number of issues at airportEvening time-slot is airp_yy 
airp_yy <- length(which(airp$Status != "Trip Completed"))
# Percentage of issue at airport is issue_airp
issue_airp_eve <- (airp_yy/airp_xx)*100
# So,issues occuring are around 59 percent of total requests at airport in Evening time-slot.   
# Only around 41 percent Requests were actually completed at airport pick-up point in Evening time-slot.



##_____________ ANALYSIS : DEMAND & SUPPLY GAP ____________## 
#__________________ BETWEEN CITY & AIRPORT ___________

#________ Assumptions are mentioned below ____ :
# Analysis of data done so far showed the evidence that there is huge Supply and Demand gap between what is required & available at the city and airport
# For Demand at CITY & AIRPORT  = Trip Completed, Cancelled, No Cabs Available are considered
# For Supply at CITY & AIRPORT  = Trip Completed, Cancelled are considered
# Reasons : Cancelled are also considered in supply because requests were made and also cars are available there, it's driver who cancelled, Because of which uber will face revenue loss   


# Calculating the Demand & supply problems at City due to both "cancellation" or "No Cars Available" issues in the Morning hours.
# No of trips completed from city to airport is city_complete in morning time-slot
city_complete <- length(which(city_morning$Status =="Trip Completed"))
# Supply of uber drivers at city is city_supply in morning time-slot
city_supply <- length(which(city_morning$Status != "No Cars Available"))
# Demand of uber drivers at city is city_demand in morning time-slot
city_demand <- nrow(city_morning)
issue.city_ds_per <- (city_supply/city_demand)*100  
# Hence,it is clear that in morning, Out of 1677 requests made at City, Inspite of having supply of 1292,
# which is around 77 percent of request's at city in morning hours, Only 472 trips are actually getting completed,
# which is around 28 percent of request's at city in morning hours

# Bar Plot to show the Supply - Demand gap at city in morning hours.
ggplot(city_morning, aes(x = Supply , fill= Pickup.point )) + geom_bar() + 
labs( y = "Count", x = "Supply in Morning time-slot", fill = "Pick-up Point")


# Calculating the Demand & supply problems at airport due to both "cancellation" or "No Cars Available" issues in the Evening hours.
# No of trips completed from airport to city is airp_complete
airp_complete <- length(which(airp_evening$Status =="Trip Completed"))
# Supply of uber drivers at airport is airp_supply
airp_supply <- length(which(airp_evening$Status !="No Cars Available"))
# Demand of uber drivers at airport is airp_demand
airp_demand <- nrow(airp_evening)
issue.airp_ds_per <- (airp_supply/airp_demand)*100  
# Hence,its clear that in evening out of 1457 requests made at airport, only supply of 390 cars was available, 
# which is around 27 percent of request's at airport in evening hours, Only 312 trips are actually getting completed,
# which is around 21 percent of request's at airport in evening hours.

# Bar Plot to show the Supply - Demand gap at airport in evening hours.
ggplot(airp_evening, aes(x = Supply , fill= Pickup.point )) + geom_bar() + 
labs( y = "Count", x = "Supply in Evening time-slot", fill = "Pick-up Point")
 
# Therefore, From above analysis, its proved that huge Supply Demand gap is occuring, Which is required to be solve.


#--------- REASON's FOR DEMAND & SUPPLY GAP --------

city %>% summarize(mean(Duration_min, na.rm = TRUE))
# Average Distance (in minutes) to be covered between City to Airport : 53 minutes.
city_morning %>% summarize(mean(Duration_min, na.rm = TRUE))
# Average Distance (in minutes) to be covered between City to Airport in morning hours : 53 minutes.
# Hence,less variation is found in average time taken between City to Airport in morning hours and throughout of the day   

airp %>% summarize(mean(Duration_min, na.rm = TRUE))
# Average Distance (in minutes) to be covered between Airport to city in evening hours : 52 minutes.
airp_evening %>% summarize(mean(Duration_min, na.rm = TRUE))
# Average Distance (in minutes) to be covered between Airport to city in evening hours : 52 minutes.
# Hence,less variation is found in average time taken between Airport to city in evening hours and throughout of the day   


# Therefore, from above analysis it is found that :
# Average Distance (in minutes) between City to Airport : 53 minutes.
# and Average Distance (in minutes) between Airport to city : 52 minutes.
# time taken between city and airport in time-slot wise analysis is showing no impact on the supply - demand gap. 

# possible reasons will be wait time at the destination, which drivers are preventing the trip to save their time.


# Recommendations :
# In order to prevent the drivers from cancellation of requests, as in case of city morning hours status, compensation of time in terms of some % of their monthly salary can be provided. Which will reduce cancellations and also help customers on reaching to their destinations on time.
# For Such situations where uber customer's face more problems what analyzed, some special cabs can be fixed to meet the purpose and their pay to dealt accordingly, as they will be spending long hours idle at destination locations.
# Such Problematic hours of the day can be fixed for some special offers for drivers to motivate them to serve the purpose and meet more customer satisfaction.
# Drivers can be paid for their fuel, in case they completing more requests in such Problematic hours and returning back from destination without any customer. 
