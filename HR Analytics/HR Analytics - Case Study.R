
##---- Problem Statement ----##
# A large company named XYZ, employs, at any given point of time, around 4000 employees. However, every year, around 15% of its employees leave the company and need to be replaced with the talent pool available in the job market. 
# The management believes that this level of attrition (employees leaving, either on their own or because they got fired) is bad for the company, because of the following reasons -
# The former employees' projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss among consumers and partners
# A sizeable department has to be maintained, for the purposes of recruiting new talent
# More often than not, the new employees have to be trained for the job and/or given time to acclimatise themselves to the company

# Hence, the management has contracted an HR analytics firm to understand what factors they should focus on, in order to curb attrition. 
# Since you are one of the star analysts at the firm, this project has been given to you.


##--- Business Understanding ---##
# To find out what changes should be made at workplace in XYZ Company, in order to get most of their employees to stay. 


##--- Goal of the Analysis ---##
# 1. To model the probability of attrition using a logistic regression. 
# 2. To find out which of the variables in the given data is most important and needs to be addressed right away.


### ------- Installing & Loading Required Packages --------- ##
# Loading packages  
library(car)
library(MASS)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
#install.packages("caret")
library(caret)
library(pscl)
library(ROCR)
library(e1071)



# Removing the used variables
remove(list = ls())


## ------- LOADING DATA --------- ##
# We are provided 5 data file and 
# also "Data Dictionary.xlsx" file describing the attribute details also provided. 
 
# Loading the given 5 data file 
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("",NA))
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("",NA))
general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("",NA))
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("",NA))
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("",NA))



## -------- DATA UNDERSTADING -------- ##

# "employee_survey_data" data frame
str(employee_survey_data)
# 4410 observations and 4 variables
sum(duplicated(employee_survey_data))
# No duplicates found


# "manager_survey_data" data frame
str(manager_survey_data)
# 4410 observations and 3 variables
sum(duplicated(manager_survey_data))
# No duplicates found


# "general_data" data frame
str(general_data)
# 4410 observations and 3 variables
sum(duplicated(general_data))
# No duplicates found


# "in_time" data frame
str(in_time)
# 4410 observations and 262 variables
sum(duplicated(in_time))
# No duplicates found
# Variable names are names of dates on which data is collected
# data contains both date and time of punching of employees.
names(in_time)
# There are missing values in the data and are required to be checked.
# Data is given for year 2015 & from January to December.


# "out_time" data frame
str(out_time)
# 4410 observations and 262 variables
sum(duplicated(out_time))
# No duplicates found
# Variable names are names of dates on which data is collected
# data contains both date and time of punching of employees.
names(out_time)
# There are missing values in the data and are required to be checked.
# Data is given for year 2015 & from January to December.



## --------- DATA PREPARATION --------- ##
# The first column in in_time & out_time data-frames is actually "EmployeeID"
# So, renaming these columns in both data frames.
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"


# X is common in all column names and to make it easier while merging the in and out time DF
# we will add unique prefixes. 
names(in_time) <- str_replace(names(in_time),"X","IN_")
names(out_time) <- str_replace(names(out_time),"X","OUT_")



## --- Missing Value Identification & Treatment ---##

# employee_survey_data
table(is.na(employee_survey_data))
# 83 total missing values 
colSums(is.na(employee_survey_data))


# manager_survey_data
table(is.na(manager_survey_data))
#No missing values here 
colSums(is.na(manager_survey_data))


# general_data
table(is.na(general_data))
# 28 total missing values 
colSums(is.na(general_data))


# in_time
table(is.na(in_time))
# 109080 total missing values and this count is very high.
# "in_names" to store the sum of missing values in all individual columns of "in_time"
in_names <- colSums(is.na(in_time))
in_names

## >> HYPOTHESIS <<   for missing in time puch of employee
# There are some days having ~200 missing values while some days have time stamp missing for all employees.
# Days which have ~200 employees missing values may convey that employees might have forgot to punch on intime,
# or they could be on leave on that day. This needs to be analyzed further.
# But the days on which all the employees are missing time data suggests that it might be a holiday.

# Checking for columns having only Missing values
in_only_NA <- in_names[colSums(is.na(in_time))== nrow(in_time)]
in_only_NA
# Count is 12 days where all 4410 employees are missing the time data.
# These may be days of public holidays or weekends(Saturday or sunday).


# out_time
table(is.na(out_time))
# 109080 total missing values and this count is very high
# "out_names" to store the sum of missing values in all individual columns of "in_time"
out_names <- colSums(is.na(out_time))
out_names

## >> HYPOTHESIS <<   for missing out time puch of employee
# Similar to in_time data, there are some days having ~200 missing values while some days have time stamp missing for all employees.
# Days which have ~200 employees missing values may convey that employees might have forgot to punch on intime,
# or they could be on leave on that day. This needs to be analyzed further.
# But the days on which all the employees are missing time data suggests that it might be a holiday.

# Checking for columns having only Missing values
out_only_NA <- out_names[colSums(is.na(out_time))== nrow(out_time)]
out_only_NA 
# Count is 12 days where all 4410 employees are missing the time data.
# These may be days of public holidays or weekends(Saturday or sunday).



##-- Checking if both intime and outime have missing values on same date's --##

# Case 1 - When all the values in the columns are NA. 

table(out_only_NA==in_only_NA)
# We have 12 days when the out-time and in-time data is missing.
# And the data we have shows same dates for in and out times, where we have all missig values.  
# This confirms the holiday at workplace.


# Case #2 - when some (~200) values in various columns have NA values.
# Extracting dates when in-time taken
in_date <- substr(names(in_names[-1]),4,nchar("IN_2015.12.31"))
in_date

# Extracting dates when out-time taken
out_date <- substr(names(out_names[-1]),5,nchar("OUT_2015.12.31"))
out_date

# Checking if dates in both in and out time is same
table(out_date==in_date)


# Checking if missing data in both dataset's are at same place,
# if True, then employee should be on leave.
sum(is.na(in_time) != is.na(out_time))
# we got the result is 0. 
# means values missing at same place in in-time and out-time datasets
# which implies that the employee was on leave on that day.


# NOTE
#
# By this we have accounted for all the NA values in the in_time and out_time DF
# The NA values are either for the public holiday or in case of employee vacation.



##  Calculating Derived Data

# Derived Data #1 - Creating a new "hours_worked" to store the total hours worked per-day by the employee
hours_worked <- data.frame("EmployeeID"= in_time$EmployeeID)

for( i in 2:ncol(in_time))
{
  in_time[,i]<- as.POSIXct(in_time[,i], "%Y-%m-%d %H:%M:%S")
  out_time[,i]<- as.POSIXct(out_time[,i], "%Y-%m-%d %H:%M:%S")
  hours_worked[,i]<- round(out_time[,i]-in_time[,i],1)
  colnames(hours_worked)[i]<- colnames(out_time)[i]
}

# ignoring the "unknown timezone" warning as it is not impacting the current analysis. Here is the
# detail discussion and solution https://stackoverflow.com/questions/4047188/unknown-timezone-name-in-r-strptime-as-posixct


# Removing the "out_" prefix from the column names in hours_worked DF
names(hours_worked)[-1] <- str_sub(names(hours_worked[-1]),5,nchar("OUT_2015.12.31"))


# Changing data type from difftime to numeric type to do further arithmetic operations.  
hours_worked<- sapply(hours_worked, as.numeric)
hours_worked <- as.data.frame(hours_worked)


# Now hours_worked DF has date wise total hours worked in a day by each employee.
# Checking out new data frame "hours_worked"
str(hours_worked)
summary(hours_worked)
class(hours_worked)
dim(hours_worked)
# 4410 observations and 262 variables.



# Derived Data #2 -  New "derived_data" Derived Matrix from "hours_worked".

# Putting "EmployeeID" as its first column
derived_data <- data.frame("EmployeeID"= hours_worked$EmployeeID)

# 1. Calculating Total number of (leaves + holidays) taken by employees: "Total_day_off".
derived_data$Total_day_off <- rowSums(is.na(hours_worked))

# 2. Calculating Total Number of hours worked by employees : "Total_hrs"
derived_data$Total_hrs <- rowSums(hours_worked[-1] ,na.rm = TRUE)

# 3 . Average number of hours worked by employees : "Avg_hrs"
derived_data$Avg_hrs <- round(rowMeans(hours_worked[-1],na.rm = TRUE),2)

# 4. Calculating Extra hours worked by employees : "Extra_hrs". Assuming 8 hours work day.
derived_data$Extra_hrs <- rowSums(hours_worked[-1] - 8, na.rm = TRUE)

# 5. Calculating Average Extra hours worked : "Average_extra_hrs"
derived_data$Average_extra_hrs <-  round(rowMeans(hours_worked[-1] - 8,na.rm = TRUE),2)

str(derived_data)
dim(derived_data)
# "derived_data" has 4410 observations of employees & 6 variables. 

# Checking for the ID of employee in "EmployeeID".
n1 <- unique(employee_survey_data$EmployeeID)
n2 <- unique(manager_survey_data$EmployeeID)
n3 <- unique(general_data$EmployeeID)
n4 <- unique(in_time$EmployeeID)
n5 <- unique(out_time$EmployeeID)

unique(ifelse((n1 == n2) & (n2== n3) & (n3 == n4) & (n4==n5),print("Yes, EmployeeID is same in all") ,print("EmployeeID is not same in all") ))
# As the output is "Yes, EmployeeID is same in all" 
# We can now merge these data frames based on EmployeeID column.


##------- MERGING DATA ---------##

merged_data <- merge(general_data, employee_survey_data, by="EmployeeID", all = FALSE)
merged_data <- merge(merged_data, manager_survey_data, by="EmployeeID", all = FALSE)
merged_data <- merge(merged_data, derived_data, by="EmployeeID", all = FALSE)

# Putting "Attrition" in first column for easy analysis.
# Attrition :: Whether the employee left in the previous year or not

merged_data <- select(merged_data,Attrition, everything()) 

str(merged_data)
summary(merged_data)
dim(merged_data)
# 4410 Observations  and 34 Variables

table(duplicated(merged_data))
# No duplicate data present.


table(is.na(merged_data))
# Missing values found : their count is 111.



# checking missing values column wise
colSums(is.na(merged_data))
# count is : NumCompaniesWorked : 19, TotalWorkingYears : 09
# WorkLifeBalance : 38, JobSatisfaction : 20, EnvironmentSatisfaction : 25

# We can best deal with the missing values by imputing mean, median or mode in the 
# futher Exploratory Data Analysis of variables. 
# As, this can give best insight into variables distribution.  


## ----------------- Exploratory Data Analysis -------------- ##

## -------- UNIVARIATE - Bivariate ANALYSIS -------- ##
# ---------- Qualitative Variables ---------- #

##  Attrition 
ggplot(data = merged_data) +
  aes(x = Attrition, fill = Attrition) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_viridis_d(option  = "viridis") +
  labs(title = "Percentage of Attrition ", y= "Percentage") +
  theme_light()

table(merged_data$Attrition)
# 711 employees left in the previous year which 16.1 % and
# 3699 did not leave the company which is 83.9%.


## BusinessTravel
ggplot(data = merged_data) +
  aes(x = BusinessTravel, fill = BusinessTravel) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Percentage of Employees travelled for business purposes", y= "Percentage" ) +
  theme_light()

table(merged_data$BusinessTravel)
# Count is  NON-TRAVEL : 450 - 10.2% , TRAVEL_FREQUENTLY : 831 - 18.8%, TRAVEL_RARELY : 3129 - 71.0% 


# BusinessTravel v/s Attrition
ggplot(data = merged_data) +
  aes(x = BusinessTravel, fill = Attrition) +
  geom_bar() +
  theme_minimal()
#  Even though employee's who TRAVEL_RARELY have the highest attrition count
#  But % wise into individual category who TRAVEL_FREQUENTLY are most likely to leave the company.


## Department
ggplot(data = merged_data) +
  aes(x = Department, fill = Department) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Departments in Company",
       y = "Percentage") +
  theme_light()

table(merged_data$Department)
# percentage of Employess is very high in RESEARCH & DEVELOPMENT department which is 65.4% , 
# while others have 4.3% in HUMAN RESOURCES, 30.3% in SALES 

# Department v/s Attrition
ggplot(data = merged_data) +
  aes(x = Department, fill = Attrition) +
  geom_bar() +
  theme_minimal()
# Employees who are in RESEARCH & DEVELOPMENT have the highest attrition count,
# but employees who are in HUMAN RESOURCES have the highest attrition rate 57 out of 189 left.


# EducationField of employee's
ggplot(data = merged_data) +
  aes(x = EducationField, fill = EducationField) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Fields of Education",
       y = "Percentage") +
  theme_light()

table(merged_data$EducationField)
# percentage of employee's in "LIFE SCIENCES" is high 41.2% 
# While less employee's are having "HUMAN RESOURCES" education field that is 1.8% only. 

# EducationField v/s Attrition
ggplot(data = merged_data) +
  aes(x = EducationField, fill = Attrition) +
  geom_bar() +
  theme_minimal()
# Employees having Education Field of life sciences have the highest attrition count,
# But employees from HUMAN RESOURCES have highest attrition rate 40% (33 out of 81 left).


# Gender
ggplot(data = merged_data) +
  aes(x = Gender, fill = Gender) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Gender of employee",
       y = "Percentage") +
  theme_light()

table(merged_data$Gender)
# Percentage of employee's who are MALE is high : 60% 
# while, Female employees are 40% of the total company population. 


# Gender v/s Attrition
ggplot(data = merged_data) +
  aes(x = Gender, fill = Attrition) +
  geom_bar() +
  theme_minimal()
# Male employees have highest attrition count and rate (~16%) where 441 out of 2646 left.


# JobRole
ggplot(data = merged_data) +
  aes(x = JobRole, fill = JobRole) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Job role in Company",
       y = "Percentage") +
  theme_light()

table(merged_data$JobRole)
# Percentage of employee's is high for "SALES EXECUTIVE" : 22.2%  
# While very low for HUMAN RESOURCES : 3.5%. 

bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  legend.position="right")
# JobRole v/s Attrition
ggplot(data = merged_data) +
  aes(x = JobRole, fill = Attrition) +
  geom_bar() +
  bar_theme
#  SALES EXECUTIVE jobrole has the highest attrition count,
# but the RESEARCH Director jobrole has highest attrition rate (~24%) where 57 out of 240 left.



# MaritalStatus
ggplot(data = merged_data) +
  aes(x = MaritalStatus, fill = MaritalStatus) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Marital status of the employee",
       y = "Percentage") +
  theme_light()

table(merged_data$MaritalStatus)
# There are more percentage of married employees : 45.8%,
# percentage of SINGLE employees  : 32.0% ,percentage of DIVORCED employees : 22.2% 


# MaritalStatus v/s Attrition
ggplot(data = merged_data) +
  aes(x = MaritalStatus, fill = Attrition) +
  geom_bar() +
  theme_minimal()
# SINGLE employee have the highest attrition rate.



# Over18
ggplot(data = merged_data) +
  aes(x = Over18, fill = Over18) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Accent") +
  labs(title = "Fields of Education",
       y = "Percentage") +
  theme_light()

table(merged_data$Over18)
# All the employees are over 18
# As, all the values are same "Y", We can drop this Variable as it wont help in
# and only increasing the number of features which affects the performance of our model.

merged_data <- merged_data[-16]
dim(merged_data)# Now we have 33 variables



## -------- UNIVARIATE - Bivariate ANALYSIS -------- ##
# ---------- Quantitative Variables ---------- #
# Outlier treatment if found in analysis.

## Age - Attrition
ggplot(merged_data,aes(y = Age )) + geom_boxplot()

ggplot(data = merged_data) +
  aes(x = Attrition, y = Age) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Younger employees are more likely to leave the company



## DistanceFromHome - Attrition
boxplot(merged_data$DistanceFromHome)

ggplot(data = merged_data) +
  aes(x = Attrition, y = DistanceFromHome) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Almost same distribution for distance from home of employees. 



## Education - Attrition
boxplot(merged_data$Education)

ggplot(data = merged_data) +
  aes(x = Attrition, y = Education) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Almost same distribution for Education of employees. 



## EmployeeCount - Attrition
table(merged_data$EmployeeCount)
# Count is 1 for all. 


## JobLevel - Attrition
boxplot(merged_data$JobLevel)

ggplot(data = merged_data) +
  aes(x = Attrition, y = JobLevel) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Lower job level employees are more likely to leave the company, 
# with few employees working at high level 



## MonthlyIncome - Attrition

boxplot(merged_data$MonthlyIncome)
# Outliers are present in MonthlyIncome,  they are required to be treated

Q1 = quantile(merged_data$MonthlyIncome,probs = c(0.25))
Q3 = quantile(merged_data$MonthlyIncome,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$MonthlyIncome[which(merged_data$MonthlyIncome > Upper_outlier)] <- Upper_outlier


ggplot(data = merged_data) +
  aes(x = Attrition, y = MonthlyIncome) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Lower Monthly Income employees are more likely to leave the company, 
# with many employees working at high Monthly Income's. 


## NumCompaniesWorked - Attrition
table(is.na(merged_data$NumCompaniesWorked))
table(merged_data$NumCompaniesWorked)
# There are 19 missing values.
# So, Treating missing values by imputing the mode at missing locations
# Finding the Mode of NumCompaniesWorked variable
tmp <- table(merged_data$NumCompaniesWorked)

# Mode value
tmp[which.max(tmp)]

merged_data$NumCompaniesWorked[is.na(merged_data$NumCompaniesWorked)] <- 1

ggplot(merged_data,aes(x = as.factor(NumCompaniesWorked))) + geom_bar()

ggplot(merged_data,aes(x = as.factor(NumCompaniesWorked), fill = Attrition)) + geom_bar()+theme_light()
# employees with NumCompaniesWorked=1 have the higest attrition count,
# but emplyees with NumCompaniesWorked=5 have highest rate at ~26% with 46 leaving out of 187.


## PercentSalaryHike - Attrition
boxplot(merged_data$PercentSalaryHike)

ggplot(data = merged_data) +
  aes(x = Attrition, y = PercentSalaryHike) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Almost same distribution shown for Percent Salary Hike for employees with relation to attributes



## StandardHours - Attrition
table(merged_data$StandardHours)
# only single same value 8 for all employees, so no other pattern can be found from it.


## StockOptionLevel - Attrition
ggplot(data = merged_data,aes(x = StockOptionLevel, fill = Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  theme_light()

table(merged_data$StockOptionLevel)
# Employees with 0 have highest attrition count (318) and highest attrition rate ~17%. 



## TotalWorkingYears - Attrition
table(is.na(merged_data$TotalWorkingYears))


# Missing value treatment
# 9 Missing values present here.
# So, Treating missing values by imputing the mode at missing locations
# Finding the Mode of TotalWorkingYears variable
tmp <- table(merged_data$TotalWorkingYears)

# calculating Mode value
tmp[which.max(tmp)]

merged_data$TotalWorkingYears[is.na(merged_data$TotalWorkingYears)] <- 10

ggplot(merged_data,aes(x = TotalWorkingYears))+geom_histogram(fill = "#f1605d") + theme_light()

# Outlier treatment.
boxplot(merged_data$TotalWorkingYears)
# Outliers are present in TotalWorkingYears,  they are required to be treated

Q1 = quantile(merged_data$TotalWorkingYears,probs = c(0.25))
Q3 = quantile(merged_data$TotalWorkingYears,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$TotalWorkingYears[which(merged_data$TotalWorkingYears > Upper_outlier)] <- Upper_outlier


ggplot(data = merged_data) +
  aes(x = Attrition, y = TotalWorkingYears) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Employees with less than 7 years of Total Working Years are more likely to leave.



## TrainingTimesLastYear - Attrition
table(merged_data$TrainingTimesLastYear)

ggplot(merged_data,aes(x = TrainingTimesLastYear))+geom_bar(fill = "#f1605d") + theme_light()

ggplot(merged_data,aes(y = TrainingTimesLastYear)) + geom_boxplot()+theme_light()


ggplot(data = merged_data) +
  aes(x = Attrition, y = TrainingTimesLastYear) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Almost same distribution for both attributes. 



## YearsAtCompany - Attrition
table(merged_data$YearsAtCompany)

ggplot(merged_data,aes(x = YearsAtCompany))+geom_histogram(fill = "#f1605d") + theme_light()

# Outlier treatment.
boxplot(merged_data$YearsAtCompany)
# Outliers are present in YearsAtCompany,  they are required to be treated

Q1 = quantile(merged_data$YearsAtCompany,probs = c(0.25))
Q3 = quantile(merged_data$YearsAtCompany,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$YearsAtCompany[which(merged_data$YearsAtCompany > Upper_outlier)] <- Upper_outlier


ggplot(data = merged_data) +
  aes(x = Attrition, y = YearsAtCompany) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Employees who spent less than 6 years at the company are more likely to leave.



## YearsSinceLastPromotion - Attrition
table(merged_data$YearsSinceLastPromotion)

ggplot(merged_data,aes(x = YearsSinceLastPromotion))+geom_bar(fill = "#f1605d") + theme_light()

# Outlier treatment.
boxplot(merged_data$YearsSinceLastPromotion)
# Outliers are present in YearsAtCompany,  they are required to be treated

Q1 = quantile(merged_data$YearsSinceLastPromotion,probs = c(0.25))
Q3 = quantile(merged_data$YearsSinceLastPromotion,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$YearsSinceLastPromotion[which(merged_data$YearsSinceLastPromotion > Upper_outlier)] <- Upper_outlier


ggplot(data = merged_data) +
  aes(x = Attrition, y = YearsSinceLastPromotion) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Almost same distribution for both attritions.


## YearsWithCurrManager - Attrition
table(merged_data$YearsWithCurrManager)

ggplot(merged_data,aes(x = YearsWithCurrManager))+geom_bar(fill = "#f1605d") + theme_light()

# Outlier treatment.
boxplot(merged_data$YearsWithCurrManager)
# Outliers are present in YearsAtCompany,  they are required to be treated

Q1 = quantile(merged_data$YearsWithCurrManager,probs = c(0.25))
Q3 = quantile(merged_data$YearsWithCurrManager,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$YearsWithCurrManager[which(merged_data$YearsWithCurrManager > Upper_outlier)] <- Upper_outlier

ggplot(data = merged_data) +
  aes(x = Attrition, y = YearsWithCurrManager) +
  geom_boxplot(fill = "#fcffa4") +
  theme_light()
# Employees who spent less than 2 Years With Current Manager are more likely to leave company.


## EnvironmentSatisfaction - Attrition
table(merged_data$EnvironmentSatisfaction)
table(is.na(merged_data$EnvironmentSatisfaction))

# Missing value treatment
# 25 missing values found.
# So, Treating missing values by imputing the mode at missing locations
# Finding the Mode of EnvironmentSatisfaction variable
tmp <- table(merged_data$EnvironmentSatisfaction)

# calculating Mode value
tmp[which.max(tmp)]

merged_data$EnvironmentSatisfaction[is.na(merged_data$EnvironmentSatisfaction)] <- 3

ggplot(merged_data,aes(x = EnvironmentSatisfaction))+geom_bar(fill = "#f1605d") + theme_light()

ggplot(data = merged_data) +
  aes(x = EnvironmentSatisfaction, fill = Attrition) +
  geom_bar() +
  labs(title = "Environment Satisfaction level v/s Attrition") +
  theme_minimal()
# No significant pattern with respect to attrition.



## JobSatisfaction - Attrition
table(merged_data$JobSatisfaction)
table(is.na(merged_data$JobSatisfaction))
# 20 missing values found.
# So, Treating missing values by imputing the mode at missing locations
# Finding the Mode of JobSatisfaction variable
tmp <- table(merged_data$JobSatisfaction)

# calculating Mode value
tmp[which.max(tmp)]

merged_data$JobSatisfaction[is.na(merged_data$JobSatisfaction)] <- 4

ggplot(merged_data,aes(x = JobSatisfaction))+geom_bar(fill = "#f1605d") + theme_light()

ggplot(data = merged_data) +
  aes(x = JobSatisfaction, fill = Attrition) +
  geom_bar() +
  labs(title = "Job satisfaction Levels v/s Attrition") +
  theme_minimal()
# employees with high job satisfaction level "HIGH" or 3 have the highest attrition count 219,
# But employees with low Job Satisfaction levels "LOW" or 1 have the highest attrition rate ~23%.


## WorkLifeBalance - Attrition
table(merged_data$WorkLifeBalance)
table(is.na(merged_data$WorkLifeBalance))
# 38 missing values found.
# So, Treating missing values by imputing the mode at missing locations
# Finding the Mode of WorkLifeBalance variable
tmp <- table(merged_data$WorkLifeBalance)

# calculating Mode value
tmp[which.max(tmp)]

merged_data$WorkLifeBalance[is.na(merged_data$WorkLifeBalance)] <- 3

ggplot(merged_data,aes(x = WorkLifeBalance))+geom_bar(fill = "#f1605d") + theme_light()

ggplot(data = merged_data) +
  aes(x = WorkLifeBalance, fill = Attrition) +
  geom_bar() +
  labs(title = "Work Life Balance v/s Attrition") +
  theme_minimal()
# Employees with "Better" or 3 score for work life balance are more likely to leave company,



## JobInvolvement - Attrition
table(merged_data$JobInvolvement)

ggplot(merged_data,aes(x = JobInvolvement))+geom_bar(fill = "#f1605d") + theme_light()

ggplot(data = merged_data) +
  aes(x = JobInvolvement, fill = Attrition) +
  geom_bar() +
  labs(title = "Job Involvement v/s Attrition") +
  theme_minimal()
# Employees with "HIGH" or 3 score for Job Involvement are more likely to leave company,

## PerformanceRating - Attrition
table(merged_data$PerformanceRating)

ggplot(merged_data,aes(x = PerformanceRating))+geom_bar(fill = "#f1605d") + theme_light()

ggplot(data = merged_data) +
  aes(x = PerformanceRating, fill = Attrition) +
  geom_bar() +
  labs(title = "Performance Rating v/s Attrition") +
  theme_minimal()
# Employees with "EXCELLENT" or 3 score for Performance Rating are more likely to leave company,




## Total_day_off - Attrition
table(merged_data$Total_day_off)

ggplot(merged_data,aes(x = Total_day_off))+geom_bar(fill = "#f1605d") + theme_light()

ggplot(merged_data,aes(y = Total_day_off)) + geom_boxplot()+theme_light()

ggplot(data = merged_data) +
  aes(x = Attrition, y = Total_day_off) +
  geom_boxplot(fill = "#a6cee3") +
  theme_light()
# Employees who are taking less than 25 leaves are more likely to leave company,
# but that median is close to the median values for those who are not leaving.



## Total_hrs - Attrition
ggplot(merged_data,aes(x = Total_hrs))+geom_histogram(fill = "#f1605d") + theme_light()

# Outlier treatment.
boxplot(merged_data$Total_hrs)
# Outliers are present in Total_hrs,  they are required to be treated

Q1 = quantile(merged_data$Total_hrs,probs = c(0.25))
Q3 = quantile(merged_data$Total_hrs,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$Total_hrs[which(merged_data$Total_hrs > Upper_outlier)] <- Upper_outlier


ggplot(data = merged_data) +
  aes(x = Attrition, y = Total_hrs) +
  geom_boxplot(fill = "#a6cee3") +
  theme_light()
# median is high for Employees who have left the company,
# as compared to median values for those who are not leaving.
# Employess working less hours are less likely to leave.


## Avg_hrs - Attrition
ggplot(merged_data,aes(x = Avg_hrs))+geom_histogram(fill = "#f1605d") + theme_light()

# Outlier treatment.
boxplot(merged_data$Avg_hrs)
# Outliers are present in Avg_hrs,  they are required to be treated

Q1 = quantile(merged_data$Avg_hrs,probs = c(0.25))
Q3 = quantile(merged_data$Avg_hrs,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$Avg_hrs[which(merged_data$Avg_hrs > Upper_outlier)] <- Upper_outlier


ggplot(data = merged_data) +
  aes(x = Attrition, y = Avg_hrs) +
  geom_boxplot(fill = "#a6cee3") +
  theme_light()
# employees working more than average 8 hours are more likely to leave company,


## Extra_hrs - Attrition
ggplot(merged_data,aes(x = Extra_hrs))+geom_histogram(fill = "#f1605d") + theme_light()

# Outlier treatment.
boxplot(merged_data$Extra_hrs)
# Outliers are present in Extra_hrs,  they are required to be treated

Q1 = quantile(merged_data$Extra_hrs,probs = c(0.25))
Q3 = quantile(merged_data$Extra_hrs,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$Extra_hrs[which(merged_data$Extra_hrs > Upper_outlier)] <- Upper_outlier

ggplot(data = merged_data) +
  aes(x = Attrition, y = Extra_hrs) +
  geom_boxplot(fill = "#a6cee3") +
  theme_light()
# employees working more extra hours are more likely to leave company,
# employees working very less extra hours are less likely to leave company,



## Average_extra_hrs - Attrition
ggplot(merged_data,aes(x = Average_extra_hrs))+geom_histogram(fill = "#f1605d") + theme_light()


# Outlier treatment.
boxplot(merged_data$Average_extra_hrs)
# Outliers are present in Average_extra_hrs,  they are required to be treated

Q1 = quantile(merged_data$Average_extra_hrs,probs = c(0.25))
Q3 = quantile(merged_data$Average_extra_hrs,probs = c(0.75))
IQR = Q3 - Q1

Upper_outlier = Q3 + (IQR * 1.5)

merged_data$Average_extra_hrs[which(merged_data$Average_extra_hrs > Upper_outlier)] <- Upper_outlier

ggplot(data = merged_data) +
  aes(x = Attrition, y = Average_extra_hrs) +
  geom_boxplot(fill = "#a6cee3") +
  theme_light()
# employees working average extra hours are more likely to leave company,



##---------- Multivariate analysis
##---------- Correlation plot 


##-------- Data Pre-Preparation-------##
 
# Checking for missing data
colSums(is.na(merged_data))
# No Missing data found.

# Check data type of all variables
str(merged_data)
# All variables have correct data types. 

# Check for variables having single unique value only.
unq <- rapply(merged_data,function(x)length(unique(x)))

unq[(unq==1)==TRUE]

# Two variables found "EmployeeCount" and "StandardHours ", so dropping these variables
merged_data <- merged_data[-c(which(unq==1))]


length(unique(merged_data$EmployeeID)) == nrow(merged_data)

# result is TRUE, so dropping this variable 
merged_data <- merged_data[-c(2)]

# Check dimensions of final data set 
dim(merged_data)
# We have 4410 observations and 30 variables left for model building.



## ------ Creating Dummy variables ( Categorical Variables ) ------ ##

# Check for character varibale on data set
chr <- rapply(merged_data,function(x) is.character(x))
which(chr)
# There are 7 character variables which are required to be converted to dummy variables for model
# Attrition, BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus.

dummy_var <- data.frame(model.matrix( ~ Attrition+BusinessTravel+Department+EducationField+Gender+JobRole+ MaritalStatus, data = merged_data))

# Excluding the first column which contains only number of rows
dummy_var <- dummy_var[-1]


# changing name from "AttritionYES"  to "Attrition"
names(dummy_var)[1] <- "Attrition"

# Checking the class of "dummy_var" 
class(dummy_var)
dim(dummy_var)
##  Final, dummy_var contains 4410 observations and 21 variables


# Creating final version of the data frame before modelling.
# "attrition_data" is final data frame after adding dummy variable and removing character variables 

attrition_data <- merged_data[-c(which(chr))]
attrition_data <- cbind(attrition_data,dummy_var) 

dim(attrition_data)
# The car_price_final data frame has 4410 observations and 44 varibales.

str(attrition_data)


##--------- MODEL BUILDING ----------##

set.seed(100)

# Dividing data randomly from "attrition_data" data frame into "train" and "test" data frames.

# train_index is randomly selected data for "train" data frame
# making seperate train and test data frames from  "attrition_data"
train_index = sample(1:nrow(attrition_data), 0.7*nrow(attrition_data))

train = attrition_data[train_index,]
test = attrition_data[-train_index,]

str(train)
str(test)
# "train" data frame contains 3087 observations and "test" data frame contains 1323 observations

# Making copy for "train" and "test" data.
training_set <- train
test_set <- test


# SCALE the data. so, that all the variables lie in the same range.
training_set[1:23] <- scale(training_set[1:23])
test_set[1:23] <- scale(test_set[1:23])


class(training_set)
# "data.frame" type


### ------------- Starting model building phase.  ---------------###

# Model #1
model_1 <- glm(Attrition~., data = training_set, family = "binomial")

# Check the summary of model_1. 
summary(model_1)

# Null deviance: 2747.7  on 3086  degrees of freedom
# Residual deviance: 2088.9  on 3043  degrees of freedom
# AIC: 2176.9
 
## Using StepAIC to select important features ##
step <- stepAIC(model_1, direction="both")

# So, many iterations have been done through the stepwise command. 
# Now we need to know our model further.

# the Step command here to see the final sorted model. 
step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignificant variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

# Degrees of Freedom: 3086 Total (i.e. Null);  3061 Residual
# Null Deviance:	    2748 
# Residual Deviance: 2102 	AIC: 2154

model_2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
      YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
      WorkLifeBalance + JobInvolvement + Total_day_off + 
        Total_hrs + BusinessTravelTravel_Frequently       + 
        BusinessTravelTravel_Rarely   + EducationFieldLife.Sciences + EducationFieldMarketing + 
        EducationFieldMedical + EducationFieldOther + 
        EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
        JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle, family = "binomial", 
      data = training_set)


# Check the summary of model_2
summary(model_2)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_2)

# Null deviance: 2747.7  on 3086  degrees of freedom
# Residual deviance: 2091.3  on 3062  degrees of freedom
# AIC: 2141.3

# Checking for variables with high VIF (VIF > 2) and high p-values > 0.05.

# MaritalStatusMARRIED : vif is 2.172776, p-values is 0.098096

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 BusinessTravelTravel_Rarely   + EducationFieldLife.Sciences + EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_3
summary(model_3)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_3)

# Null deviance: 2747.7  on 3086  degrees of freedom
# Residual deviance: 2094.9  on 3063  degrees of freedom
# AIC: 2154.9

# Checking for variables with high VIF (VIF > 2) and high p-values > 0.05.

# BusinessTravelTRAVEL_RARELY : 4.854123 VIF and p-value is	0.000364
# Removing this in model_4

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 EducationFieldLife.Sciences + EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_4
summary(model_4)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_4)

# EducationFieldOTHER : 2.712951 VIF and p-value : 7.52e-06
# Removing this in model_5

model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 EducationFieldLife.Sciences + EducationFieldMarketing + 
                 EducationFieldMedical + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_5
summary(model_5)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_5)

# EducationFieldLIFE.SCIENCES :  VIF 3.152306 and p-value : 0.087889 
# Removing this in model_6


model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 EducationFieldMarketing + 
                 EducationFieldMedical + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_6
summary(model_6)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_6)

# Now we have checked for all high vif and high p-value variables,
# Next we can look for eliminating the high p-values only, 
# as they are the insignificant variables in the model which are affecting the accuracy of our model. 

# removing EducationFieldMARKETING p-value : 0.173270 in model_7


model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 EducationFieldMedical + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_7
summary(model_7)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_7)

# removing EducationFieldMEDICAL p-value : 0.209344 in model_8


model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_8
summary(model_8)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_8)

# removing JobRoleSales.Executive p-value : 0.10876 in model_9

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_9
summary(model_9)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_9)

# removing EducationFieldTECHNICAL.DEGREE p-value : 0.099713 in model_10

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + Total_day_off + 
                 Total_hrs + BusinessTravelTravel_Frequently       + 
                 JobRoleManager + JobRoleManufacturing.Director + 
                 MaritalStatusSingle, family = "binomial", 
               data = training_set)

# Check the summary of model_10
summary(model_10)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_10)

# removing StockOptionLevel p-value : 0.106966 in model_11


model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + JobInvolvement + Total_day_off + 
                  Total_hrs + BusinessTravelTravel_Frequently       + 
                  JobRoleManager + JobRoleManufacturing.Director + 
                  MaritalStatusSingle, family = "binomial", 
                data = training_set)

# Check the summary of model_11
summary(model_11)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_11)

# removing JobInvolvement p-value : 0.079602 in model_12

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Total_day_off + 
                  Total_hrs + BusinessTravelTravel_Frequently       + 
                  JobRoleManager + JobRoleManufacturing.Director + 
                  MaritalStatusSingle, family = "binomial", 
                data = training_set)

# Check the summary of model_12
summary(model_12)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_12)

# removing Total_day_off p-value : 0.029081 in model_13

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Total_hrs + BusinessTravelTravel_Frequently + 
                  JobRoleManager + JobRoleManufacturing.Director + 
                  MaritalStatusSingle, family = "binomial", 
                data = training_set)

# Check the summary of model_13
summary(model_13)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_13)


# removing JobRoleManager p-value : 0.024988 in model_14

model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Total_hrs + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle, family = "binomial", 
                data = training_set)

# Check the summary of model_13
summary(model_14)

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
vif(model_14)


## This is the stage where we have all three stars (***) highly significant variables,
# but also we have two variables "TotalWorkingYears" with vif : 2.311958 and low p-values
# & "Age" with 1.847288 VIF and low p-values

# Checking their corrleation
cor(training_set$TotalWorkingYears,training_set$Age)
# 0.68 correlation between them found which is very high.
# Now, we can check models by removing one of them & 
# checking removing which one is more affecting others VIF value,
# As it will show


## Now trying out model by removing : Age

remove_Age <- glm(formula = Attrition ~ NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Total_hrs + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle, family = "binomial", 
                data = training_set)

# Check the summary of remove_Age
summary(remove_Age)

# Let us check for multicollinearity.  
vif(remove_Age)


## Now trying out model by removing : TotalWorkingYears

remove_TotalWorkingYears <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Total_hrs + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle, family = "binomial", 
                data = training_set)


# Check the summary of model_15
summary(remove_TotalWorkingYears)

# Let us check for multicollinearity.  
vif(remove_TotalWorkingYears)

# After checking these two Models : "remove_Age" and "remove_TotalWorkingYears"
# decrease in TotalWorkingYears is 0.650278 : 2.164822 - 1.514544 
# decrease in Age is 0.537453 : 1.855543 - 1.318090
# So, by removing Age is causing more decrease in VIF of TotalWorkingYears.

# Also we should notice here that :
# AIC is 2213.3 when Age is removed < AIC is 2216.5 when TotalWorkingYears is removed,
# and we always prefer model with low AIC value.
# Therefore, we can go forward with "remove_Age" model and hence, removing "Age" variable,

# Now we got our final model "remove_Age".
# We are having 12 significant variables in this model.
Final_model <- remove_Age

which(names(test_set)=="Attrition")
# in test dataset location for our target variable "Attrition" is 24.


##--------- Prediction on test set ----------##

pred_test.set  <- predict(Final_model, type = "response", newdata = test_set[,-24])

# Let's see the summary 
summary(pred_test.set)

anova(Final_model, test="Chisq")


test$prob <- pred_test.set
#View(test)

# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(pred_test.set >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test_set$Attrition==1,"Yes","No"))

table(test_pred_attrition,test_actual_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")  
test_conf
#Sensitivity = 0.27053
#Specificity = 0.97939


# Here, in "pred_test.set" R will output probabilities in the form of P(y=1|X). 
# Our decision boundary will be 0.5. If P(y=1|X) > 0.5 then y = 1 otherwise y=0. 
fitted.results <- ifelse(pred_test.set > 0.5,1,0)

misClasificError <- mean(fitted.results != test_set$Attrition)
print(paste('Accuracy',1-misClasificError))
# The 0.8684 ~ 86.84 %  accuracy on the test set.


##
test_pred <- factor(ifelse(pred_test.set >= 0.40, "Yes", "No"))


test_conf <- confusionMatrix(test_pred, test_actual_attrition, positive = "Yes")
test_conf

fitted.results <- ifelse(pred_test.set > 0.4,1,0)

misClasificError <- mean(fitted.results != test_set$Attrition)
print(paste('Accuracy',1-misClasificError))
# The 0.85 ~ 85 %  accuracy on the test set, which is smaller than our 0.5 cutoff value.
# 


#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred_test.set >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability

summary(pred_test.set)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


##
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

# Let's choose a cutoff value of 0.18 for final model

test_cutoff_attrition <- factor(ifelse(pred_test.set >= 0.18 , "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")


acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#View(test)


#Using McFadden's R2 [psedo R^2] to check predictive power

pR2(Final_model)
#McFadden value = 0.2

#Checking for Variable Importance [t-statistics]
varImp(Final_model)

##ROCR Curve
ROCRpred <- prediction(pred_test.set,test_set$Attrition)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
