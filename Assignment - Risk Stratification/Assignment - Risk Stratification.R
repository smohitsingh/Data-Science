

# $----$  # ----------- ASSIGNMENT - RISK STRATIFICATION -------------# $----$  #


##----- Business Understanding -----##

# A care management organisation called "WeCare" wants to identify among its diabetic patients, 
# the ones that are at high risk of getting re-admitted to the hospital. 
# They wish to intervene by providing some incentive to these patients that will help them improve their health. 


##----- Problem Statement -----##

# To identify high-risk diabetic patients through risk stratification,
# which will help the payer to decide what are the right intervention programs for these patients.


##----- Goal of Analysis -----##

# I. Data preparation ::
#        1. Remove redundant variables.
#        2. Check for missing values and treat them accordingly.
#        3. Scale numeric attributes and create dummy variables for categorical ones.
#        4. Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".
#        5. Create the derived metric 'comorbidity', according to the following scheme -

# II. Data Exploration ::
#        1. Perform basic data exploration for some categorical attributes.
#        2. Perform basic data exploration for some numerical attributes.

# III. Model Building ::
#        1.  Divide your data into training and testing dataset
#        2.  Compare the performance of at least two algorithms and decide which one to use for predicting risk of readmission for the patient
#        3.  Stratify your population into 3 risk buckets:
#                -- High risk (Probability of readmission > 0.7)
#                -- Medium risk (0.3 < Probability of readmission < 0.7)
#                -- Low risk (Probability of readmission < 0.3)



### ------- Installing & Loading Required Packages --------- ##

# call the libraries

library(dplyr)    #  data manipulation
library(ggplot2)  #  visualisation plots
library(tidyr)    #  data manipulation
library("MASS")   #  StepAIC
library("car")    #  VIF
library(caret)    #  Confusion Matrix.
library(randomForest) # Random Forest Model
library(e1071)

# Removing the used variables
remove(list = ls())
ls()      # Above command is executed sucessfully


## ------- LOADING DATA --------- ##
setwd("C:/Users/Mohit Singh/Desktop/Assignment")

getwd()
# So we checked, Directory Added successfully


# Checking for the target file's in the directory added
dir()   
# We are provided with 2 CSV file's "diabetic_data_dictionary.csv" which is data dictionary file and also "diabetic_data.csv" Main Data file.


# Now, Let's load the given Main data file, putting blank "NA" where ever we have missing data. 
diabetic_data_df1 <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("",NA))


## -------- DATA UNDERSTANDING -------- ##

# checking "diabetic_data_df1" data frame variables and their data distribution.
str(diabetic_data_df1)
# Found "?" character present in race, weight, payer_code, medical_specialty, diag_1, diag_2, diag_3 variables.

summary(diabetic_data_df1) 
# There are 1,01,766 observations and 50 variables in the given data frame.

table(duplicated(diabetic_data_df1))
# Hence, no duplicated present

# In data we have "encounter_id", lets check if contains any duplicated "encounter_id" of any patient, 
# duplicated "encounter_id" is not posssible because each registered case is given unique "encounter_id" each time for 1,01,766 patients.
table(duplicated(diabetic_data_df1$encounter_id))
# Hence, no duplicated "encounter_id" found in given data frame.



## --------- DATA PREPARATION --------- ##

## --- Missing Value Identification & Treatment ---##
sort(colSums(is.na(diabetic_data_df1)))
# Hence no Missing data found.

# As, we looked into the data and we found no missing value in fields directly because of which we used na.strings = c("",NA) command,
# So as to represent all missing data with "NA",but Now It seems that at the data collection / entry end, 
# there is possibility that the missing data is being represented as "?" , 
# So we can again load our data frame "diabetic_data_df" and replacing "?" with "NA". 

diabetic_data_df <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE,header = TRUE, na.strings = c("?",NA))

# Again checking our given loaded "diabetic_data_df" data frame,
head(diabetic_data_df)

# checking "diabetic_data_df" data frame variables and their data distribution.
str(diabetic_data_df)

summary(diabetic_data_df) 
# There are 1,01,766 observations and 50 variables in the given data frame.

table(duplicated(diabetic_data_df))
# Hence, no duplicated present

table(is.na(diabetic_data_df))
# Hence, 192849 missing data is present,

# Converting character type columns to Upper case for data uniformity and hence remove inconsistensies,
diabetic_data_df <- data.frame(lapply(diabetic_data_df, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


#----- 1.  Remove redundant variables.

# 24 features for medications, SO checking the pattern of the data of each medications

table(diabetic_data_df$metformin)
# DOWN   NO     STEADY     UP 
# 575   81778   18346     1067 

table(diabetic_data_df$repaglinide)
# Down   No     Steady     Up 
# 45    100227   1384     110 


table(diabetic_data_df$nateglinide)
# DOWN   NO     STEADY     UP 
# 11    101063    668     24


table(diabetic_data_df$chlorpropamide)
# DOWN     NO    STEADY     UP 
# 1     101680     79      6 
table(diabetic_data_df$glimepiride)
# DOWN     NO   STEADY     UP 
# 194    96575   4670    327 

table(diabetic_data_df$acetohexamide)
# NO        STEADY 
# 101765      1 

table(diabetic_data_df$glipizide)
# DOWN     NO    STEADY     UP 
# 560    89080   11356     770 

table(diabetic_data_df$glyburide)
# DOWN     NO    STEADY     UP 
# 564    91116    9274     812 

table(diabetic_data_df$tolbutamide)
# NO       STEADY 
# 101743     23 

table(diabetic_data_df$pioglitazone)
# DOWN     NO    STEADY     UP 
# 118     94438   6976    234 

table(diabetic_data_df$rosiglitazone)
# DOWN    NO   STEADY     UP 
# 87    95401   6100    178 

table(diabetic_data_df$acarbose)
# DOWN   NO     STEADY     UP 
# 3     101458    295     10 

table(diabetic_data_df$miglitol)
# DOWN   NO     STEADY     UP 
# 5     101728     31      2 

table(diabetic_data_df$troglitazone)
# NO        STEADY 
# 101763      3 

table(diabetic_data_df$tolazamide)
# NO       STEADY     UP 
# 101727     38      1 

table(diabetic_data_df$examide)
# NO 
# 101766 

table(diabetic_data_df$citoglipton)
# NO 
# 101766 

table(diabetic_data_df$insulin)
# DOWN     NO     STEADY     UP 
# 12218   47383   30849    11316

table(diabetic_data_df$glyburide.metformin)
# DOWN     NO STEADY     UP 
# 6 101060    692      8 

table(diabetic_data_df$glipizide.metformin)
# NO        STEADY 
# 101753     13 

table(diabetic_data_df$glimepiride.pioglitazone)
# NO        STEADY 
# 101765      1 

table(diabetic_data_df$metformin.rosiglitazone)
# NO        STEADY 
# 101764      2 

table(diabetic_data_df$metformin.pioglitazone)
# NO       STEADY 
# 101765      1 


# From above data it is clear only insulin has distributed data, others are having values only for one respective class
# Hence, we can remove all columns except insulin

# also below columns are having data particularly for single value and we can remove those

round(prop.table(table(diabetic_data_df$max_glu_serum)),4)*100
# 200  >300  None  Norm 
# 1.46  1.24 94.75  2.55 

round(prop.table(table(diabetic_data_df$A1Cresult)),4)*100
# >7    >8  None  Norm 
# 3.75  8.07 83.28  4.90 

# unique values in "encounter_id" variable 
length(unique(diabetic_data_df$encounter_id)) == nrow(diabetic_data_df)
# Hence in data frame we have "encounter_id" which has all unique records in it, 
# this can be treated as redundant variable for our analysis, for analysis and model building part it cannot be used.   

# unique values in "patient_nbr" variable 
length(unique(diabetic_data_df$patient_nbr))
# "patient_nbr" has 30248 duplicated patient number as any patient can visit more than once, so we can remove it as each row represent each entry details.


# defining a "drop_redund.var" for a list of redundant variables
drop_redund.var <- c( "patient_nbr" , "encounter_id", "metformin", "repaglinide", "nateglinide", "chlorpropamide", "glimepiride", 
                      "acetohexamide", "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone", 
                      "acarbose", "miglitol", "troglitazone", "tolazamide", "examide", "citoglipton", "glyburide.metformin", 
                      "glipizide.metformin", "glimepiride.pioglitazone", "metformin.rosiglitazone", "metformin.pioglitazone", 
                      "max_glu_serum", "A1Cresult")

# As we have seen above that columns are having same data or unwanted data hence we can remove redundant columns
diabetic_data_df <- diabetic_data_df[,-match(c(drop_redund.var),names(diabetic_data_df))]

# Now, Checking for the changes made in "diabetic_data_df"
dim(diabetic_data_df)
# 101766 observations and  24 variables. Hence, changes done sucessfully


#----- 2. Check for missing values and treat them accordingly. 

# Finding out missing value distribution in each column,
na_count <- sort(colSums(is.na(diabetic_data_df)))
na_count 
# So the below mentioned variables have respective missing value count,
# diag_1 : 21, diag_2 :  358, diag_3 : 1423, race : 2273, payer_code : 40256, medical_specialty : 49949, weight : 98569. 

round((na_count[which(names(na_count) == "diag_1")]/nrow(diabetic_data_df))*100,3)
# "diag_1" has 0.021 % missing data. 

round((na_count[which(names(na_count) == "diag_2")]/nrow(diabetic_data_df))*100,3)
# "diag_2" has 0.352 % missing data.

round((na_count[which(names(na_count) == "diag_3")]/nrow(diabetic_data_df))*100,3)
# "diag_3" has 1.398 % missing data.

round((na_count[which(names(na_count) == "race")]/nrow(diabetic_data_df))*100,3)
# "race" has 2.234 % missing data.

round((na_count[which(names(na_count) == "payer_code")]/nrow(diabetic_data_df))*100,3)
# "payer_code" has 39.557 % missing data.

round((na_count[which(names(na_count) == "medical_specialty")]/nrow(diabetic_data_df))*100,3)
# "medical_specialty" has 49.082 % missing data.

round((na_count[which(names(na_count) == "weight")]/nrow(diabetic_data_df))*100,3)
# "weight" has 96.858 % missing data.

# Above, we checked the percentage missing data in each column,
# and found that "payer_code", "medical_specialty" and "weight" variables have higher percentage of missing data, 
# higher enough that we cannot fill them by any means, if done so will create those variables biased.
# hence we can remove these variables.

# Also, lets create our final data frame "diabetic_final.df" as we have removed variables with unwanted data / higher missing value percentage till now. 
diabetic_final.df <- diabetic_data_df[,-match(c("payer_code", "medical_specialty", "weight"),names(diabetic_data_df))]

# Now, Checking for the changes made in "diabetic_final.df"
dim(diabetic_final.df)
# 101766 observations and  21 variables, hence, changes done sucessfully


# Above we can see that the percentage of Missing values in  diag_1, diag_2, diag_3, race variables is only between 0.021% to 2.234% of the total data only, 
# Hence, such missing data removal will not affect our amount of data available for analysis, 
diabetic_final.df <- diabetic_final.df[which(diabetic_final.df$diag_1 != "NA"),]
dim(diabetic_final.df)
# 101745 observations and  21 variables

diabetic_final.df <- diabetic_final.df[which(diabetic_final.df$diag_2 != "NA"),]
dim(diabetic_final.df)
# 101388 observations and  21 variables, 

diabetic_final.df <- diabetic_final.df[which(diabetic_final.df$diag_3 != "NA"),]
dim(diabetic_final.df)
# 100244 observations and  21 variables, 

diabetic_final.df <- diabetic_final.df[which(diabetic_final.df$race != "NA"),]
dim(diabetic_final.df)
# 98053 observations and  21 variables, 

# Also we found "UNKNOWN/INVALID" in data summary in "Gender" variable, lets check its distribution in "gender",
table(diabetic_final.df$gender)
# Female     Male    Unknown/Invalid 
# 52833      45219      1 


# As the number of records with "Unknown/Invalid" gender value is only 1 (very negligible count), 
# we can remove such records, as it would not affect the amount of data we have for further analysis,

diabetic_final.df <- diabetic_final.df[which(diabetic_final.df$gender != "UNKNOWN/INVALID"),]
# Now, Checking for the changes made in "diabetic_final.df"
dim(diabetic_final.df)
# 98052 observations and  21 variables. hence, changes done sucessfully.


#----- 4. Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".

diabetic_final.df$readmitted <- ifelse(diabetic_final.df$readmitted %in% c("<30",">30"),'YES','NO')
unique(diabetic_final.df$readmitted)
# "YES" "NO" 
class(diabetic_final.df$readmitted)


#----- 5. Create the derived metric 'comorbidity', according to the following scheme :

diabetic_final.df$comor1 <- 
  1*( 1*((as.numeric(diabetic_final.df$diag_1) > 250) &  (as.numeric(diabetic_final.df$diag_1) < 251)) 
    |
      1*((as.numeric(diabetic_final.df$diag_2) > 250) &  (as.numeric(diabetic_final.df$diag_2) < 251))
    |
      1*((as.numeric(diabetic_final.df$diag_3) > 250) &  (as.numeric(diabetic_final.df$diag_3) < 251))
  )


diabetic_final.df$comor1[is.na(diabetic_final.df$Comor1)] <- 0

diabetic_final.df$comor2 <-
  1*(
    1*((as.numeric(diabetic_final.df$diag_1) > 389) &  (as.numeric(diabetic_final.df$diag_1) < 460)) 
    |
      1*((as.numeric(diabetic_final.df$diag_2) > 389) &  (as.numeric(diabetic_final.df$diag_2) < 460))
    |
      1*((as.numeric(diabetic_final.df$diag_3) > 389) &  (as.numeric(diabetic_final.df$diag_3) < 460))
  )

diabetic_final.df$comor2[is.na(diabetic_final.df$comor2)] <- 0

str(diabetic_final.df)


diabetic_final.df$comorbidity <- ifelse(diabetic_final.df$comor1 ==0 & diabetic_final.df$comor2 == 0, 0, 
                                             ifelse(diabetic_final.df$comor1 == 1 & diabetic_final.df$comor2 == 0, 1, 
                                                    ifelse(diabetic_final.df$comor1 ==0 & diabetic_final.df$comor2 == 1, 2, 3))
)


# Now, remove columns diag_1, diag_2, diag_3, Comm1 & Comm2
diabetic_final.df <- diabetic_final.df[,-which(names(diabetic_final.df) == "diag_1")]
diabetic_final.df <- diabetic_final.df[,-which(names(diabetic_final.df) == "diag_2")]
diabetic_final.df <- diabetic_final.df[,-which(names(diabetic_final.df) == "diag_3")]
diabetic_final.df <- diabetic_final.df[,-which(names(diabetic_final.df) == "comor1")]
diabetic_final.df <- diabetic_final.df[,-which(names(diabetic_final.df) == "comor2")]

dim(diabetic_final.df)
# 98052 observations and 19 variables. hence, changes done sucessfully.
# Checking our Final Variable names in Data Frame.
names(diabetic_final.df)


## --------- DATA EXPLORATION --------- ##

str(diabetic_final.df)
# lets, define the class types of final Variables we got in our data frame "diabetic_final.df", for clarity in Data Exploration task.

# So, lets create a "factor_var", which store int type variables,
factor_var <- rapply(diabetic_final.df,function(x) is.factor(x))
which(factor_var)
# race, gender, age, insulin, change,  diabetesMed, readmitted : these are character data type columns in our data frame.
# But except "readmitted" all others are of factor data type, these are to be converted to character data type.


# Lets convert Integer variables to Numerical data type.
diabetic_final.df[which(factor_var == TRUE)] <- sapply(diabetic_final.df[which(factor_var==TRUE)] , as.character)
str(diabetic_final.df)
dim(diabetic_final.df)
# 98052 observations and  19 variables, hence changes made sucessfully.

str(diabetic_final.df)
# Here we noticed that there is a mix of numerical and integer data type, 
# So, for data uniformity lets convert the integer variables to Numerical data type. 

# So, lets create a "int_var", which store int type variables,
int_var <- rapply(diabetic_final.df,function(x) is.integer(x))
which(int_var)
#  "admission_type_id", "discharge_disposition_id", "admission_source_id", "time_in_hospital" 
# 'num_lab_procedures', "num_procedures" ,  "num_medications", "number_outpatient" 
# 'number_emergency', 'number_inpatient' ,  'number_diagnoses' 

# Lets convert Integer variables to Numerical data type.
diabetic_final.df[which(int_var == TRUE)] <- sapply(diabetic_final.df[which(int_var ==TRUE)] , as.numeric)
str(diabetic_final.df)
dim(diabetic_final.df)
# 98052 observations and  19 variables, hence changes made sucessfully and our data type is uniform now


##----  Lets start work for our given tast :; 

#----- Perform Basic Data Exploration for some Categorical attributes.

# lets, check all character variables for our analysis "char.var",
char.var <- rapply(diabetic_final.df,function(x) is.character(x))
which(char.var==TRUE)
#  race, gender, age, insulin, change, diabetesMed, readmitted are 7 Character Variable's.  


## 1. "Race"
ggplot(data = diabetic_final.df) +
  aes(x = race, fill = race) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Percentage Race distribution",
       x = "Race",
       y = "percentage distribution") +
  theme_light()


round(prop.table(table(diabetic_final.df$race)),4)*100
#  "CAUCASIAN" race has 76.6% data distribution, while "ASIAN" race has lowest 0.6 % data distribution.


##  Race v/s Readmitted
ggplot(data = diabetic_final.df) +
  aes(x = race, fill = readmitted) +
  geom_bar() +
  labs(title = "Race v/s Readmitted ",
    x = "Race",
    y = "percentage distribution") +
  theme_minimal()

# all race classes have almost same readmitted rates. 


## 2. "Gender"
ggplot(data = diabetic_final.df) +
  aes(x = gender, fill = gender) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Percentage Gender distribution",
       x = "Gender",
       y = "percentage distribution") +
  theme_light()


round(prop.table(table(diabetic_final.df$gender)),4)*100
# data distribution is more 53.88 % for females and 46.12% for male.


##  Gender v/s Readmitted
ggplot(data = diabetic_final.df) +
  aes(x = gender, fill = readmitted) +
  geom_bar() +
  labs(title = "Gender v/s Readmitted",
       x = "gender",
       y = "percentage distribution") +
  theme_minimal()


## 3. "Age"
ggplot(data = diabetic_final.df) +
  aes(x = age, fill = age) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Percentage Age distribution",
       x = "Age",
       y = "percentage distribution") +
  theme_light()

round(prop.table(table(diabetic_final.df$age)),4)*100
# 25.8 % given age groups are from class (70 - 80) and is highest of all, 
# while lowest being 0.1% for (0 - 10) given class.
# In our data distribution hump can be seen for age groups (50 - 90).

##  Age v/s Readmitted
ggplot(data = diabetic_final.df) +
  aes(x = age, fill = readmitted) +
  geom_bar() +
  labs(title = "Age v/s Readmitted",
       x = "Age",
       y = "Percentage distribution") +
  theme_minimal()


## 4. Insulin 
ggplot(data = diabetic_final.df) +
  aes(x = insulin, fill = insulin) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Percentage Insulin distribution",
       x = "Insulin",
       y = "percentage distribution") +
  theme_light()


round(prop.table(table(diabetic_final.df$insulin)),4)*100
#  46.9 % for insulin : "STEADY" and 11.1 % for insulin : "UP" 


##  Insulin  v/s Readmitted
ggplot(data = diabetic_final.df) +
  aes(x = insulin , fill = readmitted) +
  geom_bar() +
  labs(title = "Insulin  v/s Readmitted",
       x = "Insulin",
       y = "percentage distribution") +
  theme_minimal()


## 5. "Change"
ggplot(data = diabetic_final.df) +
  aes(x = change, fill = change) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Percentage change distribution",
       x = "change",
       y = "percentage distribution") +
  theme_light()

round(prop.table(table(diabetic_final.df$change)),4)*100
# data distribution is 53.82 % for "NO" change and 43.18% for "CH" change.


##  change  v/s Readmitted
ggplot(data = diabetic_final.df) +
  aes(x = change , fill = readmitted) +
  geom_bar() +
  labs(title = "Change v/s Readmitted",
       x = "change",
       y = "percentage distribution") +
  theme_minimal()


## 6. "diabetesMed"
ggplot(data = diabetic_final.df) +
  aes(x = diabetesMed, fill = diabetesMed) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Percentage diabetesMed distribution",
       x = "diabetesMed",
       y = "percentage distribution") +
  theme_light()


round(prop.table(table(diabetic_final.df$diabetesMed)),4)*100
# In 76.8% cases the diabetes Medication is provided , while for only 23.2%

##  diabetesMed  v/s Readmitted
ggplot(data = diabetic_final.df) +
  aes(x = diabetesMed , fill = readmitted) +
  geom_bar() +
  labs(title = "diabetesMed",
       x = "diabetesMed",
       y = "percentage distribution") +
  theme_minimal()


## 7. "readmitted"
ggplot(data = diabetic_final.df) +
  aes(x = readmitted, fill = readmitted) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Percentage readmitted distribution",
       x = "readmitted",
       y = "percentage distribution") +
  theme_light()

round(prop.table(table(diabetic_final.df$readmitted)),4)*100
# out of total admitted patients in given data frame 53.38% patients are not readmitted, while  46.62% patients got readmitted.

# Also in the given data frame if we see by logic in the data frame we can understand that there are columns such as 
# "race", "gender", "patient_nbr" and "age" which are representing the same patient information and will be highly collinear for model building 
# we can keep the numerical column "patient_nbr" only as others are categorical variables and 
# their dummy variables in data set is going to make less available data (ratio of number of columns to number of rows). 


##----- Perform basic data exploration for some Numerical attributes ::

# lets, check all numerical variables for our analysis "num.var",
num.var <- rapply(diabetic_final.df,function(x) is.numeric(x))
which(num.var==TRUE)

# From data dictionary, It is found that ::
# time_in_hospital, num_lab_procedures, num_procedures, num_medications 
# number_outpatient, number_emergency, number_inpatient, number_diagnoses,
# "admission_type_id", "discharge_disposition_id", "admission_source_id" are our 11 numerical variables in the data frame.


# 1.  "time_in_hospital"
ggplot(data = diabetic_final.df) +
  aes( y = time_in_hospital) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$time_in_hospital,seq(0,1,0.01))

# There is a jump between 98% - 100%, So capping values with 12  
# Outlier Treatment required here
diabetic_final.df$time_in_hospital[which(diabetic_final.df$time_in_hospital > 12  )] <- 12 


# 2.  "num_lab_procedures"
ggplot(data = diabetic_final.df) +
  aes( y = num_lab_procedures) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$num_lab_procedures,seq(0,1,0.01))

# There is a jump between 98% - 100%, So capping values with 80
# Outlier Treatment required here
diabetic_final.df$num_lab_procedures[which(diabetic_final.df$num_lab_procedures > 80  )] <- 80 


# 3.  "num_procedures"
ggplot(data = diabetic_final.df) +
  aes( y = num_procedures) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$num_procedures,seq(0,1,0.01))

# There is a jump between 98% - 100%, So capping values with 5
# Outlier Treatment required here
diabetic_final.df$num_procedures[which(diabetic_final.df$num_procedures > 5  )] <- 5 


# 4.  "num_medications"
ggplot(data = diabetic_final.df) +
  aes( y = num_medications) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$num_medications,seq(0,1,0.01))

# There is a jump between 96% - 100%, So capping values with 32
# Outlier Treatment required here
diabetic_final.df$num_medications[which(diabetic_final.df$num_medications > 32  )] <- 32


# 5.  "number_outpatient"
ggplot(data = diabetic_final.df) +
  aes( y = number_outpatient) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$number_outpatient,seq(0,1,0.01))

# There is a jump between 95% - 100%, So capping values with 5
# Outlier Treatment required here
diabetic_final.df$number_outpatient[which(diabetic_final.df$number_outpatient > 2  )] <- 2


# 6.  "number_emergency"
ggplot(data = diabetic_final.df) +
  aes( y = number_emergency) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$number_emergency,seq(0,1,0.01))

# There is a jump between 97% - 100%, So capping values with 2
# Outlier Treatment required here
diabetic_final.df$number_emergency[which(diabetic_final.df$number_emergency > 3  )] <- 3


# 7.  "number_inpatient"
ggplot(data = diabetic_final.df) +
  aes( y = number_inpatient) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$number_inpatient,seq(0,1,0.01))

# There is a jump between 95% - 100%, So capping values with 3
# Outlier Treatment required here
diabetic_final.df$number_inpatient[which(diabetic_final.df$number_inpatient > 3  )] <- 3


# 8.  "number_diagnoses"
ggplot(data = diabetic_final.df) +
  aes( y = number_diagnoses) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$number_diagnoses,seq(0,1,0.01))

# There is a jump between 99% - 100%, So capping values with 9
# Outlier Treatment required here
diabetic_final.df$number_diagnoses[which(diabetic_final.df$number_diagnoses > 9  )] <- 9


# 9.  "admission_type_id"
ggplot(data = diabetic_final.df) +
  aes( y = admission_type_id) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$admission_type_id,seq(0,1,0.01))

# There is a jump between 99% - 100%, So capping values with 8
# Outlier Treatment required here
diabetic_final.df$admission_type_id[which(diabetic_final.df$admission_type_id > 6 )] <- 6


# 10.  "discharge_disposition_id"
ggplot(data = diabetic_final.df) +
  aes( y = discharge_disposition_id) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$discharge_disposition_id,seq(0,1,0.01))

# There is a jump between 99% - 100%, So capping values with 8
# Outlier Treatment required here
diabetic_final.df$discharge_disposition_id[which(diabetic_final.df$discharge_disposition_id > 18 )] <- 18


# 11.  "admission_source_id"
ggplot(data = diabetic_final.df) +
  aes( y = admission_source_id) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$admission_source_id,seq(0,1,0.01))

# There is a jump between 99% - 100%, So capping values with 17
# Outlier Treatment required here
diabetic_final.df$admission_source_id[which(diabetic_final.df$admission_source_id > 17 )] <- 17


# 12.  "admission_source_id"
ggplot(data = diabetic_final.df) +
  aes( y = admission_source_id) +
  geom_boxplot(fill = "#fb6a4a") +
  labs(title = "DIstribution") +
  theme_minimal()

quantile(diabetic_final.df$admission_source_id,seq(0,1,0.01))

# There is a jump between 99% - 100%, So capping values with 17
# Outlier Treatment required here
diabetic_final.df$admission_source_id[which(diabetic_final.df$admission_source_id > 17 )] <- 17


##-------- DATA PRE-PREPARATION-------##

# 3.------ Creating Dummy variables ( Categorical Variables, Given Task Under : Data preparation ) 

# Check for character variable's in data set "diabetic_final.df".
chr <- rapply(diabetic_final.df,function(x) is.character(x))
which(chr)
# race, gender, age, insulin, change,  diabetesMed, readmitted. are 7 character variables, which are required to be converted to dummy variables for model Building.


# Creating "dummy_var" which will store the dummy variables of character data type Variables:  
dummy_var <- data.frame(model.matrix( ~  race + gender + age + insulin + change + diabetesMed + readmitted, data = diabetic_final.df))
dim(dummy_var)
# "dummy_var" contains 98052 observations and 21 variables
# Excluding the first column in "dummy_var", which contains only number of rows
dummy_var <- dummy_var[-1]
# View(dummy_var)
# Checking the class of "dummy_var" 
class(dummy_var)
dim(dummy_var)
##  Finally, dummy_var contains 98052 Observations & 20 variables


# Defining new data frame "final_diabetic_model.data" is final data frame after adding dummy variable and removing character variables 
final_diabetic_model.data <- diabetic_final.df[-c(which(chr))]
final_diabetic_model.data <- cbind(final_diabetic_model.data,dummy_var) 
dim(final_diabetic_model.data)
# The "final_diabetic_data" data frame has 98052 observations and 32 variables.


##--------- MODEL BUILDING ----------##

# In order to get the same sample data, we set seed = 1111 here.
set.seed(1111)

# Checking the actual distribution of "comorbidity" in the given "diabetic_final.df" data frame.
round(prop.table(table(diabetic_final.df$readmitted)),2)
#   NO    YES 
#  53 %    47 % 
# Hence the target variable classes are quite balanced.


# Dividing data randomly from "final_diabetic_model.data" data frame into "train" and "test" data frames.

# "sample_index" is randomly selected 70 % of total data for "train" data frame
# making seperate train and test data frames from  "final_diabetic_model.data"
sample_index = sample(1:nrow(final_diabetic_model.data), 0.7*nrow(final_diabetic_model.data))
train = final_diabetic_model.data[sample_index,]
test = final_diabetic_model.data[-sample_index,]

# Lets check the "train" and "test" data frames created for model building.
dim(train)
dim(test)
# "train" data frame contains 68636 observations and "test" data frame contains 29416 observations with 32 variables each.
round(prop.table(table(train$readmittedYES)),2)
round(prop.table(table(test$readmittedYES)),2)
#  0 (NO)   1 (YES) 
#   53 %      47 % 
# Hence, "readmittedYES"  have same class distribution as we got in "final_diabetic_model.data" data frame, so classes are quite balanced.
str(train)
str(test)

# Making copy for "train" and "test" data and creating new "training_set" and "test_set" for further model building. 
training_set <- train
test_set <- test


#----- 3. Scale numeric attributes ( Given Task Under : Data preparation )

# SCALE the data. so, that all the variables lie in the same range.
training_set[1:12] <- scale(training_set[1:12])
test_set[1:12] <- scale(test_set[1:12])
View(training_set)
View(test_set)
class(training_set)
# "data.frame" type

# Now our data is ready and will fulfill the assumptions made by model.

##------ Lets start Building our LOGISTIC REGRESSION MODEL :

#First Model
model_1 <- glm( readmittedYES ~., data = training_set, family = "binomial")
# Check the summary of model_1. 
summary(model_1)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89485  on 68604  degrees of freedom
# AIC: 89549


# Now, lets see how to use stepAIC
# In stepAIC function, we pass our first model i.e model_1 and 
# direction as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously. 

step <- stepAIC(model_1, direction="both")
# So, many iterations have been done through the stepwise command. 
# Now we need to know our model further.

# the Step command here to see the final sorted model. 
step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignificant variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

# Degrees of Freedom: 68635 Total (i.e. Null);  68606 Residual
# Null Deviance:	    94840 
# Residual Deviance: 89490 	AIC: 89450


model_2 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                   genderMALE + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                   age.50.60. + age.60.70. + age.70.80. + age.80.90. + age.90.100. + 
                   insulinNO + insulinSTEADY + insulinUP + diabetesMedYES, 
                   family = "binomial", data = training_set)

# Check the summary of model_2
summary(model_2)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89492  on 68608  degrees of freedom
# AIC: 89548

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_2))

# Checking for variables with high VIF (VIF > 2) and high p-values > 0.05.
# So we found "age.90.100." variable which has VIF : 57.807665 & p-values : 0.127477
# Removing this in model_3

model_3 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                   genderMALE + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                   age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinNO + insulinSTEADY + insulinUP + diabetesMedYES, 
                   family = "binomial", data = training_set)

# Check the summary of model_3
summary(model_3)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89494  on 68609  degrees of freedom
# AIC: 89548

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_3))
# Hence here we can see that by removing "age.90.100." variable, by how much impact VIF or multicollinearity with variables have come down. 


# Checking for variables with high VIF (VIF > 2) and high p-values.
# So we found "insulinNO " variable which has VIF : 3.200175 & p-values : 0.005826
# Removing this in model_4

model_4 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                    admission_source_id + time_in_hospital + num_lab_procedures + 
                    num_procedures + number_outpatient + number_emergency + number_inpatient + 
                    number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                    genderMALE + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                    age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                    insulinSTEADY + insulinUP + diabetesMedYES, 
                    family = "binomial", data = training_set)

# Check the summary of model_4
summary(model_4)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89502 on 68610  degrees of freedom
# AIC: 89554

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_4))

# Checking for variables with high VIF (VIF > 2) and high p-values 
# So we found " age.30.40. " variable which has VIF : 2.248907 & p-values : 0.000719
# Removing this in model_5

model_5 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                   genderMALE + age.10.20. + age.20.30. + age.40.50. + 
                   age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinSTEADY + insulinUP + diabetesMedYES, 
                   family = "binomial", data = training_set)

# Check the summary of model_5
summary(model_5)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89513  on 68611  degrees of freedom
# AIC: 89563

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_5))

# Checking for variables with high VIF (VIF > 2) and high p-values 
# So we found " age.40.50. " variable which has VIF : 2.271242  & p-values : 0.000231 
# Removing this in model_6.

model_6 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                   genderMALE + age.10.20. + age.20.30. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinSTEADY + insulinUP + diabetesMedYES, 
                   family = "binomial", data = training_set)

# Check the summary of model_6
summary(model_6)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89527  on 68612  degrees of freedom
# AIC: 89575

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_6))

# Checking for variables with high VIF (VIF > 2) and high p-values 
# So we found " age.70.80. " variable which has VIF : 2.056722  & p-values : 2.24e-16 *** significant values.


# Now, we have checked for all high vif and high p-value variables, and have removed them
# Next we can look for eliminating the high p-values only, 
# as they are the insignificant variables in the model which are affecting the accuracy of our model. 

# Removing "insulinUP" variable which has p-value : 0.832986 in model_7

model_7 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                   genderMALE + age.10.20. + age.20.30. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinSTEADY + diabetesMedYES, 
                   family = "binomial", data = training_set)


# Check the summary of model_7
summary(model_7)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89527 on 68613  degrees of freedom
# AIC: 89573

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_7))

# Removing "age.10.20." variable which has p-value : 0.390076 in model_8

model_8 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                   genderMALE + age.20.30. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinSTEADY + diabetesMedYES, 
                   family = "binomial", data = training_set)

# Check the summary of model_8
summary(model_8)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89528  on 68614  degrees of freedom
# AIC: 89572

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_8))

# Removing "age.20.30." variable which has p-value : 0.217611 in model_9

model_9 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceCAUCASIAN + raceOTHER + 
                   genderMALE + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinSTEADY + diabetesMedYES, 
                   family = "binomial", data = training_set)

# Check the summary of model_9
summary(model_9)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89529  on 68615  degrees of freedom
# AIC: 89571

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_9))

# Removing "raceCAUCASIAN " variable which has p-value : 0.142175 in model_10

model_10 <-  glm( formula = readmittedYES ~ admission_type_id + discharge_disposition_id + 
                   admission_source_id + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + raceASIAN + raceOTHER + 
                   genderMALE + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinSTEADY + diabetesMedYES, 
                   family = "binomial", data = training_set)

# Check the summary of model_10
summary(model_10)

# Null deviance: 94845  on 68635  degrees of freedom
# Residual deviance: 89531  on 68616  degrees of freedom
# AIC: 89571

# Let us check for multicollinearity.  
# Removing the variables if they are statistically insignificant
sort(vif(model_10))


# This is the stage where we have all variables with two star (**) or three star (***) significant p - values.
# Also we have vif values of all varibales less than 2 ( i.e  vif < 2 ), 

# We are having 19 significant variables in this model.
Final_model <- model_10

which(names(training_set)=="readmittedYES")
# In test dataset location for our target variable "readmittedYES" variable is 32.



##--------- Prediction on test set ----------##

pred_test.set  <- predict(Final_model, type = "response", newdata = test_set[,-32])

# Let's see the summary 
summary(pred_test.set)
anova(Final_model, test="Chisq")

test_set$probability <- pred_test.set

# Let's use the probability cutoff of 50%.
test_pred_diab <- factor(ifelse(pred_test.set >= 0.50, "Yes", "No"))
test_actual_diab <- factor(ifelse(test_set$readmittedYES==1,"Yes","No"))

table(test_pred_diab,test_actual_diab)
#                       test_actual_diab
#    test_pred_diab       No    Yes
#                   No   12296  7763
#                   Yes  3436   5921

test_conf <-  confusionMatrix(test_pred_diab,test_actual_diab, positive = "Yes")
test_conf


# Confusion Matrix and Statistics

# Reference
# Prediction    No   Yes
# No  12296  7763
# Yes  3436  5921

# Accuracy : 0.6193          
# 95% CI : (0.6137, 0.6248)
# No Information Rate : 0.5348          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.2188          
# Mcnemar's Test P-Value : < 2.2e-16       

# Sensitivity : 0.4327          
# Specificity : 0.7816          
# Pos Pred Value : 0.6328          
# Neg Pred Value : 0.6130          
# Prevalence : 0.4652          
# Detection Rate : 0.2013          
# Detection Prevalence : 0.3181          
# Balanced Accuracy : 0.6071          

# 'Positive' Class : Yes    


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_diab <- factor(ifelse(pred_test.set >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_diab, test_actual_diab, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability
summary(pred_test.set)

# Creating cutoff values from 0003413 to 0.8152061 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.81,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 



# Plotting ROC Curve having Sensitivity,Specificity and Accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.41,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff

# Let's choose a cutoff value = 0.4382828 at the point where sensitivity & specificity are nearest to eachother
readmitted_test_cutoff <- factor(ifelse(pred_test.set >= 0.4382828, "Yes", "No"))

conf_final <- confusionMatrix(readmitted_test_cutoff, test_actual_diab, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6105181 
sens
# Sensitivity 
# 0.6023823 
spec
# Specificity 
# 0.6175947 

fitted.results <- ifelse(pred_test.set > 0.4382828 ,1,0)

misClasificError <- mean(fitted.results != test_set$readmittedYES)
print(paste('Accuracy',1-misClasificError))
# Hence, here we got 0.61 ~ 61.05 %  accuracy on the test set.


## KS Statistic - Test Data
readmitted_test_cutoff <- ifelse(readmitted_test_cutoff == "Yes", 1, 0)
test_actual_diab <- ifelse(test_actual_diab == "Yes", 1, 0)

#on testing  data
library(ROCR)
ROCR_pred <- prediction(readmitted_test_cutoff, test_actual_diab)

readmitted_performance_test <- performance(ROCR_pred, "tpr", "fpr")

ks_table_test <- attr(readmitted_performance_test, "y.values")[[1]] - 
                     (attr(readmitted_performance_test, "x.values")[[1]])

max(ks_table_test)
# 0.2199771


#---- Starting Building our Random Forest Model -----#

readmitted.model.rf <- randomForest( readmittedYES ~ ., 
                         data = training_set, 
                         proximity = FALSE,
                         ntree = 50, mtry = 5, 
                         do.trace = TRUE, na.action = na.omit)


# Lets see what are the predictions made
readmitted_prediction_rf <- predict(readmitted.model.rf, type="response", newdata = dplyr::select(test_set, -readmittedYES))

test_set$readmitted_Predict_rf <- readmitted_prediction_rf


# Let's use the probability cutoff of 50%.
readmitted_test_pred_rf <- factor(ifelse(readmitted_prediction_rf >= 0.50, "Yes", "No"))
readmitted_test_actual_rf <- factor(ifelse(test_set$readmittedYES == 1,"Yes","No"))

table(readmitted_test_pred_rf,readmitted_test_actual_rf)

#                           readmitted_test_actual_rf
# readmitted_test_pred_rf        No     Yes
#                         No    11237   6538
#                         Yes   4495   7146

readmitted_test_conf_rf <- confusionMatrix(readmitted_test_pred_rf, readmitted_test_actual_rf, positive = "Yes")
readmitted_test_conf_rf

# Confusion Matrix and Statistics

# Reference
# Prediction    No   Yes
# No  11237  6538
# Yes  4495  7146

# Accuracy : 0.6249          
# 95% CI : (0.6194, 0.6305)
# No Information Rate : 0.5348          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.2388          

# Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5222          
#             Specificity : 0.7143          
#          Pos Pred Value : 0.6139          
#          Neg Pred Value : 0.6322          
#              Prevalence : 0.4652          
#          Detection Rate : 0.2429          
#    Detection Prevalence : 0.3957          
#       Balanced Accuracy : 0.6182          
                                          
#        'Positive' Class : Yes             
                                  

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_diab <- factor(ifelse(readmitted_prediction_rf >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_diab, readmitted_test_actual_rf, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability
summary(readmitted_prediction_rf)

# Creating cutoff values from 0003413 to 0.8152061 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.81,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# Plotting ROC Curve having Sensitivity,Specificity and Accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.41,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff

# Let's choose a cutoff value = 0.4625253 at the point where sensitivity & specificity are nearest to eachother
readmitted_test_cutoff_rf <- factor(ifelse(readmitted_prediction_rf >= 0.4625253, "Yes", "No"))

conf_final <- confusionMatrix(readmitted_test_cutoff_rf, readmitted_test_actual_rf, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6213285 
sens
# Sensitivity 
# 0.6125402 
spec
# Specificity 
# 0.6289728 



# 2.---- After Comparing the performance of "Logistic Regression" & "Random Forest" Model's , 
#   we found that "Random Forest" Model is predicting risk of readmission for the patient well with 62.1 % Accuracy.

# creating a varibale to store the given  "Risk_Buckets" Probability,
test_set$Risk_Buckets <-  ifelse(test_set$readmitted_Predict_rf > 0.7,
                                   yes = "HIGH RISK", 
                                   no = ifelse(test_set$readmitted_Predict_rf < 0.3,
                                   yes = "LOW RISK", no = "MEDIUM RISK"))


table(test_set$Risk_Buckets)
# HIGH RISK     LOW RISK    MEDIUM RISK 
#   2541           4582        22293 

# Checking Percentage of population which lie in each Risk Stratification Clasess on basis of "Random Forest" Model.  . 
round(prop.table(table(test_set$Risk_Buckets)),4)*100
# So, According to the final model used, Probability of readmission in "HIGH RISK" has 8.64 % , 
# Probability of readmission in "LOW RISK" has 15.58 % ,
# Probability of readmission in "MEDIUM RISK" has 75.79 %

