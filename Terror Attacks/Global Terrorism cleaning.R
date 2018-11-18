# # - $ -------- Business Understanding -------- $ - # 

# Information on more than 180,000 Terrorist Attacks
# The Global Terrorism Database (GTD) is an open-source database including information on terrorist attacks around the world from 1970 through 2017. 
# The GTD includes systematic data on domestic as well as international terrorist incidents that have occurred during this time period and now includes more than 180,000 attacks. 
# The database is maintained by researchers at the National Consortium for the Study of Terrorism and Responses to Terrorism (START), headquartered at the University of Maryland. 

# Geography : Worldwide
# Time period: 1970-2017, except 1993
# Unit of analysis: Attack
# Variables: >100 variables on location, tactics, perpetrators, targets, and outcomes
# Sources: Unclassified media articles (Note: Please interpret changes over time with caution. Global patterns are driven by diverse trends in particular regions, and data collection is influenced by fluctuations in access to media coverage over both time and place.)
# Definition of terrorism:
#$ "The threatened or actual use of illegal force and violence by a non-state actor to attain a political, economic, religious, or social goal through fear, coercion, or intimidation."

# Refer the below link for attribute details and meanings of short terms
# Source : https://www.kaggle.com/START-UMD/gtd/home

## ----- Business Objective ------- ## 

## ------ Goals of Analysis ------## 


# Including Required packages for Reading data, Manipulation of data, Visualization of data.

# loading libraries
library(dplyr)    
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)


##__________ SETTING-UP WORKING DIRECTORY _________

setwd("C:/Users/Mohit Singh/Desktop/Data Science Portfolio/terrorism")
getwd()        # To check whether the file is properly loaded or not
dir()          # To Check the Target file Name : 

# Clean the file for any Pre-Used valiables
remove(list = ls())


##_________ DATA LOADING _________

# Creating a New Data frame weather_pred  
# Blank spaces if any will be replaced by NA using na.strings and we dont want strings to be converted to factors automatically while reading file 
terror_data <- read.csv("globalterrorismdb_0718dist.csv",na.strings = c("",NA),header = TRUE, stringsAsFactors = FALSE)
View(terror_data)        # To view data frame uber_data in table form in R for easy analysis.          

str(terror_data)
summary(terror_data)
dim(terror_data)
names(terror_data)
# "terror_data" data set contains 1,81,691 observations and 135 attributes.


# -------- Data Understanding -------- #

# ------- Cleaning data 

# Column correction
value_na <- colSums(is.na(terror_data))

drop <- value_na[-which(value_na >= nrow(terror_data)*0.30)] 
length(drop)
# 48 attributes left 
drop

terror_attack <- names(drop)
terror_attack <- terror_data[terror_attack]
View(head(terror_attack,3)) 
# Dropping eventid
terror_attack <- terror_attack[-1]

rowSums(is.na(terror_attack))
# We are haing NA values in the rows also but they are very less and can be treated.

# Filling missing values
colSums(is.na(terror_attack))

# Renaming variables where names are not easy to understand.
terror_attack <- terror_attack %>%
             rename(year = iyear, month = imonth, day = iday, country = country_txt, region = region_txt, multiple_attack = multiple, attacktype = attacktype1_txt,
            target_type = targtype1_txt, target_sub_type = targsubtype1_txt, group_name = gname, weapon_type = weaptype1_txt)

write.csv( terror_attack, file =  "Terror_attack.csv",row.names = FALSE)
getwd()
