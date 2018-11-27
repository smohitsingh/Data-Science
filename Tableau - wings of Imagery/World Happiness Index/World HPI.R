##------ "Exploring and Clustering Happy Planet Index" ---------##

# The [Happy Planet Index (HPI)](http://happyplanetindex.org/) is an index of human well-being and environmental impact that was introduced by [NEF](http://neweconomics.org/), a UK-based economic think tank promoting social, economic and environmental justice. The index is weighted to give progressively higher scores to nations with lower ecological footprints. I downloaded the 2016 dataset from [HPI website](http://happyplanetindex.org/countries). My goal is to find correlations between several variables, then use clustering technic to seprarate these 140 countries into different clusters, according to happiness, wealth, life expectancy and carbon emissions. 

# Adding the file path .
setwd("C:/Users/Mohit Singh/Desktop/Data Science Portfolio/Happiness index")
dir()

### Load the packages

library(tidyr)
library(dplyr)
library(readxl)


happy_data <- read_xlsx("hpi-data-2016.xlsx", sheet = 5,na = "NA")
View(happy_data)

### Data Pre-processing
class(happy_data)
dim(happy_data)
# 161 rows and 14 columns  


##----- Cleaning Data
# Checked data is without missing values

rowSums(is.na(happy_data))

# Removing all empty rows
na <-  which(rowSums(is.na(happy_data))== length(happy_data))
happy_data <- happy_data[-c(na),]

na11 <-  which(rowSums(is.na(happy_data)) >= 12)
happy_data <- happy_data[-c(na11),]


# Converting to data frame
happy_data <-  as.data.frame(happy_data)
class(happy_data)
str(happy_data)
dim(happy_data)

# Transfer names from 1st row to name of columns
Var_name <- happy_data[1,]
Var_name <- as.character(Var_name)
names(happy_data) <- Var_name
happy_data <- happy_data[-1,]

# Names of columns are not proper so, correcting them
happy_data <- setNames(happy_data, c( 'HPI_Rank','country', 'region', 'life_expectancy', 'wellbeing', 'happy_years', 'footprint','inequality_outcomes', 'adj_life_expectancy', 'adj_wellbeing', 'hpi_index', 'gdp', 'population'))

colSums(is.na(happy_data))

table(duplicated(happy_data))

happy_data <- happy_data[-c(1)]

happy_data <- happy_data[c(-13)]

# change data type
happy_data$country <- as.character(happy_data$country)
happy_data$region <- as.character(happy_data$region)
happy_data$life_expectancy <- as.numeric(happy_data$life_expectancy)
happy_data$wellbeing <- as.numeric(happy_data$wellbeing)
happy_data$happy_years <- as.numeric(happy_data$happy_years)
happy_data$footprint <- as.numeric(happy_data$footprint)
happy_data$inequality_outcomes <- as.numeric(happy_data$inequality_outcomes)
happy_data$adj_life_expectancy <- as.numeric(happy_data$adj_life_expectancy)
happy_data$adj_wellbeing <- as.numeric(happy_data$adj_wellbeing)
happy_data$hpi_index <- as.numeric(happy_data$hpi_index)
happy_data$gdp <- as.numeric(happy_data$gdp)
happy_data$population <- as.numeric(happy_data$population)


# The structure of the data
str(happy_data)

# The summary of the data
summary(happy_data)

dim(happy_data)
# 140 observations and 12 columns

# Writing file to use i
write.csv(happy_data, file = "Happiness_index.csv", row.names = FALSE)
