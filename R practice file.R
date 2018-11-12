# export file as exl file 
install.packages("openxlsx")
library(openxlsx)
# write.xlsx(filename, file =  " ")

# detach("package:package_name", unload = TRUE)

# If silent = TRUE(all capital silent), it installs and loads packages without reporting. 
# If silent = FALSE, it reports successful installation of packages. Default value is silent = FALSE

# Check for installed packges 
# Remove 


# Loading the data file in R.
ls()
remove(list = ls())
rm()
# na.omit(), na.pass(), na.fail()
# setwd()  getwd()
dir()  # see the names of all files in the added directory

sum(1, 2, 3, 4, 5)        # this is working   
median(1, 2, 3, 4, 5)     # this is not working, but giving wrong result 
median(c(1, 2, 3, 4, 5))  # this is working
median(1:10)               # this is working  1:10

# 2^3  or  2**3
# Relational operators

c(3, 4 - 1, 1 + 1 + 1) == 3

c("A", "B", "C", "D") < "C"   # case sensitive here in case of strings 
c("a", "b", "c", "d") < "C" 

# Reltional operators is working different for strings.

c("Can", "you", "can", "a", "can", "as",  "a", "canner", "can", "can", "a", "can?" )==
  "can" 

#  all.equal(c(  "Can", "you", "can", "a", "can", "as",  "a", "canner", "can", "can", "a", "can?" ),"can" )
# <- or = works the same 
# Vectors are always safe to use to get proper results.

ls()
ls(pattern = "ea") # to search for any variable.
rm()   # rm(peach, plum, pear) remove multiple same time also.

# length(), table(), names()
# dim(), nrow(), ncol(), colSums(), rowsum()
# arithmetic doesn't work on lists , works only on bolean , numeric values.

# List
# changinng the name of elements : names(a_list) <- c("catalan", "months", "involutary", "arcsin") 

vect<-c(1, 6, 21, 107)
as.list(vect)             # reading each element as an element
vectl<-list(vect)         # reading complete as an element
vectl
as.numeric(vect)          #change to numeric again
vect
is.numeric(vect)

# DATA FRAMES :
# When reading a file num columns blank = NA, not on strings, we can by na.strings = ("").
# data frame can be transposed using t() function.
trans <- t(iris)
# when its is showung vector and when it shows data frame.
iris[[1]][2:10]
iris[1,1:2]
iris$Sepal.Length

rowSums(head(iris[,1:2],5))
colSums((iris[,1:4]))




a<-read.delim("companies.txt",header = TRUE,stringsAsFactors = FALSE,
              na.strings = "" ,sep = "\t")

file<-read.csv("bollywood.csv",stringsAsFactors = FALSE,header = TRUE, na.strings = "")
file

table(is.na(file))

summary(file)
str(file)

names(file)

table(file$Movie)

is.data.frame(file)


#load the excel file 
# XL connect() , gdata()


# Data manipulation


# ------- EDA

data("mtcars")  
str(mtcars)  
View(mtcars)  

fivenum(mtcars$mpg) # always work on the column of the data frame
summary(mtcars$mpg)

edit(mtcars) 

colours() # list of all colours available

# Data manipulation in R 
library(dplyr)

# Filter (), select (),  group_by(), summarise(), mutate(), arrange(), merge()
library(ggplot2)
View(diamonds)

View(c)

select(diamonds, 1:3)
select(diamonds, -(1:3))
select(diamonds, 1:10,-(5:7))
select(diamonds, 1,3,5)
select(diamonds, starts_with("c"))
select(diamonds, ends_with("r"))
select(diamonds, starts_with("c"))
select(diamonds, -starts_with("c"))
select(diamonds, contains("ic"))
select(diamonds, 1:10,-3,-6)
select(diamonds, 1:10,-c(3,6))
select(diamonds, 1:10,-c(3,6,7:8))
select(diamonds, z, y, everything())  # put z , y in the front column

# selecting random N rows  -  sample_n(diamonds,10)

distinct(diamonds)
distinct(diamonds, carat)
distinct(diamonds, carat, .keep_all = TRUE)
distinct(diamonds, carat, color, .keep_all = TRUE) #Combination of two columns


names(diamonds)
rename(diamonds, colour = color)                # to this = do this
names(diamonds)[3:4]<-c("colo","cosls","hlo")   # lengthy little bit

filter(diamonds, diamonds$price == 362)
filter(diamonds, diamonds$price != 362)
filter(diamonds, diamonds$carat == 0.24  & diamonds$price == 362)
filter(diamonds, diamonds$carat == 0.24  | diamonds$price >= 362)
filter(diamonds, diamonds$carat == 0.24  | !diamonds$price >= 362)
filter(diamonds, diamonds$cut %in% "Good" & diamonds$colo == "E")
filter(diamonds, !diamonds$cut %in% "Good" & diamonds$colo == "E")
filter(diamonds, grepl("oo", diamonds$cut))             # grepl()    

summarise(diamonds,a = mean(diamonds$table, na.rm = TRUE),b = median(diamonds$depth , na.rm = TRUE))

arrange(diamonds, diamonds$colo)  # by default asc() order
arrange(diamonds, desc(diamonds$colo))  
arrange(diamonds, diamonds$colo , diamonds$carat)  

group_by(diamonds, diamonds$cut)
# funs()


# unite(diamonds,"unite",2:3,sep = "-"))

View(unite(diamonds,"unite",2:3,sep = "-"))

lapply(1:3, function(x) x^2)
lapply(diamonds,class)

# sapply data frame to list result
sapply(diamonds,class)
sapply(1:3, function(x) x^2)

nchar("mohit singh")

regexpr('S', diamonds) # Returns position of 1st match in a string
gregexpr('S', diamonds[4]) # Returns positions of every match in a string


sub('S', z, diamonds[4]) # Changes only the 1st pattern match per string
gsub('S', z, diamonds[4]) # Changes every occurrence of a pattern match

grep('pattern', x)
substr(x, first, last)

# Handling date time  
setwd("C:/Users/Mohit Singh/Desktop/Exploratory Data analysis")
# Stadard format in R is yyyy-mm-dd
# Dates are stored as days since 1st january 1970
Sys.Date()  # current date
Sys.Date()+10
typeof(Sys.Date())
as.integer(Sys.Date())
unclass(Sys.Date())

# DATE FORMATS , converting into dates using as.dates()
typeof("25-05-2018")
d1<-as.Date("25-05-2018", format = "%d-%m-%Y")
d2<-as.Date("25/05/2018", format = "%d/%m/%Y")
typeof(d1)











# Write a function which identifies blank characters representing the unavailable values, and replaces them with NA 
remove_blank <- function(vector){
  vector[which(vector == "-"| vector == ""|vector == " ")]<- NA
  corrected_vector <- as.numeric(vector)
  return(corrected_vector)
}
remove_blank(c(21,""," " ,34,"-",78, 98))

# Write a function which identifies and removes the "*" character after some numeric values in a vector.
remove_string <- function(vector){
  corrected_vector <- as.numeric(gsub("[*]","",vector))
  return(corrected_vector)
}
remove_string(c(21,34,"99*",56,"90*", "45*"))


# A vector contains the scores of a player in various innings
centuries <- function(player_vector){
  sum_centuries <- sum(ifelse(player_vector>=100,1,0))
  return(sum_centuries)
}
centuries(c(120 ,30, 134, 16,102))


setwd("C:/Users/Mohit Singh/Desktop/Exploratory Data analysis")
data<- read.csv("grades.csv", header = TRUE, stringsAsFactors = FALSE)
View(data)









