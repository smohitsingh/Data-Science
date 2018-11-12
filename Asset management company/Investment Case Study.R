
# -- CheckPoint 1
# Load the companies and rounds data into two data frames "companies" and "rounds2" respectively.
companies <- read.delim("companies.txt", header = TRUE, na.strings=c("", "NA"), sep = "\t", stringsAsFactors = FALSE)
rounds2    <- read.csv("rounds2.csv", na.strings=c("", "NA"), stringsAsFactors = FALSE, header = TRUE)


# Find out unique companies are present in rounds2.
rounds2_unique     <- length(unique(rounds2$company_permalink))

# Find out unique companies are present in companies.
companies_unique  <- length(unique(companies$permalink))


# Merge the two data frames to the new dataframe master_frame

# Direct merge of the 2 data frames give 0 obs. in master_frame, after looking into 2 dataframes
# it's evident that the same values are present using different cases.
# So now first convert column value to same case in both the data frames
companies$permalink <- toupper(companies$permalink)
rounds2$company_permalink <- toupper(rounds2$company_permalink)

# Now the actual merge of the 2 data frames to form master_frame
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink") 



# -- CheckPoint 2

# Calculate the average investment amount for each of the four funding types

venture_round <- subset(master_frame, master_frame$funding_round_type=="venture")
venture_round_average <- mean(venture_round$raised_amount_usd, na.rm = TRUE)

angel_round <- subset(master_frame, master_frame$funding_round_type=="angel")
angel_round_average <- mean(angel_round$raised_amount_usd, na.rm = TRUE)

private_equity_round <- subset(master_frame, master_frame$funding_round_type=="private_equity")
private_equity_round_average <- mean(private_equity_round$raised_amount_usd, na.rm = TRUE)

seed_round <- subset(master_frame, master_frame$funding_round_type=="seed")
seed_round_average <- mean(seed_round$raised_amount_usd, na.rm = TRUE)


#   Which investment type do you think is the most suitable for Spark Funds?
#   Answer -  Spark Funds wants to invest between 5 to 15 million USD which means the best choice is
#             Venture!


# -- CheckPoint 3

#   Find top nine countries which have received the highest total funding. 
#   (across ALL sectors for the chosen investment type)
library(dplyr)

top9 <- head(arrange(aggregate(raised_amount_usd ~ country_code , 
                              data=master_frame[master_frame$funding_round_type=="venture" 
                              & master_frame$country_code!="",], sum, na.rm = TRUE),
                    desc(raised_amount_usd))
             , n=9)



#   Get the top 3 English speaking countries

# Installing "pdftools" package to read pdf file , which is provided to indicate countries
# having "English" as offcial language
#install.packages("pdftools")
library(pdftools)

#loading entire pdf file as simple text file
text <- pdf_text("Countries_where_English_is_an_official_language.pdf")

#loading "countrycode" package to get country name like "India" from country code like "Ind"
library(countrycode)
library(stringr)

# Identify the top three English-speaking countries in the data frame top9
#   Step 1: countrycode function would fetch country name for country code present in "top9" dataframe
#           This step is required since country name is present in pdf file and not country code. 
#   Step 2: str_detect would detect if that country "name" is present in pdf file which is 
#           now converted to text format and stored in "text".
#   Step 3: Since countrycode() would give a vector of country names for given vector of country codes
#           ifelse function is used which operates on each of the vector element.
#   Step 4: Finally head function is used to fetch only top3 countires.
  
head (top9 [ifelse (str_detect (text,pattern = countrycode (top9$country_code,
                                                            origin = "iso3c",
                                                            destination = "country.name")), 
                    TRUE,FALSE), 1], n=3)




# -- CheckPoint 4

# Extract the primary sector of each category list from the category_list column
master_frame <- separate(master_frame,category_list,into=c("primary_sector"),
                             remove = F,sep="\\|",extra = "drop",fill = "right")

# Read the mapping data file and convert it from wide to long format.
mapping <- read.csv("mapping.csv", na.strings=c("", "NA"), stringsAsFactors = FALSE)
mapping <- mapping[complete.cases(mapping),]    #remove NA values.

library(tidyr)
mapping <- gather(mapping, main_sector, my_val, Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping <- mapping[!(mapping$my_val == 0),]
mapping <- mapping[, -3]


# Merged data frame with each primary sector mapped to its main sector.
master_frame <- merge(master_frame, mapping, by.x="primary_sector", by.y="category_list", all.x=T)



# -- CheckPoint 5

# Create three separate data frames D1, D2 and D3 for each of the three countries based on given criterion
# NOTE-> Some primary sectors do not fall under provided "main sectors" and thus result in "NA" values for 
#        their "main sector". Such rows have been "ignored" in the below scenarios.

# For USA
D1 <- filter(master_frame, country_code== "USA", funding_round_type=="venture", raised_amount_usd>=5000000,
             raised_amount_usd<=15000000, !is.na(master_frame$main_sector)) %>% group_by(main_sector) %>% mutate(total_investments= n(),total_amount_invested=sum(raised_amount_usd,na.rm = TRUE))

sum(unique(D1$total_amount_invested))   #Total amount of investments
sum(unique(D1$total_investments))       #Total number of investments


# For GBR
D2 <- filter(master_frame, country_code== "GBR", funding_round_type=="venture", raised_amount_usd>=5000000,
             raised_amount_usd<=15000000, !is.na(master_frame$main_sector)) %>% group_by(main_sector) %>% mutate(total_investments= n(),total_amount_invested=sum(raised_amount_usd,na.rm = TRUE))

sum(unique(D2$total_amount_invested))   #Total amount of investments
sum(unique(D2$total_investments))       #Total number of investments


# For IND
D3 <- filter(master_frame, country_code== "IND", funding_round_type=="venture", raised_amount_usd>=5000000,
             raised_amount_usd<=15000000, !is.na(master_frame$main_sector)) %>% group_by(main_sector) %>% mutate(total_investments= n(),total_amount_invested=sum(raised_amount_usd,na.rm = TRUE))

sum(unique(D3$total_amount_invested))   #Total amount of investments
sum(unique(D3$total_investments))       #Total number of investments


# Top 3 sectors and their investment count
# For USA
head(arrange(distinct(D1, total_investments), desc(total_investments)), n=3)

# For GBR
head(arrange(distinct(D2, total_investments), desc(total_investments)), n=3)

# For IND
head(arrange(distinct(D3, total_investments), desc(total_investments)), n=3)


# For question 9 and 10
# Step 1 - For each D1, D2, D3, find the total count and amount invested in each company. (We will add them as 2 new columns)
# Step 2 - For Q9 slice based on top sector count-wise, sort the dataframe based on the "total company investment amount"
#          and pick the top company name.
# Step 3 - For Q10 slice based on 2nd best sector count-wise, sort the dataframe based on the "total company investment amount"
#          and pick the top company name.

# For USA
D11 <- group_by(D1, name) %>% mutate(total_C_invest= n(),total_C_amt_invest=sum(raised_amount_usd,na.rm = TRUE))

distinct(arrange(filter(D11, main_sector == "Others"), desc(total_C_amt_invest)), name)[1,]
distinct(arrange(filter(D11, main_sector == "Cleantech...Semiconductors"), desc(total_C_amt_invest)), name)[1,]



# For GBR
D21 <- group_by(D2, name) %>% mutate(total_C_invest= n(),total_C_amt_invest=sum(raised_amount_usd,na.rm = TRUE))

distinct(arrange(filter(D21, main_sector == "Others"), desc(total_C_amt_invest)), name)[1,]
distinct(arrange(filter(D21, main_sector == "Cleantech...Semiconductors"), desc(total_C_amt_invest)), name)[1,]



# For IND
D31 <- group_by(D3, name) %>% mutate(total_C_invest= n(),total_C_amt_invest=sum(raised_amount_usd,na.rm = TRUE))

distinct(arrange(filter(D31, main_sector == "Others"), desc(total_C_amt_invest)), name)[1,]
distinct(arrange(filter(D31, main_sector == "News..Search.and.Messaging"), desc(total_C_amt_invest)), name)[1,]

