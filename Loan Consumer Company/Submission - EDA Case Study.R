
loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(gridExtra)


#   Step 1 - removing the unwanted columns

#   Step 1.1 - remove all those columns which have only one value like NA or "n" in case of pymny_plan.
loan <- loan %>% select_if(~ length(unique(.)) > 1)


#   Step 1.2 - remove few other columns which are deemed not needed.
#     a.	Remove "member id" column as "id" column also contains all unique values.
#         NOTE: even though "id" coulmn is not needed as no user level analysis is needed, but we are
#               keeping this column just in case we need a unique reference.
#     b.	Remove "url" column as we will not be deriving any information from that in this code.
#     c.	Remove "desc" column, as at present there is no plan to do text/keyword analysis.
#     d.	Remvoe "collections_12_mths_ex_med", "chargeoff_within_12_mths", and "tax_liens" columns as they only contain "0" or NA values.
#     f.  Remove "title" column as "purpose" column will provided categories if needed.
#     g.  Remove "emp_title" as we are not planning to analyze using emploer's name.
#     h.  Remove "zip_code" as it is partial value and we already have state name in another column. 

length(unique(loan$id)) == nrow(loan$id)  #id columns has no duplicate values.

loan$member_id  <- NULL
loan$url        <- NULL
loan$desc       <- NULL
loan$collections_12_mths_ex_med   <- NULL
loan$chargeoff_within_12_mths     <- NULL
loan$tax_liens  <- NULL
loan$title      <- NULL
loan$emp_title  <- NULL
loan$zip_code   <- NULL

str(loan$revol_util)

#   Step 1.3  - Clean up/format the data
#     a.  Remove the % from the int_rate, revolve_util column values, and convert them to numeric.
#     b.  Remove months from term column and convert values to numeric.
#     c.  Remove text from emp_length column and convert to numeric.
#         NOTE: Keeping it multi step process for simplicity.
#     d.  Remove the loan stauts = current.

loan$int_rate   <- as.numeric(gsub("%", "", loan$int_rate))
loan$revol_util <- as.numeric(gsub("%", "", loan$revol_util))
loan$term       <- as.numeric(gsub(" months", "", loan$term))

loan$emp_length <- gsub("years", "", loan$emp_length)
loan$emp_length <- gsub("year", "", loan$emp_length)
loan$emp_length <- gsub("<", "", loan$emp_length)
loan$emp_length <- gsub("\\+", "", loan$emp_length)
loan$emp_length <- as.numeric(loan$emp_length)

loan <- filter(loan,loan_status != "Current")

loan$issue_d <- as.POSIXlt(paste("01-", loan$issue_d , sep = ""),format = "%d-%b-%y")

colSums(is.na(loan))
#   NOTE: Below columns have NA values present. The NA values will be cleaned only if these are used later in the analysis.
#   emp_length
#   mths_since_last_delinq
#   mths_since_last_record
#   revol_util
#   pub_rec_bankruptcies


#   Step 1.4  - Create derived value columns
#     a.  Create a new column to categorize the income into groups. e.g. <10000, 10-20K, 20-30K etc.
#     b.  Similarly create a new column for int rate groups. e.g. 5, 5-7.5, 7.5-10 etc.

loan$annual_income_slot <- ifelse(loan$annual_inc<10000,10000,
                              ifelse(loan$annual_inc<20000,20000,
                                  ifelse(loan$annual_inc<30000,30000,
                                      ifelse(loan$annual_inc<40000,40000,
                                          ifelse(loan$annual_inc<50000,50000,
                                              ifelse(loan$annual_inc<60000,60000,
                                                  ifelse(loan$annual_inc<70000,70000,
                                                      ifelse(loan$annual_inc<80000,80000,
                                                          ifelse(loan$annual_inc<90000,90000,
                                                              ifelse(loan$annual_inc<100000,100000,
                                                                  ifelse(loan$annual_inc<120000,120000,
                                                                      ifelse(loan$annual_inc<150000,150000,
                                                                          ifelse(loan$annual_inc<175000,175000,200000)))))))))))))


loan$int_rate_slot <- ifelse(loan$int_rate<5,5,
                              ifelse(loan$int_rate<7.5,7.5,
                                 ifelse(loan$int_rate<10,10,
                                    ifelse(loan$int_rate<12.5,12.5,
                                       ifelse(loan$int_rate<15,15,
                                          ifelse(loan$int_rate<17.5,17.5,
                                               ifelse(loan$int_rate<20,20,
                                                  ifelse(loan$int_rate<22.5,22.5,
                                                      ifelse(loan$annual_inc<25,25,27.5)))))))))


################           Data clean up complete.              ####################
#   write.csv(loan, file = "r_clean_loan.csv", row.names = FALSE)
#-------------------------------------------------------------------------------------------#


#   Creating sub-sets based on loan status for future use.

loan_CF <- loan[loan$loan_status== "Charged Off",]
loan_FP <- loan[loan$loan_status== "Fully Paid",]



###################       Data Analysis Begin      ##########################



#   First of all get a idea of counts of both loan types.
temp1 <- loan
temp1$issue_d <- NULL

temp <- dplyr::summarize(group_by(temp1, loan_status), count = n())
ggplot(temp, aes(x=loan_status, y=count)) + geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=count), vjust=3, color="white", size=3.5) + theme_minimal()

rm(temp)



#   Analyze loan term and loan status
ggplot(loan, aes(x=as.factor(term), fill=loan_status)) + geom_bar(position = "fill") +
  labs(x="Term of the Loan",title="Term of the Loan and Loan Status") 


#   Analyze loan grade and loan status
ggplot(loan, aes(x=grade, fill=loan_status)) + geom_bar(position="fill") +
  labs(x="Loan Grade",title="loan grade and loan status")


#   Analyze loan sub-grade and loan status
ggplot(loan, aes(x=sub_grade, fill=loan_status)) + geom_bar(position="fill") +
  labs(x="Loan Sub Grade",title="loan sub-grade and loan status")


#   Analyze loan state and loan status
ggplot(loan, aes(x=addr_state, fill=loan_status)) + geom_bar(stat = "count") + 
  geom_text(stat ='count', aes(label = ..count..), vjust = -1)

# Conclusion -  CA state has the most number of charged off loans but it also has most number of loan issued. So its not
#               necessarily a factor.


#   Analyze emp length and loan status
ggplot(loan, aes(x=emp_length, fill=loan_status)) + geom_bar(position = "fill") +
  labs(x="emp length",title="emp length and loan status")

# Conclusion - Emp Length donesn't seem to have a very clear impact on the loan status.


#   Analyze loan verification and loan status
ggplot(loan, aes(x=verification_status, fill=loan_status)) + geom_bar(position = "fill") +
  labs(x="loan verification",title="loan verification and loan status")

# Conclusion - It seems verified loans have a higher default ratio.

#   Analyze loan purpose and loan status
ggplot(loan, aes(x=purpose, fill=loan_status)) + geom_bar(position = "fill") +
  labs(x="loan purpose",title="loan purpose and loan status")


#   Analyze home ownership and loan status
ggplot(loan,aes(x=loan_status,group= home_ownership))+
  geom_bar(stat="count",aes(y=..prop.., fill = factor(..x..)))+
  geom_text(aes(label= scales::percent(..prop..),y=..prop..),stat = "count" ,vjust=-1)+
  facet_grid(~home_ownership)




#   Analyze emp length and loan status
ggplot(loan,aes(x=as.factor(emp_length),fill=loan_status))+geom_bar(stat = "count")
ggplot(loan,aes(x=as.factor(emp_length),fill=loan_status))+geom_bar(position = "fill")

# Conclusion - emp length of 10 takes most number of loans. But from second graph when we look at each 
#   value, this factor doesn't seem to have a noticable impact on loan status. As all of them have
#   almost similar loan default ratio.



ggplot(loan,aes(x=dti,fill=loan_status))+
  geom_histogram(colour = "black", binwidth = 0.5,position = "fill")+
  labs(x="DTI",title="DTI")+
  labs(fill="Loan Status")+
  labs(y="Proportion")


bin_size = (max(loan$funded_amnt) - min(loan$funded_amnt)) / 50
ggplot(loan,aes(x=funded_amnt,fill=loan_status))+
  geom_histogram(colour = "black", binwidth = bin_size, position = "fill")+
  labs(x="Funded Amount",title="Funded Amount")+
  labs(fill="Loan Status")+
  labs(y="Proportion")



bin_size = (max(loan$annual_inc) - min(loan$annual_inc)) / 50
ggplot(loan,aes(x=annual_inc,fill=loan_status))+
  geom_histogram(colour = "black", binwidth = bin_size, position = "fill")+
  labs(x="Annual Income",title="Annual Income")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

ggplot( loan, aes(x = int_rate_slot, fill = factor(loan_status))) + 
  geom_bar(stat = "count" ) + 
  labs(x ="Interest Rate SLots",title="Total Loans per Slot") + 
  labs(y ="Count of Loans") + labs(fill="Loan Status")


#   Plot charged-off loans with int rate slot to find out which rates get most charged-offs
ggplot( loan_CF, aes(x = int_rate_slot)) + geom_bar(stat = "count" ) + 
  labs(x ="Interest Rate SLots",title="Defaulted Loans per Slot") + 
  labs(y ="Count of Defaulted Loans")




#Total count of different loan status [power law Equation]
loan.status.freq <- plyr::count(loan,"loan_status")
loan.status.freq <- loan.status.freq[order(loan.status.freq$freq),] 

loan.status.freq$rank <- c(1:nrow(loan.status.freq))

loan.status.normal.graph <- ggplot(loan.status.freq,aes(loan_status,freq,stat(rank)))+geom_point()+
                              ggtitle("Frequency Plot")

loan.status.log.graph <- ggplot(loan.status.freq,aes(rank,freq))+geom_point()+
                          scale_y_log10()+geom_line()+ggtitle("powerc Law Plot")


loan.status.pie.chart <- ggplot(loan, aes(x = "",fill=as.factor(loan_status))) +
                          geom_bar(aes(y = (..count..)/sum(..count..))) +
                          scale_y_continuous(labels = scales::percent) +
                          labs(title = "Status Pie Chart", y = "", x = "") +
                          coord_polar("y")+geom_text(aes(y = ((..count..)/sum(..count..)), 
                                 label = scales::percent((..count..)/sum(..count..))), 
                                 stat = "count", position = position_stack(vjust = 0.5)) 

grid.arrange(loan.status.normal.graph, loan.status.log.graph, loan.status.pie.chart, ncol=2)

#14% of the loan borrowers have defaulited




#Mean [median] of total loan amount, total recived principal
median(loan$loan_amnt)
median(loan$total_rec_prncp)
sum(loan$loan_amnt)
sum(loan$total_rec_prncp)

# Due to the defaulters there is a gap in average and sum of total amount lended to the total amount recived.



# Analyzing how interest rate is related to loan status
loan.segmented.int.rate.plot <- ggplot(loan,aes(loan_status,int_rate)) +
                                  geom_boxplot()+ggtitle("Interest Rate Plot")
#From graph it is clear that for charged off loans average interest rates were higher compared to fully paid

#Checking how dti is related to loan status
loan.segmented.dti.plot <- ggplot(loan,aes(loan_status,dti)) +
                              geom_boxplot()+ggtitle("DTI Plot")
#Even from this graph it is evident that loans which are defaulted have people with higher average dti

grid.arrange(loan.segmented.int.rate.plot,loan.segmented.dti.plot,ncol=2)




# Analyzing correlation between loan_amount(Amount requested by borrower),funded_amnt
# (Amount committed by lender) and funded_amnt_inv (Amount committed by investors)
loan.amount.plot <- ggplot(loan, aes(loan_amnt)) + geom_histogram()
loan.lender.amount.plot <- ggplot(loan, aes(loan$funded_amnt)) + geom_histogram()
loan.investor.amt.plot  <- ggplot(loan, aes(funded_amnt_inv)) + geom_histogram()
grid.arrange(loan.amount.plot, loan.lender.amount.plot,loan.investor.amt.plot, nrow=3)
#Graph shows a balance between funding , lending and requested loan amount.



# Finding correlation between number of approved loans and number of defaulters over time
loan.count.month.wise <- aggregate(id~format(issue_d,"%d-%b-%y"),loan,length)
colnames(loan.count.month.wise)<-c("date","count")

loan.def.count.month.wise <- aggregate(id~format(issue_d,"%d-%b-%y"),loan[loan$loan_status=="Charged Off",],length)
colnames(loan.def.count.month.wise)<-c("date","count")

loan.over.time <- ggplot(loan.count.month.wise, aes(x=as.POSIXct(as.POSIXlt(date,format="%d-%b-%y")),y = count))+
                    geom_line(color = "green", size = 1.5) + ggtitle("Loans Given over Time")

loan.defaulter.over.time <- ggplot(loan.def.count.month.wise, aes(x=as.POSIXct(as.POSIXlt(date,format="%d-%b-%y")),y = count))+
                              geom_line(color = "blue", size = 1) + ggtitle("Loans Defaulters over Time")

grid.arrange(loan.over.time, loan.defaulter.over.time, nrow=2)

loan.data.date.wise <- merge(loan.count.month.wise,loan.def.count.month.wise,by='date')
#As number of loans increased number of defaulters also increased and 
#during end of 2011 default count has increased drastically



#Check if delinq_2yrs differentiates default loans from fully paid ones
loan.deliq.count <- ggplot(loan, aes(x=factor(delinq_2yrs))) + geom_bar(stat="count")
loan.deliq.count
loan.deliq.count+facet_grid(.~loan_status)
#Although a large number of fully paid of loans have 0 delinq for 2 years , maximum number of defaults are also for delinq 0



#Checking if annual income would be able to differentiate between paid and default loans
#mean of annual income for each loan status
aggregate(annual_inc~loan_status,loan,mean)
#Median of aannual status for each loan status
aggregate(annual_inc~loan_status,loan,median)
# Annual income doesnt provide any indication that default loans can be predicted.
# Conclusion - No Impact


#Check if verification_status differentiates default loans from fully paid ones
loan.verif.count <- ggplot(loan,aes(loan_status,fill=verification_status)) + geom_bar(position="dodge")
loan.verif.count
#verification_status show similarities in fully paid and default loans
# Conclusion - No Impact

#Check if pub_rec and pub_rec_bankruptcies differentiates default loans from fully paid ones
loan.pubRec.count <- ggplot(loan,aes(loan_status,fill=factor(pub_rec))) + geom_bar(position="dodge")
loan.bankrpt.count <- ggplot(loan,aes(loan_status,fill=factor(pub_rec_bankruptcies))) + geom_bar(position="dodge")
grid.arrange(loan.pubRec.count,loan.bankrpt.count,ncol=2)
# pub_rec and pub_rec_bankruptcies show similarities in fully paid and default loans
# Conclusion - No Impact



#Analysis Result and Hypothesis
#1)As per the analysis, interest rate and dti have higher average for default loans compared to fully paid loans
#interest rate
aggregate(int_rate~loan_status,loan,mean)
aggregate(int_rate~loan_status,loan,median)

# DTI
aggregate(dti~loan_status,loan,mean)
aggregate(dti~loan_status,loan,median)


#2)Also people who have defaulted have lesser Average annual salary comapred to people who have paid fully
aggregate(annual_inc~loan_status,loan,mean)
aggregate(annual_inc~loan_status,loan,median)


#3)Annual inncome against loan amount approved
#Annual income trend
loan.annual.income.trend <- ggplot(loan,aes(x=id,y=annual_inc)) + geom_smooth()

#Approved loan aount trend
loan.approved.amt.trend <- ggplot(loan,aes(x=id,y=loan_amnt)) + geom_smooth()

grid.arrange(loan.annual.income.trend,loan.approved.amt.trend,nrow=2)
#As in the graph even for lesss annual income borrowers high loan amount has been approved.


