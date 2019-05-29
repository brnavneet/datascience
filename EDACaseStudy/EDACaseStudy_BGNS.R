
#Load all the required libraries
require(gmodels)
require(gdata)
require(stringr)
require(ggplot2)
require(dplyr)


#loading dataset
loanhistory<-read.csv("loan.csv",  stringsAsFactors = F)

#Data cleaning

#Fix rows and columns
#Renaming to expand abbrevation
colnames(loanhistory)
names(loanhistory) <- gsub("amnt", "amount", names(loanhistory))
names(loanhistory) <- gsub("inv", "investor", names(loanhistory))
names(loanhistory)[names(loanhistory)=="dti"] <- "Debt_to_income_ratio"
names(loanhistory)[names(loanhistory)=="delinq_2yrs"] <- "delinquency_2yrs"
names(loanhistory) <- gsub("inq", "inquiries", names(loanhistory))
names(loanhistory) <- gsub("acc", "account", names(loanhistory))
names(loanhistory) <- gsub("mths", "months", names(loanhistory))
names(loanhistory) <- gsub("prncp", "principal", names(loanhistory))
names(loanhistory) <- gsub("dti", "Debt-to-income", names(loanhistory))
names(loanhistory)[names(loanhistory)=="last_credit_pull_d"] <- "last_credit_pull_month"
names(loanhistory)[names(loanhistory)=="int_rate"] <- "interest_rate"
names(loanhistory)[names(loanhistory)=="emp_title"] <- "employer"
names(loanhistory)[names(loanhistory)=="issue_d"] <- "issue_date"

#changing this name as inq replacement did it wrongly
names(loanhistory)[names(loanhistory)=="delinquiriesuency_2yrs"] <- "delinquency_2yrs"


#Converting to issue date to Date type

loanhistory$issue_date<-trim(loanhistory$issue_date)
loanhistory$issue_date <- paste("01-", trim(loanhistory$issue_date), sep = "")
loanhistory$issue_date
loanhistory$issue_date<-as.Date(loanhistory$issue_date, format="%d-%b-%y")

#Total varaible 111
#removing all the varaible which have only NA values
loanhistory <- loanhistory[,colSums(is.na(loanhistory))<nrow(loanhistory)]
#57 varaible remianing

#removing all the varaible with 1 value
loanhistory<-Filter(function(x)(length(unique(x))>1), loanhistory)
#51 varaible remianing

#removing all the varaible with vraiance 0 for integer varaible
nums <- unlist(lapply(loanhistory, is.numeric))  
loanhistoryvar<-Filter(function(x)(var(x,na.rm=TRUE)!=0), loanhistory[,nums])
loanhistorycat<-loanhistory[,!nums]
loanhistory<-cbind(loanhistoryvar, loanhistorycat)
#48 varaible remianing



#selected only numeric value of term and  reassigned back to term
loanhistory$term<-str_extract(loanhistory$term, "\\d\\d")
loanhistory$term<- as.factor(loanhistory$term)
#selected only digit and < 1 does not extract, so for those blank assigned 0
loanhistory$Experience<-str_extract(loanhistory$emp_length, "[\\d]*")
loanhistory[loanhistory$Experience=="",]$Experience<- "0.0"
loanhistory$Experience <- as.double(loanhistory$Experience)



#Changing this as Source Verfied is same as Verified
loanhistory[loanhistory$verification_status=="Source Verified",]$verification_status<-"Verified"

#Moving NONE to OTHER
loanhistory[loanhistory$home_ownership=="NONE",]$home_ownership<-"OTHER"

#Changing factor all the significant categorical varaibles
typeof(loanhistory$home_ownership)
loanhistory$home_ownership <- as.factor(loanhistory$home_ownership)
loanhistory$verification_status <- as.factor(loanhistory$verification_status)
loanhistory$loan_status <- as.factor(loanhistory$loan_status)
loanhistory$purpose <- as.factor(loanhistory$purpose)
loanhistory$zip_code <- as.factor(loanhistory$zip_code)

#extracted only number from interest rate
loanhistory$interest_rate<-str_extract(loanhistory$interest_rate, "[\\d]*")
loanhistory$interest_rate <- as.double(loanhistory$interest_rate)

#extracted only number from interest rate
loanhistory$revol_util<-str_extract(loanhistory$revol_util, "[\\d]*")
loanhistory$revol_util <- as.double(loanhistory$revol_util)


#Understanding the data using summary
summary(loanhistory)





#Finding the outliers by drawing plot for all the measurments
#using univaraite analysis

boxplot(loanhistory$loan_amount)
summary(loanhistory$loan_amount) #Not much difference betwenn mean and median

boxplot(loanhistory$interest_rate)
summary(loanhistory$interest_rate) #Not much difference betwenn mean and median


boxplot(loanhistory$installment)
summary(loanhistory$installment) #Not much difference betwenn mean and median

boxplot(loanhistory$annual_inc) 
summary(loanhistory$annual_inc) #Maximum value is outside the thrid quartile


#Removed the 6 milion salary which outlier 

outline_income<-quantile(loanhistory$annual_inc, 0.75) + 1.5 * IQR(loanhistory$annual_inc)

index_cutoff_inc<-which(loanhistory$annual_inc > outline_income)
loanhistory<-loanhistory[-index_cutoff_inc,]

boxplot(loanhistory$annual_inc)

#No plots showing any ouliers except recoveries

plot(loanhistory$Debt_to_income_ratio)
plot(loanhistory$delinquency_2yrs)
plot(loanhistory$inquiries_last_6months )
plot(loanhistory$open_account )
plot(loanhistory$pub_rec )
plot(loanhistory$revol_bal )
plot(loanhistory$revol_util)
plot(loanhistory$total_account)
plot(loanhistory$out_principal)
plot(loanhistory$out_principal_investor)
plot(loanhistory$total_pymnt)
plot(loanhistory$total_pymnt_investor)
plot(loanhistory$total_rec_principal)
plot(loanhistory$total_rec_late_fee)

#but inter qurtile range is 0 so keeing that column as it is
plot(loanhistory$recoveries) 
loanhistory$recoveries
typeof(loanhistory$recoveries)
outline_recoveries<-quantile(loanhistory$recoveries, 0.75) + 1.5 * IQR(loanhistory$recoveries)

plot(loanhistory$collection_recovery_fee)
plot(loanhistory$last_pymnt_amount)
plot(loanhistory$Experience)


#removing column which are not usefull 

loanhistory$id <- NULL
loanhistory$member_id <- NULL
loanhistory$funded_amount <- NULL
loanhistory$funded_amount_investor <- NULL
loanhistory$months_since_last_delinquiries <- NULL


loanhistory$months_since_last_record <- NULL
loanhistory$out_principal <- NULL
loanhistory$out_principal_investor <- NULL
loanhistory$total_pymnt_investor <- NULL
loanhistory$total_rec_late_fee <- NULL
loanhistory$collection_recovery_fee <- NULL
loanhistory$issue_date <- NULL
loanhistory$url <- NULL
loanhistory$desc <- NULL
loanhistory$title <- NULL
loanhistory$earliest_cr_line <- NULL
loanhistory$last_pymnt_d <- NULL
loanhistory$next_pymnt_d <- NULL
loanhistory$last_credit_pull_month <- NULL


#creating interest bin for the analysis and changing that to factor
loanhistory$interest_bin <- cut(loanhistory$interest_rate, 
                                                  breaks = c(0,5, 10, 20, 30), 
                                                  labels = c("0-5", "5-10", "10-20", "20+"), 
                                                  right = FALSE)

loanhistory$interest_bin<-as.factor(loanhistory$interest_bin)

#unique(loanhistory$Debt_to_income_ratio)

#creating dept_to_income bin for the analysis and changing that to factor
loanhistory$Debt_to_income_bin <- cut(loanhistory$Debt_to_income_ratio, 
                                breaks = c(0,0.5, 1,  30), 
                                labels = c("LOw", "MEDIUIM", "HIGH"), 
                                right = FALSE)

loanhistory$Debt_to_income_bin<-as.factor(loanhistory$Debt_to_income_bin)

#creating revol_util bin for the analysis and changing that to factor
loanhistory$revol_util_bin <- cut(loanhistory$revol_util, 
                                      breaks = c(0,30, 50, 70, 80, 90, 100), 
                                      labels = c("0-30", "30-50", "50-70", "70-80", "80-90", "90-100"), 
                                      right = FALSE)

loanhistory$revol_util_bin<-as.factor(loanhistory$revol_util_bin)


#creating annual_inc bin for the analysis and changing that to factor
loanhistory$annual_inc_bin <- cut(loanhistory$annual_inc, 
                                  breaks = c(4000,10000, 25000, 50000, 75000, 100000, 150000), 
                                  labels = c("4K-10K", "10K-25K", "25K-50K", "50K-75K", "75K-100K", "100K+"), 
                                  right = FALSE)

loanhistory$annual_inc_bin<-as.factor(loanhistory$annual_inc_bin)

#creating loan_amount bin for the analysis and changing that to factor
loanhistory$loan_amount_bin <- cut(loanhistory$loan_amount, 
                                  breaks = c(500,2000, 5000, 10000, 20000, 30000, 35000), 
                                  labels = c("0.5K-2K", "2K-5K", "5K-10K", "10K-20K","20K-30K", "30K+"), 
                                  right = FALSE)

loanhistory$loan_amount_bin<-as.factor(loanhistory$loan_amount_bin)

#creating total_account bin for the analysis and changing that to factor
loanhistory$total_account_bin <- cut(loanhistory$total_account, 
                                   breaks = c(0,20, 40, 60, 80, 100), 
                                   labels = c("0-20", "20-40", "40-60", "60-80","80-100"), 
                                   right = FALSE)

loanhistory$total_account_bin<-as.factor(loanhistory$total_account_bin)


#changing inquiries_last_6months to factor
loanhistory$inquiries_last_6months<-as.factor(loanhistory$inquiries_last_6months)


unique(loanhistory$open_account)


#removing the Current from the analysis as it is not clear where it is heading
loanhistory<-loanhistory[loanhistory$loan_status!="Current",]
loanhistory$isdefault<-loanhistory$loan_status == "Charged Off"

summary(loanhistory)

#Segmented univraite analysis 
#Checking the impact of home ownership on default
dataowner<-CrossTable(loanhistory$home_ownership, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
ownergraph<-as.data.frame(dataowner$prop.row)
colnames(ownergraph)<-c("home_ownership","isdefault","Freq")
ownergraph$Freq <- round(ownergraph$Freq, 2)
ownergraph$Freq <- as.double(ownergraph$Freq)
ggplot(ownergraph, aes(x=home_ownership, y=Freq, fill=isdefault , label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#There is no much difference between the ratio, unable to make any deciision

#Checking the impact of grade on default
datagrade<-CrossTable(loanhistory$grade, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
gradegraph<-as.data.frame(datagrade$prop.row)
colnames(gradegraph)<-c("grade","isdefault","Freq")
gradegraph$Freq <- round(gradegraph$Freq, 2)
gradegraph$Freq <- as.double(gradegraph$Freq)
ggplot(gradegraph, aes(x=grade, y=Freq, fill=isdefault , label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#As grade go from A-G chances of defaulting is increasing

#Checking the impact of sub_grade on default
datasubgrade<-CrossTable(loanhistory$sub_grade, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradegraph<-as.data.frame(datasubgrade$prop.row)
colnames(subgradegraph)<-c("sub_grade","isdefault","Freq")
subgradegraph$Freq <- round(subgradegraph$Freq, 2)
subgradegraph$Freq <- as.double(subgradegraph$Freq)
ggplot(subgradegraph, aes(x=sub_grade, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
subgradegraph[subgradegraph$Freq > 0.4 & subgradegraph$isdefault == TRUE  ,]$sub_grade
#F5 and G3 have more chance of defaulting (40%)

#Checking the impact of verification_status on default
datasubveri<-CrossTable(loanhistory$verification_status, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradeveri<-as.data.frame(datasubveri$prop.row)
colnames(subgradeveri)<-c("verification_status","isdefault","Freq")
subgradeveri$Freq <- round(subgradeveri$Freq, 2)
ggplot(subgradeveri, aes(x=verification_status, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#Cannot take any decision all ratio are near by


#Checking the impact of purpose on default
datasubpurpose<-CrossTable(loanhistory$purpose, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradepurpose<-as.data.frame(datasubpurpose$prop.row)
colnames(subgradepurpose)<-c("purpose","isdefault","Freq")
subgradepurpose$Freq <- round(subgradepurpose$Freq, 2)
ggplot(subgradepurpose, aes(x=purpose, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

subgradepurpose[subgradepurpose$Freq > 0.2 & subgradepurpose$isdefault == TRUE  ,]$purpose  
#In Small Business 28% are defaulting

#Checking the impact of zip_code on default
datasubzip_code<-CrossTable(loanhistory$zip_code, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradezip_code<-as.data.frame(datasubzip_code$prop.row)
colnames(subgradezip_code)<-c("zip_code","isdefault","Freq")
subgradezip_code$Freq <- round(subgradezip_code$Freq, 2)
ggplot(subgradezip_code, aes(x=zip_code, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

subgradezip_code[subgradezip_code$Freq > 0.9 & subgradezip_code$isdefault == TRUE  ,]$zip_code  
#In these zip codes, 90% cases are defaulting. 094xx 373xx 385xx 663xx 669xx 689xx 833xx 999xx


#Checking the impact of state on default
datasubaddr_state<-CrossTable(loanhistory$addr_state, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradeaddr_state<-as.data.frame(datasubaddr_state$prop.row)
colnames(subgradeaddr_state)<-c("addr_state","isdefault","Freq")
subgradeaddr_state$Freq <- round(subgradeaddr_state$Freq, 2)
ggplot(subgradeaddr_state, aes(x=addr_state, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

subgradeaddr_state[subgradeaddr_state$Freq > 0.4 & subgradeaddr_state$isdefault == TRUE  ,]$addr_state
#NE (Nebraska) more than 40% are defaulting, But number of records in NE are only 5. It may not be sufficient for conclusion

#bivraite analysis
neloanhitroy<-loanhistory[loanhistory$addr_state=="NE",]
#In Nebraska, checking which subgrade is defaulting 
datasubgrade<-CrossTable(neloanhitroy$sub_grade, neloanhitroy$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradegraph<-as.data.frame(datasubgrade$prop.row)
colnames(subgradegraph)<-c("sub_grade","isdefault","Freq")
subgradegraph$Freq <- round(subgradegraph$Freq, 2)
ggplot(subgradegraph, aes(x=sub_grade, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

subgradegraph[subgradegraph$Freq > 0.4 & subgradegraph$isdefault == TRUE  ,]$sub_grade 
#Found that B4, D4 are defaulting more than 40%

#In Nebraska, checking which purpose is defaulting 
datasubpurpose<-CrossTable(neloanhitroy$purpose, neloanhitroy$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradepurpose<-as.data.frame(datasubpurpose$prop.row)
colnames(subgradepurpose)<-c("purpose","isdefault","Freq")
subgradepurpose$Freq <- round(subgradepurpose$Freq, 2)
ggplot(subgradepurpose, aes(x=purpose, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))


subgradegraph[subgradegraph$Freq > 0.2 & subgradegraph$isdefault == TRUE  ,]$purpose 
#In Small business where we could see more than 25% defaulting

#Checking the purpose of the defaulting in sub grade B4 and D4
gradeneloanhitroy<-neloanhitroy[neloanhitroy$sub_grade=="B4" | neloanhitroy$sub_grade=="D4", ]

datasubpurpose<-CrossTable(gradeneloanhitroy$purpose, gradeneloanhitroy$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradepurpose<-as.data.frame(datasubpurpose$prop.row)
colnames(subgradepurpose)<-c("purpose","isdefault","Freq")
subgradepurpose$Freq <- round(subgradepurpose$Freq, 2)
ggplot(subgradepurpose, aes(x=purpose, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

#That remains small business

smallbuloanhitroy<-loanhistory[loanhistory$purpose=="small_business",]


#Finding which sub grade is causing default in small business
datasubgrade<-CrossTable(smallbuloanhitroy$sub_grade, smallbuloanhitroy$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
subgradegraph<-as.data.frame(datasubgrade$prop.row)
colnames(subgradegraph)<-c("sub_grade","isdefault","Freq")
subgradegraph$Freq <- round(subgradegraph$Freq, 2)
ggplot(subgradegraph, aes(x=sub_grade, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

subgradegraph[subgradegraph$Freq > 0.5 & subgradegraph$isdefault == TRUE  ,]$sub_grade
#In F5 G2 G4 G5 sub grade, 50% of are defaulting


#Checking is interests are cause for defaulting
datainterest<-CrossTable(loanhistory$interest_bin, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
datainterest<-as.data.frame(datainterest$prop.row)
colnames(datainterest)<-c("interest_bin","isdefault","Freq")
datainterest$Freq <- round(datainterest$Freq, 2)
ggplot(datainterest, aes(x=interest_bin, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

datainterest[datainterest$Freq > 0.3 & datainterest$isdefault == TRUE  ,]$interest_bin 
#If Interset rate are higher than 20+, then in this case around 40% defaulting


#Checking In which  Debt to income ratio defaulting  more
datadti<-CrossTable(loanhistory$Debt_to_income_bin, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
datadti<-as.data.frame(datadti$prop.row)
colnames(datadti)<-c("Debt_to_income_bin","isdefault","Freq")
datadti$Freq <- round(datadti$Freq, 2)
ggplot(datadti, aes(x=Debt_to_income_bin, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

datadti[datadti$Freq > 0.15 & datadti$isdefault == TRUE  ,]$Debt_to_income_bin 
#Not very clear cannot take any decision




#Checking revolving  utilization has any impact on default
datarevutil<-CrossTable(loanhistory$revol_util_bin, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
datarevutil<-as.data.frame(datarevutil$prop.row)
colnames(datarevutil)<-c("revol_util_bin","isdefault","Freq")
datarevutil$Freq <- round(datarevutil$Freq, 2)
ggplot(datarevutil, aes(x=revol_util_bin, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

datarevutil[datarevutil$Freq > 0.2 & datarevutil$isdefault == TRUE  ,]$revol_util_bin 
# 90% rvolving utilization tend to increase defaulting, but is only around 20%

#Checking income has any impact on default
dataincome<-CrossTable(loanhistory$annual_inc_bin, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
dataincome<-as.data.frame(dataincome$prop.row)
colnames(dataincome)<-c("annual_inc_bin","isdefault","Freq")
dataincome$Freq <- round(dataincome$Freq, 2)
ggplot(dataincome, aes(x=annual_inc_bin, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

dataincome[dataincome$Freq >= 0.2 & dataincome$isdefault == TRUE  ,]$annual_inc_bin 
# Income of 10K-25K have tendency to default but it is only 20%

#Checking inquiries in last 6 months has any impact on default
datainq6months<-CrossTable(loanhistory$inquiries_last_6months, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
datainq6months<-as.data.frame(datainq6months$prop.row)
colnames(datainq6months)<-c("inquiries_last_6months","isdefault","Freq")
datainq6months$Freq <- round(datainq6months$Freq, 2)
ggplot(datainq6months, aes(x=inquiries_last_6months, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

datainq6months[datainq6months$Freq > 0.25 & datainq6months$isdefault == TRUE  ,]$inquiries_last_6months 
#  In last 6 months candidtaes are inquiring credit more than 6 times tend to default


#Checking delinquency in last 2 years has any impact on default

datadlinq2Yrs<-CrossTable(loanhistory$delinquency_2yrs, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
datadlinq2Yrs<-as.data.frame(datadlinq2Yrs$prop.row)
colnames(datadlinq2Yrs)<-c("delinquency_2yrs","isdefault","Freq")
datadlinq2Yrs$Freq <- round(datadlinq2Yrs$Freq, 2)
ggplot(datadlinq2Yrs, aes(x=delinquency_2yrs, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))

unique(loanhistory$open_account)
#If candidtae had delinquented 8 times then around 50% are defaulting
#Surprisingnly delinquented more than 9 to 11 does not show any defaults.
#It may be cecuase of data issue or insuffiecient data

#Checking open_account has any impact on default

dataopenaccount<-CrossTable(loanhistory$open_account, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
dataopenaccount<-as.data.frame(dataopenaccount$prop.row)
colnames(dataopenaccount)<-c("open_account","isdefault","Freq")
dataopenaccount$Freq <- round(dataopenaccount$Freq, 2)
ggplot(dataopenaccount, aes(x=open_account, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#By lookign at the graph it shows that if more than 25 credit lines are open then it is turning to be defaulted
#But beacuse some data issue finding few cases without any default

#Checking total current credit line has any impact on default
datatotalaccount<-CrossTable(loanhistory$total_account_bin, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
datatotalaccount<-as.data.frame(datatotalaccount$prop.row)
colnames(datatotalaccount)<-c("total_account_bin","isdefault","Freq")
datatotalaccount$Freq <- round(datatotalaccount$Freq, 2)
ggplot(datatotalaccount, aes(x=total_account_bin, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#Cannot take any decision Not much difference

#Checking public records has any impact on default

datapubrec<-CrossTable(loanhistory$pub_rec, loanhistory$isdefault,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
datapubrec<-as.data.frame(datapubrec$prop.row)
colnames(datapubrec)<-c("pub_rec","isdefault","Freq")
datapubrec$Freq <- round(datapubrec$Freq, 2)
ggplot(datapubrec, aes(x=pub_rec, y=Freq, fill=isdefault, label=Freq))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#It seems some issue in data, as Number of derogatory public records 1,2 looks bad, 3 and 4 looks good
#Ideally more number would impact on defaulting

#Analyzing the loss
loanhistorydefault<-loanhistory[loanhistory$loan_status == "Charged Off",]

loanhistorydefault$losstobank<-NA

typeof(loanhistory$loan_amount)
typeof(loanhistory$total_pymnt)
loanhistorydefault$losstobank <- loanhistorydefault$loan_amount - (as.numeric(loanhistorydefault$term) * loanhistorydefault$installment)

totalloss <- sum(loanhistorydefault$losstobank)
#Total loss because of defaulting is huge, it is around 61 million

#Analyzing is ownership any impact on loss because of defaulting
ratiolosshomeowner<-loanhistorydefault %>% group_by(home_ownership) %>% summarise(percentage_loss=sum(losstobank)/totalloss)
ratiolosshomeowner$percentage_loss <- round(ratiolosshomeowner$percentage_loss, 2)
ratiolosshomeowner$percentage_loss <- as.double(ratiolosshomeowner$percentage_loss)

ggplot(ratiolosshomeowner, aes(x=home_ownership, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#User has MORTGAGE their proerty the tendency of defaulting is high 47% and even for rented which is around 45%

#Analyzing is purpose has any impact on loss because of defaulting
ratiolosspurpose<-loanhistorydefault %>% group_by(purpose) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosspurpose$percentage_loss <- round(ratiolosspurpose$percentage_loss, 2)
ratiolosspurpose$percentage_loss <- as.double(ratiolosspurpose$percentage_loss)
ggplot(ratiolosspurpose, aes(x=purpose, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
# debit_consolidation (55.5%) is contributing around 55% of loss, though number wise there is no sign but when it comes revenue this is major

#Analyzing is grade has any impact on loss because of defaulting
ratiolossgrade<-loanhistorydefault %>% group_by(grade) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolossgrade$percentage_loss <- round(ratiolossgrade$percentage_loss, 2)
ratiolossgrade$percentage_loss <- as.double(ratiolossgrade$percentage_loss)
ggplot(ratiolossgrade, aes(x=grade, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#Not much veries, unable to take any decision

#Analyzing is sub grade has any impact on loss because of defaulting
ratiolosssubgrade<-loanhistorydefault %>% group_by(sub_grade) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosssubgrade$percentage_loss <- round(ratiolosssubgrade$percentage_loss, 2)
ratiolosssubgrade$percentage_loss <- as.double(ratiolosssubgrade$percentage_loss)
ggplot(ratiolosssubgrade, aes(x=sub_grade, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#unable to take decision as contribution is very less

#Analyzing is interest has any impact on loss because of defaulting
ratiolosinterest_bin<-loanhistorydefault %>% group_by(interest_bin) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosinterest_bin$percentage_loss <- round(ratiolosinterest_bin$percentage_loss, 2)
ratiolosinterest_bin$percentage_loss <- as.double(ratiolosinterest_bin$percentage_loss)
ggplot(ratiolosinterest_bin, aes(x=interest_bin, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#If the interest rate is 10-20% catgeory loss is around 82% 

#Analyzing is Debt_to_income has any impact on loss because of defaulting
loanhistorydefault$Debt_to_income_bin
ratiolosdti<-loanhistorydefault %>% group_by(Debt_to_income_bin) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosdti$percentage_loss <- round(ratiolosdti$percentage_loss, 2)
ratiolosdti$percentage_loss <- as.double(ratiolosdti$percentage_loss)
ggplot(ratiolosdti, aes(x=Debt_to_income_bin, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
# if Debt_to_income is 10-20 loss more than 52% beaucse of defaulting

#Analyzing is revolve utilization has any impact on loss because of defaulting
ratiolosrevol_util_bin<-loanhistorydefault %>% group_by(revol_util_bin) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosrevol_util_bin$percentage_loss <- round(ratiolosrevol_util_bin$percentage_loss, 2)
ratiolosrevol_util_bin$percentage_loss <- as.double(ratiolosrevol_util_bin$percentage_loss)
ggplot(ratiolosrevol_util_bin, aes(x=revol_util_bin, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#unable to take decision not much difference

#Analyzing is anuual income has any impact on loss because of defaulting
ratiolosrevol_annual_inc_bin<-loanhistorydefault %>% group_by(annual_inc_bin) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosrevol_annual_inc_bin$percentage_loss <- round(ratiolosrevol_annual_inc_bin$percentage_loss, 2)
ratiolosrevol_annual_inc_bin$percentage_loss <- as.double(ratiolosrevol_annual_inc_bin$percentage_loss)
ggplot(ratiolosrevol_annual_inc_bin, aes(x=annual_inc_bin, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#Income having 50K-70K contrubuting 36% loss beacuse of defaulting 


#Analyzing is anuual income has any impact on loss because of defaulting
ratiolosrevol_inq_6months<-loanhistorydefault %>% group_by(inquiries_last_6months) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosrevol_inq_6months$percentage_loss <- round(ratiolosrevol_inq_6months$percentage_loss, 2)
ratiolosrevol_inq_6months$percentage_loss <- as.double(ratiolosrevol_inq_6months$percentage_loss)
ggplot(ratiolosrevol_inq_6months, aes(x=inquiries_last_6months, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#Strange result no enquiries of credit line showing 40% loss

#Analyzing is delinquency_2yrs has any impact on loss because of defaulting
ratiolosrevol_dinq_2yrs<-loanhistorydefault %>% group_by(delinquency_2yrs) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosrevol_dinq_2yrs$percentage_loss <- round(ratiolosrevol_dinq_2yrs$percentage_loss, 2)
ratiolosrevol_dinq_2yrs$percentage_loss <- as.double(ratiolosrevol_dinq_2yrs$percentage_loss)
ggplot(ratiolosrevol_dinq_2yrs, aes(x=delinquency_2yrs, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#strange result wihtout no deliquency showing 88% loss

#Analyzing is open_account has any impact on loss because of defaulting
ratiolosrevol_open_account<-loanhistorydefault %>% group_by(open_account) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosrevol_open_account$percentage_loss <- round(ratiolosrevol_open_account$percentage_loss, 2)
ratiolosrevol_open_account$percentage_loss <- as.double(ratiolosrevol_open_account$percentage_loss)
ggplot(ratiolosrevol_open_account, aes(x=open_account, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#no decision, contributing very less percentage

#Analyzing is totalaccount has any impact on loss because of defaulting
loanhistorydefault$total_account_bin
ratiolosrevol_total_account_bin<-loanhistorydefault %>% group_by(total_account_bin) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosrevol_total_account_bin$percentage_loss <- round(ratiolosrevol_total_account_bin$percentage_loss, 2)
ratiolosrevol_total_account_bin$percentage_loss <- as.double(ratiolosrevol_total_account_bin$percentage_loss)
ggplot(ratiolosrevol_total_account_bin, aes(x=total_account_bin, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#if the current opened credit line <40 is causing major loss, this is also surprise

#Analyzing is pub_rec has any impact on loss because of defaulting
ratiolosrevol_pub_rec<-loanhistorydefault %>% group_by(pub_rec) %>% summarise(percentage_loss=sum(losstobank)/totalloss) 
ratiolosrevol_pub_rec$percentage_loss <- round(ratiolosrevol_pub_rec$percentage_loss, 2)
ratiolosrevol_pub_rec$percentage_loss <- as.double(ratiolosrevol_pub_rec$percentage_loss)
ggplot(ratiolosrevol_pub_rec, aes(x=pub_rec, y=percentage_loss, label=percentage_loss))+ geom_col() + geom_text( position = position_stack(vjust = 0.5))
#surpise continue as 92% loss from  without any public record

#ggplot(loanhistorydefault,aes(x=home_ownership,fill=as.factor(delinquency_2yrs))) + geom_bar(position="dodge")
#ggplot(loan,aes(x=dti_bin,fill=as.factor(delinq_2yrs))) + geom_bar(position="dodge")

#writing to CSV for other analysis
write.csv(loanhistory,"processed.csv")

#Some more bivaraite analysis

#Annua income vs DTI for home ownership
ggplot(loanhistorydefault,aes(x=annual_inc,y=Debt_to_income_bin,color=home_ownership)) + geom_jitter() + ylab("Debt to Income Ratio") + xlab("Annual Income")

#Annua revol_bal vs revol_util for loan status
ggplot(loanhistory,aes(x=revol_bal,y=revol_util,col=loan_status)) + geom_point() + geom_smooth()



#bivraite analysis for continious varaible using correlation



cor(loanhistorydefault$interest_rate,loanhistorydefault$losstobank) #37% means not strong corelation exist

cor(loanhistorydefault$installment,loanhistorydefault$losstobank) #installment has fairly strong correlation with loss

cor(loanhistorydefault$annual_inc,loanhistorydefault$losstobank) #37% means not strong corelation exist

cor(loanhistorydefault$loan_amount,loanhistorydefault$losstobank) #As loan amount increases loss to bank also increases because of default

cor(loanhistorydefault$Debt_to_income_ratio,loanhistorydefault$losstobank) #No corelation at all

cor(loanhistorydefault$delinquency_2yrs,loanhistorydefault$losstobank) #negative corelation but not very strong though

cor(loanhistorydefault$total_account,loanhistorydefault$losstobank) #24% means not strong corelation exist

revutilframe<- loanhistorydefault[!(is.na(loanhistorydefault$revol_util)),]

cor(revutilframe$revol_util,revutilframe$losstobank) #4.8%, not corelated

cor(loanhistorydefault$revol_bal,loanhistorydefault$losstobank) #26% means not strong corelation exist

cor(loanhistorydefault$revol_bal,loanhistorydefault$losstobank)#26% means not strong corelation exist


cor(as.numeric(loanhistorydefault$term), loanhistorydefault$losstobank) #41% not strong corelation

cor(loanhistorydefault$recoveries, loanhistorydefault$losstobank) #No only 30%

cor(loanhistorydefault$total_pymnt, loanhistorydefault$losstobank) #No only 27%

cor(loanhistorydefault$total_rec_principal, loanhistorydefault$losstobank) # No only 7%

cor(loanhistorydefault$total_rec_int, loanhistorydefault$losstobank) # No only 39%

bankrec<-loanhistorydefault[!is.na(loanhistorydefault$pub_rec_bankruptcies),]
cor(bankrec$pub_rec_bankruptcies, bankrec$losstobank) # Not corelated

