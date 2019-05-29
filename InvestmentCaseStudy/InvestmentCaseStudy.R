
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("countrycode")
#install.packages("stringr")

require(countrycode)
require(tidyr)
require(dplyr)
require(stringr)

#-------------------------------------------------------------------------------------------------------------------
#Checkpoint 1
#-------------------------------------------------------------------------------------------------------------------

#Question 1: How many unique companies are present in rounds2? Ans: 66368

round2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
round2$company_permalink <- str_to_lower(round2$company_permalink, locale = "en")
unique_round2 <- length(unique(round2$company_permalink))
unique_round2

#Question 2: How many unique companies are present in the companies file? Ans: 66368
#66368 and total no of rows in companies dataset is 66368

companies <- read.delim("companies.txt", sep="\t", stringsAsFactors = FALSE)
companies$permalink <- str_to_lower(companies$permalink, locale = "en")
unique_companies <- length(unique(companies$permalink))
unique_companies

#Question 3: In the companies data frame, which column can be used as the unique key for each company? 
#Write the name of the column. Ans: permalink 

colnames(companies)[1] 

#Question 4: Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.  Ans: N

company_diff <- length(which(!(unique(round2$company_permalink) %in% companies$permalink)=="TRUE"))
if (company_diff == 0) print("N") else print("Y")

# Company_diff = 0, so we can conclude that there are no companies in the rounds2 file which are not present in companies. 

#Question 5:Merge the two data frames so that all variables (columns) in the companies frame are added to
#the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ? Ans: 114949

master_frame <- merge(x = companies,y = round2, by.x = "permalink", by.y = "company_permalink", all = TRUE)
nrow(master_frame)

#write.csv(master_frame, "master_frame.csv",row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------------
#Cehckpoint 2
#-------------------------------------------------------------------------------------------------------------------

#Question 1:Average funding amount of venture type Ans: 11748949

avg_funding_type <- aggregate(raised_amount_usd~funding_round_type, master_frame, mean)
avg_funding_venture <- avg_funding_type[avg_funding_type$funding_round_type == "venture",]
avg_funding_venture[1,2]

#Question 2:Average funding amount of angel type  Ans: 958694.5

avg_funding_angel <- avg_funding_type[avg_funding_type$funding_round_type == "angel",]
avg_funding_angel[1,2]

#Question 3:Average funding amount of seed type  Ans: 719818

avg_funding_seed <- avg_funding_type[avg_funding_type$funding_round_type == "seed",]
avg_funding_seed[1,2]

#Question 4:Average funding amount of private equity type  Ans: 73308593

avg_funding_private_equity <- avg_funding_type[avg_funding_type$funding_round_type == "private_equity",]
avg_funding_private_equity[1,2]

#Question 5:Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
#which investment type is the most suitable for them? Ans: venture 11748949.

# funding type selection from either of angel,seed, venture, private equity
# & funding type selection with investment amount > 5000000
# & funding type selection with investment amount < 15000000


most_suitable_investment_spark_fund <- avg_funding_type[(avg_funding_type$funding_round_type %in% c("venture","seed","angel","private_equity")) & (avg_funding_type$raised_amount_usd >= 5000000 & avg_funding_type$raised_amount_usd <= 15000000),]
most_suitable_investment_spark_fund[1,1]

#-------------------------------------------------------------------------------------------------------------------
#Cehckpoint 3
#-------------------------------------------------------------------------------------------------------------------

# Investments data frame towards venture funding round type
venture_frame <- subset(master_frame,master_frame$funding_round_type=="venture")
venture_frame <- filter(venture_frame,country_code!='')

# Total Fund raised country wise 
total_fund_country <- aggregate(raised_amount_usd~country_code, venture_frame, sum, na.action = na.omit)


#group by country code
group_by_country<-group_by(venture_frame,country_code)

#summarise based on count of total no of investments received by the country.
total_investments_count_country <-summarise(group_by_country,investments_count = length(country_code))

#merge the two data frames
fund_raised_frame<-merge(total_fund_country, total_investments_count_country,by="country_code")
fund_raised_desc_frame<-arrange(fund_raised_frame, desc(raised_amount_usd))


#select the top9 countries from the above list.
top9<-fund_raised_desc_frame[1:9,]

country <- countrycode(top9$country_code, "iso3c", "country.name", warn = FALSE)

top9$country <- country


#Question 1:Top English speaking country Ans: United States

top9$country[1]

#Question 2:Second English speaking country Ans: United Kingdom

top9$country[3]

#Question 3:Third English speaking country Ans: India

top9$country[4]


#-------------------------------------------------------------------------------------------------------------------
#Cehckpoint 4
#-------------------------------------------------------------------------------------------------------------------

#Sector Analysis 1#

#Loading mapping dataframe
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)
mapping<-mapping[-1,]
mapping<-mapping[,-3]

#Replace "0" (i.e, Zero) with text "na" as part of data cleaning operation for category_list column in mapping frame 
mapping$category_list<-sub("0","na",mapping$category_list)
#"Enterprise 2.0" became "Enterprise 2.na" in the previous step, corrected it in the below step.
mapping$category_list[which(mapping$category_list=="Enterprise 2.na")]<-"Enterprise 2.0"

mapping_long <- gather(mapping,main_sector,value,2:9)
sector_wise_mapping <- mapping_long[!(mapping_long$value == 0),]
sector_wise_mapping <- sector_wise_mapping[, -3]

#changing name from category_list to primary_sector in sector_wise_mapping
names(sector_wise_mapping)[1]<-paste("primary_sector")

#rename main sectors correctly.

sector_wise_mapping$main_sector[which(sector_wise_mapping$main_sector=="Automotive...Sports")]<-"Automotive & Sports"
sector_wise_mapping$main_sector[which(sector_wise_mapping$main_sector=="Cleantech...Semiconductors")]<-"Cleantech / Semiconductors"
sector_wise_mapping$main_sector[which(sector_wise_mapping$main_sector=="News..Search.and.Messaging")]<-"News, Search and Messaging"
sector_wise_mapping$main_sector[which(sector_wise_mapping$main_sector=="Social..Finance..Analytics..Advertising")]<-"Social, Finance, Analytics, Advertising"

#creating a new column named primary_sector in venture_frame 
master_frame$primary_sector <- str_split(master_frame$category_list,"\\|",simplify = TRUE)[,1]
master_frame <- filter(master_frame,country_code!='')

# sectors invested for venture funding type 
master <- merge(master_frame,sector_wise_mapping,by="primary_sector")

#write to csv for analysis in tableau
write.csv(master, "master.csv",row.names = FALSE)

#Sector Analysis 2#

master_venture <- subset(master, master$funding_round_type == "venture")
master_venture_5m_15m <- subset(master_venture,master_venture$raised_amount_usd >= 5000000 & master_venture$raised_amount_usd <= 15000000 )

D1<-subset(master_venture_5m_15m,master_venture_5m_15m$country_code=="USA")
D2<-subset(master_venture_5m_15m,master_venture_5m_15m$country_code=="GBR")
D3<-subset(master_venture_5m_15m,master_venture_5m_15m$country_code=="IND")

#D1 Group by main_sector along with count of investments and total investment made.

D1_group_by <- group_by(D1,main_sector)
D1_count <- summarise(D1_group_by,total_no_of_investments = length(main_sector))
D1_sum <- summarise(D1_group_by, total_amount_invested = sum(raised_amount_usd))
D1_sectorwise_investments <- merge(D1_count, D1_sum, by="main_sector")
D1_sectorwise_investments_desc <- arrange(D1_sectorwise_investments,desc(total_no_of_investments))

#D1 contains all the columns of the master_frame along with the primary sector and the main sector, 
#& the total number (or count) of investments for each main sector & The total amount invested in each main sector  
D1 <- merge(D1, D1_sectorwise_investments, by = "main_sector")


#D2 Group by main_sector along with count of investments and total investment made.

D2_group_by <- group_by(D2,main_sector)
D2_count <- summarise(D2_group_by,total_no_of_investments = length(main_sector))
D2_sum <- summarise(D2_group_by, total_amount_invested = sum(raised_amount_usd))
D2_sectorwise_investments <- merge(D2_count, D2_sum, by="main_sector")
D2_sectorwise_investments_desc <- arrange(D2_sectorwise_investments,desc(total_no_of_investments))

#D2 contains all the columns of the master_frame along with the primary sector and the main sector, 
#& the total number (or count) of investments for each main sector & The total amount invested in each main sector  
D2 <- merge(D2, D2_sectorwise_investments, by = "main_sector")

#D3 Group by main_sector along with count of investments and total investment made.

D3_group_by <- group_by(D3,main_sector)
D3_count <- summarise(D3_group_by,total_no_of_investments = length(main_sector))
D3_sum <- summarise(D3_group_by, total_amount_invested = sum(raised_amount_usd))
D3_sectorwise_investments <- merge(D3_count, D3_sum, by="main_sector")
D3_sectorwise_investments_desc <- arrange(D3_sectorwise_investments,desc(total_no_of_investments))

#D3 contains all the columns of the master_frame along with the primary sector and the main sector, 
#& the total number (or count) of investments for each main sector & The total amount invested in each main sector  
D3 <- merge(D3, D3_sectorwise_investments, by = "main_sector")

#-------------------------------------------------------------------------------------------------------------------#

#Question 1:Total number of Investments (count)

# Country 1, Ans: 12012
nrow(D1)

# Country 2, Ans: 619
nrow(D2)

# Country 3, Ans: 328
nrow(D3)

#-------------------------------------------------------------------------------------------------------------------#

#Question 2:Total amount of investment (USD)

# Country 1, Ans: 107318294664
sum(D1$raised_amount_usd)

# Country 2, Ans: 5365228300
sum(D2$raised_amount_usd)

# Country 3, Ans: 2949543602
sum(D3$raised_amount_usd)

#-------------------------------------------------------------------------------------------------------------------#

#Question 3:Top Sector name (no. of investment-wise)

#Country c1, Ans: Others
D1_top_sector <- D1_sectorwise_investments_desc[1,1]
D1_top_sector

#Country c2, Ans: Others
D2_top_sector <- D2_sectorwise_investments_desc[1,1]
D2_top_sector

#Country c3, Ans: Others
D3_top_sector <- D3_sectorwise_investments_desc[1,1]
D3_top_sector

#-------------------------------------------------------------------------------------------------------------------#

#Question 4:Second Sector name (no. of investment-wise)

#Country c1, Ans: Social, Finance, Analytics, Advertising
D1_2nd_top_sector <- D1_sectorwise_investments_desc[2,1]
D1_2nd_top_sector

#Country c2, Ans: Social, Finance, Analytics, Advertising
D2_2nd_top_sector <- D2_sectorwise_investments_desc[2,1]
D2_2nd_top_sector

#Country c3, Ans: Social, Finance, Analytics, Advertising
D3_2nd_top_sector <- D3_sectorwise_investments_desc[2,1]
D3_2nd_top_sector

#-------------------------------------------------------------------------------------------------------------------#

#Question 5:Third Sector name (no. of investment-wise)

#Country c1, Ans: Cleantech / Semiconductors
D1_3rd_top_sector <- D1_sectorwise_investments_desc[3,1]
D1_3rd_top_sector

#Country c2, Ans: Cleantech / Semiconductors
D2_3rd_top_sector <- D2_sectorwise_investments_desc[3,1]
D2_3rd_top_sector

#Country c3, Ans: News, Search and Messaging
D3_3rd_top_sector <- D3_sectorwise_investments_desc[3,1]
D3_3rd_top_sector

#-------------------------------------------------------------------------------------------------------------------#

#Question 6:Number of investments in top sector (3)

#Country c1, Ans: 2950
D1_top_sector_no_of_investments <- D1_sectorwise_investments_desc[1,2]
D1_top_sector_no_of_investments

#Country c2, Ans: 147
D2_top_sector_no_of_investments <- D2_sectorwise_investments_desc[1,2]
D2_top_sector_no_of_investments

#Country c3, Ans: 110
D3_top_sector_no_of_investments <- D3_sectorwise_investments_desc[1,2]
D3_top_sector_no_of_investments

#-------------------------------------------------------------------------------------------------------------------#

#Question 7:Number of investments in second sector (4))

#Country c1, Ans: 2714
D1_2nd_sector_no_of_investments <- D1_sectorwise_investments_desc[2,2]
D1_2nd_sector_no_of_investments

#Country c2, Ans: 133
D2_2nd_sector_no_of_investments <- D2_sectorwise_investments_desc[2,2]
D2_2nd_sector_no_of_investments

#Country c3, Ans: 60
D3_2nd_sector_no_of_investments <- D3_sectorwise_investments_desc[2,2]
D3_2nd_sector_no_of_investments

#-------------------------------------------------------------------------------------------------------------------#

#Question 8:Number of investments in third sector (5)

#Country c1, Ans: 2300
D1_3rd_sector_no_of_investments <- D1_sectorwise_investments_desc[3,2]
D1_3rd_sector_no_of_investments

#Country c2, Ans: 128
D2_3rd_sector_no_of_investments <- D2_sectorwise_investments_desc[3,2]
D2_3rd_sector_no_of_investments

#Country c3, Ans: 52
D3_3rd_sector_no_of_investments <- D3_sectorwise_investments_desc[3,2]
D3_3rd_sector_no_of_investments


#-------------------------------------------------------------------------------------------------------------------#

#Question 9:For point 3 (top sector count-wise), which company received the highest investment?

# Country 1 = United States, Ans: Virtustream

s1_c1 <- subset(D1,D1$main_sector == D1_top_sector)
s1_c1_group_by <- group_by(s1_c1, permalink)
s1_c1_sum <- summarise(s1_c1_group_by, sum(raised_amount_usd))
s1_c1_sum_arrange <- arrange(s1_c1_sum, desc(`sum(raised_amount_usd)`))

#Get top company key
s1_c1_top_company <- toString(s1_c1_sum_arrange[1,1])

#Get top company name
c1_top_sector_top_company_name <- companies[which(companies$permalink == s1_c1_top_company), "name"]
c1_top_sector_top_company_name

# Country 2 = United Kingdom, Ans: Electric Cloud

s2_c2 <- subset(D2,D2$main_sector == D2_top_sector)
s2_c2_group_by <- group_by(s2_c2, permalink)
s2_c2_sum <- summarise(s2_c2_group_by, sum(raised_amount_usd))
s2_c2_sum_arrange <- arrange(s2_c2_sum, desc(`sum(raised_amount_usd)`))

#Get top company key
s2_c2_top_company <- toString(s2_c2_sum_arrange[1,1])

#Get top company name
c2_top_sector_top_company_name <- companies[which(companies$permalink == s2_c2_top_company), "name"]
c2_top_sector_top_company_name

# Country 3 = India, Ans:FirstCry.com

s3_c3 <- subset(D3,D3$main_sector == D3_top_sector)
s3_c3_group_by <- group_by(s3_c3, permalink)
s3_c3_sum <- summarise(s3_c3_group_by, sum(raised_amount_usd))
s3_c3_sum_arrange <- arrange(s3_c3_sum, desc(`sum(raised_amount_usd)`))

#Get top company key
s3_c3_top_company <- toString(s3_c3_sum_arrange[1,1])

#Get top company name
c3_top_sector_top_company_name <- companies[which(companies$permalink == s3_c3_top_company), "name"]
c3_top_sector_top_company_name

#-------------------------------------------------------------------------------------------------------------------#

#Question 10:For point 4 (second best sector count-wise), which company received the highest investment? 

# Country 1 = United States, Ans: SST Inc. (Formerly ShotSpotter)

s2_c1 <- subset(D1,D1$main_sector == D1_2nd_top_sector)
s2_c1_group_by <- group_by(s2_c1, permalink)
s2_c1_sum <- summarise(s2_c1_group_by, sum(raised_amount_usd))
s2_c1_sum_arrange <- arrange(s2_c1_sum, desc(`sum(raised_amount_usd)`))

#Get top company key
s2_c1_top_company <- toString(s2_c1_sum_arrange[1,1])

#Get top company name
c1_2nd_best_sector_top_company_name <- companies[which(companies$permalink == s2_c1_top_company), "name"]
c1_2nd_best_sector_top_company_name

# Country 2 = United Kingdom,  Ans: Celltick Technologies

s2_c2 <- subset(D2,D2$main_sector == D2_2nd_top_sector)
s2_c2_group_by <- group_by(s2_c2, permalink)
s2_c2_sum <- summarise(s2_c2_group_by, sum(raised_amount_usd))
s2_c2_sum_arrange <- arrange(s2_c2_sum, desc(`sum(raised_amount_usd)`))

#Get top company key
s2_c2_top_company <- toString(s2_c2_sum_arrange[1,1])

#Get top company name
c2_2nd_best_sector_top_company_name <- companies[which(companies$permalink == s2_c2_top_company), "name"]
c2_2nd_best_sector_top_company_name

# Country 3 = India,  Ans: Manthan Systems

s2_c3 <- subset(D3,D3$main_sector == D3_2nd_top_sector)
s2_c3_group_by <- group_by(s2_c3, permalink)
s2_c3_sum <- summarise(s2_c3_group_by, sum(raised_amount_usd))
s2_c3_sum_arrange <- arrange(s2_c3_sum, desc(`sum(raised_amount_usd)`))

#Get top company key
s2_c3_top_company <- toString(s2_c3_sum_arrange[1,1])

#Get top company name
c3_2nd_best_sector_top_company_name <- companies[which(companies$permalink == s2_c3_top_company), "name"]
c3_2nd_best_sector_top_company_name
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

