##-----------------------------------------------------------------------------------------------------------##

# Load SparkR
spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

# Initialise the sparkR session
sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

#  add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

##-----------------------------------------------------------------------------------------------------------##
# # Loading packages 

library(sparklyr)
library(ggplot2)

##-----------------------------------------------------------------------------------------------------------##

#Importing the dataset Parking_Violations_Issued_-_Fiscal_Year_2017.csv into nyc_park Spark DataFrame
nyc_park <- read.df("hdfs:///common_folder/nyc_parking/", source = "csv", 
                    inferSchema = "true", header = "true")

head(nyc_park)
dim(nyc_park) #No of rows are 10803028 & no of coulums are 10
str(nyc_park)
printSchema(nyc_park)

## Filtering Data for Year 2017
nyc_park_2017 <- filter(nyc_park, year(nyc_park$`Issue Date`) == '2017')
nrow(nyc_park_2017) #5431918 

##-----------------------------------1. Examine the data-----------------------------------------------------##
## Examine nyc_park_2017 data

#Check for duplicate Summons Number
nyc_park_2017<- dropDuplicates(nyc_park_2017, "Summons Number")
dim(nyc_park_2017)
#No duplicate rows found.

# 1.1 Find the total number of tickets for the year 2017.
head(select(nyc_park_2017, countDistinct(nyc_park_2017$`Summons Number`)))
# 5431918

# 1.2 counting no. of tickets for each state
no_of_ticket_per_state <- summarize(groupBy(nyc_park_2017, nyc_park_2017$`Registration State`),
                                    count = n(nyc_park_2017$`Summons Number`))

no_of_ticket_per_state <- arrange(no_of_ticket_per_state , desc(no_of_ticket_per_state$count))

# number of unique states from where the cars that got parking tickets came from are 65
nrow(no_of_ticket_per_state) 

# state having maximum entries is NY
head(no_of_ticket_per_state , num = 1)

# Replacing Registration_State == 99 with NY
nyc_park_2017$`Registration State` <- ifelse(nyc_park_2017$`Registration State` == "99", "NY", nyc_park_2017$`Registration State`)

# counting no. of tickets for each state again after Replace value
no_of_ticket_per_state_new <- summarize(groupBy(nyc_park_2017, nyc_park_2017$`Registration State`),
                                        count = n(nyc_park_2017$`Summons Number`))

no_of_ticket_per_state_new <- arrange(no_of_ticket_per_state_new , desc(no_of_ticket_per_state_new$count))

# number of unique states from where the cars that got parking tickets came from are 64
nrow(no_of_ticket_per_state_new)
 

##-----------------------------------2. Aggregation-----------------------------------------------------##

#2.1 How often does each violation code occur? Display the frequency of the top five violation codes.

createOrReplaceTempView(nyc_park_2017, "nyc_park_2017_tbl")

# finding No. of Voilation Code occurance per day
voilation_code_per_day <- SparkR::sql("SELECT `Violation Code` , count(`Violation Code`)/365 as Count_per_day FROM nyc_park_2017_tbl Group By `Violation Code` ORDER BY Count_per_day DESC")

# Displaying top 5 No. of Voilation Code occurance per day
head(voilation_code_per_day , num = 5)
#Violation_Code Count_per_day                                                  
#1             21     2104.3479
#2             36     1815.7945
#3             38     1485.1479
#4             14     1305.9288
#5             20      875.7425

# finding No. of Voilation Code occurance per month
voilation_code_per_mnth <- SparkR::sql("SELECT `Violation Code` , count(`Violation Code`)/12 as Count_per_mnth FROM nyc_park_2017_tbl Group By `Violation Code` ORDER BY Count_per_mnth DESC")

# Displaying top 5 No. of Voilation Code occurance per month
head(voilation_code_per_mnth , num = 5)
#Violation_Code Count_per_mnth                                                  
#1             21      64007.25
#2             36      55230.42
#3             38      45173.25
#4             14      39722.00
#5             20      26637.17

# finding No. of Voilation Code occurance in 2017
voilation_code_frequency_in_2017 <- SparkR::sql("SELECT `Violation Code` , count(`Violation Code`) as Count_in_2017 FROM nyc_park_2017_tbl Group By `Violation Code` ORDER BY Count_in_2017 DESC")

# Displaying top 5 No. of Voilation Code occurance in year 2017
head(voilation_code_frequency_in_2017 , num = 5)
# Violation Code Count_in_2017                                                  
# 1             21        768087
# 2             36        662765
# 3             38        542079
# 4             14        476664
# 5             20        319646


#2.2 How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? (Hint: find the top 5 for both)

# finding No. of Vehicle Body Type occurance thoughout the year 2017
vehicle_body_type_in_2017 <- SparkR::sql("SELECT `Vehicle Body Type` , count(`Vehicle Body Type`) as Count_in_2017 FROM nyc_park_2017_tbl Group By `Vehicle Body Type` ORDER BY Count_in_2017 DESC")

# Displaying top 5 No. of Vehicle Body Type occurance in year 2017
head(vehicle_body_type_in_2017 , num = 5)
#   Vehicle Body Type Count_in_2017                                               
# 1              SUBN       1883954
# 2              4DSD       1547312
# 3               VAN        724029
# 4              DELV        358984
# 5               SDN        194197

# finding No. of Vehicle Body Type occurance per day
vehicle_body_type_per_day <- SparkR::sql("SELECT `Vehicle Body Type` , count(`Vehicle Body Type`)/365 as Count_per_day FROM nyc_park_2017_tbl Group By `Vehicle Body Type` ORDER BY Count_per_day DESC")

# Displaying top 5 No. of Vehicle Body Type occurance per day
head(vehicle_body_type_per_day , num = 5)
#Vehicle_Body_Type  Count_per_day                                               
#1              SUBN     5161.5178
#2              4DSD     4239.2110
#3               VAN     1983.6411
#4              DELV      983.5178
#5               SDN      532.0466

# finding No. of Vehicle Body Type occurance per month
vehicle_body_type_per_mnth <- SparkR::sql("SELECT `Vehicle Body Type` , count(`Vehicle Body Type`)/12 as Count_per_mnth FROM nyc_park_2017_tbl Group By `Vehicle Body Type` ORDER BY Count_per_mnth DESC")

# Displaying top 5 No. of Vehicle Body Type occurance per month
head(vehicle_body_type_per_mnth , num = 5)
#Vehicle_Body_Type Count_per_mnth                                              
#1              SUBN      156996.17
#2              4DSD      128942.67
#3               VAN       60335.75
#4              DELV       29915.33
#5               SDN       16183.08


# finding No. of Vehicle Make occurance thoughout the year 2017
vehicle_make_in_2017 <- SparkR::sql("SELECT `Vehicle Make` , count(`Vehicle Make`) as Count_in_2017 FROM nyc_park_2017_tbl Group By `Vehicle Make` ORDER BY Count_in_2017 DESC")

# Displaying top 5 No. of Vehicle Make occurance in the the year 2017
head(vehicle_make_in_2017 , num = 5)
#   Vehicle Make Count_in_2017                                                    
# 1         FORD        636844
# 2        TOYOT        605291
# 3        HONDA        538884
# 4        NISSA        462017
# 5        CHEVR        356032

# finding No. of Vehicle Make occurance per day
vehicle_make_per_day <- SparkR::sql("SELECT `Vehicle Make` , count(`Vehicle Make`)/365 as Count_per_day FROM nyc_park_2017_tbl Group By `Vehicle Make` ORDER BY Count_per_day DESC")

# Displaying top 5 No. of Vehicle Make occurance per day
head(vehicle_make_per_day , num = 5)
#Vehicle_Make Count_per_day                                                    
#1         FORD     1744.7781
#2        TOYOT     1658.3315
#3        HONDA     1476.3945
#4        NISSA     1265.8000
#5        CHEVR      975.4301

# finding No. of Vehicle Make occurance per month
vehicle_make_per_mnth <- SparkR::sql("SELECT `Vehicle Make` , count(`Vehicle Make`)/12 as Count_per_mnth FROM nyc_park_2017_tbl Group By `Vehicle Make` ORDER BY Count_per_mnth DESC")

# Displaying top 5 No. of Vehicle Make occurance per month
head(vehicle_make_per_mnth , num = 5)
#Vehicle_Make Count_per_mnth                                                   
#1         FORD       53070.33
#2        TOYOT       50440.92
#3        HONDA       44907.00
#4        NISSA       38501.42
#5        CHEVR       29669.33


#2.3 Find the (5 highest) frequency of tickets for each of the following:
#a. 'Violation Precinct'
#b. 'Issuer Precinct' 


# finding No. of tickets per day issues by Violation Precinct
violation_precinct_per_day <- SparkR::sql("SELECT `Violation Precinct` , count(`Violation Precinct`)/365 as Count_per_day FROM nyc_park_2017_tbl Group By `Violation Precinct` ORDER BY Count_per_day DESC")
head(violation_precinct_per_day , num = 6)
#Violation_Precinct Count_per_day                                              
#1                  0     2535.8795
#2                 19      751.9041
#3                 14      557.6795
#4                  1      478.6356
#5                 18      463.3726
#6                114      403.9562

# finding No. of tickets per mnth issues by Violation Precinct
violation_precinct_per_mnth <- SparkR::sql("SELECT `Violation Precinct` , count(`Violation Precinct`)/12 as Count_per_mnth FROM nyc_park_2017_tbl Group By `Violation Precinct` ORDER BY Count_per_mnth DESC")
head(violation_precinct_per_mnth , num = 6)
#Violation_Precinct Count_per_mnth                                             
#1                  0       77133.00
#2                 19       22870.42
#3                 14       16962.75
#4                  1       14558.50
#5                 18       14094.25
#6                114       12287.00

# finding No. of tickets in 2017 issues by Violation Precinct
violation_precinct_in_2017 <- SparkR::sql("SELECT `Violation Precinct` , count(`Violation Precinct`) as Count_in_2017 FROM nyc_park_2017_tbl Group By `Violation Precinct` ORDER BY Count_in_2017 DESC")
head(violation_precinct_in_2017 , num = 6)
# Violation Precinct Count_in_2017                                              
# 1                  0        925596
# 2                 19        274445
# 3                 14        203553
# 4                  1        174702
# 5                 18        169131
# 6                114        147444

# Top 5 Highest Violation Precinct are 19 , 14, 1, 18 & 114 (Not considering 0 Violation Precinct as it is erraneous)

# finding No. of tickets per day issues by Issuer Precinct
issuer_precinct_per_day <- SparkR::sql("SELECT `Issuer Precinct` , count(`Issuer Precinct`)/365 as Count_per_day FROM nyc_park_2017_tbl Group By `Issuer Precinct` ORDER BY Count_per_day DESC")
head(issuer_precinct_per_day , num = 6)
#Issuer_Precinct Count_per_day                                                 
#1               0     2954.5370
#2              19      731.4000
#3              14      549.3014
#4               1      462.3014
#5              18      446.5589
#6             114      394.6685

# finding No. of tickets per mnth issues by Issuer Precinct
issuer_precinct_per_mnth <- SparkR::sql("SELECT `Issuer Precinct`, count(`Issuer Precinct`)/12 as Count_per_mnth FROM nyc_park_2017_tbl Group By `Issuer Precinct` ORDER BY Count_per_mnth DESC")
head(issuer_precinct_per_mnth , num = 6)
#Issuer_Precinct Count_per_mnth                                                
#1               0       89867.17
#2              19       22246.75
#3              14       16707.92
#4               1       14061.67
#5              18       13582.83
#6             114       12004.50

# finding No. of tickets in 2107 issues by Issuer Precinct
issuer_precinct_in_2017 <- SparkR::sql("SELECT `Issuer Precinct` , count(`Issuer Precinct`) as Count_in_2017 FROM nyc_park_2017_tbl Group By `Issuer Precinct` ORDER BY Count_in_2017 DESC")
head(issuer_precinct_in_2017 , num = 6)
# Issuer Precinct Count_in_2017                                                 
# 1               0       1078406
# 2              19        266961
# 3              14        200495
# 4               1        168740
# 5              18        162994
# 6             114        144054

# Top 5 Highest Issues Precinct are 19 , 14, 1, 18 & 114 (Not considering 0 Issues Precinct as it is erraneous)

#2.4 Find the violation code frequency across three precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts? 

violation_code_freq_per_precinct <- SparkR::sql("SELECT `Issuer Precinct`, `Violation Code` , count(`Violation Code`) as frequency FROM nyc_park_2017_tbl WHERE `Issuer Precinct` in ('19','14','1') Group By `Issuer Precinct`,`Violation Code` ORDER BY frequency DESC limit 15")
head(violation_code_freq_per_precinct , num=6)
#  Issuer_Precinct Violation_Code frequency                                      
#1              19             46     48445
#2              14             14     45036
#3               1             14     38354
#4              19             38     36386
#5              19             37     36056
#6              14             69     30464


violation_code_freq_per_precinct_R <- as.data.frame(violation_code_freq_per_precinct)

ggplot(violation_code_freq_per_precinct_R , aes(x=as.factor(`Issuer Precinct`) , y=frequency , fill=as.factor(`Violation Code`))) + geom_bar(stat="identity")
# Zone 1 and 14 has highest Violation code 14
# Violation code 14 is the most common accross zone 19,14,1 , which are the highest Issues Preinct.


#2.5

##2.5.1 Missing value analysis

row_with_null <- SparkR::sql("SELECT * FROM nyc_park_2017_tbl WHERE `Summons Number` is NULL or `Plate ID` is NULL or `Registration State` is NULL or `Issue Date` is NULL or `Violation Code` is NULL or `Vehicle Body Type` is NULL or `Vehicle Make` is NULL or `Violation Precinct` is NULL or `Issuer Precinct` is NULL or `Violation Time` is NULL")
nrow(row_with_null)
#0
#There are no NULL Values

#2.5.2 Violation Time analysis

nyc_park_2017$M <- "M"
nyc_park_2017$`Violation Time`<-concat(nyc_park_2017$`Violation Time`,  nyc_park_2017$M)
nyc_park_2017<- drop(nyc_park_2017, c("M"))

nyc_park_2017$Violation_Hour <- substr(nyc_park_2017$`Violation Time`, 1, 2)
nyc_park_2017$Violation_Minute <- substr(nyc_park_2017$`Violation Time`, 3, 4)
nyc_park_2017$Violation_AM_PM <- substr(nyc_park_2017$`Violation Time`, 5, 6)

#Correcting Violation_Hour of 00 AM to 12 AM.
nyc_park_2017$Violation_Hour <- regexp_replace(x = nyc_park_2017$Violation_Hour,pattern = "^0{2}",replacement = "12")

# Correcting Violation Time : Concating Violation_Hour, Violation_Minute & Violation_AM_PM and assigning to Violation Time
nyc_park_2017$`Violation Time` <- concat(nyc_park_2017$Violation_Hour, nyc_park_2017$Violation_Minute, nyc_park_2017$Violation_AM_PM)

# Converting Violation time to time stamp
nyc_park_2017$`Violation Time` <-to_timestamp(x = nyc_park_2017$`Violation Time`, format = "hhmma")

#Check for NULL Violation time.
createOrReplaceTempView(nyc_park_2017, "nyc_park_2017_tbl")
rows_with_null_vt <- SparkR::sql("SELECT *, `Violation Time` FROM nyc_park_2017_tbl WHERE `Violation Time` is NULL")
nrow(rows_with_null_vt) # in total 108 NULL Violation Time.

# Dropping Null values
nyc_park_2017 <- dropna(nyc_park_2017, how = c("any", "all"))

#Checking Null Values in the nyc_park_2017_tbl
createOrReplaceTempView(nyc_park_2017, "nyc_park_2017_tbl")
row_with_null_vt_vh_vm_vampm <- SparkR::sql("SELECT * FROM nyc_park_2017_tbl WHERE `Violation Time` is NULL OR Violation_Hour is NULL OR Violation_Minute is NULL OR Violation_AM_PM is NULL")
nrow(row_with_null_vt_vh_vm_vampm)

# No Null Values found.
head(nyc_park_2017)
# Extracting Hours for bucketing from Violation Time
nyc_park_2017$Violation_Hour <- hour(cast(nyc_park_2017$`Violation Time`,dataType = "string"))

#2.5.3 & 2.5.4
# Buckting Violation_Hour in 6 bins 
createOrReplaceTempView(nyc_park_2017, "nyc_park_2017_tbl")
violation_time_bin_2017 <- SparkR::sql("select Violation_Hour,`Violation Code`,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3 
                                       THEN '0-3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4-7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8-11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12-15'
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16-19'
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20-23'
                                       END as Violation_Time_Bin
                                       from nyc_park_2017_tbl")

createOrReplaceTempView(violation_time_bin_2017, "violation_time_bin_2017_tbl")
violation_code_freq_per_bin <- SparkR::sql("SELECT Violation_Time_Bin, `Violation Code`, count(*) as frequency 
                                           FROM violation_time_bin_2017_tbl 
                                           Group By Violation_Time_Bin,`Violation Code` 
                                           ORDER BY frequency DESC")
head(violation_code_freq_per_bin, num = 15)

violation_code_freq_per_bin_r <- as.data.frame(violation_code_freq_per_bin)
ggplot(violation_code_freq_per_bin_r , aes(x=as.factor(Violation_Time_Bin) , y=frequency , fill=as.factor(`Violation Code`))) + geom_bar(stat="identity")

top_3_violation_code_freq_per_bin <- SparkR::sql("SELECT `Violation Code`,
                                                 Violation_Time_Bin, count(*) frequency
                                                 FROM violation_time_bin_2017_tbl
                                                 WHERE `Violation Code` IN (21,36,38)
                                                 GROUP BY `Violation Code`, Violation_Time_Bin
                                                 ORDER BY `Violation Code`, Violation_Time_Bin, frequency desc")


top_3_violation_code_freq_per_bin_r <- as.data.frame(top_3_violation_code_freq_per_bin)
ggplot(top_3_violation_code_freq_per_bin_r , aes(x=as.factor(Violation_Time_Bin) , y=frequency , fill=as.factor(`Violation Code`))) + geom_bar(stat="identity")
#Most commonly Vialoation are between 8 to 11 and 12 to 15 bins.
#Most commonly violation codes are 21, 36 and 38.


#2.6
#Convert Issue_Date to Date format.
#Check issue date range

nyc_park_2017$`Issue Date` <- to_date(nyc_park_2017$`Issue Date`, 'MM/dd/yyyy')

createOrReplaceTempView(nyc_park_2017, "nyc_park_2017_tbl")
Issue_Date_Range <- SparkR::sql("SELECT min(`Issue Date`)as Min_Issue_Date,
                                max(`Issue Date`)as Max_Issue_Date
                                FROM nyc_park_2017_tbl")

head(Issue_Date_Range)
nyc_park_2017$Issue_Month <- month(nyc_park_2017$`Issue Date`)

#2.6.1

createOrReplaceTempView(nyc_park_2017, "nyc_park_2017_tbl")
Season_Binning_2017 <- SparkR::sql("SELECT `Summons Number`, `Violation Code`,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 11
                                   THEN 'Fall'
                                   END as Season
                                   FROM nyc_park_2017_tbl")

head(Season_Binning_2017)

createOrReplaceTempView(Season_Binning_2017, "season_nyc_park_2017")
tktseason_2017<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_nyc_park_2017
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2017)

freq_tktseason_2017<- data.frame(head(tktseason_2017))
freq_tktseason_2017$Fiscal_Year<- c(2017,2017,2017,2017)
freq_tktseason_2017

#2.6.2 Season


season_violation_2017 <- SparkR::sql("SELECT  Season,
                                     `Violation Code`,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     `Violation Code`,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     `Violation Code`,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_nyc_park_2017
                                     GROUP BY Season, `Violation Code`))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

head(season_violation_2017, 12)

#     Season Violation Code Frequency_of_Tickets                                   
# 1    Fall             46                  231
# 2    Fall             21                  128
# 3    Fall             40                  116
# 4  Spring             21               402399
# 5  Spring             36               344834
# 6  Spring             38               271167
# 7  Summer             21               127344
# 8  Summer             36                96663
# 9  Summer             38                83518
# 10 Winter             21               238179
# 11 Winter             36               221268
# 12 Winter             38               187385

season_violation_2017_r <- as.data.frame(season_violation_2017)
ggplot(season_violation_2017_r, aes(x=as.factor(Season) , y=Frequency_of_Tickets , fill=as.factor(`Violation Code`))) + geom_bar(stat="identity")

#2.7
#The fines collected from all the parking violation constitute a revenue source for the NYC police department. 
#Let's take an example of estimating that for the three most commonly occurring codes.

createOrReplaceTempView(nyc_park_2017, "nyc_park_2017_tbl")
violation_frq_2017<- SparkR::sql("SELECT `Violation Code`, count(*) as violation_frq
                                 from nyc_park_2017_tbl
                                 group by `Violation Code`
                                 order by violation_frq desc ")
head(violation_frq_2017,3) 
# The three most common violation codes are 21,36,38.
top3 <- data.frame(head(violation_frq_2017,3))

#calculating Fine

#violation code 21
# highest-density locations of the city = $65
# rest of the city = $45
##--- Average fine = ($65 + $45)/2 = $110/2 = $55 --#

#violation code 36
# highest-density locations of the city = $50
# rest of the city = $50
##--- Average fine = ($50 + $50)/2 = $100/2 = $50 --#

#violation code 38
# highest-density locations of the city = $65
# rest of the city = $35
##--- Average fine = ($65 + $35)/2 = $100/2 = $50 --#

top3$fine<-c(55,50,50)
top3$collection <- top3$fine * top3$violation_frq
head(top3)

#    Violation.Code violation_frq fine collection
# 1             21        768087   55   42244785
# 2             36        662765   50   33138250
# 3             38        542079   50   27103950

# Violation code 21 has highest number of frequence = 768087 
# Violation code 21 has highest total collection of 42244785 

sparkR.stop()
