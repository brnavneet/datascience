#----------------------------------------------------------------------------------#
#                           HR Analysis                                            #
#----------------------------------------------------------------------------------#

#------------------Load the required libraries-------------------------------------#
require(stringr)
require(dplyr)
require(tidyr)
require(MASS)
require(car)
require(caret)
require(ggplot2)
require(GGally)
require(caTools)
require(cowplot)
require(e1071)
require(ROCR)
require(gridExtra)

#-------------------Import dataset into R------------------------------------------#

general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)

#---------------------Check each dataframe in R---------------#
str(general_data)
str(employee_survey_data)
str(manager_survey_data)
str(in_time)
str(out_time)

#Check for duplicate EmployeeID. 
sum(duplicated(general_data$EmployeeID))
sum(duplicated(employee_survey_data$EmployeeID))
sum(duplicated(manager_survey_data$EmployeeID))
sum(duplicated(in_time$EmployeeID))
sum(duplicated(out_time$EmployeeID))

#EmployeeID's are unique for each dataframe.

setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets

#Setting missing Column name for EmployeeID column in in_time and out_time dataframe 
colnames(in_time)[1]<- "EmployeeID"
colnames(out_time)[1]<- "EmployeeID"

setdiff(general_data$EmployeeID,in_time$EmployeeID) # Identical EmployeeID across these datasets
setdiff(in_time$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these datasets

#---------------Analysing in_time and out_time dataframe--------------------------------------------#

#Finding average working hours for each employee

# Find columns which has only NA values, remove them if present.
in_time <- in_time[,colSums(is.na(in_time))<nrow(in_time)]
out_time <- out_time[,colSums(is.na(out_time))<nrow(out_time)]

#check for Blank spaces 
sapply(in_time, function(x) length(which(x == "")))
sapply(out_time, function(x) length(which(x == "")))

#Formating in_time & out_time dataframe. 

in_time[-1]<-as.data.frame(sapply(in_time[-1], function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))
out_time[-1]<-as.data.frame(sapply(out_time[-1], function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))

work_time <- out_time[-1] - in_time[-1]
work_time <- data.frame(sapply(work_time, function(x) as.numeric(x)))

# create avg_work_hours dataframe with EmployeeID and average working hours. 
avg_work_hours <- data.frame(EmployeeID = seq(from = 1, to = 4410, by = 1), avg_work_hours = rowMeans(work_time,na.rm = TRUE))

#-----------------------------Master Dataframe---------------------------------------------------#
# Creating master dataframe with general_data,employee_survey_data, manager_survey_data 
# & avg_work_hours (derived data)

master <- merge(general_data,employee_survey_data, by="EmployeeID")
master <- merge(master,manager_survey_data, by="EmployeeID")
master <- merge(master,avg_work_hours, by="EmployeeID")


#----------------Data Cleaning and formating----------------------------------------------------#

#check for Blank spaces 
sapply(master, function(x) length(which(x == "")))
#No Blank values found in mergedata

#Removing coulumns that has same value in all the rows
master<-Filter(function(x)(length(unique(x))>1), master)
#columns "EmployeeCount", "Over18", "StandardHours" are removed.

#Check for columns with only NA values; remove if present.
master <- master[,colSums(is.na(master))<nrow(master)]

#Check each column for number of NA's 
colSums(is.na(master)) 
# NA's are present in columns "NumCompaniesWorked, TotalWorkingYears, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance" 

summary(as.factor(master$NumCompaniesWorked))
summary(as.factor(master$TotalWorkingYears))
summary(as.factor(master$EnvironmentSatisfaction))
summary(as.factor(master$JobSatisfaction))
summary(as.factor(master$WorkLifeBalance))

# As number of NA's are very less in comparision to other variable options, so removing them. 
master <- master[!is.na(master$NumCompaniesWorked),]
master <- master[!is.na(master$TotalWorkingYears),]
master <- master[!is.na(master$EnvironmentSatisfaction),]
master <- master[!is.na(master$JobSatisfaction),]
master <- master[!is.na(master$WorkLifeBalance),]

sum(is.na(master)) # No NA's are available in the master dataframe.

#---------------------Outlier Treatment--------------------------------------------------------------#
#Removing outliers
str(master)
numeric_col <- c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "avg_work_hours")
sapply(master[numeric_col], function(x) length(boxplot.stats(x)$out))

#1 checking col: MonthlyIncome
boxplot(master$MonthlyIncome)
quantile(master$MonthlyIncome,seq(0,1,0.01),na.rm = TRUE)
#sharp increase in MonthlyIncome is observed between 91st & 92nd percentile; replacing the outliers.
master$MonthlyIncome[which(master$MonthlyIncome>152020)] <- 152020

#2 checking col: NumCompaniesWorked
boxplot(master$NumCompaniesWorked)
quantile(master$NumCompaniesWorked,seq(0,1,0.01),na.rm = TRUE)
#sharp increase in NumCompaniesWorked is observed between 96th & 97th percentile; replacing the outliers.
master$NumCompaniesWorked[which(master$NumCompaniesWorked>8)] <- 8

#3 checking col: TotalWorkingYears
boxplot(master$TotalWorkingYears)
quantile(master$TotalWorkingYears,seq(0,1,0.01),na.rm = TRUE)
#sharp increase in TotalWorkingYears is observed between 94th & 95th percentile; replacing the outliers.
master$TotalWorkingYears[which(master$TotalWorkingYears>29)] <- 29

#4 checking col: TrainingTimesLastYear
boxplot(master$TrainingTimesLastYear)
quantile(master$TrainingTimesLastYear,seq(0,1,0.01),na.rm = TRUE)
#sharp increase in TrainingTimesLastYear is observed between 95th & 96th percentile; replacing the outliers.
master$TrainingTimesLastYear[which(master$TrainingTimesLastYear>4)] <- 4
#sharp decrease in TrainingTimesLastYear is observed between 4th & 3rd percentile; replacing the outliers.
master$TrainingTimesLastYear[which(master$TrainingTimesLastYear<1)] <- 1

#5 checking col: YearsAtCompany
boxplot(master$YearsAtCompany)
quantile(master$YearsAtCompany,seq(0,1,0.01),na.rm = TRUE)
#sharp increase in YearsAtCompany is observed between 92nd & 93rd percentile; replacing the outliers.
master$YearsAtCompany[which(master$YearsAtCompany>17)] <- 17

#6 checking col: YearsSinceLastPromotion
boxplot(master$YearsSinceLastPromotion)
quantile(master$YearsSinceLastPromotion,seq(0,1,0.01),na.rm = TRUE)
#sharp increase in YearsSinceLastPromotion is observed between 93rd & 94th percentile; replacing the outliers.
master$YearsSinceLastPromotion[which(master$YearsSinceLastPromotion>8)] <- 8

#7 checking col: YearsWithCurrManager
boxplot(master$YearsWithCurrManager)
quantile(master$YearsWithCurrManager,seq(0,1,0.01),na.rm = TRUE)
#sharp increase in YearsWithCurrManager is observed between 99th & 100th percentile; replacing the outliers.
master$YearsWithCurrManager[which(master$YearsWithCurrManager>14)] <- 14

#8 checking col: avg_work_hours
boxplot(master$avg_work_hours)
quantile(master$avg_work_hours,seq(0,1,0.01),na.rm = TRUE)
#Outlier treatment not required as sharp jump in values are not observed.

#----------------Analysing categorical valiables as per data dictionary----------------------------------#

#Education
summary(as.factor(master$Education))
master$Education[which(master$Education == 1)] <- "Below College"
master$Education[which(master$Education == 2)] <- "College"
master$Education[which(master$Education == 3)] <- "Bachelor"
master$Education[which(master$Education == 4)] <- "Master"
master$Education[which(master$Education == 5)] <- "Doctor"
summary(as.factor(master$Education))

#EnvironmentSatisfaction
summary(as.factor(master$EnvironmentSatisfaction))
master$EnvironmentSatisfaction[which(master$EnvironmentSatisfaction == 1)] <- "Low"
master$EnvironmentSatisfaction[which(master$EnvironmentSatisfaction == 2)] <- "Medium"
master$EnvironmentSatisfaction[which(master$EnvironmentSatisfaction == 3)] <- "High"
master$EnvironmentSatisfaction[which(master$EnvironmentSatisfaction == 4)] <- "Very High"
summary(as.factor(master$EnvironmentSatisfaction))

#JobInvolvement
summary(as.factor(master$JobInvolvement))
master$JobInvolvement[which(master$JobInvolvement == 1)] <- "Low"
master$JobInvolvement[which(master$JobInvolvement == 2)] <- "Medium"
master$JobInvolvement[which(master$JobInvolvement == 3)] <- "High"
master$JobInvolvement[which(master$JobInvolvement == 4)] <- "Very High"
summary(as.factor(master$JobInvolvement))

#JobSatisfaction
summary(as.factor(master$JobSatisfaction))
master$JobSatisfaction[which(master$JobSatisfaction == 1)] <- "Low"
master$JobSatisfaction[which(master$JobSatisfaction == 2)] <- "Medium"
master$JobSatisfaction[which(master$JobSatisfaction == 3)] <- "High"
master$JobSatisfaction[which(master$JobSatisfaction == 4)] <- "Very High"
summary(as.factor(master$JobSatisfaction))

#PerformanceRating
summary(as.factor(master$PerformanceRating))
master$PerformanceRating[which(master$PerformanceRating == 1)] <- "Low"
master$PerformanceRating[which(master$PerformanceRating == 2)] <- "Good"
master$PerformanceRating[which(master$PerformanceRating == 3)] <- "Excellent"
master$PerformanceRating[which(master$PerformanceRating == 4)] <- "Outstanding"
summary(as.factor(master$PerformanceRating))

#WorkLifeBalance
summary(as.factor(master$WorkLifeBalance))
master$WorkLifeBalance[which(master$WorkLifeBalance == 1)] <- "Bad"
master$WorkLifeBalance[which(master$WorkLifeBalance == 2)] <- "Good"
master$WorkLifeBalance[which(master$WorkLifeBalance == 3)] <- "Better"
master$WorkLifeBalance[which(master$WorkLifeBalance == 4)] <- "Best"
summary(as.factor(master$WorkLifeBalance))

#change all char variables to factor variable and in lower case. (except target variable)
str(master)
char_col <- c("Attrition", "BusinessTravel", "Department", "Education", "EducationField", "Gender", "JobRole", "MaritalStatus", "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating")
master[char_col] <- lapply(master[char_col], function(x) factor(tolower(x)))

#Checking uniqueness of each factor variable 
summary(master[char_col])
str(master)

#Change variables JobLevel & StockOptionLevel to categorical variable.
int_col_levels <- c("JobLevel", "StockOptionLevel")
master[int_col_levels] <- lapply(master[int_col_levels], function(x) factor(x))
str(master)

### Exploratory Data Analysis

str(master)

# Barcharts for categorical features with stacked telecom information

bar_theme1 <- theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 0.5, size = 15))

grid.arrange(ggplot(master,aes(x=BusinessTravel,fill=Attrition))+geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=Department,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=Education,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=EducationField,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             nrow=2, ncol=2)

grid.arrange(ggplot(master, aes(x=Gender,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=JobLevel,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=JobRole,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             nrow=2, ncol=2)


grid.arrange(ggplot(master, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             nrow=2, ncol=2)

grid.arrange(ggplot(master, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             ggplot(master, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+labs(fill="Attrition")+bar_theme1,
             nrow=1, ncol=2)

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(master, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(master, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(master, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master, aes(MonthlyIncome))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(NumCompaniesWorked))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(PercentSalaryHike))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(TotalWorkingYears))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(TrainingTimesLastYear))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(YearsAtCompany))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(YearsSinceLastPromotion))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(YearsWithCurrManager))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(master, aes(avg_work_hours))+ geom_histogram(bins = 30),
          ggplot(master, aes(x="",y=avg_work_hours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# Boxplots of numeric variables relative to Attrition
plot_grid(ggplot(master, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master, aes(x=Attrition,y=avg_work_hours, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

ggplot(master, aes(x = MonthlyIncome,y=TotalWorkingYears)) +  geom_line()
ggplot(master, aes(x=DistanceFromHome, y=avg_work_hours))+ geom_point(aes(colour = Attrition))

#--------------------------------------------------------------------------------------------------------#
#  converting factors with 2 levels to numerical variables
#--------------------------------------------------------------------------------------------------------#

#Attrition
master$Attrition <- ifelse(master$Attrition=="yes",1,0)

#Gender
master$Gender <- ifelse(master$Gender=="male",1,0)

#PerformanceRating
master$PerformanceRating <- ifelse(master$PerformanceRating=="outstanding",1,0)

str(master) #27 variables with 4300 observations.

# Checking Attrition %age

Attrition <- sum(master$Attrition)/nrow(master)
Attrition # 16.16% attrition rate. 

#-------------------------------------------------------------------------------------------------------#
# Creating dummy variable for variables having more than 2 levels
#-------------------------------------------------------------------------------------------------------#

# creating a dataframe of categorical features
# "BusinessTravel", "Department", "Education", "EducationField", "JobLevel", "JobRole", "MaritalStatus", "StockOptionLevel", "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance", "JobInvolvement"
master_fact<- master[,c(4,5,7,8,10,11,12,16,22,23,24,25)]

# creating dummy variables for factor variables
dummies<- data.frame(sapply(master_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master_fact))[,-1]))

master<- cbind(master[,-c(4,5,7,8,10,11,12,16,22,23,24,25)],dummies) 
View(master) 

# Normalising continuous features 
str(master)

master$Age<- scale(master$Age) 
master$DistanceFromHome<- scale(master$DistanceFromHome)
master$MonthlyIncome<- scale(master$MonthlyIncome) 
master$NumCompaniesWorked<- scale(master$NumCompaniesWorked)
master$PercentSalaryHike<- scale(master$PercentSalaryHike)
master$TotalWorkingYears<- scale(master$TotalWorkingYears)
master$TrainingTimesLastYear<- scale(master$TrainingTimesLastYear)
master$YearsAtCompany<- scale(master$YearsAtCompany)
master$YearsSinceLastPromotion<- scale(master$YearsSinceLastPromotion)
master$YearsWithCurrManager<- scale(master$YearsWithCurrManager)
master$avg_work_hours<- scale(master$avg_work_hours)

final_master <- master[-1] #56 variables with 4300 Observations.


#-------------------------------------------------------------------------------#
# splitting the data between train and test
#-------------------------------------------------------------------------------#
set.seed(100)

indices = sample.split(final_master$Attrition, SplitRatio = 0.7)

train = final_master[indices,]

test = final_master[!(indices),]

#---------------------------------------------------------------------------#
# Logistic Regression: 
#---------------------------------------------------------------------------#

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
sort(vif(model_2),decreasing = TRUE)


#Excluding EducationField.xlife.sciences
model_3<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_work_hours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                Education.xdoctor + EducationField.xmarketing + 
                EducationField.xmedical + EducationField.xother + EducationField.xtechnical.degree + 
                JobLevel.x2 + JobLevel.x5 + JobRole.xhuman.resources + JobRole.xmanager + 
                JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                JobRole.xsales.executive + MaritalStatus.xmarried + MaritalStatus.xsingle + 
                StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                WorkLifeBalance.xbetter + WorkLifeBalance.xgood + JobInvolvement.xmedium + 
                JobInvolvement.xvery.high, family = "binomial", data = train)


summary(model_3) 

# Removing multicollinearity through VIF check
sort(vif(model_3),decreasing = TRUE)

#Excluding BusinessTravel.xtravel_rarely
model_4<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_work_hours + BusinessTravel.xtravel_frequently + 
                Education.xdoctor + EducationField.xmarketing + EducationField.xmedical + 
                EducationField.xother + EducationField.xtechnical.degree + 
                JobLevel.x2 + JobLevel.x5 + JobRole.xhuman.resources + JobRole.xmanager + 
                JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                JobRole.xsales.executive + MaritalStatus.xmarried + MaritalStatus.xsingle + 
                StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbest + 
                WorkLifeBalance.xbetter + WorkLifeBalance.xgood + JobInvolvement.xmedium + 
                JobInvolvement.xvery.high, family = "binomial", data = train)

summary(model_4) 

# Removing multicollinearity through VIF check
sort(vif(model_4),decreasing = TRUE)

#Excluding MaritalStatus.xmarried
model_5<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                EducationField.xmarketing + EducationField.xmedical + EducationField.xother + 
                EducationField.xtechnical.degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xhuman.resources + JobRole.xmanager + JobRole.xmanufacturing.director + 
                JobRole.xresearch.director + JobRole.xsales.executive + 
                MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                WorkLifeBalance.xgood + JobInvolvement.xmedium + JobInvolvement.xvery.high, 
              family = "binomial", data = train)


summary(model_5) 

# Removing multicollinearity through VIF check
sort(vif(model_5),decreasing = TRUE)

#Excluding WorkLifeBalance.xbest
model_6<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                EducationField.xmarketing + EducationField.xmedical + EducationField.xother + 
                EducationField.xtechnical.degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xhuman.resources + JobRole.xmanager + JobRole.xmanufacturing.director + 
                JobRole.xresearch.director + JobRole.xsales.executive + MaritalStatus.xsingle + 
                StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                WorkLifeBalance.xbetter + WorkLifeBalance.xgood + JobInvolvement.xmedium + 
                JobInvolvement.xvery.high, family = "binomial", data = train)


summary(model_6) 

# Removing multicollinearity through VIF check
sort(vif(model_6),decreasing = TRUE)

#Excluding WorkLifeBalance.xgood
model_7<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                EducationField.xmarketing + EducationField.xmedical + EducationField.xother + 
                EducationField.xtechnical.degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xhuman.resources + JobRole.xmanager + JobRole.xmanufacturing.director + 
                JobRole.xresearch.director + JobRole.xsales.executive + MaritalStatus.xsingle + 
                StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                JobInvolvement.xmedium + JobInvolvement.xvery.high, 
              family = "binomial", data = train)

summary(model_7) 

# Removing multicollinearity through VIF check
sort(vif(model_7),decreasing = TRUE)

#Excluding Age
model_8<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                EducationField.xmarketing + EducationField.xmedical + EducationField.xother + 
                EducationField.xtechnical.degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xhuman.resources + JobRole.xmanager + JobRole.xmanufacturing.director + 
                JobRole.xresearch.director + JobRole.xsales.executive + MaritalStatus.xsingle + 
                StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                JobInvolvement.xmedium + JobInvolvement.xvery.high, family = "binomial", 
              data = train)


summary(model_8) 

# Removing multicollinearity through VIF check
sort(vif(model_8),decreasing = TRUE)

#Excluding EducationField.xmarketing
model_9<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                EducationField.xmedical + EducationField.xother + 
                EducationField.xtechnical.degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xhuman.resources + JobRole.xmanager + JobRole.xmanufacturing.director + 
                JobRole.xresearch.director + JobRole.xsales.executive + MaritalStatus.xsingle + 
                StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                JobInvolvement.xmedium + JobInvolvement.xvery.high, family = "binomial", 
              data = train)


summary(model_9) 

# Removing multicollinearity through VIF check
sort(vif(model_9),decreasing = TRUE)

#Excluding EducationField.xmedical
model_10<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 EducationField.xother + EducationField.xtechnical.degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 JobRole.xsales.executive + MaritalStatus.xsingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                 JobInvolvement.xmedium + JobInvolvement.xvery.high, family = "binomial", 
               data = train)


summary(model_10) 

# Removing multicollinearity through VIF check
sort(vif(model_10),decreasing = TRUE)

#Excluding JobInvolvement.xvery.high
model_11<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 EducationField.xother + EducationField.xtechnical.degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 JobRole.xsales.executive + MaritalStatus.xsingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                 JobInvolvement.xmedium, family = "binomial", 
               data = train)


summary(model_11) 

# Removing multicollinearity through VIF check
sort(vif(model_11),decreasing = TRUE)

#Excluding EducationField.xother
model_12<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 EducationField.xtechnical.degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 JobRole.xsales.executive + MaritalStatus.xsingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                 JobInvolvement.xmedium, family = "binomial", data = train)


summary(model_12) 

# Removing multicollinearity through VIF check
sort(vif(model_12),decreasing = TRUE)

#Excluding EducationField.xtechnical.degree
model_13<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xhuman.resources + JobRole.xmanager + JobRole.xmanufacturing.director + 
                 JobRole.xresearch.director + JobRole.xsales.executive + MaritalStatus.xsingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + EnvironmentSatisfaction.xvery.high + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                 JobInvolvement.xmedium, family = "binomial", data = train)


summary(model_13) 

# Removing multicollinearity through VIF check
sort(vif(model_13),decreasing = TRUE)

#Excluding EnvironmentSatisfaction.xvery.high
model_14<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 JobRole.xsales.executive + MaritalStatus.xsingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xlow + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                 JobInvolvement.xmedium, family = "binomial", data = train)


summary(model_14) 

# Removing multicollinearity through VIF check
sort(vif(model_14),decreasing = TRUE)

#Excluding JobRole.xsales.executive
model_15<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xlow + JobSatisfaction.xlow + JobSatisfaction.xvery.high + 
                 WorkLifeBalance.xbetter + JobInvolvement.xmedium, family = "binomial", 
               data = train)


summary(model_15) 

# Removing multicollinearity through VIF check
sort(vif(model_15),decreasing = TRUE)

#Excluding JobLevel.x5
model_16<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 JobLevel.x2 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter + 
                 JobInvolvement.xmedium, family = "binomial", data = train)


summary(model_16) 

# Removing multicollinearity through VIF check
sort(vif(model_16),decreasing = TRUE)

#Excluding JobInvolvement.xmedium
model_17<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 JobLevel.x2 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, family = "binomial", data = train)


summary(model_17) 

# Removing multicollinearity through VIF check
sort(vif(model_17),decreasing = TRUE)

#Excluding JobRole.xresearch.director
model_18<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + Education.xdoctor + 
                 JobLevel.x2 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + 
                 MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, 
               family = "binomial", data = train)


summary(model_18) 

# Removing multicollinearity through VIF check
sort(vif(model_18),decreasing = TRUE)

#Excluding Education.xdoctor
model_19<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + 
                 JobLevel.x2 + JobRole.xhuman.resources + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + MaritalStatus.xsingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + JobSatisfaction.xlow + 
                 JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, family = "binomial", 
               data = train)


summary(model_19) 

# Removing multicollinearity through VIF check
sort(vif(model_19),decreasing = TRUE)

#Excluding JobRole.xhuman.resources
model_20<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + JobLevel.x2 + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + 
                 MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, 
               family = "binomial", data = train)


summary(model_20) 

# Removing multicollinearity through VIF check
sort(vif(model_20),decreasing = TRUE)

#Excluding JobLevel.x2
model_21<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + MaritalStatus.xsingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + JobSatisfaction.xlow + 
                 JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, family = "binomial", 
               data = train)


summary(model_21) 

# Removing multicollinearity through VIF check
sort(vif(model_21),decreasing = TRUE)

#Excluding TrainingTimesLastYear
model_22<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_work_hours + BusinessTravel.xtravel_frequently + JobRole.xmanager + 
                 JobRole.xmanufacturing.director + MaritalStatus.xsingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + JobSatisfaction.xlow + 
                 JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, family = "binomial", 
               data = train)


summary(model_22) 

# Removing multicollinearity through VIF check
sort(vif(model_22),decreasing = TRUE)


#Excluding StockOptionLevel.x1
model_23<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                 BusinessTravel.xtravel_frequently + JobRole.xmanager + JobRole.xmanufacturing.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                 JobSatisfaction.xlow + JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, 
               family = "binomial", data = train)


summary(model_23) 

# Removing multicollinearity through VIF check
sort(vif(model_23),decreasing = TRUE)

#Excluding JobRole.xmanager
model_24<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                 BusinessTravel.xtravel_frequently + JobRole.xmanufacturing.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + JobSatisfaction.xlow + 
                 JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, family = "binomial", 
               data = train)


summary(model_24) 

# Removing multicollinearity through VIF check
sort(vif(model_24),decreasing = TRUE)

#Excluding JobRole.xmanufacturing.director
model_25<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                 BusinessTravel.xtravel_frequently + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + JobSatisfaction.xlow + 
                 JobSatisfaction.xvery.high + WorkLifeBalance.xbetter, family = "binomial", 
               data = train)


summary(model_25) 

# Removing multicollinearity through VIF check
sort(vif(model_25),decreasing = TRUE)

#Excluding WorkLifeBalance.xbetter
model_26<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                 BusinessTravel.xtravel_frequently + MaritalStatus.xsingle + 
                 EnvironmentSatisfaction.xlow + JobSatisfaction.xlow + JobSatisfaction.xvery.high, family = "binomial", data = train)


summary(model_26) 

# VIF check
sort(vif(model_26),decreasing = TRUE)

########################################################################
# The final model is selected with 10 significant variables.

final_model<- model_26

#######################################################################

### Model Evaluation

### Test Data ##

#predicted probabilities of Attrition

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

summary(test_pred)

test$prob <- test_pred

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

# Let's use the probability cutoff of 40%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
table(test_actual_attrition,test_pred_attrition)

#######################################################################

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#########################################################################################

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.011)] 
cutoff

# Let's choose a cutoff value of 0.1776 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1776, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.7596899

sens #0.7511962

spec #0.7613321

View(test)

############################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.5125283


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile
#At 3rd decile 72.2 percentage of Attrition are predicted. 
#At 4th decile 80.4 percentage of Attrition are predicted.

# Lift plot: Gain is 2 times at the 4th decile 

plot(attrition_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")

#Gain Chart : At 4th Decile 80.4% attritions are predicted.
plot(performance_measures_test, col="orange", lwd=2)
