#############################################
#1. Load the Date
cars <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)

str(cars)

####################################################################
#2. Data Preparation
####################################################################
#2a. Removing unwanted column
#Removing Car_ID
cars$car_ID <- NULL

#2b. Checking if any Missing Entries
sum(is.na(cars))
#No Missing Entries

#2c.Deriving  company_name from CarName

cars$company_name <- vapply(cars$CarName,function(x) stringr::str_split(x,pattern = " ",simplify = TRUE)[1,1],character(1))
cars$CarName <- NULL

#2d. Fixing Company Names Spelling
unique(cars$company_name)
cars[which(cars$company_name == "maxda" | cars$company_name == "mazda"),"company_name"] <- "mazda"
cars[which(cars$company_name == "porsche" | cars$company_name == "porcshce"),"company_name"] <- "porsche"
cars[which(cars$company_name == "vokswagen" | cars$company_name == "volkswagen" | cars$company_name == "vw"),"company_name"] <- "volkswagen"
cars[which(cars$company_name == "toyota" | cars$company_name == "toyouta"),"company_name"] <- "toyota"
cars[which(cars$company_name == "nissan" | cars$company_name == "Nissan"),"company_name"] <- "Nissan"
unique(cars$company_name)
cars$company_name <- as.factor(cars$company_name)

#2e. Fixing drivewheel values
cars[which(cars$drivewheel == "4wd" | cars$drivewheel == "fwd"),"drivewheel"] <- "fwd"
cars$drivewheel <- as.factor(cars$drivewheel)

#2f. converting categorical variables to factor

for(i in c("symboling","fueltype","aspiration","doornumber","carbody","drivewheel","enginelocation","enginetype","cylindernumber","fuelsystem","company_name")) {cars[,i] <- as.factor(cars[,i])}

#2g. Handling categorical independent variable 
dummy_1 <- data.frame(model.matrix( ~symboling, data = cars))
dummy_1 <- dummy_1[,-1]
cars_1 <- cbind(cars[,-which(colnames(cars)== "symboling")],dummy_1)


dummy_2 <- data.frame(model.matrix( ~fueltype, data = cars_1))
cars_2 <- cbind(cars_1[,-which(colnames(cars_1)== "fueltype")],dummy_2)
cars_2$X.Intercept. <- NULL

dummy_3 <- data.frame(model.matrix( ~aspiration, data = cars_2))
cars_3 <- cbind(cars_2[,-which(colnames(cars_2)== "aspiration")],dummy_3)
cars_3$X.Intercept. <- NULL

dummy_4 <- data.frame(model.matrix( ~doornumber, data = cars_3))
cars_4 <- cbind(cars_3[,-which(colnames(cars_3)== "doornumber")],dummy_4)
cars_4$X.Intercept. <- NULL

dummy_5 <- data.frame(model.matrix( ~carbody, data = cars_4))
cars_5 <- cbind(cars_4[,-which(colnames(cars_4)== "carbody")],dummy_5)
cars_5$X.Intercept. <- NULL

dummy_6 <- data.frame(model.matrix( ~drivewheel, data = cars_5))
cars_6 <- cbind(cars_5[,-which(colnames(cars_5)== "drivewheel")],dummy_6)
cars_6$X.Intercept. <- NULL

dummy_7 <- data.frame(model.matrix( ~enginelocation, data = cars_6))
cars_7 <- cbind(cars_6[,-which(colnames(cars_6)== "enginelocation")],dummy_7)
cars_7$X.Intercept. <- NULL

dummy_8 <- data.frame(model.matrix( ~enginetype, data = cars_7))
cars_8 <- cbind(cars_7[,-which(colnames(cars_7)== "enginetype")],dummy_8)
cars_8$X.Intercept. <- NULL

dummy_9 <- data.frame(model.matrix( ~cylindernumber, data = cars_8))
cars_9 <- cbind(cars_8[,-which(colnames(cars_8)== "cylindernumber")],dummy_9)
cars_9$X.Intercept. <- NULL

dummy_10 <- data.frame(model.matrix( ~fuelsystem, data = cars_9))
cars_10 <- cbind(cars_9[,-which(colnames(cars_9)== "fuelsystem")],dummy_10)
cars_10$X.Intercept. <- NULL

dummy_11 <- data.frame(model.matrix( ~company_name, data = cars_10))
cars_11 <- cbind(cars_10[,-which(colnames(cars_10)== "company_name")],dummy_11)
cars_11$X.Intercept. <- NULL

########################################################################################################################################

str(cars_11)

#3. Modelling
################################
#3a. separate training and testing data
set.seed(100)
cars_trainindices= sample(1:nrow(cars_11), 0.7*nrow(cars_11))
cars_train = cars_11[cars_trainindices,]
cars_test = cars_11[-cars_trainindices,]

#3b. Model 1

model_1 <-lm(price~.,data=cars_train)
summary(model_1)

#variables with coefficiant NA means it is highly co-related with other variable
alias(model_1)

library(MASS)
library(car)
step <- stepAIC(model_1, direction="both")

#3c. Model 2 from previous step aic
model_2 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + symboling.1 + symboling0 + symboling3 + aspirationturbo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_2)
#Multiple R-squared:  0.9806,	Adjusted R-squared:  0.9735 

#checking VIFs
vif(model_2)
#VIF value for curbweight is highest , try removing to see if impact on adjusted R square
model_3 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + symboling.1 + symboling0 + symboling3 + aspirationturbo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_3)
#Multiple R-squared:  0.9789,	Adjusted R-squared:  0.9715 

vif(model_3)
#VIF for carbodysedan is high and p value is low **
# try removing carbodysedan and see if it impacts Adjusted R square
model_4 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + symboling.1 + symboling0 + symboling3 + aspirationturbo + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_4)
#Multiple R-squared:  0.9773,	Adjusted R-squared:  0.9696

# after removing curbweight & carbodysedan on the basis of high VIF , there was a slight decrease in Adjusted R square.
#we may consider these variable later

vif(model_4)
#VIF value of enginesize is highest followed by carwidth  , but their p values are significantly low.
#Next high value is enginetypeohc, and its p values is also high

model_5 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + symboling.1 + symboling0 + symboling3 + aspirationturbo + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_5)
#Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9688
#removing enginetypeohc there was a slight decrease in Adjusted R square.

vif(model_5)
# Next high VIFs are drivewheelrwd and stroke , but their p value is significantly low so retaining them

#Looking at other VIFs value they are in pretty much same range , so removing variables based on High P Value.

#removing symboling.1
model_6 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + symboling0 + symboling3 + aspirationturbo + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_6)
#Multiple R-squared:  0.9762,	Adjusted R-squared:  0.9688

#No change in adjusted R Square

#removing symboling0
model_7 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + symboling3 + aspirationturbo + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_7)
#Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9688 
#No change in adjusted R Square

#removing symboling3
model_7 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + aspirationturbo + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_7)
#Multiple R-squared:  0.976,	Adjusted R-squared:  0.969

#No change in adjusted R Square


#removing carbodyhardtop
model_8 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + aspirationturbo + 
                carbodyhatchback + carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_8)
#Multiple R-squared:  0.9758,	Adjusted R-squared:  0.969
#No change in adjusted R Square


#removing carbodyhatchback
model_9 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + aspirationturbo + 
                carbodywagon + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_9)
#Multiple R-squared:  0.9756,	Adjusted R-squared:  0.969 
#No change in adjusted R Square

#removing carbodywagon
model_10 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                peakrpm + aspirationturbo + 
                drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + company_namebmw + 
                company_namebuick + company_namedodge + company_namehonda + 
                company_namejaguar + company_namemazda + company_namemercury + 
                company_namemitsubishi + company_nameNissan + 
                company_nameplymouth + company_namerenault + company_namesaab + 
                company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_10)
#Multiple R-squared:  0.9755,	Adjusted R-squared:  0.9692 
#Slight increase Adjusted R square

#removing fuelsystem2bbl
model_11 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberthree + company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + company_namemercury + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_11)
#Multiple R-squared:  0.9754,	Adjusted R-squared:  0.9693
#Slight increase Adjusted R square

#removing cylindernumberthree
model_12 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + company_namemercury + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_12)
#Multiple R-squared:  0.9749,	Adjusted R-squared:  0.969

# slight drop in Adjusted R square

# removing company_namemercury
model_13 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_13)
#Multiple R-squared:  0.9743,	Adjusted R-squared:  0.9685 
# slight drop in Adjusted R square

#removing enginetypel
model_14 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_14)
#Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9679
# slight drop in Adjusted R square

#removing cylindernumberfive
model_15 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + company_namesaab + 
                 company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_15)
#Multiple R-squared:  0.9729,	Adjusted R-squared:  0.9676

# no change in adjusted R square

#removing company_namesaab
model_16 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + 
                 company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_16)
#Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9673

#slight drop in adjusted R square

#removing enginetypedohcv
model_17 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + 
                 company_nametoyota + company_namevolkswagen, data = cars_train)
summary(model_17)
#Multiple R-squared:  0.9706,	Adjusted R-squared:  0.9655
#slight drop in adjusted R square

#removing company_namevolkswagen
model_18 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + company_namehonda + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + 
                 company_nametoyota, data = cars_train)
summary(model_18)

#Multiple R-squared:  0.9685,	Adjusted R-squared:  0.9633
#slight drop in adjusted R square

#removing company_namehonda
model_19 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + company_namerenault + 
                 company_nametoyota, data = cars_train)
summary(model_19)
#Multiple R-squared:  0.9672,	Adjusted R-squared:  0.9622
#slight drop in adjusted R square

#removing company_namerenault
model_20 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth + 
                 company_nametoyota, data = cars_train)
summary(model_20)
#Multiple R-squared:  0.9664,	Adjusted R-squared:  0.9615
#slight drop in adjusted R square

#removing company_nametoyota
model_21 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + company_nameNissan + 
                 company_nameplymouth, data = cars_train)
summary(model_21)
#Multiple R-squared:  0.9648,	Adjusted R-squared:   0.96 
#slight drop in adjusted R square

# removing company_nameNissan
model_22 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + 
                 company_namejaguar + company_namemazda + 
                 company_namemitsubishi + 
                 company_nameplymouth, data = cars_train)
summary(model_22)
#Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9592
#slight drop in adjusted R square

#removing company_namemazda
model_23 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + 
                 company_namejaguar + 
                 company_namemitsubishi + 
                 company_nameplymouth, data = cars_train)
summary(model_23)
#Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9583 
#slight drop in adjusted R square

#removing company_nameplymouth
model_24 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + company_namedodge + 
                 company_namejaguar + 
                 company_namemitsubishi, 
                 data = cars_train)
summary(model_24)
#Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9571
#slight drop in adjusted R square

#removing company_namedodge
model_25 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 drivewheelrwd + enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + 
                 company_namejaguar + 
                 company_namemitsubishi, 
               data = cars_train)
summary(model_25)
#Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9559
#slight drop in adjusted R square

#removing drivewheelrwd
model_26 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + 
                 company_namejaguar + 
                 company_namemitsubishi, 
               data = cars_train)
summary(model_26)
#Multiple R-squared:  0.9578,	Adjusted R-squared:  0.9539
#slight drop in adjusted R square


#removing company_namemitsubishi
model_27 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + 
                 company_namejaguar, 
                 data = cars_train)
summary(model_27)
#Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9514 
#slight drop in adjusted R square

# try adding curbweight
model_28 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + curbweight +
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + 
                 company_namejaguar, 
                 data = cars_train)
summary(model_28)
#Multiple R-squared:  0.9559,	Adjusted R-squared:  0.9519

# slight increase in Adjusted r square , but p value of curbweight is high

#removing curbweight and adding carbodysedan
model_29 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + carbodysedan +
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + 
                 company_namejaguar, 
               data = cars_train)
summary(model_29)
# slight increase in Adjusted r square , but p value of carbodysedan is high

# try adding both curbweight & carbodysedan
model_30 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + carbodysedan + curbweight +
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor + 
                 company_namebmw + 
                 company_namebuick + 
                 company_namejaguar, 
               data = cars_train)
summary(model_30)

## slight increase in Adjusted r square , but p value for both is high

#Hence Considering Model 27
summary(model_27)
#Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9514

#Predict Price for Cars test dataset
Predict_1 <- predict(model_27,cars_test[,-1])
cars_test$test_Price <- Predict_1

# Now, Validate r square between actual price and Predicted Price
r <- cor(cars_test$price,cars_test$test_Price)
r
rsquared <- r^2
rsquared



cor_1 <- cor(cars_11[,c("carwidth","enginesize","stroke","peakrpm","aspirationturbo","enginelocationrear","enginetypeohcf","enginetyperotor","company_namebmw","company_namebuick","company_namejaguar")])
cor_1
#buick cars are highly co-related with carwidth and enginesize , try removing


model_31 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor +company_namebmw + company_namejaguar ,
               data = cars_train)
summary(model_31)
#Multiple R-squared:  0.9343,	Adjusted R-squared:  0.9293

#Predict Price for Cars test dataset
Predict_2 <- predict(model_31,cars_test[,-1])
cars_test$test_Price_1 <- Predict_2

# Now, Validate r square between actual price and Predicted Price
r_1 <- cor(cars_test$price,cars_test$test_Price_1)
r_1
rsquared_1 <- r_1^2
rsquared_1

#jaguar cars are highly co-related with carwidth and enginesize , try removing

model_32 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor +company_namebmw,
               data = cars_train)
summary(model_32)
#Multiple R-squared:  0.9293,	Adjusted R-squared:  0.9246

#Predict Price for Cars test dataset
Predict_3 <- predict(model_32,cars_test[,-1])
cars_test$test_Price_2 <- Predict_3

# Now, Validate r square between actual price and Predicted Price
r_2 <- cor(cars_test$price,cars_test$test_Price_2)
r_2
rsquared_2 <- r_2^2
rsquared_2

cor_2 <- cor(cars_11[,c("carwidth","enginesize","stroke","peakrpm","aspirationturbo","enginelocationrear","enginetypeohcf","enginetyperotor","company_namebmw")])
cor_2
# try removing peakrpm on basis of p value
model_33 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 aspirationturbo + 
                 enginelocationrear + 
                 enginetypeohcf + enginetyperotor +company_namebmw,
               data = cars_train)
summary(model_33)
#Multiple R-squared:  0.9255,	Adjusted R-squared:  0.921

# try removing enginetypeohcf on basis of p value
model_34 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 aspirationturbo + 
                 enginelocationrear + 
                 enginetyperotor +company_namebmw,
               data = cars_train)
summary(model_34)
#Multiple R-squared:  0.9195,	Adjusted R-squared:  0.9153

#Predict Price for Cars test dataset
Predict_4 <- predict(model_34,cars_test[,-1])
cars_test$test_Price_3 <- Predict_4

# Now, Validate r square between actual price and Predicted Price
r_3 <- cor(cars_test$price,cars_test$test_Price_3)
r_3
rsquared_3 <- r_3^2
rsquared_3


#trying further by removing variable with highest p value i.e aspirationturbo
model_35 <- lm(formula = price ~ carwidth + enginesize + stroke + 
 
                 enginelocationrear + 
                 enginetyperotor +company_namebmw,
               data = cars_train)
summary(model_35)
#Multiple R-squared:  0.9121,	Adjusted R-squared:  0.9083

#trying further by removing variable with highest p value i.e stroke
model_36 <- lm(formula = price ~ carwidth + enginesize +
                 
                 enginelocationrear + 
                 enginetyperotor +company_namebmw,
               data = cars_train)
summary(model_36)
#Multiple R-squared:  0.906,	Adjusted R-squared:  0.9026 



#Predict Price for Cars test dataset
Predict_5 <- predict(model_36,cars_test[,-1])
cars_test$test_Price_4 <- Predict_5

# Now, Validate r square between actual price and Predicted Price
r_4 <- cor(cars_test$price,cars_test$test_Price_4)
r_4
rsquared_4 <- r_4^2
rsquared_4


#Final Model : model_36
alias(model_36)
#following Independent variables are significant in predicting price of the cars
#carwidth
#enginesize
#enginelocationrear
#enginetyperotor
#company_namebmw















