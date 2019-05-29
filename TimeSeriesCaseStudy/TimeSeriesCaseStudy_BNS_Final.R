library(forecast)
library(tseries)
require(graphics)
library("ggplot2")

######################################
#1. Data Loading
#####################################

globalstore <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE)

str(globalstore)

########################################################################################################
#2. Data Preparation
#######################################################################################################

#2a. check for Missing Values

check_miss_value <- sapply(globalstore, function(x) sum(is.na(x)))

check_miss_value  # Postal.Code has 41296 missing values and this column wont be helpful in analysis , Hence Removing 

globalstore["Postal.Code"] <- NULL

globalstore$Order.Date<-as.Date(globalstore$Order.Date,"%d-%m-%Y")
globalstore$Ship.Date<-as.Date(globalstore$Ship.Date,"%d-%m-%Y")
globalstore$Order.Month<-format(globalstore$Order.Date,"%Y-%m")
globalstore$Ship.Month<-format(globalstore$Ship.Date,"%Y-%m")
#2b. Removing Unwanted Columns
globalstore[,c("Row.ID","Order.ID", "Customer.ID" , "Product.ID")] <- NULL

#2c. subsetting data based on Market and Segments , aggregating Quantity , Profit and Sales over Months


# initializing variables and list to store final Summary Output
l = 1
list_of_bucket <- c()
list_of_agg_bucket <- c()
list_of_summary_monthly_sales_bucket <- c()
list_sum_profit_per_bucket <- c()
list_avg_profit_per_bucket <- c()
list_sd_profit_per_bucket <- c()
list_cv_profit_per_bucket <- c()
# Looping over Market and Segment to fetch aggregated Profit values and storing in Lists

for(i in unique(globalstore$Market)){
  for(y in unique(globalstore$Segment)){
    #assign(paste(i,y,sep="_"),globalstore[globalstore$Market == i & globalstore$Segment == y,])
    
    #filter data and stopring in temporary Data frame
    temp_df <- globalstore[globalstore$Market == i & globalstore$Segment == y,]
    list_of_bucket[l] <- paste(i,y,sep="_")
    assign(list_of_bucket[l],temp_df)
    # aggregating Quantity , Profit and Sales
    temp_df_total_quantity <- aggregate(temp_df$Quantity, by=list(temp_df$Market,temp_df$Segment,temp_df$Order.Month), FUN=sum)
    names(temp_df_total_quantity) <-  c("Market","Segment","Order.Month","TotalMonthlyQuantity")
    temp_df_total_profit <- aggregate(temp_df$Profit, by=list(temp_df$Market,temp_df$Segment,temp_df$Order.Month), FUN=sum)
    names(temp_df_total_profit) <-  c("Market","Segment","Order.Month","TotalMonthlyProfits")
    temp_df_total_sales <- aggregate(temp_df$Sales, by=list(temp_df$Market,temp_df$Segment,temp_df$Order.Month), FUN=sum)
    names(temp_df_total_sales) <-  c("Market","Segment","Order.Month","TotalMonthlySales")
    #total <- data.frame(temp_df_total_quantity,temp_df_total_profit$TotalMonthlyProfits,temp_df_total_sales$TotalMonthlySales)
    total <- cbind(temp_df_total_quantity,temp_df_total_profit)
    total <- cbind(total,temp_df_total_sales)
    total <- total[,c(1,2,3,4,8,12)]
    names(total) <- c("Market","Segment","Order.Month","TotalMonthlyQuantity","TotalMonthlyProfits","TotalMonthlySales")
    list_of_agg_bucket[l] <- paste(list_of_bucket[l],"agg",sep="_")
    assign(list_of_agg_bucket[l],total)
    #Plotting Time series for Quantity , Profit and Sales
    ts.plot(ts(scale(total$TotalMonthlyQuantity)), ts(scale(total$TotalMonthlyProfits)), ts(scale(total$TotalMonthlySales)),
            gpars=list(xlab="Order_Month", ylab=paste("Sales for ",list_of_bucket[l],sep=" "),col =c("red","green","blue")))
    legend("topleft", legend = c("Quantity","Profits","Sales"), col =c("red","green","blue"), lty = 1)
    
    # Storing Total Profit , Average , Standard Deviation and Coefficient of Variance on profit in the Lists
    list_sum_profit_per_bucket[l] <- sum(total$TotalMonthlyProfits)
    list_avg_profit_per_bucket[l] <- mean(total$TotalMonthlyProfits)
    list_sd_profit_per_bucket[l] <- sd(total$TotalMonthlyProfits)
    list_cv_profit_per_bucket[l] <- list_sd_profit_per_bucket[l]/list_avg_profit_per_bucket[l]
    l = l + 1
    
  }
}


# Combining Market_Segment , Total Profit , Average Profit , SD profit and CV profit into one Data frame

Summary_profit_each_market_segment <- data.frame(list_of_bucket,list_sum_profit_per_bucket,list_avg_profit_per_bucket,list_sd_profit_per_bucket,list_cv_profit_per_bucket)
names(Summary_profit_each_market_segment)<-c("Market_Segment","TotalProfit","AvgProfit","SDProfit","CVProfit")
Summary_profit_each_market_segment <- tidyr::separate(Summary_profit_each_market_segment, Market_Segment, into=c("Market", "Segment"), sep="_")



# Plotting Market Segments Vs Total Profit
Segments <- Summary_profit_each_market_segment$Segment
plot1 <- ggplot(Summary_profit_each_market_segment,aes(x=Summary_profit_each_market_segment$Market,y=Summary_profit_each_market_segment$TotalProfit,fill=Segments))

plot1 <- plot1 + geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Total Profits")+ggtitle("Market Segments Vs Total Profit")

plot1

# Plotting Market Segments Vs Coefficient of Variance of Profit


plot2 <- ggplot(Summary_profit_each_market_segment,aes(x=Summary_profit_each_market_segment$Market,y=Summary_profit_each_market_segment$CVProfit,fill=Segments))

plot2 <- plot2 + geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("CV Profits")+ggtitle("Market Segments Vs CV of  Profit")

plot2



# Based on the plot below are the top 2 Market Segments with High profits and Low Coefficient of Variance

# 1. APAC Consumer   
# 2. EU Consumer

##################################################################################################################################

#3. Model Building and Evaluation

##############################################################################################################################

#3a. Model Building and Evaluation for APAC Consumer Sales
############################################################################################

################################ Forcasting Sales for APAC Consumer #############################################


timevals <- c(1:nrow(APAC_Consumer_agg))
total_timeseries_APAC_Consumer_sales <- ts(APAC_Consumer_agg$TotalMonthlySales)
total_timeseries_APAC_Consumer_sales_df <- as.data.frame(cbind(timevals, as.vector(total_timeseries_APAC_Consumer_sales)))
colnames(total_timeseries_APAC_Consumer_sales_df) <- c('Month', 'Sales')

### Plotting Decomposition of Multiplicative Time Series for APAC consumer Sales
plot(decompose(ts(total_timeseries_APAC_Consumer_sales_df$Sales, frequency = 12), "multiplicative"))

# Seperating last 6 months for testing
APAC_Consumer_sales_train = total_timeseries_APAC_Consumer_sales_df[1:42,]
APAC_Consumer_sales_test = total_timeseries_APAC_Consumer_sales_df[43:48,]


timeseries_APAC_Consumer_train_Sales <- ts(APAC_Consumer_sales_train$Sales)


title_sales1 <- c("Global Mart Sales in APAC Market Consumer Segment from 2011-Jan to 2014-Dec")
xlab1 <- c("Months 2011-Jan to 2014-Dec")
ylab_sales <- c("Sales")
cols <- c("red", "blue")
labels <- c("Original", "MVSmoothed")

######## Forecasting Sales for APAC Consumer ##########################################################

# Moving Average Smoothening

plot(timeseries_APAC_Consumer_train_Sales, main=title_sales1 , xlab = xlab1, ylab = ylab_sales , col=cols[1])

w<-2
smoothed_timeseries_APAC_Consumer_train_Sales <- filter(timeseries_APAC_Consumer_train_Sales, 
                                                        filter=rep(1/(2*w+1),(2*w+1)), 
                                                        method='convolution', sides=2)
diff <- smoothed_timeseries_APAC_Consumer_train_Sales[w+2] - smoothed_timeseries_APAC_Consumer_train_Sales[w+1]
for (i in seq(w,1,-1)) {
  smoothed_timeseries_APAC_Consumer_train_Sales[i] <- smoothed_timeseries_APAC_Consumer_train_Sales[i+1] - diff
}

n <- length(timeseries_APAC_Consumer_train_Sales)
diff <- smoothed_timeseries_APAC_Consumer_train_Sales[n-w] - smoothed_timeseries_APAC_Consumer_train_Sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed_timeseries_APAC_Consumer_train_Sales[i] <- smoothed_timeseries_APAC_Consumer_train_Sales[i-1] + diff
}

lines(smoothed_timeseries_APAC_Consumer_train_Sales, col=cols[2], lwd=2)

legend("topleft", labels, col=cols, lwd=2)

## exponential Smoothening

plot(timeseries_APAC_Consumer_train_Sales, main=title_sales1 , xlab = xlab1, ylab = ylab_sales)

cols <- c("red", "blue", "green", "yellow" , "black")
alphas <- c(0.02, 0.1, 0.2, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedexp_timeseries_APAC_Consumer_train_Sales <- HoltWinters(timeseries_APAC_Consumer_train_Sales, alpha=alphas[i],
                                                                  beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedexp_timeseries_APAC_Consumer_train_Sales)[,1], col=cols[i], lwd=2)
}

legend("topleft", labels, col=cols, lwd=2)

#Considering Moving Average Smoothened for Model Building 

month_no_train <-APAC_Consumer_sales_train$Month
smoothed_APAC_Consumer_sales_df <- as.data.frame(cbind(month_no_train, as.vector(smoothed_timeseries_APAC_Consumer_train_Sales)))
colnames(smoothed_APAC_Consumer_sales_df) <- c('Month', 'Sales')

# Looking at the Graph there is Seasonality with upward Trend
# hence Fitting Multiplicative Model with Sinusoid Function
lmfit_APAC_Consumer_sales <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                                + Month, data=smoothed_APAC_Consumer_sales_df)

#Global Component
global_predict_APAC_Consumer_Sales <- predict(lmfit_APAC_Consumer_sales, Month=month_no_train)
summary(global_predict_APAC_Consumer_Sales)
plot(global_predict_APAC_Consumer_Sales, col='red', type = "l" , main=c("Global Predict for APAC Consumer Sales") , xlab = xlab1, ylab = ylab_sales)


#Local Component
local_predict_APAC_Consumer_Sales <- timeseries_APAC_Consumer_train_Sales - global_predict_APAC_Consumer_Sales
plot(local_predict_APAC_Consumer_Sales, col='red', type = "l" , main=c("Local Predict for APAC Consumer Sales") , xlab = xlab1, ylab = ylab_sales)
acf(local_predict_APAC_Consumer_Sales)
acf(local_predict_APAC_Consumer_Sales, type="partial")
armafit_APAC_Consumer_sales <- auto.arima(local_predict_APAC_Consumer_Sales)

tsdiag(armafit_APAC_Consumer_sales)
armafit_APAC_Consumer_sales


# checking for white noise in Residual series

residual_APAC_Consumer_sales <- local_predict_APAC_Consumer_Sales - fitted(armafit_APAC_Consumer_sales)

adf.test(residual_APAC_Consumer_sales,alternative = "stationary") #p-value = 0.01
kpss.test(residual_APAC_Consumer_sales) #p-value = 0.1

# Hence the Time series is Stationary


month_no_test <- APAC_Consumer_sales_test$Month

global_predict_APAC_Consumer_Sales_test <- predict(lmfit_APAC_Consumer_sales,data.frame(Month =month_no_test))

forecast_sales_APAC_Consumer <- global_predict_APAC_Consumer_Sales_test

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_classic_decomp_sales_APAC_Consumer <- accuracy(forecast_sales_APAC_Consumer,APAC_Consumer_sales_test[,2])[5]
MAPE_classic_decomp_sales_APAC_Consumer


#### Plotting  the predictions and  original values for APAC Consumer Sales
labels = c("original","predicted")
classic_decomp_predict_APAC_Consumer_Sales <- c(ts(global_predict_APAC_Consumer_Sales),ts(global_predict_APAC_Consumer_Sales_test))
plot(total_timeseries_APAC_Consumer_sales, col = "black" , xlab = xlab1 , ylab = ylab_sales , main = title_sales1)
lines(classic_decomp_predict_APAC_Consumer_Sales, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##########################################################################################
### Using ARIMA Fit for Model Building and Evaluation for APAC Consumer Sales
##########################################################################################

autoarima_APAC_Consumer_Sales <- auto.arima(timeseries_APAC_Consumer_train_Sales)
autoarima_APAC_Consumer_Sales
tsdiag(autoarima_APAC_Consumer_Sales)
plot(autoarima_APAC_Consumer_Sales$x, col="black")
lines(fitted(autoarima_APAC_Consumer_Sales), col="red")

#checking if the residual series is white noise

residual_APAC_Consumer_Sales_auto_arima <- timeseries_APAC_Consumer_train_Sales - fitted(autoarima_APAC_Consumer_Sales)

adf.test(residual_APAC_Consumer_Sales_auto_arima,alternative = "stationary") #p-value = 0.01
kpss.test(residual_APAC_Consumer_Sales_auto_arima)  #p-value = 0.1


#Evaluating  the model using MAPE
forecast_sales_APAC_Consumer_auto_arima <- predict(autoarima_APAC_Consumer_Sales, n.ahead = 6)

MAPE_auto_arima_sales_APAC_Consumer <- accuracy(forecast_sales_APAC_Consumer_auto_arima$pred,APAC_Consumer_sales_test[,2])[5]
MAPE_auto_arima_sales_APAC_Consumer
#27.68952

##### plotting the predictions and original values for Auto ARIMA APAC Consumer Sales
labels = c("original","predicted")
auto_arima_predict_APAC_Consumer_Sales <- c(fitted(autoarima_APAC_Consumer_Sales),ts(forecast_sales_APAC_Consumer_auto_arima$pred))
plot(total_timeseries_APAC_Consumer_sales, col = "black" , xlab = xlab1 , ylab = ylab_sales , main = paste(title_sales1 , "(Auto ARIMA)" , sep= " "))
lines(auto_arima_predict_APAC_Consumer_Sales, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##################################################################################################################################################
##################################################################################################################################################

#3b. Model Building and Evaluation for APAC Consumer Demand
############################################################################################

################################ Forcasting Quantity for APAC Consumer #############################################


timevals <- c(1:nrow(APAC_Consumer_agg))
total_timeseries_APAC_Consumer_Quantity <- ts(APAC_Consumer_agg$TotalMonthlyQuantity)
total_timeseries_APAC_Consumer_Quantity_df <- as.data.frame(cbind(timevals, as.vector(total_timeseries_APAC_Consumer_Quantity)))
colnames(total_timeseries_APAC_Consumer_Quantity_df) <- c('Month', 'Quantity')

### Plotting Decomposition of Multiplicative Time Series for APAC consumer Quantity
plot(decompose(ts(total_timeseries_APAC_Consumer_Quantity_df$Quantity, frequency = 12), "multiplicative"))

# Seperating last 6 months for testing
APAC_Consumer_Quantity_train = total_timeseries_APAC_Consumer_Quantity_df[1:42,]

APAC_Consumer_Quantity_test = total_timeseries_APAC_Consumer_Quantity_df[43:48,]


timeseries_APAC_Consumer_train_Quantity <- ts(APAC_Consumer_Quantity_train$Quantity)


title_Quantity1 <- c("Global Mart Demand in APAC Market Consumer Segment from 2011-Jan to 2014-Dec")
xlab1 <- c("Months 2011-Jan to 2014-Dec")
ylab_Quantity <- c("Quantity")
cols <- c("red", "blue")
labels <- c("Original", "MVSmoothed")

######## Forecasting Quantity for APAC Consumer ##########################################################

# Moving Average Smoothening

plot(timeseries_APAC_Consumer_train_Quantity, main=title_Quantity1 , xlab = xlab1, ylab = ylab_Quantity , col=cols[1])

w<-2
smoothed_timeseries_APAC_Consumer_train_Quantity <- filter(timeseries_APAC_Consumer_train_Quantity, 
                                                           filter=rep(1/(2*w+1),(2*w+1)), 
                                                           method='convolution', sides=2)
diff <- smoothed_timeseries_APAC_Consumer_train_Quantity[w+2] - smoothed_timeseries_APAC_Consumer_train_Quantity[w+1]
for (i in seq(w,1,-1)) {
  smoothed_timeseries_APAC_Consumer_train_Quantity[i] <- smoothed_timeseries_APAC_Consumer_train_Quantity[i+1] - diff
}

n <- length(timeseries_APAC_Consumer_train_Quantity)
diff <- smoothed_timeseries_APAC_Consumer_train_Quantity[n-w] - smoothed_timeseries_APAC_Consumer_train_Quantity[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed_timeseries_APAC_Consumer_train_Quantity[i] <- smoothed_timeseries_APAC_Consumer_train_Quantity[i-1] + diff
}

lines(smoothed_timeseries_APAC_Consumer_train_Quantity, col=cols[2], lwd=2)

legend("topleft", labels, col=cols, lwd=2)

## exponential Smoothening

plot(timeseries_APAC_Consumer_train_Quantity, main=title_Quantity1 , xlab = xlab1, ylab = ylab_Quantity)

cols <- c("red", "blue", "green", "yellow" , "black")
alphas <- c(0.02, 0.1, 0.2, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedexp_timeseries_APAC_Consumer_train_Quantity <- HoltWinters(timeseries_APAC_Consumer_train_Quantity, alpha=alphas[i],
                                                                     beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedexp_timeseries_APAC_Consumer_train_Quantity)[,1], col=cols[i], lwd=2)
}

legend("topleft", labels, col=cols, lwd=2)

#Considering Moving Average Smoothened for Model Building 

month_no_train <-APAC_Consumer_Quantity_train$Month
smoothed_APAC_Consumer_Quantity_df <- as.data.frame(cbind(month_no_train, as.vector(smoothed_timeseries_APAC_Consumer_train_Quantity)))
colnames(smoothed_APAC_Consumer_Quantity_df) <- c('Month', 'Quantity')

# Looking at the Graph there is Seasonality with upward Trend
# hence Fitting Multiplicative Model with Sinusoid Function
lmfit_APAC_Consumer_Quantity <- lm(Quantity ~ sin(0.4*Month) * poly(Month,3) + cos(0.4*Month) * poly(Month,3)
                                   + Month, data=smoothed_APAC_Consumer_Quantity_df)

#Global Component
global_predict_APAC_Consumer_Quantity <- predict(lmfit_APAC_Consumer_Quantity, Month=month_no_train)
summary(global_predict_APAC_Consumer_Quantity)
plot(global_predict_APAC_Consumer_Quantity, col='red', type = "l" , main=c("Global Predict for APAC Consumer Demand") , xlab = xlab1, ylab = ylab_Quantity)


#Local Component
local_predict_APAC_Consumer_Quantity <- timeseries_APAC_Consumer_train_Quantity - global_predict_APAC_Consumer_Quantity
plot(local_predict_APAC_Consumer_Quantity, col='red', type = "l" , main=c("Local Predict for APAC Consumer Demand") , xlab = xlab1, ylab = ylab_Quantity)
acf(local_predict_APAC_Consumer_Quantity)
acf(local_predict_APAC_Consumer_Quantity, type="partial")
armafit_APAC_Consumer_Quantity <- auto.arima(local_predict_APAC_Consumer_Quantity)

tsdiag(armafit_APAC_Consumer_Quantity)
armafit_APAC_Consumer_Quantity


# checking for white noise in Residual series

residual_APAC_Consumer_Quantity <- local_predict_APAC_Consumer_Quantity - fitted(armafit_APAC_Consumer_Quantity)

adf.test(residual_APAC_Consumer_Quantity,alternative = "stationary") #p-value = 0.01
kpss.test(residual_APAC_Consumer_Quantity) #p-value = 0.1

# Hence the Time series is Stationary


month_no_test <- APAC_Consumer_Quantity_test$Month

global_predict_APAC_Consumer_Quantity_test <- predict(lmfit_APAC_Consumer_Quantity,data.frame(Month =month_no_test))

forecast_Quantity_APAC_Consumer <- global_predict_APAC_Consumer_Quantity_test

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_classic_decomp_Quantity_APAC_Consumer <- accuracy(forecast_Quantity_APAC_Consumer,APAC_Consumer_Quantity_test[,2])[5]
MAPE_classic_decomp_Quantity_APAC_Consumer


#### Plotting  the predictions and  original values for APAC Consumer Quantity
labels = c("original","predicted")
classic_decomp_predict_APAC_Consumer_Quantity <- c(ts(global_predict_APAC_Consumer_Quantity),ts(global_predict_APAC_Consumer_Quantity_test))
plot(total_timeseries_APAC_Consumer_Quantity, col = "black" , xlab = xlab1 , ylab = ylab_Quantity , main = title_Quantity1)
lines(classic_decomp_predict_APAC_Consumer_Quantity, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##########################################################################################
### Using ARIMA Fit for Model Building and Evaluation for APAC Consumer Quantity
##########################################################################################

autoarima_APAC_Consumer_Quantity <- auto.arima(timeseries_APAC_Consumer_train_Quantity)
autoarima_APAC_Consumer_Quantity
tsdiag(autoarima_APAC_Consumer_Quantity)
plot(autoarima_APAC_Consumer_Quantity$x, col="black")
lines(fitted(autoarima_APAC_Consumer_Quantity), col="red")

#Again, let's check if the residual series is white noise

residual_APAC_Consumer_Quantity_auto_arima <- timeseries_APAC_Consumer_train_Quantity - fitted(autoarima_APAC_Consumer_Quantity)

adf.test(residual_APAC_Consumer_Quantity_auto_arima,alternative = "stationary") #p-value = 0.01
kpss.test(residual_APAC_Consumer_Quantity_auto_arima) #p-value = 0.1

#Also, let's evaluate the model using MAPE
forecast_Quantity_APAC_Consumer_auto_arima <- predict(autoarima_APAC_Consumer_Quantity, n.ahead = 6)

MAPE_auto_arima_Quantity_APAC_Consumer <- accuracy(forecast_Quantity_APAC_Consumer_auto_arima$pred,APAC_Consumer_Quantity_test[,2])[5]
MAPE_auto_arima_Quantity_APAC_Consumer
#26.24458

##### plotting the predictions and original values for Auto ARIMA APAC Consumer Quantity
labels = c("original","predicted")
auto_arima_predict_APAC_Consumer_Quantity <- c(fitted(autoarima_APAC_Consumer_Quantity),ts(forecast_Quantity_APAC_Consumer_auto_arima$pred))
plot(total_timeseries_APAC_Consumer_Quantity, col = "black" , xlab = xlab1 , ylab = ylab_Quantity , main = paste(title_Quantity1 , "(Auto ARIMA)" , sep= " "))
lines(auto_arima_predict_APAC_Consumer_Quantity, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##################################################################################################################################################
##################################################################################################################################################
#3c. Model Building and Evaluation for EU Consumer Sales
############################################################################################

################################ Forcasting Sales for EU Consumer #############################################


timevals <- c(1:nrow(EU_Consumer_agg))
total_timeseries_EU_Consumer_sales <- ts(EU_Consumer_agg$TotalMonthlySales)
total_timeseries_EU_Consumer_sales_df <- as.data.frame(cbind(timevals, as.vector(total_timeseries_EU_Consumer_sales)))
colnames(total_timeseries_EU_Consumer_sales_df) <- c('Month', 'Sales')

### Plotting Decomposition of Multiplicative Time Series for EU consumer Sales
plot(decompose(ts(total_timeseries_EU_Consumer_sales_df$Sales, frequency = 12), "multiplicative"))

# Seperating last 6 months for testing
EU_Consumer_sales_train = total_timeseries_EU_Consumer_sales_df[1:42,]
EU_Consumer_sales_test = total_timeseries_EU_Consumer_sales_df[43:48,]


timeseries_EU_Consumer_train_Sales <- ts(EU_Consumer_sales_train$Sales)


title_sales1 <- c("Global Mart Sales in EU Market Consumer Segment from 2011-Jan to 2014-Dec")
xlab1 <- c("Months 2011-Jan to 2014-Dec")
ylab_sales <- c("Sales")
cols <- c("red", "blue")
labels <- c("Original", "MVSmoothed")

######## Forecasting Sales for EU Consumer ##########################################################

# Moving Average Smoothening

plot(timeseries_EU_Consumer_train_Sales, main=title_sales1 , xlab = xlab1, ylab = ylab_sales , col=cols[1])

w<-2
smoothed_timeseries_EU_Consumer_train_Sales <- filter(timeseries_EU_Consumer_train_Sales, 
                                                      filter=rep(1/(2*w+1),(2*w+1)), 
                                                      method='convolution', sides=2)
diff <- smoothed_timeseries_EU_Consumer_train_Sales[w+2] - smoothed_timeseries_EU_Consumer_train_Sales[w+1]
for (i in seq(w,1,-1)) {
  smoothed_timeseries_EU_Consumer_train_Sales[i] <- smoothed_timeseries_EU_Consumer_train_Sales[i+1] - diff
}

n <- length(timeseries_EU_Consumer_train_Sales)
diff <- smoothed_timeseries_EU_Consumer_train_Sales[n-w] - smoothed_timeseries_EU_Consumer_train_Sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed_timeseries_EU_Consumer_train_Sales[i] <- smoothed_timeseries_EU_Consumer_train_Sales[i-1] + diff
}

lines(smoothed_timeseries_EU_Consumer_train_Sales, col=cols[2], lwd=2)

legend("topleft", labels, col=cols, lwd=2)

## exponential Smoothening

plot(timeseries_EU_Consumer_train_Sales, main=title_sales1 , xlab = xlab1, ylab = ylab_sales)

cols <- c("red", "blue", "green", "yellow" , "black")
alphas <- c(0.02, 0.1, 0.2, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedexp_timeseries_EU_Consumer_train_Sales <- HoltWinters(timeseries_EU_Consumer_train_Sales, alpha=alphas[i],
                                                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedexp_timeseries_EU_Consumer_train_Sales)[,1], col=cols[i], lwd=2)
}

legend("topleft", labels, col=cols, lwd=2)

#Considering Moving Average Smoothened for Model Building 

month_no_train <-EU_Consumer_sales_train$Month
smoothed_EU_Consumer_sales_df <- as.data.frame(cbind(month_no_train, as.vector(smoothed_timeseries_EU_Consumer_train_Sales)))
colnames(smoothed_EU_Consumer_sales_df) <- c('Month', 'Sales')

# Looking at the Graph there is Seasonality with upward Trend
# hence Fitting Multiplicative Model with Sinusoid Function
lmfit_EU_Consumer_sales <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                              + Month, data=smoothed_EU_Consumer_sales_df)

#Global Component
global_predict_EU_Consumer_Sales <- predict(lmfit_EU_Consumer_sales, Month=month_no_train)
summary(global_predict_EU_Consumer_Sales)
plot(global_predict_EU_Consumer_Sales, col='red', type = "l" , main=c("Global Predict for EU Consumer Sales") , xlab = xlab1, ylab = ylab_sales)
#lines(month_no_train, global_predict_EU_Consumer_Sales, col='pink', lwd=2)

#Local Component
local_predict_EU_Consumer_Sales <- timeseries_EU_Consumer_train_Sales - global_predict_EU_Consumer_Sales
plot(local_predict_EU_Consumer_Sales, col='red', type = "l" , main=c("Local Predict for EU Consumer Sales") , xlab = xlab1, ylab = ylab_sales)
acf(local_predict_EU_Consumer_Sales)
acf(local_predict_EU_Consumer_Sales, type="partial")
armafit_EU_Consumer_sales <- auto.arima(local_predict_EU_Consumer_Sales)

tsdiag(armafit_EU_Consumer_sales)
armafit_EU_Consumer_sales


# checking for white noise in Residual series

residual_EU_Consumer_sales <- local_predict_EU_Consumer_Sales - fitted(armafit_EU_Consumer_sales)

adf.test(residual_EU_Consumer_sales,alternative = "stationary") #p-value = 0.01
kpss.test(residual_EU_Consumer_sales) #p-value = 0.1

# Hence the Time series is Stationary

month_no_test <- EU_Consumer_sales_test$Month

global_predict_EU_Consumer_Sales_test <- predict(lmfit_EU_Consumer_sales,data.frame(Month =month_no_test))

forecast_sales_EU_Consumer <- global_predict_EU_Consumer_Sales_test

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_classic_decomp_sales_EU_Consumer <- accuracy(forecast_sales_EU_Consumer,EU_Consumer_sales_test[,2])[5]
MAPE_classic_decomp_sales_EU_Consumer
#28.27462

#### Plotting  the predictions and  original values for EU Consumer Sales
labels = c("original","predicted")
classic_decomp_predict_EU_Consumer_Sales <- c(ts(global_predict_EU_Consumer_Sales),ts(global_predict_EU_Consumer_Sales_test))
plot(total_timeseries_EU_Consumer_sales, col = "black" , xlab = xlab1 , ylab = ylab_sales , main = title_sales1)
lines(classic_decomp_predict_EU_Consumer_Sales, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##########################################################################################
### Using ARIMA Fit for Model Building and Evaluation for EU Consumer Sales
##########################################################################################

autoarima_EU_Consumer_Sales <- auto.arima(timeseries_EU_Consumer_train_Sales)
autoarima_EU_Consumer_Sales
tsdiag(autoarima_EU_Consumer_Sales)
plot(autoarima_EU_Consumer_Sales$x, col="black")
lines(fitted(autoarima_EU_Consumer_Sales), col="red")

#Again, let's check if the residual series is white noise

residual_EU_Consumer_Sales_auto_arima <- timeseries_EU_Consumer_train_Sales - fitted(autoarima_EU_Consumer_Sales)

adf.test(residual_EU_Consumer_Sales_auto_arima,alternative = "stationary") #p-value = 0.01
kpss.test(residual_EU_Consumer_Sales_auto_arima) #p-value = 0.1

#Also, let's evaluate the model using MAPE
forecast_sales_EU_Consumer_auto_arima <- predict(autoarima_EU_Consumer_Sales, n.ahead = 6)

MAPE_auto_arima_sales_EU_Consumer <- accuracy(forecast_sales_EU_Consumer_auto_arima$pred,EU_Consumer_sales_test[,2])[5]
MAPE_auto_arima_sales_EU_Consumer
#28.9226

##### plotting the predictions and original values for Auto ARIMA EU Consumer Sales
labels = c("original","predicted")
auto_arima_predict_EU_Consumer_Sales <- c(fitted(autoarima_EU_Consumer_Sales),ts(forecast_sales_EU_Consumer_auto_arima$pred))
plot(total_timeseries_EU_Consumer_sales, col = "black" , xlab = xlab1 , ylab = ylab_sales , main = paste(title_sales1 , "(Auto ARIMA)" , sep= " "))
lines(auto_arima_predict_EU_Consumer_Sales, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##################################################################################################################################################
##################################################################################################################################################

#3d. Model Building and Evaluation for EU Consumer Demand
############################################################################################

################################ Forcasting Quantity for EU Consumer #############################################


timevals <- c(1:nrow(EU_Consumer_agg))
total_timeseries_EU_Consumer_Quantity <- ts(EU_Consumer_agg$TotalMonthlyQuantity)
total_timeseries_EU_Consumer_Quantity_df <- as.data.frame(cbind(timevals, as.vector(total_timeseries_EU_Consumer_Quantity)))
colnames(total_timeseries_EU_Consumer_Quantity_df) <- c('Month', 'Quantity')

### Plotting Decomposition of Multiplicative Time Series for EU consumer Quantity
plot(decompose(ts(total_timeseries_EU_Consumer_Quantity_df$Quantity, frequency = 12), "multiplicative"))

# Seperating last 6 months for testing
EU_Consumer_Quantity_train = total_timeseries_EU_Consumer_Quantity_df[1:42,]
EU_Consumer_Quantity_test = total_timeseries_EU_Consumer_Quantity_df[43:48,]


timeseries_EU_Consumer_train_Quantity <- ts(EU_Consumer_Quantity_train$Quantity)


title_Quantity1 <- c("Global Mart Demand in EU Market Consumer Segment from 2011-Jan to 2014-Dec")
xlab1 <- c("Months 2011-Jan to 2014-Dec")
ylab_Quantity <- c("Quantity")
cols <- c("red", "blue")
labels <- c("Original", "MVSmoothed")

######## Forecasting Quantity for EU Consumer ##########################################################

# Moving Average Smoothening

plot(timeseries_EU_Consumer_train_Quantity, main=title_Quantity1 , xlab = xlab1, ylab = ylab_Quantity , col=cols[1])

w<-2
smoothed_timeseries_EU_Consumer_train_Quantity <- filter(timeseries_EU_Consumer_train_Quantity, 
                                                         filter=rep(1/(2*w+1),(2*w+1)), 
                                                         method='convolution', sides=2)
diff <- smoothed_timeseries_EU_Consumer_train_Quantity[w+2] - smoothed_timeseries_EU_Consumer_train_Quantity[w+1]
for (i in seq(w,1,-1)) {
  smoothed_timeseries_EU_Consumer_train_Quantity[i] <- smoothed_timeseries_EU_Consumer_train_Quantity[i+1] - diff
}

n <- length(timeseries_EU_Consumer_train_Quantity)
diff <- smoothed_timeseries_EU_Consumer_train_Quantity[n-w] - smoothed_timeseries_EU_Consumer_train_Quantity[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed_timeseries_EU_Consumer_train_Quantity[i] <- smoothed_timeseries_EU_Consumer_train_Quantity[i-1] + diff
}

lines(smoothed_timeseries_EU_Consumer_train_Quantity, col=cols[2], lwd=2)

legend("topleft", labels, col=cols, lwd=2)

## exponential Smoothening

plot(timeseries_EU_Consumer_train_Quantity, main=title_Quantity1 , xlab = xlab1, ylab = ylab_Quantity)

cols <- c("red", "blue", "green", "yellow" , "black")
alphas <- c(0.02, 0.1, 0.2, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedexp_timeseries_EU_Consumer_train_Quantity <- HoltWinters(timeseries_EU_Consumer_train_Quantity, alpha=alphas[i],
                                                                   beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedexp_timeseries_EU_Consumer_train_Quantity)[,1], col=cols[i], lwd=2)
}

legend("topleft", labels, col=cols, lwd=2)

#Considering Moving Average Smoothened for Model Building 
month_no_train <-EU_Consumer_Quantity_train$Month
smoothed_EU_Consumer_Quantity_df <- as.data.frame(cbind(month_no_train, as.vector(smoothed_timeseries_EU_Consumer_train_Quantity)))
colnames(smoothed_EU_Consumer_Quantity_df) <- c('Month', 'Quantity')

# Looking at the Graph there is Seasonality with upward Trend
# hence Fitting Multiplicative Model with Sinusoid Function
lmfit_EU_Consumer_Quantity <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                                 + Month, data=smoothed_EU_Consumer_Quantity_df)

#Global Component
global_predict_EU_Consumer_Quantity <- predict(lmfit_EU_Consumer_Quantity, Month=month_no_train)
summary(global_predict_EU_Consumer_Quantity)
plot(global_predict_EU_Consumer_Quantity, col='red', type = "l" , main=c("Global Predict for EU Consumer Demand") , xlab = xlab1, ylab = ylab_Quantity)


#Local Component
local_predict_EU_Consumer_Quantity <- timeseries_EU_Consumer_train_Quantity - global_predict_EU_Consumer_Quantity
plot(local_predict_EU_Consumer_Quantity, col='red', type = "l" , main=c("Local Predict for EU Consumer Demand") , xlab = xlab1, ylab = ylab_Quantity)
acf(local_predict_EU_Consumer_Quantity)
acf(local_predict_EU_Consumer_Quantity, type="partial")
armafit_EU_Consumer_Quantity <- auto.arima(local_predict_EU_Consumer_Quantity)

tsdiag(armafit_EU_Consumer_Quantity)
armafit_EU_Consumer_Quantity


# checking for white noise in Residual series

residual_EU_Consumer_Quantity <- local_predict_EU_Consumer_Quantity - fitted(armafit_EU_Consumer_Quantity)

adf.test(residual_EU_Consumer_Quantity,alternative = "stationary") #p-value = 0.01
kpss.test(residual_EU_Consumer_Quantity) #p-value = 0.1

# Hence the Time series is Stationary

#month_no_test <- seq(43:48)
month_no_test <- EU_Consumer_Quantity_test$Month

global_predict_EU_Consumer_Quantity_test <- predict(lmfit_EU_Consumer_Quantity,data.frame(Month =month_no_test))

forecast_Quantity_EU_Consumer <- global_predict_EU_Consumer_Quantity_test

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_classic_decomp_Quantity_EU_Consumer <- accuracy(forecast_Quantity_EU_Consumer,EU_Consumer_Quantity_test[,2])[5]
MAPE_classic_decomp_Quantity_EU_Consumer
#31.39889

#### Plotting  the predictions and  original values for EU Consumer Quantity
labels = c("original","predicted")
classic_decomp_predict_EU_Consumer_Quantity <- c(ts(global_predict_EU_Consumer_Quantity),ts(global_predict_EU_Consumer_Quantity_test))
plot(total_timeseries_EU_Consumer_Quantity, col = "black" , xlab = xlab1 , ylab = ylab_Quantity , main = title_Quantity1)
lines(classic_decomp_predict_EU_Consumer_Quantity, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##########################################################################################
### Using ARIMA Fit for Model Building and Evaluation for EU Consumer Quantity
##########################################################################################

autoarima_EU_Consumer_Quantity <- auto.arima(timeseries_EU_Consumer_train_Quantity)
autoarima_EU_Consumer_Quantity
tsdiag(autoarima_EU_Consumer_Quantity)
plot(autoarima_EU_Consumer_Quantity$x, col="black")
lines(fitted(autoarima_EU_Consumer_Quantity), col="red")

#Again, let's check if the residual series is white noise

residual_EU_Consumer_Quantity_auto_arima <- timeseries_EU_Consumer_train_Quantity - fitted(autoarima_EU_Consumer_Quantity)

adf.test(residual_EU_Consumer_Quantity_auto_arima,alternative = "stationary") #p-value = 0.01
kpss.test(residual_EU_Consumer_Quantity_auto_arima) #p-value = 0.1

#Also, let's evaluate the model using MAPE
forecast_Quantity_EU_Consumer_auto_arima <- predict(autoarima_EU_Consumer_Quantity, n.ahead = 6)

MAPE_auto_arima_Quantity_EU_Consumer <- accuracy(forecast_Quantity_EU_Consumer_auto_arima$pred,EU_Consumer_Quantity_test[,2])[5]
MAPE_auto_arima_Quantity_EU_Consumer
#30.13319

##### plotting the predictions and original values for Auto ARIMA EU Consumer Quantity
labels = c("original","predicted")
auto_arima_predict_EU_Consumer_Quantity <- c(fitted(autoarima_EU_Consumer_Quantity),ts(forecast_Quantity_EU_Consumer_auto_arima$pred))
plot(total_timeseries_EU_Consumer_Quantity, col = "black" , xlab = xlab1 , ylab = ylab_Quantity , main = paste(title_Quantity1 , "(Auto ARIMA)" , sep= " "))
lines(auto_arima_predict_EU_Consumer_Quantity, col = "red")
legend("topleft", labels , col=c("black","red"), lwd=2)
##################################################################################################################################################
##################################################################################################################################################
#Summary:-
#MAPE_classic_decomp_sales_APAC_Consumer
#23.86525
#MAPE_auto_arima_sales_APAC_Consumer
#27.68952
#MAPE_classic_decomp_Quantity_APAC_Consumer
#21.71966
#MAPE_auto_arima_Quantity_APAC_Consumer
#26.24458
#MAPE_classic_decomp_sales_EU_Consumer
#28.27462
#MAPE_auto_arima_sales_EU_Consumer
#28.9226
#MAPE_classic_decomp_Quantity_EU_Consumer
#31.39889
#MAPE_auto_arima_Quantity_EU_Consumer
#30.13319


