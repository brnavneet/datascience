#Loading the Data
uber <- read.csv("Uber Request Data.csv", sep=",")
library(tidyr)
library(dplyr)


## DATA CLEANING
########################################################################
#Standardizing Date Time format on colum : Request.timestamp
uber$Request.timestamp <- as.character(uber$Request.timestamp)
uber$Request.timestamp_Temp <- uber$Request.timestamp
uber$Request.timestamp <- format(as.POSIXct(uber$Request.timestamp,format="%d/%m/%Y %H:%M"), format="%d-%m-%Y %H:%M:%S")
uber$Request.timestamp_Temp <- as.character(uber$Request.timestamp_Temp)
uber[which(is.na(uber$Request.timestamp)),'Request.timestamp'] <- uber[which(is.na(uber$Request.timestamp)),'Request.timestamp_Temp']
uber$Request.timestamp <- as.POSIXct(uber$Request.timestamp,format="%d-%m-%Y %H:%M:%S")
uber$Request.timestamp_Temp <- NULL

#Standardizing Date Time format on colum : Drop.timestamp
uber$Drop.timestamp <- as.character(uber$Drop.timestamp)
uber$Drop.timestamp_Temp <- uber$Drop.timestamp
uber$Drop.timestamp <- format(as.POSIXct(uber$Drop.timestamp,format="%d/%m/%Y %H:%M"), format="%d-%m-%Y %H:%M:%S")
uber$Drop.timestamp_Temp <- as.character(uber$Drop.timestamp_Temp)
uber[which(is.na(uber$Drop.timestamp)),'Drop.timestamp'] <- uber[which(is.na(uber$Drop.timestamp)),'Drop.timestamp_Temp']
uber$Drop.timestamp <- as.POSIXct(uber$Drop.timestamp,format="%d-%m-%Y %H:%M:%S")
uber$Drop.timestamp_Temp <- NULL

#######################################################################

## DERIVE METRICES
##################################################################################
#1. Derive Request Hour , Minute and Day of the week
uber$Request.Hour <- format(uber$Request.timestamp, format="%H")
uber$Request.Hour <- format(uber$Request.timestamp, format="%H")
uber$Request.Minute <- format(uber$Request.timestamp, format="%M")
uber$Request.day <- weekdays(uber$Request.timestamp , abbreviate = TRUE)

#2. Derive Request Hour , Minute and Day of the week
uber$Drop.Hour <- format(uber$Drop.timestamp, format="%H")
uber$Drop.Hour <- format(uber$Drop.timestamp, format="%H")
uber$Drop.Minute <- format(uber$Drop.timestamp, format="%M")
uber$Drop.day <- weekdays(uber$Drop.timestamp, abbreviate = TRUE)

#3. Derive Drop point
uber$Drop.point <- vapply(uber$Pickup.point, function(x) if(x == "City"){"Airport"}else{"City"}, character(1))
###################################################################################

## Visual Plots for most pressing problems for Uber
########################################################################################
library("ggplot2")

#1a. Frequency of requests that get cancelled or show 'no cars available'
ggplot(uber,aes(x=Status)) + geom_bar()
#Analysis :- Unavailability of Cars is High.
#1b. Frequency of requests that get cancelled or show 'no cars available' Vs and the time slots (early mornings, late evenings etc.)
ggplot(uber,aes(x=Request.Hour,fill=Status)) + geom_histogram(position = "dodge", stat="count")
# Analysis : No. of Cancellation is High during early morning from 5 AM to 9 AM , No. of Unavailability of cars are high in the evening from 5pm to 10pm
#1c. Frequency of requests that get cancelled or show 'no cars available' Vs and the time slots (early mornings, late evenings etc.) for types of requests (city to airport / airport to city etc.) 
ggplot(uber,aes(x=Request.Hour,fill=Status)) + geom_histogram(position = "dodge", stat="count") + facet_wrap(vars(Pickup.point))
#Analysis : Cancellation is high at early morning for City Pickups , Unavailability of cars are high in evening for Airport Pickups.
########################################################################################

#Find out the gap between supply and demand 
########################################################################
#2a. Find the time slots when the highest gap exists
uber_gap <- uber[which(uber$Status == "Cancelled" | uber$Status == "No Cars Available"),]
uber_gap$Request.Hour <- as.factor(uber_gap$Request.Hour)
uber_gap_group <- group_by(uber_gap , Request.Hour)
uber_gap_summ <- summarise(uber_gap_group,count=n())
uber_gap_summ <- arrange(uber_gap_summ , desc(count))
highest_gap_time_slot <- uber_gap_summ[1,1]
highest_gap_time_slot
#Highest Gap exist at arounf 6PM - 7PM in the evening
#Plotting the gap for the time slot
ggplot(uber_gap,aes(x=Request.Hour,fill=Status)) + geom_histogram(position = "dodge", stat="count")

#2b.finding the type of request for which gap is the most severe in the identified time slot.
uber_gap_at_6 <- uber_gap[which(uber_gap$Request.Hour == "18"),]
uber_gap_at_6 <- uber_gap[which(uber_gap$Request.Hour == "18"),]
uber_gap_at_6$Pickup.point <- as.factor(uber_gap_at_6$Pickup.point)
uber_gap_at_6_group <- group_by(uber_gap_at_6,Pickup.point)
uber_gap_at_6_summ <- summarise(uber_gap_at_6_group,count=n())
uber_gap_at_6_summ <- arrange(uber_gap_at_6_summ,desc(count))
highest_gap_at_6_pickuppoint <- uber_gap_at_6_summ[1,1]
highest_gap_at_6_pickuppoint
#Plotting The gap for type of reuqest for the identified time-slots
ggplot(uber_gap_at_6,aes(x=Pickup.point,fill=Status)) + geom_histogram(position = "dodge", stat="count")


###############################################################################################
#. 3 Reason for demand supply gap
#further Drill down to find why cars are unavailable from Airport at aorunf 6-7PM
ggplot(uber[which(!is.na(uber$Drop.Hour)),],aes(x=Drop.Hour,fill=Drop.point)) + geom_bar(position="dodge")
ggplot(uber[which(!is.na(uber$Request.Hour)),],aes(x=Request.Hour,fill=Pickup.point)) + geom_bar(position="dodge")

#The above 2 Plots shows clearly that 
#a. the amount of cars coming into the Airport are way less than no. of cars requested from  Airport
#b. demand from city to Airport is way less from 2PM to 8 PM
#c. The Drop at the airport is high at around 6 AM to 11AM and the pickup demand Airport is High at around 6PM-7PM. Hence the waiting time for many drivers could be high.
# Average time from City to Airport is 52 min , AT 6PM-7Pm is heavy traffic is expected due to office goers.
uber$traveltime = uber$Drop.timestamp - uber$Request.timestamp
avg_time_cit_airport <- mean(uber[which(uber$Drop.point == "Airport" & ! is.na(uber$traveltime)),'traveltime'])
avg_time_cit_airport



##################################################################################

#recommendations
####################################################################################################
#1. Airport Shared Shuttles :- for people who wont mind to pool the cab to go to common locations.
#2. Allow User to Pre-schedule Cabs , this would help the drivers to know their schedule and avoid wait time.
#3. Uber could do some Pre-dictive analysis of the flights take off and landing schedule.