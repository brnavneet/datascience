###############################################
#Load Data
###############################################

beermart <- read.csv("beer_data.csv", stringsAsFactors = FALSE)

str(beermart)
nrow(beermart)  # no of rows = 475984
ncol(beermart)  # no of columns = 3

# Missing values
check_na <- sapply(beermart, function(x) sum(is.na(x)))
check_na  # No Missing Value
check_emty <- sapply(beermart, function(x) sum(x==""))
check_emty

# removing empty values
beermart <- beermart[!(beermart$review_profilename==""),]

# Looking for duplicate entries and removing it
beermart <- dplyr::distinct(beermart,beer_beerid,review_profilename,.keep_all = TRUE)

#No. of Unique Beer
length(unique(beermart$beer_beerid))  #40304

#No. of Unique Profile Names
length(unique(beermart$review_profilename)) #22497

#############################################
library(recommenderlab)

##############################################
#1. Data Preparation
##############################################

#1.1 Choose only those beers that have at least N number of reviews

# aggregating by review count group by beer
#install.packages('sqldf')
library(sqldf)
library(ggplot2)
library(dplyr)
library(tidyr)
agg_beermart <- sqldf("select beer_beerid,count(review_overall) as no_of_reviews
                 from beermart
                 group by beer_beerid order by no_of_reviews desc;")
ggplot(agg_beermart,aes(no_of_reviews)) +geom_histogram(binwidth = 10)
# No. of reviews for maximum beers seems to be very less.

nrow(agg_beermart[which(agg_beermart$no_of_reviews >= 50),])
# No. of Beers having greater than or equal to 50 reviews are 2069

nrow(agg_beermart[which(agg_beermart$no_of_reviews < 10),])
#33355 out of 40308  Beers have less than 10 reviews.

nrow(agg_beermart[which(agg_beermart$no_of_reviews < 5),])
#29225 out of 40308  Beers have less than 5 reviews.

nrow(agg_beermart[which(agg_beermart$no_of_reviews == 1),])
#18056 out of 40308  Beers have just 1 review.

#Selecting value of N as 50 i.e selecting beers with greater than or equal to 50 reviews.
N <- 50

#Finally choosing beers with atleast 50 reviews.
#beermart_final <- sqldf("select * from beermart 
#                        where beer_beerid in (select beer_beerid from agg_beermart 
#                        where no_of_reviews >= 50);")


# Now aggregating review count based on users or review_profilename
agg_user_beermart <- sqldf("select review_profilename,count(review_overall) as no_of_reviews
                      from beermart
                      group by review_profilename order by no_of_reviews desc;")

ggplot(agg_user_beermart,aes(no_of_reviews)) +geom_histogram(binwidth = 10)
# No. of reviews by maximum users seems to be very less.
nrow(agg_user_beermart[which(agg_user_beermart$no_of_reviews >= 30),])
# No. of users rating greater than or equal to 30 are 2496

nrow(agg_user_beermart[which(agg_user_beermart$no_of_reviews < 20),])
# No. of users rating less than 20  are 16127

nrow(agg_user_beermart[which(agg_user_beermart$no_of_reviews < 10),])
# No. of users rating less than 20 are 14370

nrow(agg_user_beermart[which(agg_user_beermart$no_of_reviews < 5),])
# No. of users rating less than 5 are 11916

#Finally choosing beer with atleast 50 ratings users provided atleast 30 ratings.
beermart_final <- sqldf("select review_profilename , beer_beerid , review_overall from beermart 
                        where review_profilename in (select review_profilename from agg_user_beermart 
                        where no_of_reviews >= 30) and 
                        beer_beerid in (select beer_beerid from agg_beermart 
                        where no_of_reviews >= 50);")

beermart_final$review_profilename <- as.factor(beermart_final$review_profilename)
beermart_final$beer_beerid <- as.factor(beermart_final$beer_beerid)
summary(beermart_final)



#1.2 Convert this data frame to a "realratingMatrix" before you build your collaborative filtering models

beermart_realratingmatrix <- as(beermart_final, "realRatingMatrix")
class(beermart_realratingmatrix)

# get some informtaion
dimnames(beermart_realratingmatrix)
rowCounts(beermart_realratingmatrix)
colCounts(beermart_realratingmatrix)
rowMeans(beermart_realratingmatrix)

#########################################################################################################

#2. Data Exploration
#############################################################################################
#2.1  Determine how similar the first ten users are with each other and visualise it

beermart_similar_users <- similarity(beermart_realratingmatrix[1:10,],
                            method = "cosine",
                            which = "users")

#User Similarity matrix
as.matrix(beermart_similar_users)

#Visualise User similarity matrix
image(as.matrix(beermart_similar_users), main = "Beermart User similarity")
# User 2 & 10 are similiar and User 3 & 9 are similiar.

#2.2 Compute and visualise the similarity between the first 10 beers

beermart_similar_items <- similarity(beermart_realratingmatrix[,1:10],
                            method = "cosine",
                            which = "items")
#Beer Similarity matrix
as.matrix(beermart_similar_items)

#Visualise Beer similarity matrix
image(as.matrix(beermart_similar_items), main = "Beermart Item similarity")
# Beer 3 and 5 are similiar.


#2.3 What are the unique values of ratings?
unique(beermart_final$review_overall)
#3.0 4.5 4.0 5.0 3.5 2.0 2.5 1.5 1.0

#2.4 Visualise the rating values and notice:

qplot(getRatings(beermart_realratingmatrix), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

#Most of the rating for the beers are 4 and 4.5

summary(getRatings(beermart_realratingmatrix)) # Skewed to the right

#Average Beer ratings 3.87

qplot(getRatings(normalize(beermart_realratingmatrix, method = "Z-score")), binwidth = 0.5, 
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(beermart_realratingmatrix, method = "Z-score")))


qplot(rowCounts(beermart_realratingmatrix), binwidth = 10, 
      main = "Beers Rated on average", 
      xlab = "no. of users", 
      ylab = "no. of beers rated")
#Most users rate less number of Beer.
#Very few users have rated more Beer
summary(rowCounts(beermart_realratingmatrix))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00   31.00   49.00   72.45   89.00  518.00 

#The average number of ratings given by the users : ~72

qplot(colCounts(beermart_realratingmatrix), binwidth = 10, 
      main = "Beers Rated on average", 
      xlab = "no. of beer", 
      ylab = "no. of beers rated")
#Most beers have recieved less review.
#Very few beers have recieved high no. of reviews.
summary(colCounts(beermart_realratingmatrix))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#26.0    57.0    82.0   111.5   133.0   600.0 

#The average number of ratings given to the beers : ~111

##################################################################################
#3. Recommendation Models
###############################################################################
#3.1 Divide your data into training and testing datasets

# method = 'split' , dividing data into train = 90% and test= 10%
scheme_split <- evaluationScheme(beermart_realratingmatrix, method = "split", train = .9,
                           k = 1, given = 2, goodRating = 4)
scheme_split
#Evaluation scheme with 2 items given
#Method: 'split' with 1 run(s).
#Training set proportion: 0.900
#Good ratings: >=4.000000
#Data set: 3176 x 2064 rating matrix of class 'realRatingMatrix' with 230088 ratings.

# method = 'cross-validation' , dividing data into train = 90% and test= 10%
scheme_cv <- evaluationScheme(beermart_realratingmatrix, method = "cross-validation", train = .9,
                                 k = 5, given = 2, goodRating = 4)
scheme_cv
#Evaluation scheme with 2 items given
#Method: 'cross-validation' with 5 run(s).
#Good ratings: >=4.000000
#Data set: 3176 x 2064 rating matrix of class 'realRatingMatrix' with 230088 ratings.

# 3.2 Build IBCF and UBCF models

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# run algorithms for method split, predict next n beers
results_with_split <- evaluate(scheme_split, algorithms, n=c(1, 3, 5, 10, 15, 20))
#UBCF run fold/sample [model time/prediction time]

#Available parameter (with default values):
#  method	 =  cosine
#nn	 =  25
#sample	 =  FALSE
#normalize	 =  center
#verbose	 =  FALSE
#[0.14sec/4.88sec] 
#IBCF run fold/sample [model time/prediction time]
#1  [62.97sec/0.11sec] 
class(results_with_split)

# Draw ROC curve
plot(results_with_split, annotate = 1:4, legend="topleft")

# run algorithms for method cross-validation, predict next n beers
results_with_cv <- evaluate(scheme_cv, algorithms, n=c(1, 3, 5, 10, 15, 20))
#UBCF run fold/sample [model time/prediction time]
#IBCF run fold/sample [model time/prediction time]
#1  [77.57sec/0.22sec] 
#2  [77.18sec/0.34sec] 
#3  [74.68sec/0.23sec] 
#4  [72.93sec/0.24sec] 
#5  [79.78sec/0.34sec] 

class(results_with_cv)

# Draw ROC curve
plot(results_with_cv, annotate = 1:4, legend="topleft")

#3.3 Compare the performance of the two models and suggest the one that should be deployed

#Plot the ROC curves for UBCF and IBCF and compare them

#UBCF performs better than IBCF for both valuation scheme split as well as cross-validation

#3.4 Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"

#Recommendation for Users
recommend_ubcf <- Recommender(beermart_realratingmatrix,method = "UBCF")

recommend_ubcf

# Recommandation for "cokes" top 5 beers
beer_recommedn_cokes <- predict(recommend_ubcf, beermart_realratingmatrix['cokes'], n=5)
as(beer_recommedn_cokes,"list")

#Top 5 beers recommendation for "cokes" are
#"7971" "645"  "1346" "582"  "2041"

# Recommandation for "genog" top 5 beers
beer_recommedn_genog <- predict(recommend_ubcf, beermart_realratingmatrix['genog'], n=5)
as(beer_recommedn_genog,"list")

#Top 5 beers recommendation for "genog" are
#"57908" "1160"  "1093"  "1161"  "1445" 

# Recommandation for "giblet" top 5 beers
beer_recommedn_giblet <- predict(recommend_ubcf, beermart_realratingmatrix['giblet'], n=5)
as(beer_recommedn_giblet,"list")

#Top 5 beers recommendation for "giblet" are
#"19960" "4083"  "582"   "11757" "2041"  


