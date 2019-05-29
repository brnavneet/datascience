#####################################################################################

# 1. Business Understanding: 

#The objective is to identify digits (0-9) written in an image.


#Goal is to to develop a model using Support Vector Machine which should correctly classify the handwritten digits based on the pixel values given as features.

#####################################################################################

#2 Data Preparation

#Loading required Libraries

library('caret')
library('kernlab')
library('dplyr')
library('readr')
library('ggplot2')
library('gridExtra')

#2a. Data loading

mnist_train <- read.csv("mnist_train.csv", stringsAsFactors = F,header = F)
mnist_test <- read.csv("mnist_test.csv", stringsAsFactors = F,header = F)


#2b. Changing column names only for digits.

colnames(mnist_train)[1] <- "digits"
colnames(mnist_test)[1] <- "digits"

# checking if columnnames are same

sum(colnames(mnist_train) != colnames(mnist_test))  #0 ,column names are same

# converting digits column to factor
mnist_train$digits <- as.factor(mnist_train$digits)
mnist_test$digits <- as.factor(mnist_test$digits)

#Understanding Dimensions

dim(mnist_train)

#Structure of the dataset

str(mnist_train)

#printing first few rows

head(mnist_train)

#Exploring the data

summary(mnist_train)

#checking missing value

chk_miss_value <- sapply(mnist_train, function(x) sum(is.na(x)))  

sum(chk_miss_value) #0 , No Missing value

# As the training data set is huge we sample it for 15% and considering full 15% as training data
set.seed(1)
trainindices = sample(1:nrow(mnist_train), 0.15*nrow(mnist_train))
dig_train = mnist_train[trainindices, ]

# using 15%  mnist_test data for testing
set.seed(1)
testindices = sample(1:nrow(mnist_test), 0.15*nrow(mnist_test))
dig_test = mnist_test[testindices, ]


#########################################################################################################

#3. Constructing Model

#Checking wiht Linear Kernel
linear_kernel_model <- ksvm(digits~ ., data = dig_train, scale = FALSE, kernel = "vanilladot")
evaluate_linear_kernel_model <- predict(linear_kernel_model, dig_test)

#confusion matrix - Linear Kernel
confusionMatrix(evaluate_linear_kernel_model,dig_test$digits)

#Accuracy : 0.908   (90.8%)


#Using RBF Kernel
rbf_kernel_model <- ksvm(digits~ ., data = dig_train, scale = FALSE, kernel = "rbfdot")
evaluate_rbf_kernel_model <- predict(rbf_kernel_model, dig_test)

#confusion matrix - RBF Kernel
confusionMatrix(evaluate_rbf_kernel_model,dig_test$digits)


# Accuracy : 0.9527  (95.27%)

#######################################################################################
#4. Hyperparameter tuning and Cross-Validation

trainControl <- trainControl(method="cv", number=2, allowParallel = TRUE, verboseIter = TRUE)
metric <- "Accuracy"
set.seed(100)
grid <- expand.grid(.sigma=c(0.00000005, 0.00000016355 , 0.0000002), .C=c(0.1,0.5,1,2) )


fit_svm_model <- train(digits~., data=dig_train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
# Fitting sigma = 2e-07, C = 2 on full training set

print(fit_svm_model)

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 2e-07 and C = 2.

plot(fit_svm_model)

# Validating the model results on test data
evaluate_non_linear_svm<- predict(fit_svm_model, dig_test)
confusionMatrix(evaluate_non_linear_svm, dig_test$digits)

#Accuracy : 0.958  (95.8%)



#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity           1.00000   0.9828   0.9497  0.95172   0.9506  0.96183  0.96622  0.94194   0.9527  0.92424
#Specificity           0.99491   0.9962   0.9970  0.99705   0.9940  0.99489  0.99778  0.99554   0.9977  0.98830
#Pos Pred Value        0.94697   0.9716   0.9742  0.97183   0.9506  0.94737  0.97945  0.96053   0.9817  0.88406
#Neg Pred Value        1.00000   0.9977   0.9941  0.99485   0.9940  0.99634  0.99631  0.99332   0.9940  0.99266
#Prevalence            0.08333   0.1160   0.1060  0.09667   0.1080  0.08733  0.09867  0.10333   0.1127  0.08800
#Detection Rate        0.08333   0.1140   0.1007  0.09200   0.1027  0.08400  0.09533  0.09733   0.1073  0.08133
#Detection Prevalence  0.08800   0.1173   0.1033  0.09467   0.1080  0.08867  0.09733  0.10133   0.1093  0.09200
#Balanced Accuracy     0.99745   0.9895   0.9734  0.97439   0.9723  0.97836  0.98200  0.96874   0.9752  0.95627
