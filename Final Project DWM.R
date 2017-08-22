# Course : CS 513
# First Name : Drashti Patel
# Second Name : Rashmi Basavaraj
# ID: 10411997(Drashti Patel)
# purpose: Final Project
###########################################################
#1. DESCRIPTION
#Classify whether or not somebody will experience 
#financial distress in the next two years
#Data Source https://www.kaggle.com/c/GiveMeSomeCredit/data
###########################################################

## remove all objects
rm(list=ls())

#Use class library
library(class)
library("ggplot2")

#Read Data from CSV file
data1<- read.csv("C:/Users/Pinank/Desktop/Project/cs-training.csv")
options(scipen=999)

#Copy the data to another variable
copy_data1<- data1
options(scipen=999)#to remove exponent formatting of values and display in decimal format 

#2. Data Exploration
summary(copy_data1$NumberOfDependents)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   0.000   0.000   0.757   1.000  20.000    3924 
qplot(copy_data1$NumberOfDependents, geom="histogram") 
sum(is.na(copy_data1$NumberOfDependents))#3924

summary(copy_data1$NumberRealEstateLoansOrLines)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   1.018   2.000  54.000 
qplot(copy_data1$NumberRealEstateLoansOrLines, geom="histogram", xlim = c(0,15)) 
sum(is.na(copy_data1$NumberRealEstateLoansOrLines)) #0

summary(copy_data1$NumberOfTime30.59DaysPastDueNotWorse)
qplot(copy_data1$NumberOfTime30.59DaysPastDueNotWorse, geom = "histogram", xlim = c(0,10)) # a small value 98 will have something.
sum(copy_data1$NumberOfTime30.59DaysPastDueNotWorse==98) #264 entries have 98 

summary(copy_data1$DebtRatio)
sum(is.na(copy_data1$DebtRatio)) #0

summary(copy_data1$MonthlyIncome)
qplot(copy_data1$MonthlyIncome, xlim = c(0,40000))
qplot(copy_data1$MonthlyIncome, xlim = c(0,60000))
plot(copy_data1$MonthlyIncome) # one outlier value
sum(is.na(copy_data1$MonthlyIncome)) #29731

summary(copy_data1$NumberOfTimes90DaysLate)
qplot(copy_data1$NumberOfTimes90DaysLate)
sum(is.na(copy_data1$NumberOfTimes90DaysLate)) #0


summary(copy_data1$NumberOfOpenCreditLinesAndLoans)
qplot(copy_data1$NumberOfOpenCreditLinesAndLoans)
sum(is.na(copy_data1$NumberOfOpenCreditLinesAndLoans)) #0


summary(copy_data1$RevolvingUtilizationOfUnsecuredLines)
qplot(copy_data1$RevolvingUtilizationOfUnsecuredLines, xlim = c(500,37500))
plot(copy_data1$RevolvingUtilizationOfUnsecuredLines, ylim = c(500, 37500))
sum(copy_data1$RevolvingUtilizationOfUnsecuredLines>1000) # >1=3321,>2 371, >3=292, >4=264 >100=223 
#so I think we can delete >3 values.
sum(is.na(copy_data1$RevolvingUtilizationOfUnsecuredLines))#0

summary(copy_data1$age)
plot(copy_data1$age, ylim = c(0,150)) # there are a few age>100 and one age=0
sum(copy_data1$age>100) # 13 entries with age>100 
sum(is.na(copy_data1$age)) #0

# To predict missing values of monthly income
#create new dataset from original dataset
NewData <- subset(copy_data1, is.na(copy_data1$MonthlyIncome))
summary(NewData$MonthlyIncome)

#how many total predicted 1 data entries in overall data
totalData<- data1[data1$SeriousDlqin2yrs==1,] #10026
#find out Total NA In Monthly Income
New_DataFrame<- data1[is.na(data1$MonthlyIncome),]
View(New_DataFrame)
New_DataFramermNA<- na.omit(data1)
View(New_DataFramermNA)

qplot(New_DataFramermNA$MonthlyIncome, geom="histogram", xlim = c(0,20000)) 
nrow(New_DataFrame) #29731
New_Frame1<-New_DataFrame[New_DataFrame$SeriousDlqin2yrs==0,]
#View(New_Frame1)
nrow(New_Frame1) #28062
#total How Many NA Monthly Income containing NA values contains 1.
New_Frame2<-New_DataFrame[New_DataFrame$SeriousDlqin2yrs==1,]
#View(New_Frame2)
nrow(New_Frame2) #1669
#Percentage
per_1sInTotalData<- ((nrow(totalData)/nrow(data1))*100) #6.684
per_1sInIncomeNA<- ((nrow(New_Frame2)/nrow(New_DataFrame))*100) #5.61366

plot(New_DataFramermNA$DebtRatio,New_DataFramermNA$MonthlyIncome, xlim = c(0,10000), ylim = c(0,10000))
summary(MonthlyIncome)
qplot(MonthlyIncome, geom="histogram", xlim = c(0,20000)) 
#plot(training$DebtRatio,training$MonthlyIncome, xlim = c(0,10000),ylim = c(0,90000),col='red',main="Without Changing",xlab="Debt Ratio",ylab="Monthly Income")

#Removing NA Values
library(class)
View(New_DataFramermNA)
New_DF_rmNA <- subset(New_DataFramermNA[,-c(1,4,5,9,10,11,12)])
View(New_DF_rmNA)

mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                                           min (x, na.rm = TRUE)))}


training_norm <-data.frame(apply(New_DF_rmNA,2,mmnorm))
View(training_norm)

#First Attempt KNN
library(class)
set.seed(84188)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))

test<- training_norm[-idx,]
training_ds <- training_norm[idx,]

summary(training_norm)

set.seed(84188)
predict_knn_k03 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=3)
table(predict_knn_k03,test$SeriousDlqin2yrs )
#predict_knn_k03     0     1
#              0 32935  2396
#              1   608   142
Accuracy <- (32935+142)/(32935+2396+608+142) #0.9167
Errorate<- (608+2396)/(32935+2396+608+142)
Errorate
#[1] 0.08325712

set.seed(84188)
predict_knn_k10 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=10)
table(predict_knn_k10,test$SeriousDlqin2yrs )
#predict_knn_k10     0     1
#               0 33477  2519
#               1    66    19
Accuracy <- (33477+19)/(33477+2519+66+19)   #0.9283556
Accuracy
predict_knn_k50 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=50)
table(predict_knn_k50,test$SeriousDlqin2yrs )
#predict_knn_k50 0     1
#            0 33543  2538
#            1     0     0
Accuracy <- (33543)/(33543+2538)
Accuracy  #0.9296


#Repalce salary Na Values by 1
View(data1)
data1$X<-NULL
newData<- data1
Salary<-1
newData$MonthlyIncome[is.na(newData$MonthlyIncome)] <- Salary
newData[ , "NewDebt"] <- newData$DebtRatio
#View(newData)

expenditure<- newData$DebtRatio * newData$MonthlyIncome
as.integer(expenditure)
newData[,"expenditure"]<- expenditure
#View(newData)

New_DF_rm <- subset(newData[,-c(3,4,5,8,9,10,11,12)])
View(New_DF_rm)

mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min (x, na.rm = TRUE)))}

training_norm <-data.frame(apply(New_DF_rm,2,mmnorm))
View(training_norm)

set.seed(84188)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))

test<- training_norm[-idx,]
training_ds <- training_norm[idx,]

summary(training_norm)

set.seed(84188)
predict_knn_k03 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=3)
table(predict_knn_k03,test$SeriousDlqin2yrs )
#predict_knn_k03     0     1
#             0 41260  2952
#             1   696    92
Accuracy <- (41260+92)/(41260+2952+696+92)
Accuracy
#[1] 0.9189333
set.seed(84188)
predict_knn_k10 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=10)
table(predict_knn_k10,test$SeriousDlqin2yrs )
#predict_knn_k10     0     1
#               0 41919  3029
#               1    37    15
Accuracy <- (41919+15)/(41919+15+3029+37)
Accuracy
#[1] 0.9318667
Errorate<- 1- Accuracy
Errorate
#[1] 0.06813333

set.seed(84188)
predict_knn_k50 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=50)
table(predict_knn_k50,test$SeriousDlqin2yrs )
#predict_knn_k50     0     1
#               0 41956  3044
#               1     0     0
Accuracy <- (41956)/(41956+3044)
Accuracy
Errorate<- 1- Accuracy
Errorate
#[1] 0.06764444
#Replace Salary NA values by Median. 

New_DF_Median <- data1  
New_DF_Median <- subset(New_DF_Median[,-c(3,4,8,9,10,11)])

View(New_DF_Median)
# calculate median
Salary<- median(New_DF_Median$MonthlyIncome, na.rm = TRUE)

New_DF_Median$MonthlyIncome[is.na(New_DF_Median$MonthlyIncome)] <- Salary
New_DF_Median$MonthlyIncome <- ifelse(New_DF_Median$MonthlyIncome==0,1,New_DF_Median$MonthlyIncome)

expenditure<- New_DF_Median$DebtRatio * New_DF_Median$MonthlyIncome
New_DF_Median[,"expenditure"]<-expenditure
View(New_DF_Median)

library(class)
mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                                           min (x, na.rm = TRUE)))}


training_norm <-data.frame(apply(New_DF_Median,2,mmnorm))
View(training_norm)

#First Attempt KNN
library(class)
set.seed(84188)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))

test<- training_norm[-idx,]
training_ds <- training_norm[idx,]

summary(training_norm)

set.seed(84188)
predict_knn_k03 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=3)
table(predict_knn_k03,test$SeriousDlqin2yrs )
#predict_knn_k03     0     1
#               0 41239  2902
#               1   717   142
Accuracy <- (41239+142)/(2902+142+717+41239)
Accuracy #0.91958
Errorate<- 1- Accuracy
Errorate
#[1] 0.08042222

set.seed(84188)
predict_knn_k10 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=10)
table(predict_knn_k10,test$SeriousDlqin2yrs )
#predict_knn_k10     0     1
#             0 41901  3023
#             1    55    21
Accuracy <- (41901+21)/(21+41901+3023+55)
Accuracy
#0.9316

set.seed(84188)
predict_knn_k50 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=50)
table(predict_knn_k50,test$SeriousDlqin2yrs )
#predict_knn_k50     0     1
#             0 41956  3044
#             1     0     0
Accuracy <- (41956+0)/(41956+3044)
Accuracy
#0.9323556

#KNN over everything
New_DF_Median1 <- data1  
age_median <- median(New_DF_Median1$age,na.rm=TRUE)
New_DF_Median1$age <- ifelse(New_DF_Median1$age==0,age_median,New_DF_Median1$age)
number_of_real_estate <- median(New_DF_Median1$NumberRealEstateLoansOrLines, na.rm=TRUE)
New_DF_Median1$NumberRealEstateLoansOrLines <- ifelse(New_DF_Median1$NumberRealEstateLoansOrLines==54,number_of_real_estate,New_DF_Median1$NumberRealEstateLoansOrLines)
New_DF_Median1$NumberOfDependents <- ifelse(is.na(New_DF_Median1$NumberOfDependents),0,New_DF_Median1$NumberOfDependents)

New_DF_Median1$MonthlyIncome<- ifelse(New_DF_Median1$MonthlyIncome==0, 1,New_DF_Median1$MonthlyIncome)
Salary<- median(New_DF_Median1$MonthlyIncome, na.rm = TRUE)
New_DF_Median1$MonthlyIncome[is.na(New_DF_Median1$MonthlyIncome)] <- Salary
expenditure<- New_DF_Median1$DebtRatio * New_DF_Median1$MonthlyIncome

New_DF_Median1[,"expenditure"]<-expenditure
New_DF_Median1$RevolvingUtilizationOfUnsecuredLines[New_DF_Median1$RevolvingUtilizationOfUnsecuredLines>3]<-NA
New_DF_Median1$DebtRatio[New_DF_Median1$DebtRatio==0]<-NA
New_DF_Median1<- na.omit(New_DF_Median1)

View(New_DF_Median1)

write.csv(New_DF_Median1,"C:/Users/Pinank/Desktop/Project/Cleaned_Dataset.csv")

library(class)
mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                                           min (x, na.rm = TRUE)))}


training_norm <-data.frame(apply(New_DF_Median1,2,mmnorm))
View(training_norm)
training_norm$X<- NULL
#First Attempt KNN
library(class)
set.seed(84188)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))

test<- training_norm[-idx,]
training_ds <- training_norm[idx,]

summary(training_norm)

set.seed(84188)
predict_knn_k03 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=3)
table(predict_knn_k03,test$SeriousDlqin2yrs )
#predict_knn_k03     0     1
#               0 40255  2495
#               1   595   334
Accuracy<-(39179+354)/(39179+354+2521+574)
Accuracy
#[1] 0.9273951
Errorate_1<- (2495)/(2829)
Errorate_1
#[1] 0.8819371

set.seed(84188)
predict_knn_k10 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=10)
table(predict_knn_k10,test$SeriousDlqin2yrs )
#predict_knn_k10     0     1
#               0 40663  2650
#               1   187   179
Accuracy <- (40663+179)/(40663+2650+187+179)
Accuracy
#[1] 0.9350489
Errorate<- 1- Accuracy
Errorate
#[1] 0.06495112

Errorate_1<- (2650)/(2829)
Errorate_1
#[1] 0.9367268

set.seed(84188)
predict_knn_k6 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=6)
table(predict_knn_k6,test$SeriousDlqin2yrs )
#predict_knn_k6     0     1
#               0 40501  2575
#               1   349   254
Accuracy <- (40501+254)/(40501+254+2575+349)
Accuracy
#[1] 0.9330571
Errorate<- 1- Accuracy
Errorate
#[1] 0.06694292

Errorate_1<- (2575)/(2829)
Errorate_1
#[1] 0.9102156

set.seed(84188)
predict_knn_k50 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=50)
table(predict_knn_k50,test$SeriousDlqin2yrs )
#predict_knn_k50     0     1
#               0 40794  2770
#               1    56    59
Accuracy <- (40794+59)/(40794+2770+56+59)
Accuracy
#[1] 0.9353007
Errorate<- 1- Accuracy
Errorate
#[1] 0.06469928
Errorate_1<- (2770)/(2829)
Errorate_1
#[1] 0.9791446

set.seed(84188)
predict_knn_k100 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=100)
table(predict_knn_k100,test$SeriousDlqin2yrs )
#predict_knn_k100     0     1
#                 0 39721  2836
#                 1    32    39

rm(list=ls())

#########################################################################
#       Decision TREE CART over Unbalanced cleaned Data
#########################################################################

train<- read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Cleaned_Dataset.csv")
#View(train)
train$X<-NULL
set.seed(50000)
idx_DT <- sample (nrow(train), as.integer(.70*nrow(train)))


test_dt <- train[-idx_DT,]
training_dt <- train[idx_DT,]

#View(training_dt)
training_dt <-  subset(training_dt[,-c(5,6)])
test_dt <-  subset(test_dt[,-c(5,6)])

training_dt$SeriousDlqin2yrs <-ifelse(training_dt$SeriousDlqin2yrs==0,'CR_No','CR_Yes')
test_dt$SeriousDlqin2yrs <-ifelse(test_dt$SeriousDlqin2yrs==0,'CR_No','CR_Yes')

#install.packages(rpart.plot)
library(rpart)

library(rpart.plot)
library(rattle)
library(RColorBrewer)

mytree <- rpart(training_dt$SeriousDlqin2yrs~., data=training_dt)

fancyRpartPlot(mytree)
test_dt_sub <- subset(test_dt[,-1])
Prediction <- data.frame(predict(mytree,test_dt_sub , type ='class'))

Prediction <-edit(Prediction)

table(Prediction$predict.mytree..test_dt_sub..type....class.., test_dt$SeriousDlqin2yrs)
#       CR_No CR_Yes
#CR_No  40411   2454
#CR_Yes   344    470
Accuracy<- (40411+470)/(40411+2454+344+470) #0.9359417569

#####################################################################################################
#                         Decision TREE C5.0 over Unbalanced Cleaned Data
#####################################################################################################
#install.packages("C50")

library(C50)

train<- read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Cleaned_Dataset.csv")
#View(train)
set.seed(50000)
idx <- sample (nrow(train), as.integer(.70*nrow(train)))


test_ds <- train[-idx,]
training_ds <- train[idx,]

test_ds<- subset(test_ds[,-c(1,6,7,8,10,13)])
training_ds<- subset(training_ds[,-c(1,6,7,8,10,13)])



training_ds$SeriousDlqin2yrs <- factor(training_ds$SeriousDlqin2yrs)
C50_treeModel <- C5.0(x = training_ds[, -1], y = training_ds$SeriousDlqin2yrs)

C5imp(C50_treeModel)
#Overall
#NumberOfTimes90DaysLate               100.00
#NumberOfTime60.89DaysPastDueNotWorse   98.90
#RevolvingUtilizationOfUnsecuredLines    7.43
#NumberOfTime30.59DaysPastDueNotWorse    4.94
#age                                     1.62
#NumberOfDependents                      1.02

C5imp(C50_treeModel, metric = "splits")
#Overall
#NumberOfTime30.59DaysPastDueNotWorse 19.148936
#NumberOfTime60.89DaysPastDueNotWorse 19.148936
#NumberOfTimes90DaysLate              19.148936
#age                                  17.021277
#RevolvingUtilizationOfUnsecuredLines 17.021277
#NumberOfDependents                    8.510638

C50_treeModel_predict <- predict(C50_treeModel,test_ds[,-1])

table(x=C50_treeModel_predict,y=test_ds[,1])
#   y   0       1
#x  
# 0 40416  2427
# 1   339   497

Accuracy<- (40416+497)/(339+497+2427+40416)
Accuracy
#0.936



##############################################################################################################

# 1) Over sampling, undersampling and SMOTE#
# 2) Undersampling leads to loss of information
# 3) Oversampling leads to overestimation of minority class
# 4) In SMOTE, the algorithm looks at n nearest neighbors,measures the distance between them and
#    introduces a new observation at the center of n observations.

########################################### OVER & UNDER SAMPLING ############################################
##############################################################################################################
#install.packages("mlr")
#install.packages("DMwR")
#install.packages("unbalanced")
#install.packages("pROC")
#install.packages("class")

library(unbalanced)
library(class)
library(DMwR)
# remove all objects
rm(list=ls())
data2<- read.csv("C:/Users/Pinank/Desktop/Project/Cleaned_Dataset.csv")
data2$X<-NULL
train <- data2
#View(train)

dput(colnames(train))
#names<- read.csv("C:/Users/Pinank/Desktop/Project/Cleaned_Dataset.names", header = F, sep = '\t')[[1]]
colnames(train) <- c("target", "RevolvingUtilizationOfUnsecuredLines", 
                     "age", "NumberOfTime30.59DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome", 
                     "NumberOfOpenCreditLinesAndLoans", "NumberOfTimes90DaysLate", 
                     "NumberRealEstateLoansOrLines", "NumberOfTime60.89DaysPastDueNotWorse", 
                     "NumberOfDependents", "expenditure")
head(train,2)
table(train$target)
#0      1 
#135938   9657 

prop.table(table(train$target)) #overall Probability of the Table
#0          1 
#0.93367217 0.06632783 

str(train)
ind <- sapply(train, is.character)
train[ind] <- lapply(train[ind], as.numeric)
ind <- sapply(train, is.numeric)
ind
#str(train)
library(caret)
library(pROC)
library(DMwR)
set.seed(10000)
splitIndex <- createDataPartition(train$target, p=.50, list = FALSE, times =  1)
trainSplit <- train[splitIndex,]
testSplit <- train[ -splitIndex,]

table(trainSplit$target)
#0     1 
#67947  4851 
prop.table(table(trainSplit$target))
#0          1 
#0.93336355 0.06663645 

ctrl <- trainControl(method = "cv", number = 5)

tbmodel <- train(target ~ ., data= trainSplit, method = "treebag", trControl = ctrl)
predictors <- names(trainSplit)[names(trainSplit)!='target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])
#install.packages("pROC")
library(pROC)
library(DMwR)
auc<-roc(testSplit$target, pred)
print(auc)
#Call:
#roc.default(response = testSplit$target, predictor = pred)
#
#Data: pred in 67991 controls (testSplit$target 0) < 4806 cases (testSplit$target 1).
#Area under the curve: 0.8078

#install.packages("DMwR")
library(DMwR)
#rm(list=ls())
trainSplit$target<-as.factor(trainSplit$target)

#trainSplit<- SMOTE(target ~ ., trainSplit, perc.over=100, perc.under=200) #meaning over sample 100 % target variable and give me undersample twice the oversample.
#table(trainSplit$target)
#0    1 
#9702 9702

#trainSplit<- SMOTE(target ~ ., trainSplit, perc.over=200, perc.under=800)

#Best balanced data with undersampling 300 and oversampling 200 rows. 
set.seed(10000)
trainSplit<- SMOTE(target ~ ., trainSplit, perc.over=200, perc.under=300)
table(trainSplit$target)
#0     1 
#28320 14160 
prop.table(table(trainSplit$target)) #overall Probability of the Table
#0         1 
#0.6666667 0.3333333 
#trainSplit<- SMOTE(target ~ ., trainSplit, perc.over=300, perc.under=400)
#table(trainSplit$target)
#0      1 
#174636  58212 

dim(trainSplit)
#[1] 58212    12
#[1] 368676     12
#[1] 43659    12
#[1] 232848     12
#[1] 33957    12 BEST LATEST 200 300
trainSplit$target<- as.numeric(trainSplit$target)
tbmodel <- train(target ~ ., data= trainSplit, method = "treebag", trControl = ctrl)
pred <- predict(tbmodel$finalModel, testSplit[,predictors])
auc<-roc(testSplit$target, pred)
print(auc)
#Call:
# roc.default(response = testSplit$target, predictor = pred)

#Data: pred in 67991 controls (testSplit$target 0) < 4806 cases (testSplit$target 1).
#Area under the curve: 0.8314

#Call: for 200 & 300
# roc.default(response = testSplit$target, predictor = pred)

#Data: pred in 67991 controls (testSplit$target 0) < 4806 cases (testSplit$target 1).
#Area under the curve: 0.8315

#Call: for 300 & 400.
# roc.default(response = testSplit$target, predictor = pred)

#Data: pred in 67991 controls (testSplit$target 0) < 4806 cases (testSplit$target 1).
#Area under the curve: 0.7782

###################################################################################
#             KNN Classification
#
#####################################################################################
#View(trainSplit)
trainSplit<- read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Balanced Data.csv")
trainSplit$X<-NULL
#KNN
library(class)
mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                                           min (x, na.rm = TRUE)))}


training_norm <-data.frame(apply(trainSplit,2,mmnorm))
#View(training_norm)

#First Attempt KNN
library(class)

set.seed(10000)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))

test<- training_norm[-idx,]
training_ds <- training_norm[idx,]

summary(training_norm)

set.seed(10000)
predict_knn_k03 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=3)

table(predict_knn_k03,test$target )
#predict_knn_k03    0    1
#             0 7515  914
#             1 1249 3420
accuracy <- (7515+3420)/(7515+914+1249+3420) #0.8700565 #200 300
Accuracy
#predict_knn_k03    0    1
#               0 4700  715
#               1 1144 3629
Accuracy<- (4700+3629)/(4700+715+1144+3629) #0.8175 200 200
Accuracy
set.seed(10000)
predict_knn_k04 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=4)
table(predict_knn_k04,test$target )
#predict_knn_k04    0    1
#               0 7438 1092
#               1 1326 3242
Accuracy<- (7438+3242)/(7438+3242+1092+1326) #0.8153
Accuracy

set.seed(10000)
predict_knn_k06 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=6)
table(predict_knn_k06,test$target )
#predict_knn_k06    0    1
#               0 7491 1227
#               1 1273 3107
Accuracy<- (7491+3107)/(7491+3107+1227+1273) #0.80913116
Accuracy
set.seed(10000)
predict_knn_k01 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=1)
table(predict_knn_k01,test$target )
#Accuracy<-(4761+3616)/(1083+728+4761+3616) #0.8222414 for k=2

#predict_knn_k01    0    1
#             0 4929  431
#             1  915 3913
#Accuracy<-(4929+3913)/(4929+915+431+3913) #0.86788 for k=1


set.seed(10000)
predict_knn_k10 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=10)
table(predict_knn_k10,test$target )
#predict_knn_k10    0    1
#               0 7560 1417
#               1 1204 2917
Accuracy<- (7560+2917)/(7560+2917+1204+1217) #0.7799

Accuracy
set.seed(10000)
predict_knn_k50 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=50)
table(predict_knn_k50,test$target )
#predict_knn_k50    0    1
#             0 7612 1845
#             1 1152 2489
Accuracy<- (7612+2489)/(7612+1845+1152+2489) #0.771186
Accuracy
set.seed(10000)
predict_knn_k100 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=100)
table(predict_knn_k100,test$target )
#predict_knn_k100    0    1
#             0 7567 1877
#             1 1197 2457
#Accuracy<-(7567+2457)/(7567+1877+1197+2457) #0.76530
set.seed(10000)
predict_knn_k15 <- knn(training_ds[,-1],test[,-1],training_ds[,1], k=15)
table(predict_knn_k15,test$target )

#########################################################
## 
##      Decision Tree 
##           
##
#########################################################
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

rm(list=ls())
train<-read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Balanced Data.csv")
train$X<-NULL
set.seed(900)
idx_DT <- sample (nrow(train), as.integer(.70*nrow(train)))

training_dt <- train[idx_DT,]
test_dt <- train[-idx_DT,]
#View(test_dt)


training_dt <-  subset(training_dt[,-c(5,6)])
test_dt <-  subset(test_dt[,-c(5,6)])
#View(test_dt)
#View(training_dt)

training_dt$target <-ifelse(training_dt$target==1,'CR_No','CR_Yes')
test_dt$target <-ifelse(test_dt$target==1,'CR_No','CR_Yes')

#Grow the tree 
#mytree<-rpart( cocaine~racegrp+gender+mcs,data=mysubset)
#mytree

#Grow the tree 
mytree <- rpart(training_dt$target~., data=training_dt)
# much fancier graph
fancyRpartPlot(mytree)

test_dt_sub <- subset(test_dt[,-1])
Prediction <- data.frame(predict(mytree,test_dt_sub , type ='class'))
#View(Prediction)
#Prediction <-edit(Prediction)

#use the table to interpret the resutls.
table(Prediction$predict.mytree..test_dt_sub..type....class.., test_dt$target)
#predict.mytree..test_dt_sub..type....class..

#       CR_No CR_Yes
#CR_No   8072   1320
#CR_Yes   591   3115

Accuracy <- (8072+3115)/(591+1320+8072+3115) #0.85409986
Accuracy
################################# Decision TREE C5.0 ################################################
#install.packages("C50")
#####################################################################################################
library(C50)
train<-read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Balanced Data.csv")
set.seed(900)
#View(train)
idx <- sample (nrow(train), as.integer(.70*nrow(train)))


test_ds <- train[-idx,]
training_ds <- train[idx,]

test_ds<- subset(test_ds[,-c(1,6,7,8,10)])
training_ds<- subset(training_ds[,-c(1,6,7,8,10)])
training_ds$target <- factor(training_ds$target)
C50_treeModel <- C5.0(x = training_ds[, -1], y = training_ds$target)

C5imp(C50_treeModel)
#                                     Overall
#NumberOfTimes90DaysLate               100.00
#NumberOfTime60.89DaysPastDueNotWorse   87.84
#NumberOfTime30.59DaysPastDueNotWorse   81.93
#RevolvingUtilizationOfUnsecuredLines   77.35
#age                                    23.51
#expenditure                            19.68
#NumberOfDependents                      7.83
C5imp(C50_treeModel, metric = "splits")
#Overall
#age                                       25
#RevolvingUtilizationOfUnsecuredLines      24
#NumberOfTime30.59DaysPastDueNotWorse      17
#expenditure                               15
#NumberOfDependents                        12
#NumberOfTime60.89DaysPastDueNotWorse       4
#NumberOfTimes90DaysLate                    3
C50_treeModel_predict <- predict(C50_treeModel,test_ds[,-1])

table(x=C50_treeModel_predict,y=test_ds[,1])

#  y
#x      1    2
#  1 8309 1371
#  2  354 3064
Accuracy <- (8309+3064)/(8309+3064+354+1371) 
Accuracy
###############################################################################################
#
#             Random Forest over Balanced Data
#
##############################################################################################
rm(list = ls())
# Load library
library(randomForest)
# Help on ramdonForest package and function
#library(help=randomForest)
#help(randomForest)

#Read DATA.
termCrosssell<-read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Balanced Data.csv")
termCrosssell$X<-NULL

## Explore data frame
names(termCrosssell)

#input dataset has 11 independent variables and a target variable. The target variable target is binary.
table(termCrosssell$target)/nrow(termCrosssell)
#  1         2 
#0.6666667 0.3333333 
#33% of the observations has target variable "yes" and remaining 66%% observations take value "no".

#Now, we will split the data sample into development and validation samples.
sample.ind <- sample(2,nrow(termCrosssell),replace = T,prob = c(0.7,0.3))
cross.sell.dev <- termCrosssell[sample.ind==1,]#training
cross.sell.val <- termCrosssell[sample.ind==2,]#Testing

table(cross.sell.dev$target)/nrow(cross.sell.dev)
## 
##   1Yes     2No 
##0.6657838 0.3342162 
table(cross.sell.val$target)/nrow(cross.sell.val)
## 
##  1Yes         2No 
##0.6687314 0.3312686 

#Both development and validation samples have similar target variable distribution. This is just a sample validation.
#If target variable is factor, classification decision tree is built. We can check the type of response variable.
cross.sell.dev$target <- factor(cross.sell.dev$target) #convert ineteger to factor.
cross.sell.val$target <- factor(cross.sell.val$target)
class(cross.sell.dev$target)
class(cross.sell.val$target)

#Class of target or response variable is factor, so a classification Random Forest will be built. The current data frame has a list of independent variables,
#so we can make it formula and then pass as a parameter value for randomForest.
varNames <- names(cross.sell.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("target")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("target", varNames1, sep = " ~ "))

#Now, we have a sample data and formula for building Random Forest model.
#Let's build 200 decision trees using Random Forest.
set.seed(200)
cross.sell.rf <- randomForest(rf.form,
                              cross.sell.dev,
                              ntree=200,
                              importance=T)

plot(cross.sell.rf,main = "Random Forest Errorrate (200 Dtrees)")

#100 decision trees or a forest has been built using the Random Forest algorithm based learning. We can plot the error rate across decision trees. The plot seems to indicate that after 100 decision trees,
#there is not a significant reduction in error rate.

#Variable importance plot is also a useful tool and can be plotted using varImpPlot function.
#Top 5 variables are selected and plotted based on Model Accuracy and Gini value.
#We can also get a table with decreasing order of importance based on a measure
#(1 for model accuracy and 2 node impurity)

# Variable Importance Plot
varImpPlot(cross.sell.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)

# Variable Importance Table
var.imp <- data.frame(importance(cross.sell.rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
#                                     MeanDecreaseGini                            Variables
#NumberOfTime30.59DaysPastDueNotWorse        2494.7360 NumberOfTime30.59DaysPastDueNotWorse
#NumberOfTimes90DaysLate                     2002.4125              NumberOfTimes90DaysLate
#RevolvingUtilizationOfUnsecuredLines        1963.4157 RevolvingUtilizationOfUnsecuredLines
#NumberOfTime60.89DaysPastDueNotWorse        1322.4477 NumberOfTime60.89DaysPastDueNotWorse
#DebtRatio                                    990.6584                            DebtRatio
#expenditure                                  974.3565                          expenditure
#age                                          970.5989                                  age
#MonthlyIncome                                865.2160                        MonthlyIncome
#NumberOfOpenCreditLinesAndLoans              840.4978      NumberOfOpenCreditLinesAndLoans
#NumberRealEstateLoansOrLines                 512.9280         NumberRealEstateLoansOrLines
#NumberOfDependents                           285.1260                   NumberOfDependents

# Predicting response variable
cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)

table(cross.sell.dev$predicted.response)
# Load Library or packages
library(e1071)
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
# Create Confusion Matrix
confusionMatrix(data=cross.sell.dev$predicted.response,
                reference=cross.sell.dev$target)
#Reference
#Prediction     0       1
#       0     20361    24
#       1      0      10197
Accuracy<- ((20361+10197)/(20361+10197+24))*100
Accuracy
#98.92152

##########################################################################################
#
#       Random Forest over Unbalanced Data
#
#########################################################################################
rm(list=ls())
#Read DATA.
termCrosssell<-read.csv("C:/Users/Pinank/Desktop/Project/Cleaned_Dataset.csv")
termCrosssell$X<-NULL

## Explore data frame
names(termCrosssell)

#input dataset has 11 independent variables and a target variable. The target variable target is binary.
table(termCrosssell$SeriousDlqin2yrs)/nrow(termCrosssell)
#  1         2 
#0.6666667 0.3333333 
#33% of the observations has target variable "yes" and remaining 66%% observations take value "no".

#Now, we will split the data sample into development and validation samples.
sample.ind <- sample(2,nrow(termCrosssell),replace = T,prob = c(0.7,0.3))
cross.sell.dev <- termCrosssell[sample.ind==1,]#training
cross.sell.val <- termCrosssell[sample.ind==2,]#Testing

table(cross.sell.dev$SeriousDlqin2yrs)/nrow(cross.sell.dev)
## 
##   1Yes     2No 
##0.6657838 0.3342162 
table(cross.sell.val$SeriousDlqin2yrs)/nrow(cross.sell.val)
## 
##  1Yes         2No 
##0.6687314 0.3312686 

#Both development and validation samples have similar target variable distribution. This is just a sample validation.
#If target variable is factor, classification decision tree is built. We can check the type of response variable.
cross.sell.dev$SeriousDlqin2yrs <- factor(cross.sell.dev$SeriousDlqin2yrs) #convert ineteger to factor.
cross.sell.val$SeriousDlqin2yrs <- factor(cross.sell.val$SeriousDlqin2yrs)
class(cross.sell.dev$SeriousDlqin2yrs)
class(cross.sell.val$SeriousDlqin2yrs)

#Class of target or response variable is factor, so a classification Random Forest will be built. The current data frame has a list of independent variables,
#so we can make it formula and then pass as a parameter value for randomForest.
varNames <- names(cross.sell.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("SeriousDlqin2yrs")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("SeriousDlqin2yrs", varNames1, sep = " ~ "))

#Now, we have a sample data and formula for building Random Forest model.
#Let's build 200 decision trees using Random Forest.
set.seed(200)
cross.sell.rf <- randomForest(rf.form,
                              cross.sell.dev,
                              ntree=200,
                              importance=T)

plot(cross.sell.rf,main = "Random Forest Errorrate (200 Dtrees)")

#100 decision trees or a forest has been built using the Random Forest algorithm based learning. We can plot the error rate across decision trees. The plot seems to indicate that after 100 decision trees,
#there is not a significant reduction in error rate.

#Variable importance plot is also a useful tool and can be plotted using varImpPlot function.
#Top 5 variables are selected and plotted based on Model Accuracy and Gini value.
#We can also get a table with decreasing order of importance based on a measure
#(1 for model accuracy and 2 node impurity)

# Variable Importance Plot
#varImpPlot(cross.sell.rf,
 #          sort = T,
  ##        n.var=5)

# Variable Importance Table
var.imp <- data.frame(importance(cross.sell.rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
#                                     MeanDecreaseGini                            Variables
#NumberOfTime30.59DaysPastDueNotWorse        2494.7360 NumberOfTime30.59DaysPastDueNotWorse
#NumberOfTimes90DaysLate                     2002.4125              NumberOfTimes90DaysLate
#RevolvingUtilizationOfUnsecuredLines        1963.4157 RevolvingUtilizationOfUnsecuredLines
#NumberOfTime60.89DaysPastDueNotWorse        1322.4477 NumberOfTime60.89DaysPastDueNotWorse
#DebtRatio                                    990.6584                            DebtRatio
#expenditure                                  974.3565                          expenditure
#age                                          970.5989                                  age
#MonthlyIncome                                865.2160                        MonthlyIncome
#NumberOfOpenCreditLinesAndLoans              840.4978      NumberOfOpenCreditLinesAndLoans
#NumberRealEstateLoansOrLines                 512.9280         NumberRealEstateLoansOrLines
#NumberOfDependents                           285.1260                   NumberOfDependents

# Predicting response variable
cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)
set.seed(200)
table(cross.sell.dev$predicted.response)
# Load Library or packages
library(e1071)
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
# Create Confusion Matrix
confusionMatrix(data=cross.sell.dev$predicted.response,
                reference=cross.sell.dev$SeriousDlqin2yrs)


#$table
#Reference
#Prediction     0     1
#0 95046   179
#1     0  6584

#$overall
#Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 
#9.982418e-01   9.856482e-01   9.979648e-01   9.984898e-01   9.335717e-01   0.000000e+00 
#McnemarPValue 
#2.183997e-40 

