##################################################################################################
#                               ANN Algorithm                                                    #
##################################################################################################
#install.packages("neuralnet")
rm(list=ls())
library("neuralnet")

train<-read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Balanced Data.csv")
#train$X<-NULL
View(train)

mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                                           min (x, na.rm = TRUE)))}

training_norm <-data.frame(apply(train,2,mmnorm))
training_norm <- subset(training_norm[,-1])
View(training_norm)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))


test_ds <- training_norm[-idx,]
training_ds <- training_norm[idx,]

test_ds<- subset(test_ds[,-c(5,6)])
training_ds<- subset(training_norm[,-c(5,6)])


#View(test_ds)

View(training_ds)
#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
set.seed(5)
#weights_list <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

nn = neuralnet(target~RevolvingUtilizationOfUnsecuredLines+age+NumberOfTime30.59DaysPastDueNotWorse+NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents+expenditure,data = training_ds,hidden=2,err.fct = "ce",linear.output = FALSE, stepmax = 1e6) #10^6 = 1e6                   
print(nn) 
#$result.matrix
#1
#error                                             17062.374174477049
#reached.threshold                                     0.009545719561
#steps                                            187047.000000000000
#Intercept.to.1layhid1                                 0.643384017614
#RevolvingUtilizationOfUnsecuredLines.to.1layhid1     -3.822102112319
#age.to.1layhid1                                       0.938462815677
#NumberOfTime30.59DaysPastDueNotWorse.to.1layhid1      7.506417083513
#NumberOfOpenCreditLinesAndLoans.to.1layhid1          -1.849090483707
#NumberOfTimes90DaysLate.to.1layhid1              -18705.257082102780
#NumberRealEstateLoansOrLines.to.1layhid1             -1.382834954243
#NumberOfTime60.89DaysPastDueNotWorse.to.1layhid1    -50.579989121970
#NumberOfDependents.to.1layhid1                        0.245437274340
#expenditure.to.1layhid1                              19.062425906263
#Intercept.to.1layhid2                                -4.049668701861
#RevolvingUtilizationOfUnsecuredLines.to.1layhid2     -1.318007562400
#age.to.1layhid2                                       0.534780233268
#NumberOfTime30.59DaysPastDueNotWorse.to.1layhid2 -18704.415409774268
#NumberOfOpenCreditLinesAndLoans.to.1layhid2          -0.594315394078
#NumberOfTimes90DaysLate.to.1layhid2                  -2.857220964992
#NumberRealEstateLoansOrLines.to.1layhid2             -1.343959171083
#NumberOfTime60.89DaysPastDueNotWorse.to.1layhid2 -18701.718339638861
#NumberOfDependents.to.1layhid2                        0.249306317331
#expenditure.to.1layhid2                               9.684408067291
#Intercept.to.target                                   2.393028897272
#1layhid.1.to.target                                  -4.662174908067
#1layhid.2.to.target                                -121.790994458099

#attr(,"class")
#[1] "nn"

#Plot the neural network
plot(nn)
nn$net.result #overall result i.e output for each replication
nn$weights
#[[1]]
#[[1]][[1]]
#[,1]              [,2]
#[1,]      0.6433840176     -4.0496687019
#[2,]     -3.8221021123     -1.3180075624
#[3,]      0.9384628157      0.5347802333
#[4,]      7.5064170835 -18704.4154097743
#[5,]     -1.8490904837     -0.5943153941
#[6,] -18705.2570821028     -2.8572209650
#[7,]     -1.3828349542     -1.3439591711
#[8,]    -50.5799891220 -18701.7183396389
#[9,]      0.2454372743      0.2493063173
#[10,]     19.0624259063      9.6844080673

#[[1]][[2]]
#[,1]
#[1,]    2.393028897
#[2,]   -4.662174908
#[3,] -121.790994458
#write.csv(ANN_model, "C:/Users/Pinank/Desktop/Project/Final Project/Ann_Model")
test <- test_ds[,-1]
net.results <- compute(nn, test) #Run them through the neural network

#training_ds$target
nn$net.result[[1]]

nn1 = ifelse(nn$net.result[[1]]>0.5, 1,0)#check the results are more than 50% if they are assign 1 or 2
nn1
misClassificationError = mean(training_ds$target != nn1) #0.169793

misClassificationError

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

test <- test_ds[,-1]
net.results <- compute(nn, test) #Run them through the neural network
#Lets see what properties net.sqrt has
ls(net.results)
#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(test,sqrt(test),as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput) 


Error_rate <- 17062/43659
Accuracy <- 1- Error_rate
Accuracy #0.6091
#################################################################################
#.......................ANN For Unbalanced Cleaned Data.........................#
#################################################################################
rm(list=ls())
library("neuralnet")

train<- read.csv("C:/Users/Pinank/Desktop/Project/Final Project/Cleaned_Dataset.csv")
#train$X<-NULL
View(train)

mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                                           min (x, na.rm = TRUE)))}

training_norm <-data.frame(apply(train,2,mmnorm))
training_norm <- subset(training_norm[,-1])
View(training_norm)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))

training_ds <- training_norm[idx,]
test_ds <- training_norm[-idx,]

training_ds<- subset(training_norm[,-c(5,6)])
test_ds<- subset(test_ds[,-c(5,6)])

#View(test_ds)

View(training_ds)
#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
set.seed(1)
#weights_list <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

nn = neuralnet(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+age+NumberOfTime30.59DaysPastDueNotWorse+NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents+expenditure,data = training_ds,hidden=2,err.fct = "ce",linear.output = FALSE, stepmax = 1e6) #10^6 = 1e6                   
print(nn) 
#$result.matrix
#1
#error                                            26017.340393762075
#reached.threshold                                    0.009241643634
#steps                                            75691.000000000000
#Intercept.to.1layhid1                                4.153977249740
#RevolvingUtilizationOfUnsecuredLines.to.1layhid1    -3.215152214245
#age.to.1layhid1                                     14.171192247820
#NumberOfTime30.59DaysPastDueNotWorse.to.1layhid1   -23.784992751907
#NumberOfOpenCreditLinesAndLoans.to.1layhid1          3.559534008540
#NumberOfTimes90DaysLate.to.1layhid1               -157.576437961806
#NumberRealEstateLoansOrLines.to.1layhid1           -65.581140086494
#NumberOfTime60.89DaysPastDueNotWorse.to.1layhid1   -74.809709973207
#NumberOfDependents.to.1layhid1                       1.463743474247
#expenditure.to.1layhid1                            -54.588550234403
#Intercept.to.1layhid2                               -1.514559766838
#RevolvingUtilizationOfUnsecuredLines.to.1layhid2    -2.421886555396
#age.to.1layhid2                                      0.597948138934
#NumberOfTime30.59DaysPastDueNotWorse.to.1layhid2   -32.047270231011
#NumberOfOpenCreditLinesAndLoans.to.1layhid2         -0.843173261451
#NumberOfTimes90DaysLate.to.1layhid2                -91.021554092377
#NumberRealEstateLoansOrLines.to.1layhid2            -0.294012145850
#NumberOfTime60.89DaysPastDueNotWorse.to.1layhid2   -67.615649713241
#NumberOfDependents.to.1layhid2                      -0.064418890873
#expenditure.to.1layhid2                              4.731153591305
#Intercept.to.SeriousDlqin2yrs                        1.038649489748
#1layhid.1.to.SeriousDlqin2yrs                       -0.930186569762
#1layhid.2.to.SeriousDlqin2yrs                      -24.890719223458

#attr(,"class")
#[1] "nn"
#Plot the neural network
plot(nn)
nn$net.result #overall result i.e output for each replication
nn$weights
#[[1]]
#[[1]][[1]]
#[,1]            [,2]
#[1,]    4.153977250  -1.51455976684
#[2,]   -3.215152214  -2.42188655540
#[3,]   14.171192248   0.59794813893
#[4,]  -23.784992752 -32.04727023101
#[5,]    3.559534009  -0.84317326145
#[6,] -157.576437962 -91.02155409238
#[7,]  -65.581140086  -0.29401214585
#[8,]  -74.809709973 -67.61564971324
#[9,]    1.463743474  -0.06441889087
#[10,]  -54.588550234   4.73115359131

#[[1]][[2]]
#[,1]
#[1,]   1.0386494897
#[2,]  -0.9301865698
#[3,] -24.8907192235

test <- test_ds[,-1]
net.results <- compute(nn, test) #Run them through the neural network

#training_ds$target
nn$net.result[[1]]

nn1 = ifelse(nn$net.result[[1]]>0.5, 1,0)#check the results are more than 50% if they are assign 1 or 2
nn1
misClassificationError = mean(training_ds$SeriousDlqin2yrs != nn1) #[1] 0.06242659432

misClassificationError

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

test <- test_ds[,-1]
net.results <- compute(nn, test) #Run them through the neural network
#Lets see what properties net.sqrt has
ls(net.results)
#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(test,sqrt(test),as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput) 

Error_Rate <- (26017)/(145595)
Accuracy <- 1- Error_Rate
Accuracy
