MachineLearning
===============

Practical Machine Learning - Course Project

# Introduction 
This projects aims to determine correctly and incorrectly performed exercise,
by investigating data collected from a range of different devices such as (x,y,z).
For more information about the data and how it has been collected see the following link.
   
The following project will include:
1) Initialization of R environment
2) Cleaning of Data sets 
3) Evaluation of Machine Learning Algorithms  
4) Results 
5) Discussion 
   
# Initialization 
The package used for analysis is the caret package see information at (...)

    install.packages("caret")
    library(caret)
    Loading data set - classifying NA and empty strings as NA
    dataTraning <- read.csv("trainingData.csv", header=T, na.strings= c("",NA))
    dataTesting <- read.csv("testingData.csv", header=T, na.strings= c("",NA))

# Data Cleaning 
The data collected contains a large number of factors containing a large number of missing values.
I looped through the data set and removed any values with over 50% NA values.


    cleaned_data = data[1]
    for (i in 2:dim(data)[2]) {
    frac_missing <- sum(!is.na(data[i]))/nrow(data[i])
    if(frac_missing > 0.5) 
    cleaned_data[,names(data[i])] <- data[i]
    }

Additionally the data set contained time stamp values as well as window size measurement and user-information. 
The user information is not necessary since we are aggregating data, and we are not training the data against any specific user. 
    Side note - (individual user information would be interesting for calibration purposes to generate a cleaner signal/noise ratio)

    cleaned_data <- subset(cleaned_data, select = -c(raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,X,new_window,num_window,user_name) )
 

# Setting up training and test set:
To be able to run any the machine learning algorithms, I set the classe varible into a factor. The data set is divided into a 70/30 split between trainign and test set.
 
    as.factor(cleaned_data$classe)
    inTrain <- createDataPartition(y=cleaned_data$classe,p=0.7,list=FALSE)
    training <-cleaned_data[inTrain,]
    testing <- cleaned_data[-inTrain,]

# Evaluation of Models 
The models I chose to include in the analysis is:
     1) KNN-model with and without PCA-preprocessing steps. 
     2) PCA-preprocessed Random Forest Model with five cross-validations  
 

Single Model - without pre-processing
    
    seed.set(500)
    knnPredictTrain <-predict(knn,training)
    knnTrainAccuracy <- confusionMatrix(training$classe, knnPredictTrain)
    knnPredictTest <-predict(knn,testing)
    KnnTestAccuracy <- confusionMatrix(testing$classe, knnPredictTest)

Single Model - with PCA-preprocessing
    
    knnPCA <- train(classe~., training, method = "knn", preProcess=c("pca"), 
        trControl = trainControl(method = "cv"))
    knnPredictTrainPCA <- predict(knnPCA,training)
    KnnTrainAccuracyPCA <- confusionMatrix(training$classe, knnPredictTrainPCA)
    knnPredictTestPCA <- predict(knnPCA,testing)
    KnnTestAccuracyPCA <- confusionMatrix(testing$classe, knnPredictTestPCA)

# Random Forest model 

    seed.set(500)
    RandomF <- train(classe~., training, method = "rf", preProcess=c("pca"), 
        trControl = trainControl(method = "cv"))
    rfPredict = predict(RandomF,training)
    rfAccuracyTraining = confusionMatrix(training$classe,rfPredict)
    rfPredictTesting <- predict(RandomF,testing)
    rfAccuracytesting = confusionMatrix(testing$classe,rfPredictTesting)

# Results 


