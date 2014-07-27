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
	dataTraning <- read.csv("trainingData.csv", header=T, na.strings= c("",NA))
	dataTesting <- read.csv("testingData.csv", header=T, na.strings= c("",NA))
=======
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

    cleaned_data = data[1]
    for (i in 2:dim(data)[2]) {
    frac_missing <- sum(!is.na(data[i]))/nrow(data[i])
    if(frac_missing > 0.5) 
    cleaned_data[,names(data[i])] <- data[i]
    }

Additionally the data set contained time stamp values as well as window size measurement and user-information. 
The user information is not necessary since we are aggregating data, and we are not training the data against any specific user. Side note - (individual user information would be interesting for calibration purposes to generate a cleaner signal/noise ratio)

    cleaned_data <- subset(cleaned_data, select = -c(raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,X,new_window,num_window,user_name) )

Checking for Correlations between predictor variables
	
    correlations <- cor(training[-53])
    corrplot(correlations, order = 'hclust',tl.cex = .5)
 
It appears that several of the prediction variables is correlated according to the plot above. Since the data-set contains correlated varibles, liner models will run into collinear issues. 
To solve this problem a preprocessing step will be required, PCA - analysis will be run prior all the prediction models. 
 
# Setting up training and test set:
To be able to run any the machine learning algorithms, I set the classe variable into a factor. 
The data was pre-processed into training, testing and validation data-set. 
The cleaned data-set was split into 70/30 split between training and test set.  

    as.factor(cleaned_data$classe)
    inTrain <- createDataPartition(y=cleaned_data$classe,p=0.7,list=FALSE)
    training <-cleaned_data[inTrain,]
    testing <- cleaned_data[-inTrain,]

# Evaluation of Models 

A wide range of models were included in the screening process including 
  1)PLS,
  2)KNN(pre-processed and not pre-processed),
  3)Random Forest
  4)Boosting 
  5)SVM-polynomial 
  6)Naive Bayes

The cross-validation was performed by a 10-fold cross validation, 
across all models to be able to standardize and compare performance.
  

    set.seed(1)
    controlObject <- trainControl(method = 'repeatedcv',
                              repeats = 5,
                              number=10)
    
	kPlsModel <- train(classe~., training, method = "pls", 
          preProcess=c("pca",'center','scale'),
          trControl = controlObject
	
	set.seed(1)
    ldaModel <- train(classe~.,data=training,
                preProc=c('center','scale','pca')
                method='lda', 
                trControl=controlObject
    set.seed(1)
	nbModel <- train(classe~.,data=training,
                method='nb', 
                preproc=c('center',scale,pca)
                trControl=trControl=controlObject

	set.seed(1)
	knnModel <- train(classe~., training, method = "knn", 
    trControl = controlObject
	
	set.seed(1)
	knnPcaModel <- train(classe~.,  training, method = "knn",
                           preProcess=c("pca",'center','scale'), 
                           trControl = controlObject
	seed.set(1)
    rfModel <- train(classe~., training, method = "rf",
 	                           preProcess=c("pca",'center','scale'), 
                               trControl = controlObject)  

	set.seed(1)
	gbmModel <- train(classe ~ ., data = training,
                 method = "gbm",
                 preProc=c('center','scale','pca')
                 trControl=controlObject 
                 )
    set.seed(1)
    svmModel <- train(classe ~ ., data = training,
                 method = "svmPoly",
                 preProc=c('center','scale','pca'),
                 trControl=controlObject 
                 )

The in-sample accuracy was compared across all models to give an estimate of performance.  

ModelComparison <- resamples(list("NaiveB"=nbModel,
                                 "LDA" = ldaModel,
                                 "SVMPoly"=SVMModel,
                                 "GBM" = gbmModel,
                                 "RF"=rfModel,
                                 "Knn"=knnModel,
                                 "knnPCA"=knnPCAModel,
                                 "kernel-PLS"=kPlsModel))  
parallelplot(ModelComparison)  


In sample out Sample error 

KNN    

    knnPredictTrain <-predict(knnModel,training)
    KnnTrainAccuracy <- confusionMatrix(training$classe, knnPredictTrain)
    knnPredictTest <-predict(knnModel,testing)
    KnnTestAccuracy <- confusionMatrix(testing$classe, knnPredictTest)

KnnPCA

    knnPredictTrainPCA <- predict(knnPcaModel,training)
    KnnTrainAccuracyPCA <- confusionMatrix(training$classe, knnPredictTrainPCA)
    knnPredictTestPCA <- predict(knnPCAModel,testing)
    KnnTestAccuracyPCA <- confusionMatrix(testing$classe, knnPredictTestPCA)

Random Forest model 

    rfPredict = predict(rfModel,training)
    rfAccuracyTraining = confusionMatrix(training$classe,rfPredict)
    rfPredictTesting <- predict(rfModel,testing)
    rfAccuracytesting = confusionMatrix(testing$classe,rfPredictTesting)

Boosting 

    gbmPredTrain <- predict(gbmModel,training)
    gbmPredTest <- predict(gbmModel,testing)
    gbmAccuracyTrain <- confusionMatrix(training$classe,gbmPredTrain)
    gbmAccuracyTest <- confusionMatrix(testing$classe,gbmPredTest)

Support Vector Machine 

    SVMPredTrain <- predict(svmModel,training)
    SMVPredTest <- predict(svmModel,testing)
    SVMAccuracyTrain <- confusionMatrix(training$classe,SVMPredTrain)
    SVMAccuracyTest <- confusionMatrix(testing$classe,SMVPredTest)

Linear Discriminant Analysis 

    ldaPredTrain <-predict(ldaModel,training)
    ldaPredTest <-predict(ldaModel,testing)
    ldaAccuracyTrain<-confusionMatrix(training$classe,ldaPredTrain)
    ldaAccuracyTest <- confusionMatrix(testing$classe,ldaPredTest)

Naive Bayes 

    nbPredTrain <-predict(nbModel,training)
    nbPredTest <-predict(mbModelFit,testing)
    nbAccuracyTrain<-confusionMatrix(training$classe,ldaPredTrain)
    nbAccuracyTest <- confusionMatrix(testing$classe,ldaPredTest) 

# Results 
