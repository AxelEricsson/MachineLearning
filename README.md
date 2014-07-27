MachineLearning
===============

Practical Machine Learning - Course Project

# Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. 
These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. 
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 
In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 
They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 
More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 
   
The following project will include:

    1) Initialization of R environment
    2) Cleaning of Data sets 
    3) Three step evaluation of Machine learning Models  
    4) Prediction Validation set.   
   
# Initialization 
The package used for the analysis is the R - package caret (http://caret.r-forge.r-project.org/).
The package can handle a multitude of Machine learning procedures, such as data splitting, model training, prediction and much more. 
 
    install.packages("caret")
    library(caret)
    Loading data set - classifying NA and empty strings as NA
    dataTraning <- read.csv("trainingData.csv", header=T, na.strings= c("",NA))
    dataValidation <- read.csv("testingData.csv", header=T, na.strings= c("",NA))

# Data Cleaning 
The data generated from the devices contained a large number of missing values. 
The missing values was specific to certain prediction variables which easily could be removed.  
The overall data set was clean, hence no imputation of values were needed for the retained prediction variables. 
I looped through the data set and removed any values with over 50% NA values. 

    cleaned_data = data[1]
    clean_missing <- function(x){ 
	for (i in 2:dim(data)[2]) {
    frac_missing <- sum(!is.na(data[i]))/nrow(data[i])
    if(frac_missing > 0.5) 
    cleaned_data[,names(data[i])] <- data[i]
    }
    }
clean_train<- clean_missing(dataTraining) 
clean_Validation <- clean_missing(dataValidation)

Since certain values contain information which is not necessary for prediction such as time-stamp values and user information.  
The user information is not necessary since we are aggregating data, and we are not training the data against any specific user. 
Side note - (individual user information would be interesting for calibration purposes to generate a cleaner signal/noise ratio)

    clean_train <- subset(clean_train, select = -c(raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,X,new_window,num_window,user_name) )
    clean_Validation  <- subset(clean_Validation, select = -c(raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,X,new_window,num_window,user_name) )

After cleaning the missing values and time-stamp and user information the data set contained 52 prediction values out if the 159 initial values, excluding the outcome value. 
To get a better understanding on how the data set is structured I investigated the relationship between the predictors by plotting a pairwise corrleation matrix(see image below)

    correlations <- cor(clean_train[-53])
    corrplot(correlations, order = 'hclust',tl.cex = .5)
 ![My image](https://github.com/AxelEricsson/MachineLearning/blob/master/correlations.png)
 
It appears that several of the prediction variables is correlated according to the plot above. Since the data-set contains correlated variables, liner models will run into collinear issues. 
To solve this problem a preprocessing step will be required, PCA - analysis will be run prior all the prediction models. 
 
# Setting up training and test set:
To be able to run any the machine learning algorithms, I set the classe variable into a factor. 
The data was pre-processed into training, testing and validation data-set. 
To be able to measure in-sample vs out-sample performance the given training set was split into training and test set.  
The ratio of the split was 70/30 split resulting in 13737/5885 -  training/test split.

    as.factor(clean_Train)$classe)
    inTrain <- createDataPartition(y=cleaned_train$classe,p=0.7,list=FALSE)
    training <-cleaned_data[inTrain,]
    testing <- cleaned_data[-inTrain,]

# Evaluation of Models 

The evaluation process was divided into a three steps. 

Step.1 Model Discovery: 
A large number of models was tested and cross-validated. The accuracy measured across a 10-fold cross validation procedure.  
across all models to be able to standardize and compare performance. A fixed set seed of 1 was implemented to minimize variation between models. 
The models included in the analysis was the following: 
 
    1)PLS,
    2)KNN(pre-processed and not pre-processed),
    3)Random Forest
    4)Boosting 
    5)SVM-polynomial 
    6)Naive Bayes

The code implementing the models was the following: 

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

The model discovery phase was completed by comparing in-sample performance across the different model. 
This step was introduced to filter out the low-performing models.  

    ModelComparison <- resamples(list("NaiveB"=nbModel,
                                 "LDA" = ldaModel,
                                 "SVMPoly"=SVMModel,
                                 "GBM" = gbmModel,
                                 "RF"=rfModel,
                                 "Knn"=knnModel,
                                 "knnPCA"=knnPCAModel,
                                 "kernel-PLS"=kPlsModel))  
    parallelplot(ModelComparison)  

![Model Comparison](https://github.com/AxelEricsson/MachineLearning/blob/master/modelComparison.jpg)

PLS, naive bayes and LDA performed poor on the dataset and was excluded from any further analysis. 
The linear models was expected to perform worse than non-linear models, but they also provide less complexity, which can be desired since the results in general is easier to interpret. 

Step.2 Out sample Error: 
The top models selected was KNNPCA,Random Forest, Boosting and SVM. 
SVM and Random Forest was indicating to be the strongest models in the set. 

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



# Results and prediction of validation set:
The four models where assessed by comparing accuracy in-sample versus out-sample. The Random Forest performed very well in training set with 100% accuracy. 
This is an indication of over-fitting, which later was proven by the running the model on the test set. The SVM-polynomial was superior in regards of out of sample accuracy 99.23%;
and an evenly distributed error rate across the different cases(shown in ConfusionMatrix). See figure for comparison of accuracy across models. 

     
![Out of sample Accuracy](https://github.com/AxelEricsson/MachineLearning/blob/master/InOutError.png)

The SVM-polynomial model was selected for the final stage of prediction to validate the 20 samples provided in the cleaned data set. 
In sample performance and tuning parameters are shown in image below 
![SVM-Polynomial](https://github.com/AxelEricsson/MachineLearning/blob/master/svmPolu.jpeg)

Step.3 Prediction of Validation Set.  
The prediction of the validation set was carried out by the following code and submitted on Coursera. All of the 20 validation was correctly classified. 

Predicting of validation set: 

    SMVPredTest <- predict(SVMFit,clean_Validation)
    pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
          filename = paste0("problem_id_",i,".txt")
          write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
    }
    plm_write_files(SVMPredTest)

