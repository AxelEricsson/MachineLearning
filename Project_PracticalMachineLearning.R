Cleaning Data set of predictors above 50% NA 

## Setting up Environment 

## Read data and adress NAs 
data <- read.csv("trainingData.csv", header=T, na.strings= c("",NA))
data <- read.csv("testingData.csv", header=T, na.strings= c("",NA))

## Remove Columns with high NAs - retain 60/160
cleaned_data = data[1]
for (i in 2:dim(data)[2]) {
frac_missing <- sum(!is.na(data[i]))/nrow(data[i])
if(frac_missing > 0.5) 
cleaned_data[,names(data[i])] <- data[i]
}

## Remove timestamp data and 
cleaned_data <- subset(cleaned_data, select = -c(raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,X,new_window,num_window,user_name) )

## Classe Varible to factor 
as.factor(cleaned_data$classe)

## Check for correlated prediction varibles.
png('correlations.png')
correlations <- cor(training[-53])
corrplot(correlations, order = 'hclust',tl.cex = .5)
dev.off()

## PCA - Analysis 

modelEval <-function(model,train,test) {
    predTrain <- predict(model,train)
    predTest <- predict(model,test)
    TrainAccuracy <-confusionMatrix(train$classe,predTrain)
    TestAccuracy  <- confusionMatrix(testing$classe,predTest)
    output <- list(TrainAccuracy, TestAccuracy)
    return(output)
}

## Create a training and test set
Setting up training and test set:
inTrain <- createDataPartition(y=cleaned_data$classe,p=0.7,list=FALSE)
training <-cleaned_data[inTrain,]
testing <- cleaned_data[-inTrain,]

## Train model model
set.seed(1)
controlObject <- trainControl(method = 'repeatedcv',
                              repeats = 5,
                              number=10)

## K-fold nerest neighbor:
seed.set(1)
knn <- train(classe~., training, method = "knn", 
    trControl = trainControl(method = "cv"))


)
knnPerformance <- modelEval(knn,training,testing)

knnPredictTrain <-predict(knn,training)
KnnTrainAccuracy <- confusionMatrix(training$classe, knnPredictTrain) - 95.47%
knnPredictTest <-predict(knn,testing)
KnnTestAccuracy <- confusionMatrix(testing$classe, knnPredictTest)
 
Single Model - with PCA-preprocessing
set.seed(1)
knnGrid <- expand.grid(.k=c(2:5))
knnPCA <- train(classe~.,  training, method = "knn",
                           preProcess=c("pca",'center','scale'), 
                           trControl = trainControl(method = "cv"))

knnPCA25 <- train(classe~.,  training, method = "knn",
                           preProcess=c("pca",'center','scale'), 
                           tuneGrid = knnGrid,
                           trControl = controlObject)

plot(knnPCA)

knnPredictTrainPCA <- predict(knnPCA,training)
KnnTrainAccuracyPCA <- confusionMatrix(training$classe, knnPredictTrainPCA) 98.05%
knnPredictTestPCA <- predict(knnPCA,testing)
KnnTestAccuracyPCA <- confusionMatrix(testing$classe, knnPredictTestPCA) 95.58%

set.seed(1)
kernelPls <- train(classe~., training, method = "pls", 
          preProcess=c("pca",'center','scale'),
          trControl = trainControl(method = "cv"))

## Random Forest model 
seed.set(1)
RandomF <- train(classe~., training, method = "rf", preProcess=c("pca"), 
             preProc=c('center','scale','pca')
             trControl = trainControl(method = "cv"))
plot(RandomF)

rfPredict = predict(RandomF,training)
rfAccuracyTraining = confusionMatrix(training$classe,rfPredict)
rfPredictTesting <- predict(RandomF,testing)
rfAccuracytesting = confusionMatrix(testing$classe,rfPredictTesting)


## Boosting 
set.seed(1)
gbmFit1 <- train(classe ~ ., data = training,
                 method = "gbm",
                 preProc=c('center','scale','pca')
                 trControl=trainControl(method='cv') 
                 )


gbmPredTrain <- predict(gbmFit1,training)
gbmPredTest <- predict(gbmFit1,testing)
gbmAccuracyTrain <- confusionMatrix(training$classe,gbmPredTrain)
gbmAccuracyTest <- confusionMatrix(testing$classe,gbmPredTest)


## SVM
set.seed(1)
SVMFit <- train(classe ~ ., data = training,
                 method = "svmPoly",
                 preProc=c('center','scale','pca'),
                 trControl=trainControl(method='cv') 
                 )

SVMPredTrain <- predict(SVMFit,training)
SMVPredTest <- predict(SVMFit,cleaned_data)
SVMAccuracyTrain <- confusionMatrix(training$classe,SVMPredTrain)
SVMAccuracyTest <- confusionMatrix(testing$classe,SMVPredTest)




## Model Based analysis 
set.seed(1)
ldaFit <- train(classe~.,data=training,
                preProc=c('center','scale','pca')
                method='lda', 
                trControl=trainControl(method='cv'))


ldaPredTrain <-predict(ldaFit,training)
ldaPredTest <-predict(ldaFit,testing)
ldaAccuracyTrain<-confusionMatrix(training$classe,ldaPredTrain)
ldaAccuracyTest <- confusionMatrix(testing$classe,ldaPredTest)

set.seed(1)
nbFit <- train(classe~.,data=training,
                method='nb', 
                preproc=c('center',scale,pca)
                trControl=trainControl(method='cv'))

nbPredTrain <-predict(ldaFit,training)
nbPredTest <-predict(ldaFit,testing)
nbAccuracyTrain<-confusionMatrix(training$classe,ldaPredTrain)
nbAccuracyTest <- confusionMatrix(testing$classe,ldaPredTest)

## Compare Results 
ModelComparison <- resamples(list("NaiveB"=nbFit,
                                 "LDA" = ldaFit,
                                 "SVMPoly"=SVMFit,
                                 "GBM" = gbmFit1,
                                 "RF"=RandomF,
                                 "Knn"=knn,
                                 "knnPCA"=knnPCA,
                                 "PLS-kernel"=kernelPls))
  
parallelplot((ModelComparison))

jpeg("modelComparison.jpg")
dev.off()

plot(SVMFit)
plot(gbmFit1)
gbmFit1


# Grouped Bar Plot

svmAccuracy <- c(SVMAccuracyTrain$overall[1],SVMAccuracyTest$overall[1])
rfAccuracy <- c(rfAccuracyTraining$overall[1],rfAccuracytesting$overall[1])
gbmAccuracy <-c(gbmAccuracyTrain$overall[1],gbmAccuracyTest$overall[1])




counts <- table(chart$svmAccuracy, chart$rfAccuracy)
barplot(counts, main= "In-Sample vs Out-Sample Accuracy",
  xlab="Models Categories",ylab= 'Accuracy' ,col=c("darkblue","red"),
 	 legend = rownames(counts), beside=TRUE)


#  Specificity and Sensitivity performance in-sample and out-sample  

detectionAcrossSamples <-function(DM) {
    frame <- matrix(ncol=2)
    colnames(frame) <-c('Sensitivity','Specificity')
    for (i in 1:5){
        varible <- c(DM[i], DM[i+5])
        frame <-rbind(frame,varible)
    }
   return(frame)
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

  
