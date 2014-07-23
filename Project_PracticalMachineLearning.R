Cleaning Data set of predictors above 50% NA 

## Read data and adress NAs 
data <- read.csv("Training_Set.csv", header=T, na.strings= c("",NA))

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
classe <- as.factor(cleaned_data$classe)

## Create a training and test set
Setting up training and test set:
inTrain <- createDataPartition(y=cleaned_data$classe,p=0.7,list=FALSE)
training <-cleaned_data[inTrain,]
testing <- cleaned_data[-inTrain,]

## Create a cross validation set
Kfolds <- createFolds(y=training,k=10,list=TRUE,returnTrain=TRUE)k
Ksample<- createResample(y=df,times=10,list=FALSE)
Ktime  <- createTimeSlices(y=tme,initialWindow=windowSlices,horizon=10)

## PreProcessing with PCA
preProc <- preProcess(training[-53],method='pca',thresh=0.9)
PCA <- predict(preProc,training[-53])

## Train model model

K-fold nerest neighbor:
seed.set(500)

knn <- train(classe~., training, method = "knn", 
    trControl = trainControl(method = "cv"))

knnPCA <- train(classe~., training, method = "knn", preProcess=c("pca"), 
    trControl = trainControl(method = "cv"))

plot

##RandomF
seed.set(500)
RandomF <- train(classe~., training, method = "rf", preProcess=c("pca"), 
    trControl = trainControl(method = "cv"))

Resample
trainControl 
method: (boot,boot632,cv,repeatedcv,LOOCV,
Number: for cv and boot
Repeates: number of repeats sub sampling
