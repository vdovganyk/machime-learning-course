tr<-
    "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
ts<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(tr, "data/training.csv")
download.file(ts, "data/testing.csv")

trainingGD<-read.csv("data/training.csv")
testing<-read.csv("data/testing.csv")

head(training)
training$classe
library (caret)
set.seed (321)
intrain<-createDataPartition (y=trainingGD$classe, p=.6, list=F)
training<-trainingGD[intrain,]
validation<-trainingGD[-intrain,]

dim(training); dim(validation); dim (testing)


##exp analisys
par(mfrow=c(2,2))
plot(training$pitch_belt, training$roll_belt, col=training$classe,
     xlab="pitch_belt", ylab="roll_belt")

plot(training$roll_belt, training$yaw_belt, col=training$classe,
     xlab="roll_belt", ylab="yaw_belt")

plot(training$roll_belt, training$yaw_arm, col=training$classe,
     xlab="roll_belt", ylab="yaw_belt")
plot(training$yaw_dumbbell, training$yaw_belt, col=training$classe,
     xlab="yaw_dumbbell", ylab="yaw_belt")

##NA processing
summary(training)
training$skewness_roll_forearm

plot(training$yaw_arm, training$var_yaw_forearm, col=training$classe)
ftraining

##train exp (pca, rpart, rf)
fit<-train(classe~., data=training, preProcess=c("pca"),
           method="rpart")


nsv<-nearZeroVar(training, saveMetrics=TRUE)
nsv


clearData<-function(indata, nsv) {
    i<-which(nsv$nzv==FALSE)
    ##i<-which(nsv$nzv==FALSE&nsv$percentUnique>5)
    outData<-indata[,i]
    outData<-outData[ , colSums(is.na(outData)) == 0]
    outData$classe<-NULL
    outData$classe<-indata$classe
    outData<-outData[,!names(outData)%in%c("X", "raw_timestamp_part_2", 
                                           "cvtd_timestamp", "raw_timestamp_part_1",
                                           "user_name")]
    return (outData)
}
finalTraining<-clearData(training, nsv)
summary(finalTraining)

##some pca
tr<-finalTraining[,1:53]
tr.pca <- prcomp(finalTraining[,1:53],
                 center = TRUE,
                 scale. = TRUE) 
print(tr.pca)

summary(tr.pca)
loadings(tr.pca)
plot(fit,type="lines")

preProc<-preProcess(finalTraining[,-54], method="pca", pcaComp=16)
trainPC<-predict(preProc, finalTraining[,-54])
fitPC<-train(finalTraining$classe~., data=trainPC, method="rpart")
print(fitPC$finalModel)

fitPC.rf<-train(finalTraining$classe~., 
                data=trainPC, 
                method="rf",
                ntree = 20,
                tuneLength = 4)
print(fitPC.rf$finalModel)


##validation
finalValidation<-clearData(validation, nsv)
validPC.rf<-predict(preProc, finalValidation[,-54])
predClasse<-predict(fitPC.rf, newdata=validPC.rf)

validPC.rf$predRight<-predClasse==finalValidation$classe
table(predClasse,validPC.rf$predRight)

print(fitPC.rf$finalModel)
plot(fitPC.rf$finalModel)
text(fitPC.rf$finalModel, use.n=T, all=T, cex=.5)
##testing
finalTesting<-clearData(testing, nsv)
testingPC.rf<-predict(preProc, finalTesting[,-54])
predClasse<-predict(fitPC.rf, newdata=testingPC.rf)
predClasse

##log pca
tr.log<-log(abs(finalTraining[,1:53])+1)
tr.log.pca <- prcomp(tr.log,
                 center = TRUE,
                 scale. = TRUE) 
print(tr.log.pca)

summary(tr.log.pca)

preProc.log<-preProcess(log(abs(finalTraining[,-54])+1), method="pca")
trainPC.log<-predict(preProc.log, finalTraining[,-54])
fitPC.log<-train(finalTraining$classe~., data=trainPC.log, method="rpart")
print(fitPC.log$finalModel)

##validation
finalValidation<-clearData(validation, nsv)
validPC.log<-predict(preProc.log, finalValidation[,-54])
predClasse<-predict(fitPC.log, newdata=validPC.log)

validPC.log$predRight<-predClasse==finalValidation$classe
table(predClasse,validPC.log$predRight)



##Get some results
fit<-train(classe~.,
           data=finalTraining,
           method="rpart",
           preProc = c("center", "scale","BoxCox"))###scale, center

print(fit$finalModel)
plot(fit$finalModel, uniform=TRUE)
text(fit$finalModel, use.n=T, all=T, cex=.5)



finalValidation<-clearData(validation, nsv)
predClasse<-predict(fit, newdata=finalValidation)
finalValidation$predRight<-predClasse==finalValidation$classe
table(predClasse,finalValidation$predRight)


fit0<-train(classe~.,
           data=finalTraining,
           method="rpart")
print(fit0$finalModel)
plot(fit0$finalModel, uniform=TRUE)
text(fit0$finalModel, use.n=T, all=T, cex=.5)


finalValidation<-clearData(validation, nsv)
predClasse<-predict(fit0, newdata=finalValidation)
finalValidation$predRight<-predClasse==finalValidation$classe
table(predClasse,finalValidation$predRight)

#try at night
fit1<-train(classe~.,
           data=finalTraining,
           method="gbm",
           verbose=F)

##try at night
fit3<-train(classe~.,
            data=finalTraining,
            method="rda",
            preProc = c("center", "scale"),
            tuneLength = 15)



##worste perf
fit2<-train(classe~.,
            data=finalTraining,
            method="pls",
            preProc = c("center", "scale"),
            tuneLength = 15)
print(fit2$finalModel)
plot(fit2$finalModel, uniform=TRUE)
text(fit2$finalModel, use.n=T, all=T, cex=.5)
predClasse<-predict(fit2, newdata=finalValidation)
finalValidation$predRight<-predClasse==finalValidation$classe
table(predClasse,finalValidation$predRight)




fitRFNP<-train(classe~., data=finalTraining,
           method="rf", prox=TRUE, ntree=10)



fit<-train(classe~., data=finalTraining, preProcess=c("pca"),
           method="rf", prox=TRUE, ntree=100)
print(fit$finalModel)
plot(fit$finalModel, uniform=TRUE)

finalValidation$predClasse<-NULL
finalValidation$predClasse<-predict(fit, newdata=finalValidation)


##validation and result comparision
