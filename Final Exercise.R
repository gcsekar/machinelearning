library(caret)
library(dplyr)
setwd("~/R-Projects/Machine Learning")
## Load the training data set

#               GETTING DATA

pmltraining <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", sep = ",", header = TRUE, na.string=c("NA","#DIV/0!",""))
pmltesting  <-  read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", sep = ",", header = TRUE, na.string=c("NA","#DIV/0!",""))
##
## Looking at Human Activity Recognition Website - http://groupware.les.inf.puc-rio.br/har
## the classification were derived from four main sensors 
## 

#Define a function to return a list of matching keywords
matchfn <- function(key, data=names(pmltraining)){ contains(match = key,vars = data)}
excludeList <- c("new_window", "raw_timestamp_part_1", "raw_timestamp_part_2","cvtd_timestamp","var_","avg_","min_","max_","stddev_")
#
# Remove columnts that we don't need from the dataset
x <- unlist(lapply(excludeList, matchfn))
pmltraining <- pmltraining[,-x]
#
# Remove columns that more NA values
#
temp <- pmltraining
for(i in 1:length(pmltraining)) { 
  if( sum( is.na( pmltraining[, i] ) ) /nrow(pmltraining) >= .75 ) { 
    for(j in 1:length(temp)) {
      if( length( grep(names(pmltraining[i]), names(temp)[j]) ) == 1)  {
        temp <- temp[ , -j] 
      }   
    } 
  }
}
#
pmltraining <- temp
rm(temp)
#
# Remove the ID Column
#
pmltraining <- pmltraining[-1]
#
#
trainCols <- colnames(pmltraining)
testCols <- colnames(pmltraining[-length(trainCols)])
pmltesting <- pmltesting[testCols]
#
set.seed(32323)
inTraining <- createDataPartition(pmltraining$classe, p=2/3, list=FALSE)
training <- pmltraining[inTraining,]
testing <- pmltraining[-inTraining,]
#
#
#     START THE TRAINING PROCESS
#     LET'S APPLY DIFFERENT MODEL AND CHOOSE THE BEST ONE
#
#
set.seed(76867)
#
#
fit.rf = train(classe ~ ., data=training, method="rf")
fit.cart <- train(classe ~ ., data=training, method="rpart")
fit.knn <-  train(classe ~ ., data=training, method="knn")
fit.svm <-  train(classe ~ ., data=training, method="svmRadial")
fit.lda <-  train(classe ~ ., data=training, method="lda")

pred.cart <- predict(fit.cart, testing)
pred.knn <- predict(fit.knn, testing)
pred.svm <- predict(fit.svm, testing)
pred.lda <- predict(fit.lda, testing)
pred.rf <- predict(fit.rf, testing)
pred.comb <- predict(fit.comb, testing)

#STACK RUN

newDF <- data.frame(classe = testing$classe, pred.rf, pred.lda,pred.svm)
fit.comb <- train(classe ~ ., data=newDF, method="rf")

results <- resamples(list(RF=fit.rf, CART=fit.cart, KNN=fit.knn, SVM=fit.svm, LDA=fit.lda))
dotplot(results)
summary(results)
confusionMatrix(pred.rf, testing$classe)
confusionMatrix(pred.svm, testing$classe)
predict(fit.rf, pmltesting)
