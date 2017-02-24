new.env(parent = baseenv())
##About the Project
###1.Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

###2.Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

###3.What you should submit

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.  

***

###4.References
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf

***

##Project Approach

###1.Prerequisites
####Install required packages

```r
library(caret)
library(dplyr)
```

####Getting Data

```r
pmltraining <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", sep = ",", header = TRUE, na.string=c("NA","#DIV/0!",""))
pmltesting  <-  read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", sep = ",", header = TRUE, na.string=c("NA","#DIV/0!",""))
```
###2.Analyze & clean the data 
>Analyze data

```r
summary(pmltraining)
```
It can be observed there are several columns with values *NA* and many columns are have calculated values (like min, max, std.dev) which can be excluded.

>Prepare and exclude the columns which can be removed

```r
#Define a function to return a list of matching keywords
matchfn <- function(key, data=names(pmltraining)){ contains(match = key,vars = data)}
excludeList <- c("new_window", "raw_timestamp_part_1", "raw_timestamp_part_2","cvtd_timestamp","var_","avg_","min_","max_","stddev_")
#
# Remove columnts that we don't need from the dataset
x <- unlist(lapply(excludeList, matchfn))
pmltraining <- pmltraining[,-x]
```

>Remove columns that has *NA* for over 75% of it's values

```r
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
```
>Remove the first column with running sequence number

```r
#
pmltraining <- pmltraining[-1]
#
```
>Ensure our training and testing set will have same set of columns

```r
trainCols <- colnames(pmltraining)
# Testing set didn't have 'classe', (the last column), so let's remove it, rest should match
testCols <- colnames(pmltraining[-length(trainCols)])
pmltesting <- pmltesting[testCols] # both training and testing should have same column except 'classe'
```

###3.Partition Data

Partition the data to create *training* and *testing* set. We will train our model on *training* set and cross verify in our *testing* model to check accuracy. Once we are satisfied, we will apply the best of the model to *pmltesting* data provided to us.


```r
set.seed(32323)
inTraining <- createDataPartition(pmltraining$classe, p=2/3, list=FALSE)
training <- pmltraining[inTraining,]
testing <- pmltraining[-inTraining,]
```
From the *pmltraining* data we are randomly selecing *two-thirds* of the data for training the model.


```r
# Look at the resulting dimensions
dim(pmltraining)
```

```
## [1] 19622    55
```

```r
dim(training)       # Our training set 
```

```
## [1] 13083    55
```

```r
dim(testing)        # Our testing set for cross verification
```

```
## [1] 6539   55
```

```r
dim(pmltesting)     # Our final testing set for result submission
```

```
## [1] 20 54
```
###4.Machine Learning

```r
set.seed(76867)
```
>Training the model

Let's try five different models and pick the one with highest accuracy. Following models are adopted from **caret** package using the **train** function:

1. **Random Forest**                          *(method = 'rf')*
2. **Linear Discriminant Analysis**           *(method = 'lda')*
3. **k-Nearest Neighbors**                    *(method = 'knn')*
4. **Support Vector Machines**                *(method = 'svmRadial')*
5. **CART**                                   *(method = 'rpart')*


```r
fit.rf    <- train(classe ~ ., data=training, method="rf")
fit.lda   <- train(classe ~ ., data=training, method="lda")
fit.knn   <- train(classe ~ ., data=training, method="knn")
fit.svm   <- train(classe ~ ., data=training, method="svmRadial")
fit.cart  <- train(classe ~ ., data=training, method="rpart")
```

>Testing the model

```r
pred.lda  <- predict(fit.lda, testing)
pred.knn  <- predict(fit.knn, testing)
pred.cart <- predict(fit.cart, testing)
pred.rf   <- predict(fit.rf, testing)
pred.svm  <- predict(fit.svm, testing)
```

>Selecting the best model

```r
results <- resamples(list(rf=fit.rf, cart=fit.cart, knn=fit.knn, svm=fit.svm, lda=fit.lda))
```

```
## Error in eval(expr, envir, enclos): could not find function "resamples"
```

```r
dotplot(results)
```

```
## Error in eval(expr, envir, enclos): could not find function "dotplot"
```


