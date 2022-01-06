##set the dir
setwd('C:/Users/96654/Downloads/BA/R lab exercise/Data_file')
set.seed(1)

## read csv file
train <- read.csv("./Data_train.csv")
test <- read.csv("./Data_test.csv")

##
train$DEFAULT<-as.factor(train$DEFAULT)
train$EDUCATION<-as.factor(train$EDUCATION)
train$MARRIAGE<-as.factor(train$MARRIAGE)
train$SEX<-as.factor(train$SEX)

test$EDUCATION<-as.factor(test$EDUCATION)
test$MARRIAGE<-as.factor(test$MARRIAGE)
test$SEX<-as.factor(test$SEX)

##
summary(train)
head(train)

##

males <- train[train$SEX == 1, ]
females  <- train[train$SEX == 2, ]

par(mfrow = c(2,2))

plot(males$DEFAULT, xlab = "DEFAULT", ylab = "Count", main = "Males defaults")
plot(females$DEFAULT, xlab = "DEFAULT", ylab = "Count", main = "Females defaults")
plot( x = train$EDUCATION, main = "Distribution of Education", 
      xlab = "Education",
      ylab = "Count")
plot( x = train$MARRIAGE, main = "Distribution of Marriage", 
      xlab = "Marriage",
      ylab = "Count")

##



boxplot(train$PAY_1 ~ train$DEFAULT, xlab = "Default", ylab ="PAY_1", main = "PAY 1 VS Default")
boxplot(train$BILL_AMT1 ~ train$DEFAULT, xlab = "Default", ylab ="BILL_AMT1", main = "BILL_AMT1 VS Default")
boxplot(train$PAY_AMT1 ~ train$DEFAULT, xlab = "Default", ylab ="PAY_AMT1", main = "PAY_AMT1 VS Default")

##

mean(0 != train$DEFAULT)

##
?sample

set.seed(1)
CV_index = sample(1 : nrow(train), nrow(train))
head(CV_index)
tri<-train[CV_index,]
head(train)
head(tri)
##fix(train)
##count(tri)
##count(train)
#fix(tri)

##GLM
library(boot)

cv.glm <-function (data, model, yname, K, val) {
    n <- nrow(data)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      glm.fit=glm(model, data=data[train.index,],family = "binomial")
      
      
      #observed test set y
      glm.y <- data[test.index, yname]
      #predicted test set y
      glm.predy=predict(glm.fit, data[test.index,], type = "response")
      
      pred <- rep(0, length(glm.predy))
      pred[glm.predy > val] = 1
      
  
      #observed - predicted on test data
      error= mean(glm.y!=pred)
      #error rates 
      CV=c(CV,error)
    }
    #Output
    list(call = model, K = K, 
         glm_error_rate = mean(CV))  
  }

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.1)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.2)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.3)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.4)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.5)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.6)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.7)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.8)
er_glm$glm_error_rate

er_glm=cv.glm(data=tri,model=DEFAULT~., yname="DEFAULT", K=10, val=0.9)
er_glm$glm_error_rate


##LDA
library(MASS)

cv.lda <-
  function (data, model, yname, K) {
    n <- nrow(data)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      lda.fit=lda(model, data=data[train.index,])
      #observed test set y
      lda.y <- data[test.index, yname]
      #predicted test set y
      lda.predy=predict(lda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(lda.y!=lda.predy)
      #error rates 
      CV=c(CV,error)
    }
    #Output
    list(call = model, K = K, 
         lda_error_rate = mean(CV))  
  }

er_lda=cv.lda(data=tri,model=DEFAULT~., yname="DEFAULT", K=10)
er_lda$lda_error_rate


##KNN
library(class)
?knn

cv.knn<- function (dataY, dataX, kn=1, K=10) {
  n <- nrow(dataX)
  f <- ceiling(n/K)
  s <- sample(rep(1:K, f), n)  
#  dataX=scale(dataX)
  CV=NULL;PvsO=NULL
  
  for (i in 1:K) { 
    test.index <- seq_len(n)[(s == i)] #test data
    train.index <- seq_len(n)[(s != i)] #training data
    
    train.X <- dataX[train.index,]
    test.X <- dataX[test.index,]
    train.y <- dataY[train.index]
    test.y <- dataY[test.index]
    #predicted test set y
    knn.pred=knn(train.X, test.X, train.y, k=kn) 
    #observed - predicted on test data 
    error= mean(knn.pred!=test.y) 
    #error rates 
    CV=c(CV,mean(error))
    predvsobs=data.frame(knn.pred,test.y)
    PvsO=rbind(PvsO,predvsobs)
  } 
  
  #Output
  list(k = K,
       knn_error_rate = mean(CV), confusion=table(PvsO[,1],PvsO[,2]))
}

cv.knn(dataY=tri$DEFAULT, dataX=tri[,-23], kn=2, K=10)


cv.error=NULL
for (i in 1:20) {
  cv.error[i] <- cv.knn(dataY=tri$DEFAULT, dataX=tri[,-23], kn=i, 
                        K=10)$knn_error_rate
}
cv.error
bk=which(cv.error==min(cv.error))
print(bk)



##
library(randomForest)
?randomForest
cv.rf <-
  function (data, model, yname, K, mtry) {
    n <- nrow(data)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    print(mtry)
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      rf.fit=randomForest(model, data=data[train.index,], ntree=200, mtry = mtry)
      #observed test set y
      rf.y <- data[test.index, yname]
      #predicted test set y
      
      rf.predy=predict(rf.fit, data[test.index,])
      
      #observed - predicted on test data
      error= mean(rf.y!=rf.predy)
      #error rates 
      CV=c(CV,error)
    }
    #Output
    list(call = model, K = K, 
         rf_error_rate = mean(CV),rf.fit=rf.fit) 
  }


rf.cv.error=NULL
for (i in 1:10) {
  rf.cv.error[i] <- cv.rf(data=tri,model=DEFAULT~.,yname="DEFAULT", K=10, mtry = i)$rf_error_rate
  
}

rf.cv.error
bestm=which(rf.cv.error==min(rf.cv.error))
print(bestm)
plot(rf.cv.error)

###
rdf= randomForest(DEFAULT ~ .,data = tri, ntree=200,
                  subset = CV_index, mtry = bestm, importance = TRUE)
rfp = predict(rdf, newdata = tri)

mean(rfp != train$DEFAULT)
##0.34256
importance(rdf)
##PAY_1     65.364938 56.148057420            89.014836        877.26757



## Make predictions

best <- cv.rf(data=tri,model=DEFAULT~.,yname="DEFAULT", K=10, mtry = bestm)$rf.fit

bestpre<- predict(best, newdata = test)

## Save the predictions to a .csv file
# Finally, put the Id and predictions in a data frame and save it as a .csv file
submission <- data.frame(DEFAULT = bestpre)
df <- data.frame(test, submission)
write.csv(new, file = "Data_test.csv", row.names = F)



