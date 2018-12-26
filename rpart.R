require(rpart)
require(ggplot2)
require(rpart.plot)
require(e1071)
require(dplyr)
require(caret)
mydata <- read.csv("clean.csv")
attach(mydata)
var <- mental_illness~family_history+Age+remote_work+no_employees
used_attributes <- c("mental_illness","family_history","Age","remote_work","no_employees")
mydata<-mydata [as.vector(which(!is.na(mental_illness))),]
mydata <- mydata [,names(mydata) %in% used_attributes ]
temp <- sample(2, nrow(mydata), replace = T, prob = c(0.7, 0.3))
train <- mydata[temp == 1,]
sapply(mydata,function(x) sum(is.na(x)))
test <- mydata[temp == 2,]
train %<>% remove_missing()
sapply(train, function(x) sum(is.na(x)))
model<- rpart(var,data=train,method = "class")
rpart.plot::rpart.plot(model, type = 2, fallen.leaves = FALSE, extra = 4)
testPred <- predict(model, newdata = test, type="class")
predictability <- sum(testPred == test$mental_illness)/ length(test$mental_illness)*100
print(predictability)
cm <- table(testPred,test$mental_illness)
precision<- diag(cm)/colSums(cm)
recall <- diag(cm) / rowSums(cm)
F1 <- (2 * precision * recall) / (precision + recall)
svm_model <- svm(mental_illness~.,data=train,method="class")
svm_predict <- predict(svm_model,newdata = test,type="class")
print(svm_predict)
svmconfmat <- table(svm_predict, test$mental_illness) 
predictability1 <- sum(svm_predict == test$mental_illness)/ length(test$mental_illness)*100
precision1<- diag(svmconfmat)/colSums(svmconfmat)
recall1 <- diag(svmconfmat) / rowSums(svmconfmat)
F1_1 <- (2 * precision1 * recall1) / (precision1 + recall1)

