library("rpart")
library("rpart.plot")
df <- read.csv(file="survey.csv", header=TRUE, sep=",")
train <-as.data.frame(df[1:600,])
test <-as.data.frame(df[601:750,])

#Build a classification tree from the training data
model <-rpart(MYDEPV ~ Price + Income + Age, data=train, parms=c(split = "information"))
summary(model)
rpart.plot(model)
pred <- predict(model,newdata = train[,2:4])

#Score the model with the training data and create
#the model’s confusion matrix
conf <-table(actual=train[,1],predicted=pred)
conf

library(ROCR)
predObj = prediction(pred, train$MYDEPV)
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
# creates ROC curve obj
aucObj = performance(predObj, measure="auc")  # auc object
auc = aucObj@y.values[[1]]  
auc
plot(rocObj, main = paste("Area under the curve:", auc))

#test
predtest <- predict(model,newdata = test[,2:4])
predObjtest = prediction(predtest, test$MYDEPV)
rocObjtest = performance(predObjtest, measure="tpr", x.measure="fpr")
aucObjtest = performance(predObjtest, measure="auc")  # auc object
auctest = aucObjtest@y.values[[1]]  
auctest
plot(rocObjtest, main = paste("Area under the curve:", auctest))

#gini
modelgini <-rpart(MYDEPV ~ Price + Income + Age, data=train, parms=c(split = "gini"), xval=3)
summary(modelgini)
rpart.plot(modelgini, type=4, extra=1)
predgini <- predict(model,newdata = train[,2:4])
#according to misclassification rate "0" is better predicted
confgini <-table(actual=train[,1],predicted=predgini)
confgini

tree<-prune(modelgini,cp=0.1)
prunegini <- predict(tree,newdata = train[,2:4])
confgini1 <-table(actual=train[,1],predicted=prunegini)
confgini1
#after pruning classes "0" and "1" disappear 

