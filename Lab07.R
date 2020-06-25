#lab exercises
Mydata<-read.csv("survey.csv",header=TRUE,sep=",")
table(Mydata$MYDEPV)
with(Mydata, table(Price, MYDEPV))
summary(Mydata$Age)
cor.mat <-cor(Mydata[,-1])
cor.mat

mylogit <-glm(MYDEPV ~ Income + Age + as.factor(Price), data=Mydata,family=binomial(link="logit"), na.action=na.pass)
summary(mylogit)              
confint(mylogit)
exp(mylogit$coefficients)

attributes(mylogit)
1-with(mylogit, deviance/null.deviance)
plot(mylogit)

Mydata$pricefactor = relevel(as.factor(Mydata$Price), "30")
mylogit2 = glm(MYDEPV ~ Income + Age + pricefactor, data= Mydata,family=binomial(link="logit"), na.action=na.pass)
summary(mylogit2)

pred = predict(mylogit, type="response")
predObj = prediction(pred, Mydata$MYDEPV)

?performance

rocObj = performance(predObj, measure="tpr", x.measure="fpr")  
# creates ROC curve obj
aucObj = performance(predObj, measure="auc")  # auc object

auc = aucObj@y.values[[1]]  
auc

plot(rocObj, main = paste("Area under the curve:", auc))

Price <-c(10,20,30)
Age <-c(mean(Mydata$Age))
Income <-c(mean(Mydata$Income))
newdata1 <-data.frame(Income,Age,Price)
newdata1

newdata1$PurchaseP <-predict(mylogit,newdata=newdata1,type="response")
newdata1

newdata2 <- data.frame(Age=seq(min(Mydata$Age),max(Mydata$Age),2), Income=mean(Mydata$Income),Price=30)
newdata2$AgeP<-predict(mylogit,newdata=newdata2,type="response")
cbind(newdata2$Age,newdata2$AgeP)

plot(newdata2$Age,newdata2$AgeP)

#in lab tasks
Mydata$predict <- predict(mylogit, type="response")
odd_rat <- exp(mylogit$coefficients)
(odd_rat[2] - 1)*100
(odd_rat[5] - 1)*100
sum(Mydata$MYDEPV)
sum(Mydata$predict)
newdata = data.frame(Price = 20, Income = 58, Age=25)
predict(mylogit, newdata, type="response")

newdata = data.frame(Price = 10, Income = 20.245, Age=94)
predict(mylogit, newdata, type="response")

