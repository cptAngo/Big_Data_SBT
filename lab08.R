library("e1071")
# read the data into a table from the file
data <- read.table("nbtrain.csv",header=TRUE,sep=",")
# we will now define the data frames to use the NB classifier
traindata <- as.data.frame(data[1:9010,])
testdata <- as.data.frame(data[9011:10010,])
#Display data frames
traindata
testdata
#Bulid the model maually
tprior <- table(traindata$income)
tprior <- tprior/sum(tprior)
#colnames(traindata)
#"age"    "sex"    "educ"   "income"
ageCounts <-table(traindata[,c("income", "age")])
ageCounts <- ageCounts/rowSums(ageCounts)
sexCounts <- table(traindata[,c("income", "sex")])
sexCounts <- sexCounts/rowSums(sexCounts)
educCounts <- table(traindata[,c("income", "educ")])
educCounts<-educCounts/rowSums(educCounts)

# use the NB classifier 
model <- naiveBayes(income ~.,traindata)
model
results <- predict (model,testdata)
# display results
results
conf <- table(actual=testdata$income,predicted=results)
conf
mistake  <- 1 - sum(diag(conf))/sum(conf)
mistake

model2 <- naiveBayes(sex ~., traindata)
model2
results2 <- predict(model2, testdata)
conf2 <- table(actual=testdata$sex, predicted=results2)
conf2
mistake2 <- 1 - sum(diag(conf2))/sum(conf2)
mistake2
1 - conf2[1]/(conf2[1] + conf2[3])
1 - conf2[4]/(conf2[2] + conf2[4])
