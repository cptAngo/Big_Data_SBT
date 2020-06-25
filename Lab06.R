data  <- read.table("new_zeta_nodupes.csv", sep=",", header=TRUE)
data$X <- NULL
data$log_income <- log10(data$meanhouseholdincome)

library(plotly)
plot_ly(data, x = ~age, y = ~education, z = ~employment, color=~education, colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'))

y <- data$log_income

#Create a scatter plot showing the effect age has on log_income and paste it here
x <- data$age
plot(x, y)

# Create a linear regression model between log_income and age
d <-lm(y ~ x)
str(d)
print(d)
par(mfrow=c(2,2))
plot(d)

ypred <-predict(d)
par(mfrow=c(1,1))
plot(y, y, type="l", xlab="true y", ylab="predicted y")
points(y, ypred)

d1 <-summary(d)
print(d1)

#Create a scatter plot showing the effect education has on log_income
x <- data$education
d <-lm(y ~ x)
str(d)
print(d)
par(mfrow=c(2,2))
plot(d)

ypred <-predict(d)
par(mfrow=c(1,1))
plot(y, y, type="l", xlab="true y", ylab="predicted y")
points(y, ypred)

d1 <-summary(d)
print(d1)

#Analyze a detailed summary of a linear regression model between the dependent variable log_income,
#and the independent variables age, education, and employment
x1 <- data$age
x2 <- data$education
x3 <- data$employment

m <- lm(y ~ x1 + x2 + x3)
str(m)
print(m)
par(mfrow=c(2,2))
plot(m)

ypred <-predict(m)
par(mfrow=c(1,1))
plot(y, y, type="l", xlab="true y", ylab="predicted y")
points(y, ypred)

d1 <-summary(m)
print(d1)





