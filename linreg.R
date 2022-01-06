
# Lab: Linear Regression
## Libraries
###
library(MASS)
library(ISLR2)


## Simple Linear Regression

###name of the dataset
names(Boston)
?Boston
head(Boston)

###
plot(lstat, medv)
attach(Boston)
plot(lstat, medv)

###Fitting Linear Models
?lm
lm.fit <- lm(medv ~ lstat, data = Boston)

###
lm.fit
summary(lm.fit)
###
names(lm.fit)
coef(lm.fit)

###to get the confidence interval lower and upper bound
confint(lm.fit)
###for the x when it equals each of the three values
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
    interval = "prediction")

###abline is to draw the estimated line 
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")

###
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

###residual is the distance of lines from the regression line
plot(predict(lm.fit), residuals(lm.fit))


## Multiple Linear Regression

###lstate and age are dependent vars
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

###. means the rest of vars
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

### rest of vars except age
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

### rest of vars except age and indus cuz their
## p value is high
lm.fit1 <- lm(medv ~ . - age -indus, data = Boston)
summary(lm.fit1)

##we can only identify the least important vars
##but not the most important


## Interaction Terms

###the two ways to get with an without interaction
summary(lm(medv ~ lstat + age + lstat:age, data = Boston))

summary(lm(medv ~ lstat * age, data = Boston))


## Non-linear Transformations of the Predictors

###
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

plot(lstat, medv)
points(lstat, fitted(lm.fit2), col=2, pch=20)


### 5 polynomial orders of lstat
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

plot(lstat, medv)
##plots the predicted points of the fitted model 
##if i increased the order of the poly to much 
##i might overfit to data instead of 
##getting a better model
points(lstat, fitted(lm.fit2), col=2, pch=20)
points(lstat, fitted(lm.fit5), col=4, pch=20)

###the log of var named rm
summary(lm(medv ~ log(rm), data = Boston))


## Qualitative Predictors

###
head(Carseats)
?Carseats
names(Carseats)
summary(Carseats)

###using interaction
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
    data = Carseats)
summary(lm.fit)

###to see how qualitative vars are represented
attach(Carseats)
contrasts(ShelveLoc)
contrasts(Urban)
contrasts(US)


## Writing  Functions

###
plus <- function(a, b) {
 return(a+b)
}

###
plus(1,2)


###
regplot = function(x, y){
  fit = lm(y~x)
  plot(x, y, col=1)
  abline(fit, col=2)
}

attach(Carseats)
regplot(Price, Sales)

