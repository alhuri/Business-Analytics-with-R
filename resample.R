
# Lab: Cross-Validation and the Bootstrap


## The Validation Set Approach

###
library(ISLR2)

?sample
set.seed(1)
#to get 196 observations from 1to 392
train = sample(1:392, 196)

###
?Auto
attach(Auto)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
summary(lm.fit)

plot(horsepower, mpg, col=1, pch=1)
abline(lm.fit, col=2, lwd=3)

###
predict(lm.fit, Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

###
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
#to get MSE of the testing set we squared it
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

#print plots over each other
plot(horsepower, mpg, col=1, pch=1)
points(horsepower, predict(lm.fit, Auto), col=2, pch=2)
points(horsepower, predict(lm.fit2, Auto), col=3, pch=3)
points(horsepower, predict(lm.fit3, Auto), col=4, pch=4)


### seed here is different
set.seed(2)
train = sample(1:392, 196)

lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
#to get MSE of the testing set
mean((mpg - predict(lm.fit3, Auto))[-train]^2)


#model selection
mse_over_order = function(seed){
  set.seed(seed)
  train = sample(1:392, 196)
  ## rep is used to replicate 0 10 times
  mse = rep(0,10)
  for (i in 1:10) {
    lm.fit = lm(mpg ~ poly(horsepower, i), data = Auto, subset = train)
    mse[i] = mean((mpg - predict(lm.fit, Auto))[-train]^2)
  }
  
  return(mse)
}

MSE1 = mse_over_order(1)
MSE2 = mse_over_order(2)
MSE3 = mse_over_order(3)
MSE4 = mse_over_order(4)
MSE5 = mse_over_order(5)

plot(1:10, MSE1, type='l', col=1, ylim=c(12,30))
points(1:10, MSE2, type='l', col=2)
points(1:10, MSE3, type='l', col=3)
points(1:10, MSE4, type='l', col=4)
points(1:10, MSE5, type='l', col=5)



## Leave-One-Out Cross-Validation

###cuz there is not family parameter 
#it will do a linear regression
glm.fit = glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
###same
lm.fit = lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

###to do crossval ..all data
library(boot)

glm.fit = glm(mpg ~ horsepower, data = Auto)
?cv.glm
cv.err = cv.glm(Auto, glm.fit)
#The first component of delta is the average mean-squared error 
cv.err$delta[1]

###
cv.error = rep(0, 10)
for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
plot(1:10, cv.error, type='l', col=1)



## k-Fold Cross-Validation to specify k
###
set.seed(17)
cv.error.10 = rep(0, 10)

for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10

plot(1:10, cv.error, type='l', col=1, lwd=2)
points(1:10, cv.error.10, type='l', col=2, lwd=2)

## The Bootstrap
### Estimating the Accuracy of a Statistic of Interest
### to find alpha to minimize risk
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y)))
}

###
alpha.fn(Portfolio, 1:100)

###
set.seed(7)
#samples from 1 to 100 and the number of samples is 100
alpha.fn(Portfolio, sample(1:100, 100, replace = T))

###boot is to use bootstrap to return alpha.. R= number of bootstrap
boot.out = boot(Portfolio, alpha.fn, R = 1000)
boot.out
plot(boot.out)


### Estimating the Accuracy of a Linear Regression Model

###
boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
  
boot.fn(Auto, 1:392)

###
set.seed(1)
boot.fn(Auto, sample(1:392, 392, replace = T))

set.seed(2)
boot.fn(Auto, sample(1:392, 392, replace = T))


##to automate the above process use boot
boot(Auto, boot.fn, 1000)

###
summary(lm(mpg ~ horsepower, data = Auto))$coef


###
boot.fn = function(data, index){
  coef(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
}
##to automate the process use boot
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
###
