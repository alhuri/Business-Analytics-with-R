
# Lab: Classification Methods

## The Stock Market Data

###
library(ISLR2)

###
?Smarket
names(Smarket)
dim(Smarket)
head(Smarket)

###
summary(Smarket)
pairs(Smarket)
##colored plots based on direction
pairs(Smarket, col=Smarket$Direction)

###calculate the corr
cor(Smarket)
## corr without the direction col
cor(Smarket[, -9])

###
attach(Smarket)
plot(Volume)
plot(Direction, Lag1)
plot(Direction, Lag2)
plot(Direction, Lag3)
plot(Direction, Lag4)
plot(Direction, Lag5)
plot(Direction, Volume)


## Logistic Regression

###
glm.fits = glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial)
summary(glm.fits)

###
coef(glm.fits)

summary(glm.fits)$coef
# to only get p value
summary(glm.fits)$coef[,4] #p-values

### response gets us the probability
glm.probs = predict(glm.fits, type = "response")
## predict from 1 to 10
glm.probs[1:10]
#the representation of up and down in numbers
contrasts(Direction)

###extract classes from probs
glm.pred = ifelse(glm.probs > .5, 'Up', 'Down')

###confusion table
table(glm.pred, Direction)
##two ways to get accuracy
(507 + 145) / 1250
mean(glm.pred == Direction)


### to get a training data
train = (Year < 2005)

## to get out test set
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
## to get out test set
Direction.2005 = Direction[!train]

###
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial, subset = train)
summary(glm.fits)

glm.probs = predict(glm.fits, Smarket.2005, type = "response")

###
glm.pred = ifelse(glm.probs > .5, 'Up', 'Down')

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)


### fit a smaller model
glm.fits = glm(Direction ~ Lag1 + Lag2, data = Smarket,
    family = binomial, subset = train)

glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = ifelse(glm.probs > .5, 'Up', 'Down')

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

summary(Direction[train])
summary(Direction.2005)
141 / (111+141)

106 / (106 + 76)


###prediction
predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5),  Lag2 = c(1.1, -0.8)),
    type = "response")


## Linear Discriminant Analysis

###
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
##show the vars used in the model like pi , mean and sigma
lda.fit

###
lda.pred = predict(lda.fit, Smarket.2005)
#components of the class
names(lda.pred)
#return the classes predicted
lda.pred$class
#return the probabilities
lda.pred$posterior


###confusion matrix
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

#change the threshold
ifelse(lda.pred$posterior[, 1] > 0.6, 'Down', 'Up')

## Quadratic Discriminant Analysis

###
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

###
qda.pred = predict(qda.fit, Smarket.2005)
qda.class = qda.pred$class

table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
121/(121+81)



## $K$-Nearest Neighbors

###
library(class)
##cbind make a combination to create a matrix
train.X = cbind(Lag1, Lag2)[train, ]
test.X = cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]

###set seed to control the randomness in case two points have 
##the same distance to what we want to predict
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)

table(knn.pred, Direction.2005)
(83 + 43) / 252

###
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)


###
?Caravan
dim(Caravan)

attach(Caravan)
summary(Purchase)
348 / 5822

###makes each col has same variance
##used when every col has different unit
standardized.X = scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])

mean(Caravan[, 1])
mean(Caravan[, 2])
mean(standardized.X[, 1])
mean(standardized.X[, 2])


###
test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
table(test.Y, knn.pred)
mean(test.Y == knn.pred)
mean(test.Y == "No")

###
table(test.Y, knn.pred)
9 / (68 + 9)
summary(test.Y)
59 / (941+59)

###
knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5 / 26

knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4 / 15


###
glm.fits = glm(Purchase ~ ., data = Caravan,
    family = binomial, subset = -test)
glm.probs = predict(glm.fits, Caravan[test, ],
    type = "response")

glm.pred = ifelse(glm.probs > .5, 'Yes', 'No')
table(glm.pred, test.Y)

glm.pred = ifelse(glm.probs > .25, 'Yes', 'No')
table(glm.pred, test.Y)
11 / (22 + 11)
