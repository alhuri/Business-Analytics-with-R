
# Lab: Decision Trees

## Fitting Classification Trees

###
library(ISLR2)

?Carseats
dim(Carseats)
head(Carseats)
summary(Carseats)

attach(Carseats)
hist(Sales)
#factor to convert categories to factors or numbers.. using also if-else
High = factor(ifelse(Sales <= 8, "No", "Yes"))
# to add high to carseats
Carseats = data.frame(Carseats, High)
head(Carseats)

###
set.seed(2)
train = sample(1:nrow(Carseats), 200)

Carseats.test = Carseats[-train, ]
High.test = High[-train]


###
library(tree)

?tree
tree.carseats = tree(High ~ . - Sales, Carseats, subset = train)

summary(tree.carseats)
#plot the brances of the tree
plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.pred = predict(tree.carseats, Carseats.test, type = "class")

table(tree.pred, High.test)
(104 + 50) / 200


###doing the pruning with our target being the rate of misclassification
set.seed(7)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)

names(cv.carseats)
cv.carseats

###
plot(cv.carseats$size, cv.carseats$dev, type = "b")


###selecting 9 as the best size of tree based on the previous results
prune.carseats = prune.misclass(tree.carseats, best = 9)

plot(prune.carseats)
text(prune.carseats, pretty = 0)

###
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(97 + 58) / 200

## Fitting Regression Trees
?Boston
dim(Boston)
nrow(Boston)
ncol(Boston)

###
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston = tree(medv ~ ., Boston, subset = train)
summary(tree.boston)

###
plot(tree.boston)
text(tree.boston, pretty = 0)

###
cv.boston = cv.tree(tree.boston)
cv.boston

plot(cv.boston$size, cv.boston$dev, type = "b")

###
prune.boston = prune.tree(tree.boston, best = 7)
plot(prune.boston)
text(prune.boston, pretty = 0)

###
yhat = predict(prune.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]

plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2)



## Bagging and Random Forests

###
library(randomForest)
?randomForest

set.seed(1)
bag.boston = randomForest(medv ~ ., data = Boston, ntree=500,
    subset = train, mtry = 12, importance = TRUE)
bag.boston


###
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])

plot(yhat.bag, boston.test)
abline(0, 1)

mean((yhat.bag - boston.test)^2)


###
bag.boston = randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 12, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - boston.test)^2)


###
sqrt(12)

set.seed(1)
rf.boston = randomForest(medv ~ ., data = Boston, ntree=500,
    subset = train, mtry = 4, importance = TRUE)

yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)

###
?importance
importance(rf.boston)
###
varImpPlot(rf.boston)


## Boosting

###
library(gbm)

?gbm
##Gaussian cuz it regression if its classification ill use Bernoulli
set.seed(1)
boost.boston = gbm(medv ~ ., data = Boston[train, ],
    distribution = "gaussian", n.trees = 500,
    interaction.depth = 2)

###
summary(boost.boston)

###
yhat.boost = predict(boost.boston,
    newdata = Boston[-train, ], n.trees = 500)

mean((yhat.boost - boston.test)^2)


