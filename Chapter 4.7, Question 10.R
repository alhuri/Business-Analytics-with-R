library(ISLR)
summary(Weekly)
names(Weekly)
dim(Weekly)
cor(Weekly[, -9])

pairs(Weekly)

fit<- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(fit)


probs <- predict(fit, type = "response")
pred <- rep("Down", length(probs))
pred[probs > 0.5] <- "Up"
c<-table(pred, Weekly$Direction)
c
acc <- (c["Down", "Down"] + c["Up", "Up"])/sum(c)
acc

attach(Weekly)
train = (Year < 2009)
Weeklyhigh = Weekly[!train,] 
Direction.high = Direction[!train]
fit1 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit1)


probs1 <- predict(fit1, Weeklyhigh, type = "response")
pred1 <- rep("Down", length(probs1))
pred1[probs1 > 0.5] <- "Up"
cm<-table(pred1, Direction.high)
cm
accuracy <- (cm["Down", "Down"] + cm["Up", "Up"])/sum(cm)
accuracy


library(MASS)
fitlda <- lda(Direction ~ Lag2, data=Weekly, subset=train)
ldapreds <- predict(fitlda, newdata=Weeklyhigh)
cmlda <- table(Direction.high, ldapreds$class)
cmlda
acc1<- (cmlda["Down", "Down"] + cmlda["Up", "Up"])/sum(cmlda)
acc1


fitqda <- qda(Direction ~ Lag2, data=Weekly, subset=train)
predqda <- predict(fitqda, newdata=Weeklyhigh)
cmqda <- table(Direction.high, predqda$class)
cmqda
acc2<- (cmqda["Down", "Down"] + cmqda["Up", "Up"])/sum(cmqda)
acc2

library(class)
train1 <- as.matrix(Lag2[train])
test1 <- as.matrix(Lag2[!train])
trainDir <- Direction[train]
set.seed(1)
predknn <- knn(train1, test1, trainDir, k = 1)
cmknn<-table(predknn, Direction.high)
accknn <- (cmknn["Down", "Down"] + cmknn["Up", "Up"])/sum(cmknn)
accknn

##

fit3 <- glm(Direction ~ Lag1+Lag2, data = Weekly, family = binomial, subset = train)
probs3 <- predict(fit3, Weeklyhigh, type = "response")
pred3 <- rep("Down", length(probs3))
pred3[probs3 > 0.5] = "Up"
cm3<-table(pred3, Direction.high)
cm3
mean(pred3 == Direction.high)


lda2 <- lda(Direction ~ Lag1+Lag2+Lag3, data = Weekly, subset = train)
predlda <- predict(lda2, Weeklyhigh)
table(predlda$class, Direction.high)
mean(predlda$class == Direction.high)

qda2 <- qda(Direction ~ Lag2 + Lag3, data = Weekly, subset = train)
predqda <- predict(qda2, Weeklyhigh)
table(predqda$class, Direction.high)
mean(predqda$class == Direction.high)

predknn10 <- knn(train1, test1, trainDir, k = 10)
cmknn10<-table(predknn10, Direction.high)
cmknn10
accknn10 <- (cmknn10["Down", "Down"] + cmknn10["Up", "Up"])/sum(cmknn10)
accknn10


predknn100 <- knn(train1, test1, trainDir, k = 100)
cmknn100<-table(predknn100, Direction.high)
cmknn100
accknn100 <- (cmknn100["Down", "Down"] + cmknn100["Up", "Up"])/sum(cmknn100)
accknn100



