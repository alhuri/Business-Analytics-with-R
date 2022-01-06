###Q15

library(MASS)
attach(Boston)
head(Boston)

fit1 <- lm(crim ~ rm)
summary(fit1)

fit2 <- lm(crim ~ age)
summary(fit2)

fit3 <- lm(crim ~ dis)
summary(fit3)

fit4 <- lm(crim ~ zn)
summary(fit4)

fit5 <- lm(crim ~ nox)
summary(fit5)

fit6<- lm(crim ~ rad)
summary(fit6)

fit7 <- lm(crim ~ tax)
summary(fit7)

fit8 <- lm(crim ~ ptratio)
summary(fit8)

fit9 <- lm(crim ~ indus)
summary(fit9)

fit10 <- lm(crim ~ black)
summary(fit10)

fit11 <- lm(crim ~ lstat)
summary(fit11)

fit12 <- lm(crim ~ medv)
summary(fit12)

chas <- as.factor(chas)
fit13 <- lm(crim ~ chas)
summary(fit13)

pairs(Boston)

fit <- lm(crim ~ ., data = Boston)
summary(fit)

sr <- vector("numeric",0)
sr <- c(sr, fit1$coefficient[2])
sr <- c(sr, fit2$coefficient[2])
sr <- c(sr, fit3$coefficient[2])
sr <- c(sr, fit4$coefficient[2])
sr <- c(sr, fit5$coefficient[2])
sr <- c(sr, fit6$coefficient[2])
sr <- c(sr, fit7$coefficient[2])
sr <- c(sr, fit8$coefficient[2])
sr <- c(sr, fit9$coefficient[2])
sr <- c(sr, fit10$coefficient[2])
sr <- c(sr, fit11$coefficient[2])
sr <- c(sr, fit12$coefficient[2])
sr <- c(sr, fit13$coefficient[2])
mr <- vector("numeric", 0)
mr <- c(mr, fit$coefficients)
mr <- mr[-1]
plot(sr, mr)



fit14 <- lm(crim ~ poly(indus, 3))
summary(fit14)


fit15 <- lm(crim ~ poly(dis, 3))
summary(fit15)


fit16 <- lm(crim ~ poly(zn, 3))
summary(fit16)

fit17 <- lm(crim ~ poly(medv, 3))
summary(fit17)

fit18 <- lm(crim ~ poly(rad, 3))
summary(fit18)

fit19 <- lm(crim ~ poly(nox, 3))
summary(fit19)

fit20 <- lm(crim ~ poly(rm, 3))
summary(fit20)

fit21 <- lm(crim ~ poly(age, 3))
summary(fit21)

fit22 <- lm(crim ~ poly(ptratio, 3))
summary(fit22)

fit23 <- lm(crim ~ poly(black, 3))
summary(fit23)

fit24 <- lm(crim ~ poly(tax, 3))
summary(fit24)

fit25 <- lm(crim ~ poly(lstat, 3))
summary(fit25)

