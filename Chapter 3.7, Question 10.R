
library(ISLR)
## Q10
data(Carseats)
fit <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit)

fitNo2 <- lm(Sales ~ Price + US, data = Carseats)
summary(fitNo2)
confint(fitNo2)

plot(fitNo2)











