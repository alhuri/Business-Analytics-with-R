library(ISLR)

attach(Weekly)
?Weekly
fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(fit)

fit1 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = "binomial")
summary(fit1)

probs = predict(fit1, newdata = Weekly[1, ], type = "response")
pred <- rep("No", length(probs))
pred[probs > 0.5] = "Yes"
table(pred, Direction[1])

mean(pred == Direction[1])

error <- rep(0, dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
  fit2 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ],  family = "binomial")
  uppred <- predict.glm(fit2, Weekly[i, ], type = "response") > 0.5
  true <- Weekly[i, ]$Direction == "Up"
  if (uppred != true)
    error[i] <- 1
}
error

mean(error)



