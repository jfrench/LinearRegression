#options(width = 50) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

# Examples from LMWR2
# 4.2 Predicting Body Fat

# load data
data(fat, package = "faraway")

# fit model with 13 predictors
lmod <- lm(brozek ~ age + weight + height + neck + chest +
             abdom + hip + thigh + knee + ankle + biceps +
             forearm + wrist, data = fat)
summary(lmod)

# extract x matrix
x <- model.matrix(lmod)
# determine median values of predictor variables
(x0 <- apply(x, 2, median))
# manually predict mean/future response
(y0hat <- sum(x0*coef(lmod)))

# data frame x0 values for which predictions are desired
newdf <- data.frame(t(x0))
head(newdf)
# calculate point estimate, prediction interval, and confidence interval for median predictor values
predict(lmod, new = newdf)
predict(lmod, new = newdf, interval = "prediction", level = 0.95)
predict(lmod, new = newdf, interval = "confidence", level = 0.95)

# extrapolation
# calculate the 0.95 quantile of each column of x
(x1 <- apply(x, 2, quantile, prob = 0.95))
predict(lmod, new=data.frame(t(x1)), interval="prediction")
predict(lmod, new=data.frame(t(x1)), interval="confidence")

### compare width of confidence interval (and prediction interval)
# as a function of predictor values

# create a model using only weight as a predictor
lms <- lm(brozek ~ weight, data = fat)
weight = seq(118, 375, len = 100)
# construct confidence interval for mean response
ci <- predict(lms, new = data.frame(weight), interval = "confidence")
# construct prediction interval for new response
pi <- predict(lms, new = data.frame(weight), interval = "prediction")

# plot fitted model
plot(weight, ci[,1], ylab = "% fat", ylim = range(pi), type = "l")
# plot confidence bands
lines(weight, ci[,2], col = "orange") # lower conf. lim
lines(weight, ci[,3], col = "orange") # upper conf. lim
# plot prediction bands
lines(weight, pi[,2], col = "blue") # lower pred. lim
lines(weight, pi[,3], col = "blue") # upper pred. lim
# sample mean of weight variable
abline(v = mean(fat$weight))
# legend and title
legend("bottomright",
       legend = c("pi", "ci"),
       lty = 1, col = c("blue", "orange"))
title("pi vs ci")
