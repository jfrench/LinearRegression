options(digits = 5, scipen = 2, show.signif.stars = FALSE)

# diabetes example
#
data(diabetes, package = "faraway")

# fit model
fm <- chol ~ stab.glu + hdl + glyhb + age + gender + height + weight + waist + hip
lmod <- lm(fm, data = diabetes)

# summarize results
summary(lmod)

# fitted values
fitted(lmod)
fitted(lmod)[3]
lmod$fitted.values[3]

yhat <- fitted(lmod)

# residual plot
plot(residuals(lmod) ~ fitted(lmod))
abline(0, 0, lty = 2)

# y vs yhat
plot(lmod$model$chol ~ fitted(lmod), xlab = "yhat", ylab = "y")
abline(lm(lmod$model$chol ~ fitted(lmod)))
summary(lmod)$r.squared

# orthogonality
cov(model.matrix(lmod))
colSums(model.matrix(lmod))
