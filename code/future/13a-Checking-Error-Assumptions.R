# Checking error assumptions

#options(width = 50) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)
library(car)

### Checking mean-zero and nonconstant variance

## savings example
data(savings, package = "faraway") # load data
lmod = lm(sr ~ ., data = savings) #fit full model

# plot of residuals versus fitted values
residualPlot(lmod, quadratic = FALSE)

# plot of residuals versus fitted values
plot(lmod, which = 1)

# plot of residuals versus predictors
residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# plot sqrt absolute residuals vs fitted values
plot(lmod, which = 3)

### Checking normality of residuals
# calibrating q-q plots
par(mfrow = c(2, 2))
set.seed(53)
qqPlot(rnorm(50), ylab = "observed data",
       xlab = "normal quantiles", main = "normal data")
qqPlot(exp(rnorm(50)), ylab = "observed data",
       xlab = "normal quantiles", main = "positively-skewed data") 
qqPlot(rcauchy(50), ylab = "observed data",
       xlab = "normal quantiles", main = "heavy-tailed data")
qqPlot(runif(50), ylab = "observed data",
       xlab = "normal quantiles", main = "light-tailed data")
par(mfrow = c(1, 1))

# better q-q plot of studentized residuals with confidence bands
qqPlot(lmod)

# q-q plot of savings residuals
plot(lmod, which = 2)

# assess normality of errors using shapiro-wilk test for savings data
shapiro.test(residuals(lmod))

### Checking autocorrelation among the residuals
data(globwarm, package = "faraway")

# fit model
lmod = lm(nhtemp ~ wusa + jasper + westgreen + chesapeake +
          tornetrask + urals + mongolia + tasman, data = globwarm)

# residual vs time
plot(residuals(lmod) ~ year, data = na.omit(globwarm), 
     ylab = "residuals")
abline(h = 0)

# plot successive pairs of residuals
n = nobs(lmod)
plot(tail(residuals(lmod), n - 1) ~ head(residuals(lmod), n - 1), 
     xlab = expression(hat(epsilon)[i]), 
     ylab = expression(hat(epsilon)[i + 1]))
abline(h = 0, v = 0, col = grey(0.75))

library(lmtest)
dwtest(nhtemp ~ wusa + jasper + westgreen + chesapeake +
       tornetrask + urals + mongolia + tasman, data = globwarm)


