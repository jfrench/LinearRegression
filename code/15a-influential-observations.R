### Identifying unusual observations

#options(width = 50) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

### savings example
library(faraway) # load library
library(car)
data(savings, package = "faraway") # load data
lmod <- lm(sr ~ ., data = savings) #fit full model

### Identifying unusual observations
# extract leverage values
h <- hatvalues(lmod)

# half-normal plot of leverages
# get country name for each observation
# useful for labeling leverage points
countries <- row.names(savings)
halfnorm(h, nlab = 2, labs = countries, ylab = "leverage")

# index plot of leverage values
infIndexPlot(lmod, vars = "hat")

### Identifying outliers
# obtain studentized residuals
stud <- rstudent(lmod)
# largest magnitude studentized residual
max(abs(stud))
# since we are doing a two-sided test, we need the
# 1 - alpha/2n quantile not 1-alpha/n.
# df = 50 - 5 - 1
qt(1 - .05/(50*2), df = 44)

# perform outler check using Bonferroni correction
outlierTest(lmod)

# index plots of studentized residuals and Bonferroni p-values
infIndexPlot(lmod, vars = c("Studentized", "Bonf"))

# star example
data(star, package = "faraway")
par(mfrow = c(1, 1))
plot(light ~ temp, data = star, xlab = "log(Temperature)",
     ylab = "log(Light Intensity)")
lm1 <- lm(light ~ temp, data = star)
lm2 <- lm(light ~ temp, data = star, subset = (temp > 3.6))
abline(lm1)
abline(lm2, lty = 2)
legend("bottomleft", c("all", "non-outliers"), lty = c(1, 2))

### Identifying influential observations
# get cooks statistics for savings data
cook <- cooks.distance(lmod)
halfnorm(cook, n = 3, labs = countries, ylab = "Cook's distances")

infIndexPlot(lmod, var = "Cook", id = list(n = 3))
plot(lmod, which = 4)

# refit model after removing Libya
lmod2 <- lm(sr ~ ., data = savings, subset = (countries != "Libya"))
summary(lmod2)

# see the coefficients for the
# two models side-by-side
compareCoefs(lmod, lmod2)

# index plot of dfbetas
dfbetasPlots(lmod, id.n = 3, lab = countries)

# influence plot of model
influencePlot(lmod)
plot(lmod, which = 5)
