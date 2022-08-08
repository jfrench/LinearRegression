# options(width = 50, strict.width = TRUE) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

### 2.2 of ALR4
### Simple Linear Regression example
### Forbes Data
data(Forbes, package = "alr4") # load Forbes data
lmod = lm(lpres ~ bp, data = Forbes) # fit model
lmod # look at coefficients

# extract response and regressor from Forbes data frame
y = Forbes$lpres
x = Forbes$bp
n = length(y)
(beta1hat = (sum(x*y) - sum(x)*sum(y)/n)/(sum(x^2) - sum(x)^2/n))
(beta0hat = mean(y) - beta1hat * mean(x))

# plot lpres vs bp, changing symbol for points using pch
plot(lpres ~ bp, data = Forbes, pch = 19,
     xlab = "boiling point (degrees F)",
     ylab = "lpres")
# add fitted line to plot, change line type using lty
abline(lmod, lty = 2)

# using lattice
library(lattice)
xyplot(lpres ~ bp, data = Forbes, pch = 20,
     xlab = "boiling point (degrees F)",
     ylab = "lpres", type = c("p", "r"))

# using ggplot2
library(ggplot2)
ggplot(Forbes, aes(x = bp, y = lpres)) +
  geom_point() + # add points to plot
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + # add least-squares fit
  xlab("boiling point (degrees F)") + # change x-label
  theme_bw() # change theme

### All other code follows the sections in LMWR2
### 2.6 Gala Example

# load Galapagos data from faraway package
data(gala, package = "faraway")
str(gala) # basic structure of gala data

# Fit linear model using R.
# Must use data = gala since data vectors are in gala data.frame
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
faraway::sumary(lmod) # the author's modified version of summary, which
             # produces too much information
# extract least-squares coefficients from lmod
coef(lmod)

# construct predictor matrix (intercept is automatically added)
x <- model.matrix( ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
y <- gala$Species

# estimated coefficients
solve(t(x) %*% x, t(x) %*% y)
# slightly better
solve(crossprod(x), crossprod(x, y))

# quantities we can extract from model object
names(lmod)
#quantities we can extract summary object
lmodsum <- summary(lmod)
names(lmodsum)

# estimate sigma
sqrt(deviance(lmod)/df.residual(lmod))
lmodsum$sigma
sqrt(sum(residuals(lmod)^2)/df.residual(lmod))
sigma(lmod)

# estimated ses of estimated coefficients
lmodsum$coef[,2]

# vcov provides the estimated variance/covariance
# matrix of the regression coefficients
vcov(lmod)

# estimated standard errors of estimated regression coefficients
sqrt(diag(vcov(lmod)))

# extract fitted values from lmod
yhat <- fitted(lmod)

# extract residuals from lmod
e <- residuals(lmod)

# extract rss from lmod
deviance(lmod)

# calculate rss using matrix notation
t(e) %*% e

# 2.9 Goodness of fit
# compare r-squared for various data sets

# Calculate fitted model and coefficient of determination for various data sets
# four data sets with estimated coefficients and r-squared
data(anscombe) # load Anscombe's quartet
# fit linear models for each data set
lmod1 = lm(y1 ~ x1, data = anscombe)
lmod2 = lm(y2 ~ x2, data = anscombe)
lmod3 = lm(y3 ~ x3, data = anscombe)
lmod4 = lm(y4 ~ x4, data = anscombe)
# summarize each model
lmod1sum = summary(lmod1)
lmod2sum = summary(lmod2)
lmod3sum = summary(lmod3)
lmod4sum = summary(lmod4)
# print coefficients for each model
coef(lmod1); coef(lmod2); coef(lmod3); coef(lmod4)
# print r-squared for each model
lmod1sum$r.squared; lmod2sum$r.squared; lmod3sum$r.squared; lmod4sum$r.squared;

# compare fits to data
# base graphics
par(mfrow = c(2, 2))
plot(y1 ~ x1, data = anscombe)
abline(lmod1)
plot(y2 ~ x2, data = anscombe)
abline(lmod2)
plot(y3 ~ x3, data = anscombe)
abline(lmod3)
plot(y4 ~ x4, data = anscombe)
abline(lmod4)

# ggplot2 graphics
library(ggplot2)
adf = data.frame(x = unlist(anscombe[,1:4]),
                 y = unlist(anscombe[,5:8]),
                 set = factor(rep(1:4, each = 11)))
ggplot(adf, aes(x = x, y = y, group = set, col = set)) +
  geom_point(col = "black") +
  geom_smooth(aes(group = set), method = "lm", se = FALSE) +
  facet_grid(. ~ set) +
  theme_bw()

# lattice graphics
library(lattice)
xyplot(y ~ x | set, data = adf, type = c("p", "r"))

# 2.10 Identifiability
# unidentifiable
gala$Adiff <- gala$Area - gala$Adjacent
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz +
             Adjacent + Adiff, gala)
summary(lmod)

# Almost undentifiable
set.seed(123)
Adiffe <- gala$Adiff + 0.001*(runif(30) - 0.5)
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz +
             Adjacent + Adiffe, gala)
sumary(lmod)

#2.11 Orthogonality
data(odor, package = "faraway")
head(odor) #first six rows
# extract x matrix
x <- model.matrix(~ temp + gas + pack, data = odor)
crossprod(x) # matrix of crossproducts

# since the regressors are centered, we can also consider the covariance
cov(x) #covariance of regressors (excluding response)
colSums(x) # intercept is orthogonal to predictors since their sums are zero

# fit model with all three predictors
lmod <- lm(odor ~ temp + gas + pack, odor)
sumary(lmod)
# refit model without temp predictor
lmod <- lm(odor ~ gas + pack, odor)
sumary(lmod) #coefficients same, std errors not

