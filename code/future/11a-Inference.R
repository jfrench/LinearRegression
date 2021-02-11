### Based off Chapter 5 of LMWR2

#options(width = 50) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

# read Galapagos data
data(gala, package = "faraway")

### 3.2
### Test whether any of the (non-constant) predictors should be in the model
# Fit full model
lmod = lm(Species ~ Area + Elevation + Nearest + 
            Scruz + Adjacent, data = gala)
# Fit null model
nullmod = lm(Species ~ 1, data = gala)
# F test for all (non-constant) predicts
anova(nullmod, lmod)

# manually verify results
(rss0 = deviance(nullmod)) # pulls out rss
(rss = deviance(lmod)) # pulls out rss
(df0 = df.residual(nullmod)) # pulls out residual df
(df = df.residual(lmod)) # pulls out residual df
(f = ((rss0 - rss)/(df0 - df))/(rss/df))
1 - pf(f, df1 = df0 - df, df2 = df)

# Test statistic and p-value available from summary function
library(faraway) # for sumary function
sumary(lmod)

### Test whether regression coefficient for Area is significant

# fit reduced model (full without Area)
lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala) 
anova(lmods, lmod)

# notice p-values are the same and t1^2 = F
summary(lmod)

# compare t1^2 and F
summary(lmod)$coeff[2,3]^2
anova(lmods, lmod)$F[2]

# compare results to test of beta_Area = 0 when no other predictors in model
summary(lm(Species ~ Area, gala))

# Test whether regression coefficients Area and Adjacent should simultaneously
# be included, assuming Elevation, Nearest and Scruz are in our model

# Determine full and reduced models
lmods <- lm(Species ~ Elevation + Nearest + Scruz, data = gala) # fit reduced model
anova(lmods, lmod) # compare models using general f-test

### Test whether regression coefficients for Area and Adjacent are equal to,
### assuming other predictors in model

# Fit linear subspace model
lmods <- lm(Species ~ I(Adjacent + Area) + Elevation + Nearest + Scruz, gala) 
# The I() means evaulate this function before creating the model.
#  This allows us to create transformed predictors in the model statement.
anova(lmods, lmod) # compare models using general f-test

### Test whether beta_Area = 0.5 assuming other predictors are in the model

# Fit reduced model
lmods <- lm(Species ~ Area + offset(0.5*Elevation) + Nearest + Scruz + Adjacent, gala) 
# the offset term indicates that this term is a constant and not to be estimated.
anova(lmods, lmod) # compare models using general f-test

# Same test using t test
(tstat <- (coef(lmod)[3] - 0.5)/sqrt(vcov(lmod)[3,3])) # test statistic
2 * (1 - pt(abs(tstat), df = df.residual(lmod))) # p-value
tstat^2

### 3.3 Permutation tests
### Test whether Nearest and Scruz predictors needed
lmod <- lm(Species ~ Nearest + Scruz, data = gala)
lms <- summary(lmod)

# f statistic and p-value
# Test statistic available from summary function
fobs <- lms$fstatistic[1]
1 - pf(lms$fstatistic[1], lms$fstatistic[2], lms$fstatistic[3])

# Randomly sample responses (4000 times), recompute model and fstatistic
nreps <- 4000
set.seed(123) # reproducible results
fstats <- numeric(nreps) # to store permuted test statistics
for (i in 1:nreps) {
  lmodp <- lm(sample(Species) ~ Nearest + Scruz, gala) # permute response
  # and then regress permuted response on Nearest and Scruz
  lmodps <- summary(lmodp) # summarize fit from lmodp
  # extract the fstatistic from the summary of lmodp
  fstats[i] <- lmodps$fstatistic[1]
}

# compute p-value (the proportion of simulated test statistics at least as extreme
# as our observed test statistics).
mean(fstats >= fobs)

# compare statistics for permuted data to observed statistic
plot(density(fstats), xlim = c(0, 10))
abline(v = fobs)

### Test test whether the Scruz predictor should be in the model when Nearest is. 
# Test statistic available from summary function
tobs <- lms$coef[3,3]

# Randomly sample Scruz (4000 times), recompute model and tstatistic
nreps <- 4000
tsim <- numeric(nreps)
set.seed(123) # reproducability
for (i in 1:nreps) {
  # fit model with permuted Scruz
  lmodp = lm(Species ~ Nearest + sample(Scruz), gala)
  lmodps = summary(lmodp) # summarize results
  # extract the t statistic for the permuted data
  tsim[i] <- lmodps$coef[3,3]
}

# visual comparison of test statistics from permuted data to observed data
hist(tsim, freq = FALSE)
abline(v = tobs)

# compute p-value (the proportion of simulated test statistics at least as extreme
# as our observed test statistics).
mean(abs(tsim) >= abs(tobs))

#### 3.5 Confidence Intervals for Beta
# Fit model
lmod = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
sumary(lmod)
qt(.975, df = df.residual(lmod))
# manually construct interval
-.02394 + c(-1, 1) * 2.0639 * .02242

# construct 95% confidence intervals of all parameters
confint(lmod, level = 0.95)

# construct 95% joint confidence intervals for beta_area and beta_adjancent.
library(car)
confidenceEllipse(lmod, which.coef = c(2, 6), ylim = c(-0.13, 0), levels = 0.95)
# add origin to plot
points(0, 0)
# add vertical and horizontal lines for individual confidence intervals
abline(v = confint(lmod)[2,], lty = 2) # plots vertical lines
abline(h = confint(lmod)[6,], lty = 2) # plots horizontal lines

#### 3.6 Bootstrap confidence intervals

# bootstrap CIs for Galapagos data
set.seed(123)
nb = 4000 # number of bootstrap samples
coefmat = matrix(0, nb, 6)
resids = residuals(lmod) #extract residuals
preds = fitted(lmod) # fitted values
for (i in 1:nb) {
  booty <- preds + sample(resids, replace = TRUE) # create bootstrap data
  # fit regression model to bootstrap data
  bmod <- lm(booty ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
  coefmat[i,] = coef(bmod) # extract estimated coefficients from bmod and store them for later
}
# construct intervals
colnames(coefmat) = c("Intercept", colnames(gala[,3:7])) # rename columns of coefmat
coefmat <- data.frame(coefmat) # convert to data frame
cis = apply(coefmat, 2, quantile, probs = c(.025, .975)) # construct 95% CIs for each coefficients using the apply function
cis

# plot density for bootstrap coefficients for Area
# along with the 95% bootstrap CI for Area coefficient
plot(density(coefmat$Area), xlab = "Area", main = "") # plot density
title("Bootstrap distribution for betahat_Area") #title
abline(v = c(-.0628, .0185), lty = 2) # plot ci

# same thing for Adjacent
plot(density(coefmat$Adjacent), xlab = "Adjacent", main = "") # plot density
title("Bootstrap distribution for betahat_Adjacent") #title
abline(v = c(-.104, -.041), lty = 2) # plot ci

library(ggplot2) # same plots using ggplot2
# x = Area means that we are only doing univariate plot
# geom_density maps the variables values to the density geometry
# geom_vline adds vertical lines at the specified values
# theme_classic makes the plot look more like base R graphics
ggplot(coefmat, aes(x = Area)) + geom_density() + geom_vline(xintercept = c(-.0628, .0185), lty = 2) + theme_bw()
ggplot(coefmat, aes(x = Adjacent)) + geom_density() + geom_vline(xintercept = c(-.104, -.0409), lty = 2) + theme_bw()

# just messing around with themes in ggplot2
library(ggthemes)
p = ggplot(coefmat, aes(x = Adjacent)) + geom_density() + geom_vline(xintercept = c(-.104, -.0409), lty = 2)
# base R
p + theme_base()
# wall street journal
p + theme_wsj()
# edward tufte style
p + theme_tufte()
# stata style
p + theme_stata()
# excel style
p + theme_excel()
# google docs style
p + theme_gdocs()
# fivethirtyeight.com style
p + theme_fivethirtyeight()

### bootstrap cis for all parameters simultaneously
# create tall data frame for bootstrap estimates of coefficients
coefdf = data.frame(parameter = rep(names(coefmat), each = nb), estimates = unlist(coefmat))
# create tall data frame for bootstrap cis
cidf = data.frame(parameter = rep(colnames(cis), each = 2), cis = c(cis))

ggplot(coefdf, aes(x = estimates)) + # create ggplot object with estimates as x-variable
  geom_density() + # map estimates variable to density
  facet_wrap(~ parameter, scales = "free") + # create a diferent density for each parameter
  geom_vline(data = cidf, aes(xintercept=cis)) + # plot vertical lines for each bootstrap ci
  theme_bw() # simpler theme
