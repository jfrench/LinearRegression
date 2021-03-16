# options(width = 50, digits = 5)
options(digits = 5, scipen = 2)

### 7.2 Scaling
library(faraway)
data(savings)

# original fit
lm1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
sumary(lm1)

# scale predictor.  coefficient multiplied by scale change.
lm2 <- lm(sr ~ pop15 + pop75 + I(dpi/1000) + ddpi, data = savings)
sumary(lm2)

# scale response.  standard error and coefficients multiplied by scale.
lm3 <- lm(I(sr*1000) ~ pop15 + pop75 + dpi + ddpi, data = savings)
sumary(lm3)

# standardize savings variables
scsavings <- data.frame(scale(savings))
lm4 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = scsavings)
sumary(lm4)

# plot confidence intervals of estimated coefficients for scaled predictors
edf <- data.frame(coef(lm4),confint(lm4))[-1,]
names(edf) <- c('Estimate','lb','ub')
library(ggplot2)
p <- ggplot(aes(y=Estimate,ymin=lb,ymax=ub,x=row.names(edf)),data=edf) + geom_pointrange()
p + coord_flip() + xlab("Predictor") + geom_hline(yintercept=0, col=gray(0.75)) + theme_bw()

savings$age <- ifelse(savings$pop15 > 35, 0, 1)
savings$dpis <- (savings$dpi-mean(savings$dpi))/(2*sd(savings$dpi))
savings$ddpis <- (savings$ddpi - mean(savings$ddpi))/(2*sd(savings$ddpi))
sumary(lm(sr ~ age + dpis + ddpis, savings))

### 7.3 Collinearity

data(seatpos)

# fit model with all predictors
lm1 <- lm(hipcenter ~ ., data = seatpos)
sumary(lm1) # p-value for f test small, individual predictors not significant

# examine pairwise correlation of predictors
round(cor(seatpos), 3) # There are several large pairwise correlations between predictors and between predictors and the response.

# calculate the vifs
vif(lm1)

# condition indices of scaled X (with intercept
# but not centered)
library(perturb)
colldiag(lm1)

set.seed(1) # allows us to reproduce results
# add a little measurement error and see how coefficients change
lm2 <- lm(hipcenter + 10 * rnorm(38) ~ ., data = seatpos)
sumary(lm2)

# compare coefficients after
# noise added to response
library(car)
compareCoefs(lm1, lm2, se = FALSE)

# identifying problem predictors
# same diagnostic with variance decomposition proportions
# If a large condition index is associated
# two or more variables with large variance
# decomposition proportions, these variables may
# be causing collinearity problems.
# Belsley et al suggest that a large proportion
# is 50 percent or more.
# D. Belsley, E. Kuh, and R. Welsch (1980). Regression Diagnostics. Wiley.

colldiag(lm1)
# compute variance proportion manually
X = model.matrix(lm1)
X = scale(X, center = FALSE)
svdX <- svd(X)
Phi = svdX$v %*% diag(1/svdX$d)
Phi <- t(Phi^2)
round(prop.table(Phi, 2), 3)

# amputate some of the predictors
lm3 = update(lm1, . ~ . - HtShoes)
summary(lm3)
# recheck condition indices
colldiag(lm3)

lm4 = update(lm3, . ~ . - Seated)
summary(lm4)
# recheck condition indices
colldiag(lm4)

lm5 = update(lm4, . ~ . - Arm)
# recheck condition indices
colldiag(lm5)

lm6 = update(lm5, . ~ . - Leg)
summary(lm6)
# recheck condition indices
colldiag(lm6)

lm7 = update(lm6, . ~ . - Weight)
# recheck condition indices
colldiag(lm7)

lm8 = update(lm7, . ~ . - Thigh)
# recheck condition indices
colldiag(lm8)
summary(lm8)

