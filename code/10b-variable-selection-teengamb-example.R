options(digits = 5, show.signif.stars = FALSE, scipen = TRUE) # improve look of output

### 8.2 Testing-based methods
library(faraway)
data(teengamb) # load data

# fit full model
lmod <- lm(gamble ~ ., data = teengamb)
sumary(lmod) # determine least significant predictor

# perform backward elimination using update function on previous model
# use alpha_crit = 0.05
lmod <- update(lmod, . ~ . - status)
sumary(lmod)
lmod <- update(lmod, . ~ . - verbal)
sumary(lmod)

# model selection in terms of AIC
library(leaps)
# model selection by exhaustive search
b <- regsubsets(gamble ~ ., data = teengamb)
rs <- summary(b) # summarize model that minimizes RSS for each p
rs$which # best subset models (in terms of RSS)

# calculate AIC of each model
n = nobs(lmod)
aic <-  n * log(rs$rss/n) + n * log(2 * pi) + n + 2 * (2:5)
# easier, but differs by a constant (will have same results)
aic2 <- rs$bic + (2 - log(n)) * 2:5
# plot AIC vs p
plot(2:5, aic, xlab = "p", ylab = "AIC")

# calculate BIC of each model
# plot BIC vs p
library(car)
subsets(b, statistic = "bic", legend = FALSE)

# Construct Cp plot
subsets(b, statistic = "cp", legend = FALSE)
abline(1, 1)

# Construct adjusted R^2 plot
subsets(b, statistic = "adjr2", legend = FALSE)

# backward elimination
lmod = lm(gamble ~ ., data = teengamb)
step(lmod) #with AIC
step(lmod, k = log(n)) # with BIC

# p = 3 or 4 models appear to be best
# use CV on these models
plot(b)

library(caret)
f1 = gamble ~ sex + income
f2 = gamble ~ sex + verbal + income

cv_5fold = trainControl(method = "cv", number = 5) # 5-fold crossvalidation train/test data
modela = train(f1, data = teengamb, trControl = cv_5fold,
               method = "lm")
modelb = train(f2, data = teengamb, trControl = cv_5fold,
               method = "lm")

# compare mse (rmse) for the two models using 5-fold cv
print(modela) # p = 3
print(modelb) # p = 4

# we prefer the model with smaller RMSE and MAE
# this can switch depending on the random 5-fold data set selected

# trying an interaction model
f3 = gamble ~ sex*income
modelc = train(f3, data = teengamb, trControl = cv_5fold,
               method = "lm")
print(modelc) # even better
library(car)
residualPlots(lm(f3, data = teengamb))

# try a transformed model
# trying another model
f4 = sqrt(gamble) ~ sex*income
modeld = train(f4, data = teengamb, trControl = cv_5fold,
               method = "lm")
print(modeld)
residualPlots(lm(f4, data = teengamb))

# not comparable to previous model since the response is
# transformed.  Should really go through variable selection
# process again.

