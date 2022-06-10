options(digits = 5, show.signif.stars = FALSE, scipen = TRUE) # improve look of output

### Backward selection with p-value using State Data
library(faraway)
data(state) # load data
# reformat data
statedata <- data.frame(state.x77, row.names = state.abb)

### Example 1
# fit full model
lmod <- lm(Life.Exp ~ ., data = statedata)
sumary(lmod) # determine least significant predictor
# perform backward elimination using update function on previous model
# use alpha_crit = 0.05
lmod <- update(lmod, . ~ . - Area)
sumary(lmod)
lmod <- update(lmod, . ~ . - Illiteracy)
sumary(lmod)
lmod <- update(lmod, . ~ . - Income)
sumary(lmod)
lmod <- update(lmod, . ~ . - Population)
sumary(lmod)

# compare R^2
lmodf <- lm(Life.Exp ~ ., data = statedata)
sumary(lmodf)

lmodr <- lm(Life.Exp ~ Murder + HS.Grad + Frost, data = statedata)
sumary(lmodr)

# an eliminated variable may still have a connection with the response
# depending on the other variables in the model
sumary(lm(Life.Exp ~ Illiteracy + Murder + Frost, statedata))

library(leaps)
# model selection by exhaustive search
b <- regsubsets(Life.Exp ~ ., data = statedata)
rs <- summary(b) # summarize model that minimizes RSS for each p
rs # best subset models (in terms of RSS)

### Example 2 (best subset w/ AIC)
p = 2:8 # number of coefficients
# calculate AIC of each model from the BIC
# subtract p*log(n) and add 2p
aic = rs$bic + p * (2 - log(50))
# plot AIC vs p
plot(aic ~ p)

# determine BIC of each model
library(car)
# plot BIC vs # of predictors
plot(rs$bic ~ p, ylab = "BIC")
subsets(b, statistic = "bic", legend = FALSE)

### Example 3 (best subset w/ Cp)
# construct Cp plot
cp = rs$cp
plot(cp ~ p, ylab = expression(paste(C[p], " statistic")))
abline(0, 1)
subsets(b, statistic = "cp", legend = FALSE)
abline(1, 1) # corresponds to 45 degree line offset by 1 unit vertically


### Example 4 (best subset w/ adj_rsq)
# Construct adjusted R^2 plot
adjr = rs$adjr
plot(adjr ~ p, ylab = expression({R^2}[a]))
subsets(b, statistic = "adjr2", legend = FALSE)

### Example 5 (stepwise selection w/ AIC)
# Stepwise selection function
lmod <- lm(Life.Exp ~ ., data = statedata)
step(lmod, direction = "both")

### Example 6 (compare two models using MSE via 10-fold and loo crossvalidation)
# crossvalidation example
library(caret)
# define training/test (control) data
cv_10fold = trainControl(method = "cv", number = 10) # 10-fold crossvalidation train/test data
cv_loo = trainControl(method = "LOOCV") # leave-one-out crossvalidation train/test data
cv_loo_slow = trainControl(method = "cv", number = 50) # loo crossvalidation train/test data

# train the full model
f1 = Life.Exp ~ . # formula for full model
# formula for reduced model with p = 5
f2 = Life.Exp ~ Population + Murder + HS.Grad + Frost
# formula for reduced model 2 with p = 4
f3 = Life.Exp ~ Murder + HS.Grad + Frost

# the train function needs:
# formula - to formula for the model you want to fit,
# data - the data frame where the variables are located
# trControl - the training/testing data sets created using the trainControl function
# method - the type of model you want to fit.  There are a lot of choices.  We simply need "lm"
modela = train(f1, data = statedata, trControl = cv_10fold,
               method = "lm")
modelb = train(f2, data = statedata, trControl = cv_10fold,
               method = "lm")
modelc = train(f3, data = statedata, trControl = cv_10fold,
               method = "lm")

# compare mse (rmse) for the two models using 10-fold cv
print(modela) # full, 10-fold
print(modelb) # reduced, 10-fold
print(modelc) # reduced 2, 10-fold

modeld = train(f1, data = statedata, trControl = cv_loo,
               method = "lm")
modele = train(f2, data = statedata, trControl = cv_loo,
               method = "lm")
modelf = train(f3, data = statedata, trControl = cv_loo,
               method = "lm")
# compare mse (rmse) for the two models using loo cv
print(modeld) # full, loo
print(modele) # reduced, loo
print(modelf) # reduced 2, loo

# slow loo cross-validation
modelg = train(f1, data = statedata, trControl = cv_loo_slow,
               method = "lm")

# test performance of the two loo methods
# library(microbenchmark)
loof = function() train(f1, data = statedata,
                     trControl = cv_loo, method = "lm")
loos = function() train(f1, data = statedata,
                     trControl = cv_loo_slow , method = "lm")
microbenchmark(loof(), loos(), times = 20)

### outliers and transformations affect model selection
# check for high leverage points
lmod <- lm(Life.Exp ~ ., data = statedata)
library(car)
influencePlot(lmod)

# excluding Alaska
b = regsubsets(Life.Exp ~., data = statedata,
               subset = (state.abb != "AK"))
rs = summary(b)
rs$which[which.max(rs$adjr), ]
# choose 5 regressor model instead of 4 regressor model from original data set (with Alaska)

# plot of scaled predictors
par(mfrow = c(1, 1))
stripchart(data.frame(scale(statedata)), vertical = TRUE,
           method = "jitter")

# model selection for transformed data
b <- regsubsets(Life.Exp ~ log(Population) + Income + Illiteracy +
                  Murder + HS.Grad + Frost + log(Area), statedata)
rs <- summary(b)
rs$which[which.max(rs$adjr),]
rs$adjr[which.max(rs$adjr)]