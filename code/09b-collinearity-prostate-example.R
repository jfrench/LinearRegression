data(prostate, package = "faraway")

library(car)
# scatterplot matrix of all variables
scatterplotMatrix(prostate)

# fit model with all predictors
lmod = lm(lpsa ~ ., data = prostate)

# extract X matrix from model
x = model.matrix(lmod)
x = x[,-1] # remove intercept

# correlation matrix
round(cor(x), 2)

# variance inflation factor
vif(lmod)

# condition numbers
library(perturb)
colldiag(lmod)

# condition index 9 has high variance
# decomposition proportions (vdp) for gleason
lmod2 = update(lmod, . ~ . - gleason)
colldiag(lmod2)

# perhaps remove age