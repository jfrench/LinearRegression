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

# condition indices
library(perturb)

# condition index 9 has high variance
# decomposition proportions (vdp) for gleason
colldiag(lmod, scale = TRUE, add.intercept = TRUE)

# remove gleason from model
lmod2 = update(lmod, . ~ . - gleason)

# condition index 9 has high variance
# decomposition proportions (vdp) for age
colldiag(lmod2, scale = TRUE, add.intercept = TRUE)


# remove age from model
lmod3 = update(lmod2, . ~ . - age)
colldiag(lmod3)

