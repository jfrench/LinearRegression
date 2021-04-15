#options(width = 45) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

library(car)

# load data
data(savings, package = "faraway")

# fit model
lmod = lm(sr ~ ., data = savings)

# examine residual plots
residualPlots(lmod)

# examine marginal model plot
marginalModelPlots(lmod)

# examine added variable plots
avPlots(lmod)

# examine component plus residual plots
crPlots(lmod)

lmod2 = lm(sr ~ pop15 + pop75 + dpi + sqrt(ddpi), data = savings)
crPlots(lmod2)

residualPlots(lmod2)
