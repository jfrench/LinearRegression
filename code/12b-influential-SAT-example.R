library(faraway)
library(car)
data(sat)

# fit model
lmod = lm(total ~ expend + salary + ratio + takers,
          data = sat)
# names of each observation
names = row.names(sat)

# identify leverage points
h = hatvalues(lmod)
halfnorm(h, labs = names)
infIndexPlot(lmod, vars = "Hat")

# identify outliers
outlierTest(lmod)

# identify influential points
d = cooks.distance(lmod)
halfnorm(d, nlab = 1, labs = names)
infIndexPlot(lmod, vars = "Cook")

# is Utah influential
lmod2 = lm(total ~ expend + salary + ratio + takers,
           data = sat,
           subset = (names != "Utah"))
compareCoefs(lmod, lmod2)
confint(lmod)

influencePlot(lmod)

# f test for a regression relationship
nullmod = lm(total ~ 1, data = sat)
anova(nullmod, lmod)

# confidence interval for beta_takes
confint(lmod)
