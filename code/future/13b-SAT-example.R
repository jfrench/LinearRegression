library(faraway)
library(car)
data(sat)

# fit model
lmod <- lm(total ~ expend + salary + ratio + takers, data = sat)

# check constant variance
# plot residuals vs fitted values.  No clear problems.  Maybe lower in middle?
# fitted values vs regressors
residualPlot(lmod, quadratic = FALSE)

# fitted values vs regressors
residualPlots(lmod, fitted = FALSE, tests = FALSE, quadratic = FALSE)

# check normality assumption
# no major evidence of a problem
qqPlot(residuals(lmod))
