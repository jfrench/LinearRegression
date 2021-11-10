library(faraway)
library(car)
data(sat)

# fit model
lmod <- lm(total ~ expend + salary + ratio + takers, data = sat)

residualPlot(lmod, quadratic = FALSE)
residualPlots(lmod)
marginalModelPlots(lmod)
avPlots(lmod)
crPlots(lmod)
ceresPlots(lmod)

# try a transformation
newmod <- lm(total ~ expend + salary + ratio + poly(takers, 2), data = sat)
residualPlot(newmod, quadratic = FALSE)
crPlots(newmod)
