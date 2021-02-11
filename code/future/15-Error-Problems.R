options(width = 50, digits = 5) # for nice R formatting at 20 pts

### 8.1

library(faraway)
data(globwarm, package="faraway") #load global warming data

# regress northern hemisphere temperature on eight proxy variables
lmod <- lm(nhtemp ~ wusa + jasper + westgreen + chesapeake +
           tornetrask + urals + mongolia + tasman, 
           data = globwarm)

sumary(lmod) # summarize model fit

n = nobs(lmod) # number of observations in data
# quantify correlation of succesive residuals
cor(residuals(lmod)[-1], residuals(lmod)[-n])

library(nlme) # load nlme package
# fit AR1 model to the data
glmod <- gls(nhtemp ~ wusa + jasper + westgreen + 
             chesapeake + tornetrask + urals + mongolia + 
             tasman,  correlation = corAR1(form = ~year),  
             data = na.omit(globwarm))
summary(glmod) # summarize results of AR1 model

# confidence interval for correlation parameter
intervals(glmod, which="var-cov")

# view oat data
head(oatvar)
# fit compound symmetry model to the oat data
glmod <- gls(yield ~ variety, data = oatvar, 
             correlation = corCompSymm(form = ~1 | block))
summary(glmod) # look at results
# confidence interval for correlation parameter
intervals(glmod)
