### Primarily based off of Chapter 4 of ALR4

# options(width = 50) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

### 4.1 Understanding Parameter Estimates
data(fuel2001, package = "alr4")
# create new regressors/transformed responses to fuel2001 data frame
fuel2001$Fuel <- 1000*fuel2001$FuelC/fuel2001$Pop
fuel2001$Dlic <- 1000*fuel2001$Drivers/fuel2001$Pop
fuel2001$Income1K <- fuel2001$Income/1000

# fit model
lmod <- lm(Fuel ~ Tax + Dlic + Income1K + log(Miles), data = fuel2001)
# summarize model
faraway::sumary(lmod)

# sample means of regressors
colMeans(fuel2001)
# sample mean of log(Miles)
mean(log(fuel2001$Miles))

library(effects) # for Effect function
# effect plot for Tax regressor
plot(Effect("Tax", lmod))

### 4.3 Interpretation Depends on Other Terms in the Mean Function

# load Berkeley growth data, create new regressors
data(BGSgirls, package = "alr4")
head(BGSgirls)

# plot four variables
BGSgirls$DW9 <- BGSgirls$WT9-BGSgirls$WT2
BGSgirls$DW18 <- BGSgirls$WT18-BGSgirls$WT9
BGSgirls$DW218 <- BGSgirls$WT18-BGSgirls$WT2

# basic scatterplot matrix
pairs(~ BMI18 + WT2 + WT9 + WT18, data = BGSgirls)
pairs(~ BMI18 + DW9 + DW18, data = BGSgirls)

# fit 3 different models, summarize results
m1 <- lm(BMI18 ~ WT2 + WT9 + WT18, BGSgirls)
summary(m1)

m2 <- lm(BMI18 ~ WT2 + DW9 + DW18, BGSgirls)
summary(m2)

m3 <- lm(BMI18 ~ WT2 + WT9 + WT18 + DW9 + DW18, BGSgirls)
summary(m3)

coef(m1)
coef(m2)
coef(m3)

### 4.1.6 Regressors in Logarithmic Scale
# effects plot on log scale
plot(Effect("Miles", lmod),
     transform.x=list(Miles=c(trans=log, inverse=exp)),
     ticks.x=list(at=round(exp(7:13))), main = "log(Miles) effect plot")
# effects plot on original sale
plot(Effect("Miles", lmod, xlevels = list(Miles = seq(1, 3e5, len = 301))))

# The call to Effect is the same, but additional arguments are needed in the call to plot:
# transform.x tells to transform Miles on the horizontal axis to its logarithm; you must also specify the inverse of the log-function. # Finally, you need to tell the function where to draw the tick marks. Since log(Miles) is between about 7 and 13, the ticks are found by exponentiating these values and then rounding them.
# The argument rotx=45 rotates the labels on the horizontal axis by 45 degrees so the numbers do not overlap.
# xlevels indicates the values of x for which to draw the effects line
# See ?plot.eff to see all the arguments.

# Fuel model on log10 scale for Miles
lm(Fuel ~ Tax + Dlic + Income1K + log10(Miles), data = fuel2001)
