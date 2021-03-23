#options(width = 50) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

### Example from ALR4
# 5.1.1 UN11 Data
library(alr4)
data(UN11)

### One-way model
library(car)
# explore one-way data
Boxplot(lifeExpF ~ group, data = UN11) # easily identify outliers of boxplot

## creating dummy variables
# with functions means: using this data frame, perform the following actions ...
# we determine whether the value of group equal to ith level of group, add 0 to convert TRUE/FALSE to 1/0
levels(UN11$group)
U1 <- with(UN11, (group == levels(group)[1]) + 0)
U2 <- with(UN11, (group == levels(group)[2]) + 0)
U3 <- with(UN11, (group == levels(group)[3]) + 0)
head(data.frame(group = UN11$group, U1, U2, U3), 10)

(UN11$group == "oecd") + 0
levels(UN11$group)

# add dummy variables to data frame
UN11$U1 = U1
UN11$U2 = U2
UN11$U3 = U3

# fit overparameterized model
lm(lifeExpF ~ U1 + U2 + U3, data = UN11)

# fit proper model
lm(lifeExpF ~ U2 + U3, data = UN11)
lm(lifeExpF ~ group, data = UN11)

# do f test for whether effects differ
summary(lm(lifeExpF ~ group, data = UN11))

# calculate average lifeExpF for each level of group
# tapply applies a function (mean in this case)
# to the lifeExpF values for each level group
with(UN11, tapply(lifeExpF, group, mean))
# or using the dplyr package
library(dplyr)
# on the UN11 package, group each set of observations by group,
# summarize the sample_mean by calculating the mean of lifeExpF
UN11 %>% group_by(group) %>% summarize(sample_mean = mean(lifeExpF))

# fit one-way model
lmod <- lm(lifeExpF ~ group, data = UN11)

# effects plot of one-way model
plot(allEffects(lmod, default.levels=50), ylim=c(60, 85),
     grid=TRUE, multiline=TRUE)

# plot fitted lines for each group for one-way model
# using ggplot2
mean_lifeExpF = tapply(UN11$lifeExpF, UN11$group, mean) # compute mean lifeExpF for each group
# create data frame with means for each group
mean_df = data.frame(group = names(mean_lifeExpF), mean = mean_lifeExpF)
library(ggplot2)
ggplot(UN11, aes(x = log(ppgdp), y = lifeExpF, col = group)) + geom_point() + geom_hline(aes(yintercept = mean, col = group), mean_df) + theme_bw() + theme(legend.position="top")
ggplot(UN11, aes(x = log(ppgdp), y = lifeExpF)) + geom_point() + facet_wrap(~ group)

# using base graphics
grp_col = as.numeric(UN11$group)
# plot data, using different colors/symbols for each group
plot(lifeExpF ~ log(ppgdp), data = UN11, col = grp_col, pch = grp_col)
# fitted lines for each group, changing color for each group.  Make sure to match the color number with color number used in plot!
abline(h = mean_lifeExpF, col = 1:3)
# create the left
# first argument is position
# legend = ... specifies the names of each object you wan to label
# col is the color of each object (make sure to match things)
# pch is the symbol
# lty is linetype (default is 1)
legend("topleft", legend = c("oecd", "other", "africa"), col = 1:3, pch = 1:3, lty = 1)

# confidence interval for difference
# in mean for oepc and other
confint(lmod, "groupother")

# confidence interval for difference
# in mean for oepc and africa
confint(lmod, "groupafrica")

# confidence interval for difference in mean for other and africa
a = c(0, 1, -1)
diff_23 = t(a) %*% coef(lmod)
se_diff_23 = sqrt(t(a) %*% vcov(lmod) %*% a)
diff_23 + c(-1, 1) * qt(.975, df.residual(lmod)) * se_diff_23

# produce Tukey's HSD intervals
(thsd = TukeyHSD(aov(lmod)))

# plot Tukey's HSD intervals
plot(thsd)

# manually produce different between other and africa
# estimated difference +/- tukey multipler * sehat(estimated difference)
qmult <- qtukey(.95, 3, df.residual(lmod))/sqrt(2)
coef(lmod)[2] + c(-1, 1) * qmult * sqrt(diag(vcov(lmod))[2])
coef(lmod)[3] + c(-1, 1) * qmult * sqrt(diag(vcov(lmod))[3])
-diff_23 + c(-1, 1) * qmult * se_diff_23

### Categorical and quantitative predictors
ggplot(UN11, aes(y = lifeExpF, x = log(ppgdp), col = group, pch = group)) + geom_point() + theme_bw()

library(lattice)
xyplot(lifeExpF ~ log(ppgdp), data = UN11, group = group, auto.key = TRUE)

# using base graphics
# create different colors/symbols for each group
grp_col = as.numeric(UN11$group)
# plot data, using different colors/symbols for each group
plot(lifeExpF ~ log(ppgdp), data = UN11, col = grp_col, pch = grp_col)
# create the legend
legend("topleft", legend = c("oecd", "other", "africa"), col = 1:3, pch = 1:3)

## Fit interaction model
library(faraway)
lmodi = lm(lifeExpF ~ log(ppgdp)*group, data = UN11)
coef(lmodi) # coefficients

# X matrix for model
head(model.matrix(lmodi), 10)

# plot separate lines/interaction model
# using ggplot2
ggplot(UN11, aes(x = log(ppgdp), y = lifeExpF, col = group)) + geom_point() + geom_smooth(method = "lm") + theme_bw() + theme(legend.position="top")
# using lattice
xyplot(lifeExpF ~ log(ppgdp), data = UN11, group = group, type = c("p", "r"), auto.key = TRUE)
# using base graphics
# using base graphics
# create different colors/symbols for each group
grp_col = as.numeric(UN11$group)
# plot data, using different colors/symbols for each group
plot(lifeExpF ~ log(ppgdp), data = UN11, col = grp_col, pch = grp_col)
# fitted lines for each group, changing color for each group.  Make sure to match the color number with color number used in plot!
abline(59.21366, 2.24254, col = 1)
abline(59.21366 - 11.17310, 2.24254 + 0.92944, col = 2)
abline(59.21366 - 22.98484, 2.24254 + 1.09498, col = 3)
# create the legend
legend("topleft", legend = c("oecd", "other", "africa"), col = 1:3, pch = 1:3, lty = 1)

# effect plot for log(ppgdp) by group
ppgdpEffect = Effect(c("ppgdp", "group"),
                     mod = lmodi,
                     xlevels = list(ppgdp = seq(1, 106000, len = 1000)))
# effect plot of ppgdp on original scale,
# all line on same graph (multiline = TRUE)
plot(ppgdpEffect,
     multiline = TRUE)

# effect plot of ppgdp on log scale,
# all line on same graph (multiline = TRUE)
# transform.x is used to plot the x-axis
# on the exp scale
# ticks.x is used to decide placement of ticks
plot(ppgdpEffect,
     rug=FALSE, grid=TRUE, multiline=TRUE,
     transform.x=list(ppgdp=list(trans=log, inverse=exp)),
     ticks.x =list(ppgdp = list(at= c(100, 1000, 5000, 30000))))

## Main effects model
# fit main effects model
lmodm = lm(lifeExpF ~ log(ppgdp) + group, data = UN11)
coef(lmodm) # coefficients

# plot main effects
# using ggplot2
# log(ppgdp) values for prediction for each group
# this limits the line to the range of the observed data (avoiding extrapolation)
predx = unlist(tapply(log(UN11$ppgdp), UN11$group, function(x) seq(min(x), max(x), len = 2)))
# associated group for each x-value
g = rep(c("oecd", "other", "africa"), each = 2)
# predit response for each combination of x, group
predy = predict(lmodm, newdata = data.frame(ppgdp = exp(predx), group = g))
# combine together
fitted_df = data.frame(ppgdp = exp(predx), group = g, lifeExpF = predy)

# plot main effects/ancova model using ggplot2
ggplot(UN11, aes(x = log(ppgdp), y = lifeExpF, col = group)) + geom_point() + geom_line(data = fitted_df, aes(x = log(ppgdp), y = lifeExpF)) + theme_bw()  + theme(legend.position="top")

# plot main effects/ancova model using ggplot2
# ancovaplot using lattice package (essentially)
# doesn't like log(ppgdp), so we need to createa this variable
library(HH)
UN11$lppgdp = log(UN11$ppgdp)
ancovaplot(lifeExpF ~ lppgdp + group, data = UN11)

# using base graphics
# create different colors/symbols for each group
grp_pch = as.numeric(UN11$group)
grp_col = grp_pch + 3
# plot data, using different colors/symbols for each group
plot(lifeExpF ~ log(ppgdp), data = UN11, col = grp_col, pch = grp_pch)
# fitted lines for each group, changing color for each group.  Make sure to match the color number with color number used in plot!
abline(49.53, 3.2, col = 4)
abline(48, 3.2, col = 5)
abline(37.36, 3.2, col = 6)
# create the legend
legend("topleft", legend = c("oecd", "other", "africa"),
       col = 4:6, pch = 1:3, lty = 1)

# effect plot for log(ppgdp) by group
ppgdpEffect = allEffects(mod = lmodm,
                     xlevels = list(ppgdp = seq(1, 106000, len = 1000)))
# effect plot of ppgdp on original scale for main effects model
plot(ppgdpEffect)

## Compare main effects and interaction model
anova(lmodm, lmodi)
